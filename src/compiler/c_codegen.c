#include "c_codegen_internal.h"

/*

Design:

- Faults jump to c->catch as soon as they appear, as in LLVM. An optional function returns its fault and writes the
  value through a leading pointer parameter (the LLVM ABI, c_abi_func_create).
- Sema makes every conversion an expression, so stores (c_emit_store) only copy and assert equal C types. Conversions
  live in their expressions: EXPR_RVALUE (pointer casts), EXPR_ADDR_CONVERSION, and per cast kind
  c_emit_scalar_convert / c_emit_reinterpret / c_emit_vec_recast.
- int128 / uint128 values only go through the runtime's __c3_u128_* / __c3_i128_* helpers (c_codegen_int128.c), so
  compilers without a 128-bit integer use a struct. -D__C3_NO_INT128 forces the struct on any compiler.
- The generated C is the same whatever C compiler builds it: compiler differences go in c_runtime.h, never in codegen
  keyed on --cc.
- Supported compilers are gcc, clang and their derivatives (console SDKs, most embedded toolchains) and cl.exe; tcc
  is best effort. Others get the GNU branch of c_runtime.h and GCC flags, plus C11 fallbacks where they cost little
  (atomics). There is no plain C99 branch: thread locals are __thread, over-aligned structs use
  __attribute__((aligned)), and __builtin_expect is used as is. A compiler without atomics builds no program, as
  compiler_rt's __atomic_load etc. (kept for LLVM's libcalls) use them. C3 has no type-based aliasing (LLVM gets no
  TBAA) and the C accesses memory through pointers of any type, so the C compiler must not apply C's strict aliasing
  rules: gcc and clang get -fno-strict-aliasing, and cl.exe doesn't apply them (an int is reloaded after a store
  through a float* at /O2).
- With debug info, #line maps the C to the C3 source, for debuggers and backtraces (c_emit_line). Each C line of a
  statement repeats its #line, as C numbers the lines after one onwards, and after a function the C file's own
  numbering returns (c_write_mapped_lines).
- C names are C3's, so debuggers show them. Codegen writes every variable as ___var_N and c_write_named_line gives
  the ones with a C3 name name_: a plain name could hide a C function the same function calls. A repeated name gets
  name__2, and a name the file also declares stays ___var_N. A struct member keeps its name, with a trailing `_` for a C keyword, header macro
  or *_t name (c_name_is_reserved). Temporaries stay ___var_N, anonymous members are __mN.
- Under --test the generated C is checked with the C compiler's -fsyntax-only (/Zs for cl, each file compiled for
  tcc), as LLVM verifies its IR. A file list longer than cmd.exe takes goes in a response file.
- The C is for --target: C3 picks the stdlib's platform code and every layout for it. Any --cc builds it, a cross
  compiler for another target; __c3_runtime.h stops the build when the C compiler's architecture or OS differs, and
  the C compiler also links. Explicit CPU options (--x86cpu, --cpu-flags on x86, --riscv-cpu, --riscv-abi) become
  -march / -mabi, or /arch for cl.exe; without them the C compiler keeps its own default target.
- The C compiler checks C3's layout at compile time (__C3_ASSERT_LAYOUT): the basic types in __c3_runtime.h, and every
  struct whose layout comes from attributes (@packed, @align, flexible arrays) in its file.
- A packed struct is laid out under #pragma pack(1), with C3's gaps as padding fields and designated initializers.
  Members whose C type has an alignment attribute (int128, @simd, @align structs) are bytes, as #pragma pack can't
  lower that alignment on Windows. Their constants are written as bytes; one holding an address is built with stores
  at runtime (c_emit_const_stores). Loads and stores through an address less aligned than its C type (CValue.align)
  go through memcpy, as LLVM loads with a lower alignment.
- An extern C function is declared with C3's types, which differ from the C library's (`uint8_t *` for `char *`), so
  C calls it through a name of its own, __C3_EXT(sym), bound to the symbol by an asm label (c_runtime.h); cl.exe and
  wasm use the symbol itself.
- Integer + - * << and negation wrap as in C3 without -fwrapv: signed types, and types C promotes to a signed int,
  go through an unsigned type (c_int_wrap_type).
- @init, @finalizer and runtime global initializers are constructors (__C3_INIT / __C3_INIT_ENTRY): GNU
  constructor attributes, also on every prototype as gcc drops the priority otherwise, or MSVC's .CRT$XC* table with
  LLVM's section names for Windows, where a finalizer registers itself with atexit.
- Every C3 link unit emits the runtime and std definitions, so they merge: weak under GNU, and under cl
  __declspec(selectany) data and __inline functions, which cl emits as pick-any COMDATs, as LLVM's weak_odr on Windows.
- Function attributes (@inline, @noinline, @noreturn, @naked, @section) are c_runtime.h macros. @inline is
  clang's always_inline and cl.exe's __forceinline only on the definition; gcc fails to compile an always_inline
  call it can't inline, which LLVM's per-call always-inline never does. A naked function holds only its asm.
- Atomics are the GNU __atomic builtins, always seq_cst. c_runtime.h maps them to tcc's <stdatomic.h>, MSVC
  intrinsics or C11 <stdatomic.h>.

Known issues:

1. float128 lowers to double: 8 bytes in C but 16 to c3c, so the layout of a struct with one differs from c3c's,
   and it has double's precision.

2. cl.exe: inline assembly is emitted as GNU extended asm, which cl.exe rejects (#error). A compiler with neither
   GNU constructors nor MSVC's .CRT$XC* table never runs @finalizer, and runs @init and runtime global initializers
   only from an entry point (__c3_init_runtime), so a C3 static library linked into a C program gets none.

3. tcc miscompiles structs aligned to 32 or more passed by value (the callee reads zeros); not worked around, as the
   C must not depend on the compiler.
*/

static HTable c_decl_name_cache;
static HTable c_positional_name_owner;

const char *c_intern(const char *str)
{
	if (!str)
	{
		return "_anon";
	}
	uint32_t len   = (uint32_t)strlen(str);
	TokenType type = TOKEN_INVALID_TOKEN;
	return symtab_add(str, len, fnv1a(str, len), &type);
}

static inline char c_sanitize_char(char c)
{
	if (c == '*') return 'p';
	if (char_is_alphanum_(c)) return c;
	return '_';
}

const char *c_sanitize_name(const char *name)
{
	if (!name) return "_anon";
	const char *p = name;
	while (*p && c_sanitize_char(*p) == *p) p++;
	if (!*p) return c_intern(name);

	scratch_buffer_push();
	scratch_buffer_clear();
	for (p = name; *p; p++)
	{
		if (*p == '.')
		{
			scratch_buffer_append("__");
		}
		else
		{
			scratch_buffer_append_char(c_sanitize_char(*p));
		}
	}
	const char *interned = scratch_buffer_interned();
	scratch_buffer_pop();
	return interned;
}

static inline bool c_is_builtin_symbol_collision(const char *name)
{
	return str_start_with(name, "__atomic_") || str_start_with(name, "__builtin_");
}

static const char *c_check_collision(const char *name)
{
	if (name && c_is_builtin_symbol_collision(name))
	{
		return c_intern(str_printf("__c3_ext_%s", name));
	}
	return name;
}

INLINE bool c_extname_is_asm(const char *extname)
{
	return extname && extname[0] == '\x01';
}

static const char *c_extname_identifier(const char *extname)
{
	if (c_extname_is_asm(extname))
	{
		return c_intern(str_printf("__c3_asm_%s", c_sanitize_name(extname + 1)));
	}
	return c_sanitize_name(extname);
}

const char *c_get_decl_asm_name(Decl *decl)
{
	if (!decl)
	{
		return NULL;
	}
	decl = c_decl_unwrap(decl);
	if (decl->has_extname && decl->extname)
	{
		return c_extname_is_asm(decl->extname) ? decl->extname + 1 : decl->extname;
	}
	if (decl->is_extern && decl->name)
	{
		return decl->name;
	}
	return NULL;
}

// Whether `decl` is an extern C function, named __C3_EXT(sym) in C (c_runtime.h).
static bool c_decl_is_ext_function(Decl *decl)
{
	return decl->is_extern && decl->decl_kind == DECL_FUNC && !(decl->has_extname && c_extname_is_asm(decl->extname))
	       && c_get_decl_asm_name(decl);
}

// Export / import attributes LLVM sets on the symbol: wasm-import-name / wasm-import-module on extern functions and
// wasm-export-name on exported functions defined here (llvm_emit_function_decl), DLL export of exported symbols on
// Windows without a .def file (llvm_set_decl_linkage). Empty when none apply.
const char *c_decl_export_attrs(Decl *decl, bool is_definition)
{
	const char *attrs = "";
	bool is_wasm_fn   = arch_is_wasm(compiler.platform.arch) && decl->decl_kind == DECL_FUNC;
	if (is_wasm_fn && (decl->is_extern || (decl->is_export && is_definition)))
	{
		scratch_buffer_push();
		scratch_buffer_set_extern_decl_name(decl, true);
		const char *name = scratch_buffer_copy();
		scratch_buffer_pop();
		const char *module = decl->attrs_resolved ? decl->attrs_resolved->wasm_module : NULL;
		if (!decl->is_extern)
		{
			attrs = str_printf("__attribute__((export_name(\"%s\"))) ", name);
		}
		else if (module)
		{
			attrs = str_printf("__attribute__((import_name(\"%s\"), import_module(\"%s\"))) ", name, module);
		}
		else
		{
			attrs = str_printf("__attribute__((import_name(\"%s\"))) ", name);
		}
	}
	if (compiler.platform.os == OS_TYPE_WIN32 && decl->is_export && !decl->is_extern && !compiler.build.win.def
	    && decl->name != kw_main && decl->name != kw_mainstub
	    && !(decl->decl_kind == DECL_VAR && decl->var.is_static))
	{
		attrs = str_printf("%s__declspec(dllexport) ", attrs);
	}
	return attrs;
}

// The __C3_EXT_SYM of an extern C function, with the target's symbol prefix, or NULL.
const char *c_decl_ext_symbol(Decl *decl)
{
	decl = c_decl_unwrap(decl);
	if (!c_decl_is_ext_function(decl)) return NULL;
	const char *prefix = compiler.platform.object_format == OBJ_FORMAT_MACHO ? "_" : "";
	return str_printf("__C3_EXT_SYM(\"%s%s\")", prefix, c_get_decl_asm_name(decl));
}

bool c_decl_needs_asm_label(Decl *decl)
{
	const char *asm_name = c_get_decl_asm_name(decl);
	if (!asm_name) return false;
	decl = c_decl_unwrap(decl);
	if (c_decl_is_ext_function(decl)) return false;
	return (decl->has_extname && c_extname_is_asm(decl->extname)) || c_is_builtin_symbol_collision(asm_name);
}

// Macro copies share a source position; suffix later ones with _2, _3, ...
static const char *c_unique_positional_name(const char *name, Decl *decl)
{
	const char *candidate = name;
	for (unsigned i = 2; htable_get(&c_positional_name_owner, (void *)candidate); i++)
	{
		candidate = c_intern(str_printf("%s_%u", name, i));
	}
	htable_set(&c_positional_name_owner, (void *)candidate, decl);
	return candidate;
}

static const char *c_compute_decl_name(Decl *decl)
{
	if (c_decl_is_ext_function(decl))
	{
		return c_intern(str_printf("__C3_EXT(%s)", c_sanitize_name(c_get_decl_asm_name(decl))));
	}
	if (decl->is_extern)
	{
		const char *name = NULL;
		if (decl->has_extname && decl->extname)
		{
			name = c_extname_identifier(decl->extname);
		}
		else if (decl->name)
		{
			name = c_sanitize_name(decl->name);
		}
		if (name)
		{
			return c_check_collision(name);
		}
	}

	if (!decl->name || decl->name[0] == '$')
	{
		scratch_buffer_push();
		scratch_buffer_clear();
		scratch_buffer_append("__c3_anon");
		scratch_buffer_append_decl_pos(decl);
		const char *anon = scratch_buffer_copy();
		scratch_buffer_pop();
		return c_unique_positional_name(c_sanitize_name(anon), decl);
	}

	if (decl->has_extname && decl->extname)
	{
		return c_check_collision(c_extname_identifier(decl->extname));
	}

	if (!decl->unit || !decl->unit->module)
	{
		return c_check_collision(c_sanitize_name(decl->name));
	}

	scratch_buffer_push();
	bool positional = scratch_buffer_set_stable_extern_decl_name(decl, true);
	char *str = scratch_buffer_copy();
	scratch_buffer_pop();
	const char *name = c_check_collision(c_sanitize_name(str));
	return positional ? c_unique_positional_name(name, decl) : name;
}

const char *c_get_decl_name(Decl *decl)
{
	if (!decl) return "unknown";
	decl = c_decl_unwrap(decl);
	const char *cached = htable_get(&c_decl_name_cache, decl);
	if (cached) return cached;
	const char *name = c_compute_decl_name(decl);
	htable_set(&c_decl_name_cache, decl, (void *)name);
	return name;
}

const char *c_get_enum_assoc_name(Decl *enum_decl, Decl *param)
{
	enum_decl = c_decl_unwrap(enum_decl);
	param     = c_decl_unwrap(param);
	return c_intern(str_printf("%s__assoc_%s", c_get_decl_name(enum_decl), (param && param->name) ? param->name : "_val"));
}

void c_write_bytes(GenContext *c, const uint8_t *bytes, size_t len)
{
	if (!bytes || len == 0)
	{
		return;
	}
	if (c->buffer)
	{
		byte_buffer_write(c->buffer, bytes, len);
		return;
	}
	fwrite(bytes, 1, len, c->file);
}

void c_emit_line(GenContext *c, SourceLocId loc)
{
	SourceLoc *location = sourcelocptrzero(loc);
	if (compiler.build.debug_info == DEBUG_INFO_NONE || !location || !location->row) return;
	File *file = source_file_by_id(location->file_id);
	if (!file) return;
	PRINTF("#line %u \"", location->row);
	for (const char *p = file->full_path; *p; p++)
	{
		if (*p == '\\' || *p == '"') PRINT("\\");
		c_write_bytes(c, (const uint8_t *)p, 1);
	}
	PRINT("\"\n");
}

// Writes a C line, renaming each ___var_N in GenContext.var_names. A name the file also declares, like an extern
// `dgemm_`, stays ___var_N, as it would hide the declaration.
static void c_write_named_line(FILE *f, const char *start, size_t len, GenContext *c)
{
	static const char prefix[] = "___var_";
	const size_t plen          = sizeof(prefix) - 1;
	const char *end            = start + len;
	const char *p              = start;
	while (p < end)
	{
		const char *hit = p;
		while ((hit = memchr(hit, '_', end - hit)) != NULL && ((size_t)(end - hit) < plen || memcmp(hit, prefix, plen) != 0)) hit++;
		if (!hit) break;
		const char *digits = hit + plen;
		const char *after  = digits;
		int id = 0;
		while (after < end && char_is_digit(*after)) id = id * 10 + (*after++ - '0');
		bool starts_word = hit == start || !char_is_alphanum_(hit[-1]);
		const char *name = starts_word && after > digits ? htable_get(&c->var_names, c_id_to_ptr(id)) : NULL;
		if (name && (htable_get(&c->decl_names, (void *)name) || htable_get(&c->emitted_global_decls, (void *)name))) name = NULL;
		if (!name)
		{
			fwrite(p, 1, (size_t)(after - p), f);
			p = after > hit ? after : hit + 1;
			continue;
		}
		fwrite(p, 1, (size_t)(hit - p), f);
		fputs(name, f);
		p = after;
	}
	fwrite(p, 1, (size_t)(end - p), f);
}

// Writes `len` bytes of C with c_emit_line markers to `f`, where `line` C lines are already written. A marker names
// its file only when that changes, every C line after it repeats its line number, as a C compiler numbers the lines
// after a #line onwards, and C_LINE_RESET becomes the real line of the C file.
static void c_write_mapped_lines(FILE *f, GenContext *c, const char *bytes, size_t len, uint32_t *line)
{
	const char *c_filename = c->c_filename;
	const char *row  = NULL; // "#line N" of the C3 line being emitted, or NULL in the C file's own lines.
	size_t row_len   = 0;
	const char *file = NULL; // The quoted file the last #line named.
	size_t file_len  = 0;
	bool repeat      = false;
	for (size_t i = 0; i < len;)
	{
		const char *start = bytes + i;
		const char *nl    = memchr(start, '\n', len - i);
		size_t line_len   = nl ? (size_t)(nl - start) + 1 : len - i;
		i += line_len;
		if (line_len < 6 || memcmp(start, "#line ", 6) != 0)
		{
			if (row && repeat)
			{
				fwrite(row, 1, row_len, f);
				fputc('\n', f);
				(*line)++;
			}
			repeat = true;
			c_write_named_line(f, start, line_len, c);
			(*line)++;
			continue;
		}
		if (line_len == strlen(C_LINE_RESET) && memcmp(start, C_LINE_RESET, line_len) == 0)
		{
			if (!row) continue;
			row  = NULL;
			file = NULL;
			fprintf(f, "#line %u \"", *line + 2);
			for (const char *p = c_filename; *p; p++)
			{
				if (*p == '\\' || *p == '"') fputc('\\', f);
				fputc(*p, f);
			}
			fputs("\"\n", f);
			(*line)++;
			continue;
		}
		// `#line N "file"\n` from c_emit_line.
		const char *quote = memchr(start + 6, '"', line_len - 6);
		size_t new_row_len = (size_t)(quote - 1 - start);
		size_t new_file_len = line_len - 1 - (size_t)(quote - start);
		bool same_file = file && new_file_len == file_len && memcmp(quote, file, file_len) == 0;
		if (same_file && row && new_row_len == row_len && memcmp(start, row, row_len) == 0) continue;
		row     = start;
		row_len = new_row_len;
		repeat  = false;
		fwrite(start, 1, same_file ? row_len : line_len - 1, f);
		fputc('\n', f);
		(*line)++;
		file     = quote;
		file_len = new_file_len;
	}
}

void c_print(GenContext *c, const char *str)
{
	if (!str)
	{
		return;
	}
	c_write_bytes(c, (const uint8_t *)str, strlen(str));
}

void c_printf(GenContext *c, const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	if (c->buffer)
	{
		char buf[1024];
		va_list copy;
		va_copy(copy, args);
		int n = vsnprintf(buf, sizeof(buf), fmt, copy);
		va_end(copy);
		if (n <= 0)
		{
			va_end(args);
			return;
		}
		if ((size_t)n < sizeof(buf))
		{
			byte_buffer_write(c->buffer, (const uint8_t *)buf, (size_t)n);
		}
		else
		{
			char *heap = cmalloc((size_t)n + 1);
			vsnprintf(heap, (size_t)n + 1, fmt, args);
			byte_buffer_write(c->buffer, (const uint8_t *)heap, (size_t)n);
			free(heap);
		}
	}
	else
	{
		vfprintf(c->file, fmt, args);
	}
	va_end(args);
}

void c_emit_string_literal(GenContext *c, const char *bytes, ArrayIndex len)
{
	PRINT("\"");
	for (ArrayIndex i = 0; i < len; i++)
	{
		unsigned char b = (unsigned char)bytes[i];
		switch (b)
		{
			case '\n': PRINT("\\n"); break;
			case '\r': PRINT("\\r"); break;
			case '\t': PRINT("\\t"); break;
			case '\\': PRINT("\\\\"); break;
			case '\"': PRINT("\\\""); break;
			case '?': PRINT("\\?"); break;
			default:
				if (b >= 32 && b < 127)
				{
					c_write_bytes(c, &b, 1);
				}
				else
				{
					PRINTF("\\%03o", (unsigned int)b);
				}
				break;
		}
	}
	PRINT("\"");
}

static void c_emit_string_slice(GenContext *c, const char *str)
{
	PRINTF("(%s){(uint8_t*)", c_type_name(c, type_get_slice(type_char)));
	c_emit_string_literal(c, str, (ArrayIndex)strlen(str));
	PRINTF(", %zu}", strlen(str));
}

// Call the panic function then abort. `args` is a C expression for an `any*` holding `arg_count` format arguments.
static void c_emit_panic_call(GenContext *c, const char *summary, SourceLocId loc, const char *format, const char *args, int arg_count)
{
	SourceLoc *location  = sourcelocptrzero(loc);
	bool use_location    = panic_use_location();
	File *file           = source_file_by_id(location ? location->file_id : 0);
	const char *funcname = c->cur_func_name ? c->cur_func_name : (c->cur_func_decl ? c->cur_func_decl->name : "");
	const char *panic_name = c_get_decl_name(compiler.context.panic_var);
	c_emit_global_decl(c, compiler.context.panic_var);
	const char *str_slice  = c_type_name(c, type_get_slice(type_char));
	const char *any_slice  = c_type_name(c, type_get_slice(type_any));
	PRINTF("if (%s) { ((void (*)(%s, %s, %s, int32_t, %s, %s))%s)(", panic_name, str_slice, str_slice, str_slice, str_slice, any_slice, panic_name);
	c_emit_string_slice(c, summary);
	PRINT(", ");
	c_emit_string_slice(c, use_location && file ? file->name : "");
	PRINT(", ");
	c_emit_string_slice(c, use_location ? funcname : "");
	PRINTF(", %u, ", use_location && location ? location->row : 0);
	if (format)
	{
		c_emit_string_slice(c, format);
		PRINTF(", (%s){ .ptr = %s, .len = %d }); } __c3_abort();\n", any_slice, args, arg_count);
		return;
	}
	PRINTF("(%s){0}, (%s){0}); } __c3_abort();\n", str_slice, any_slice);
}

// Compiler-rt traps instead of calling the panic function.
static bool c_panic_enabled(GenContext *c)
{
	if (no_panic() || !compiler.context.panic_var) return false;
	return !c->current_module || !c->current_module->is_compiler_rt;
}

// Panic with a user message, optionally formatted with `args`.
void c_emit_panic(GenContext *c, const char *message, SourceLocId loc, Expr **args)
{
	if (!c_panic_enabled(c))
	{
		PRINT("__c3_abort();\n");
		return;
	}
	int num_args = vec_size(args);
	if (!num_args)
	{
		c_emit_panic_call(c, message ? message : "", loc, NULL, NULL, 0);
		return;
	}
	VariableId arr_var = c_emit_alloca_array(c, type_any, num_args);
	for (int i = 0; i < num_args; i++)
	{
		CValue arg_val = {0};
		c_emit_expr(c, &arg_val, args[i]);
		c_value_rvalue(c, &arg_val);
		// The typeid comes from the type as written, not the lowered one, so that e.g. a String prints as text.
		if (arg_val.kind != CV_ADDRESS) c_value_addr(c, &arg_val);
		PRINTF("___var_%d[%d] = (__c3_any__){ .ptr = (void*)&___var_%d, .typeid = (c3typeid_t)&%s };\n",
		       arr_var, i, arg_val.var, c_typeid_ref(c, type_no_optional(args[i]->type)));
	}
	c_emit_panic_call(c, panic_fn_desc[PANIC_FN_ASSERT_VIOLATION].format, loc, message, str_printf("___var_%d", arr_var), num_args);
}

// Emit a compiler generated panic. `texts` and `values` hold the counts given by `panic_fn_desc[id]`.
void c_emit_panic_fn(GenContext *c, PanicFunction id, SourceLocId loc, const char **texts, CPanicValue *values)
{
	if (!c_panic_enabled(c))
	{
		PRINT("__c3_abort();\n");
		return;
	}
	const PanicFunctionDescriptor *info = &panic_fn_desc[id];
	int text_count  = panic_fn_text_count(id);
	int value_count = panic_fn_value_count(id);
	if (id == PANIC_FN_MESSAGE)
	{
		c_emit_panic_call(c, texts[0], loc, NULL, NULL, 0);
		return;
	}
	if (text_count + value_count == 0)
	{
		c_emit_panic_call(c, info->summary ? info->summary : info->format, loc, NULL, NULL, 0);
		return;
	}
	// The format takes the first value, then the texts, then the remaining values.
	PRINT("{\n");
	const char *typeids[PANIC_FN_MAX_TEXTS + PANIC_FN_MAX_VALUES];
	int count = 0;
	for (int i = 0; i < value_count; i++)
	{
		if (i == 1)
		{
			for (int j = 0; j < text_count; j++)
			{
				PRINTF("%s __c3_panic_arg%d = ", c_type_name(c, type_get_slice(type_char)), count);
				c_emit_string_slice(c, texts[j]);
				PRINT(";\n");
				typeids[count++] = c_typeid_ref(c, type_string);
			}
		}
		// Integers are widened to 64 bits, as the LLVM backend does.
		Type *type = c_safe_type_lower(values[i].type);
		if (type_is_integer(type) && type_bit_size(type) <= 64) type = type_is_unsigned(type) ? type_ulong : type_long;
		PRINTF("%s __c3_panic_arg%d = %s;\n", c_type_name(c, type), count, c_scalar_convert_str(c, type, values[i].type, str_printf("(%s)", values[i].expr)));
		typeids[count++] = c_typeid_ref(c, type);
	}
	if (value_count <= 1)
	{
		for (int j = 0; j < text_count; j++)
		{
			PRINTF("%s __c3_panic_arg%d = ", c_type_name(c, type_get_slice(type_char)), count);
			c_emit_string_slice(c, texts[j]);
			PRINT(";\n");
			typeids[count++] = c_typeid_ref(c, type_string);
		}
	}
	PRINTF("__c3_any__ __c3_panic_args[%d] = {", count);
	for (int i = 0; i < count; i++)
	{
		PRINTF("%s{ .ptr = (void*)&__c3_panic_arg%d, .typeid = (c3typeid_t)&%s }", i ? ", " : " ", i, typeids[i]);
	}
	PRINT(" };\n");
	c_emit_panic_call(c, info->summary, loc, info->format, "__c3_panic_args", count);
	PRINT("}\n");
}

// Emit a panic taken when the C expression `cond` is true.
void c_emit_panic_if(GenContext *c, const char *cond, PanicFunction id, SourceLocId loc, const char *text, CPanicValue *value_1, CPanicValue *value_2)
{
	CPanicValue values[2];
	if (value_1) values[0] = *value_1;
	if (value_2) values[1] = *value_2;
	PRINTF("if (__builtin_expect(!!(%s), 0)) {\n", cond);
	c_emit_panic_fn(c, id, loc, &text, values);
	PRINT("}\n");
}

static void c_traverse_decl(GenContext *c, Decl *d, CDeclVisitor visitor, void *userdata)
{
	if (!d)
	{
		return;
	}
	if (d->decl_kind == DECL_GROUP)
	{
		FOREACH(Decl *, gd, d->decl_list)
		{
			c_traverse_decl(c, gd, visitor, userdata);
		}
		return;
	}
	if (d->decl_kind == DECL_GENERIC)
	{
		FOREACH(Decl *, inst, d->generic_decl.instances)
		{
			c_traverse_decl(c, inst, visitor, userdata);
		}
		return;
	}
	if (d->decl_kind == DECL_GENERIC_INSTANCE)
	{
		FOREACH(Decl *, gd, d->instance_decl.generated_decls)
		{
			c_traverse_decl(c, gd, visitor, userdata);
		}
		return;
	}
	d = c_decl_unwrap(d);
	if (!d || d->decl_kind == DECL_POISONED || d->decl_kind == DECL_ERASED || d->replacement)
	{
		return;
	}
	if (d->decl_kind == DECL_MACRO || d->decl_kind == DECL_IMPORT || d->decl_kind == DECL_ATTRIBUTE || d->decl_kind == DECL_CONTRACT)
	{
		return;
	}
	if (d->is_template)
	{
		return;
	}
	// A method of an interface that is itself unused can still be called through an interface inheriting from it.
	bool is_live = (!strip_unused() || d->is_live) &&
	               !(d->decl_kind == DECL_FUNC && ((d->func_decl.attr_test && !compiler.build.build_test) ||
	                                               (d->func_decl.attr_benchmark && !compiler.build.build_benchmark)));
	if (!is_live && !(c->traverse_unused_interfaces && d->decl_kind == DECL_INTERFACE))
	{
		return;
	}
	visitor(c, d, userdata);

	if (decl_has_members(d) && d->strukt.members)
	{
		FOREACH(Decl *, m, d->strukt.members)
		{
			if (m && m->decl_kind != DECL_VAR)
			{
				c_traverse_decl(c, m, visitor, userdata);
			}
		}
	}
	if (d->decl_kind == DECL_INTERFACE && d->interface_methods)
	{
		FOREACH(Decl *, im, d->interface_methods)
		{
			c_traverse_decl(c, im, visitor, userdata);
		}
	}
	if (decl_has_interface(d) && d->method_table && d->method_table->methods)
	{
		FOREACH(Decl *, m, d->method_table->methods)
		{
			c_traverse_decl(c, m, visitor, userdata);
		}
	}
}

static void c_traverse_unit(GenContext *c, CompilationUnit *unit, CDeclVisitor visitor, void *userdata)
{
	if (!unit)
	{
		return;
	}
	FOREACH(Decl *, t, unit->types)
	{
		c_traverse_decl(c, t, visitor, userdata);
	}
	FOREACH(Decl *, v, unit->vars)
	{
		c_traverse_decl(c, v, visitor, userdata);
	}
	FOREACH(Decl *, f, unit->functions)
	{
		c_traverse_decl(c, f, visitor, userdata);
	}
	FOREACH(Decl *, m, unit->methods)
	{
		c_traverse_decl(c, m, visitor, userdata);
	}
	FOREACH(Decl *, l, unit->lambdas)
	{
		c_traverse_decl(c, l, visitor, userdata);
	}
	FOREACH(Decl *, e, unit->enums)
	{
		c_traverse_decl(c, e, visitor, userdata);
	}
	FOREACH(Decl *, g, unit->generic_decls)
	{
		c_traverse_decl(c, g, visitor, userdata);
	}
	if (unit->main_function)
	{
		c_traverse_decl(c, unit->main_function, visitor, userdata);
	}
}

void c_traverse_all_modules(GenContext *c, CDeclVisitor visitor, void *userdata)
{
	FOREACH(Module *, mod, compiler.context.module_list)
	{
		if (!mod)
		{
			continue;
		}
		FOREACH(CompilationUnit *, unit, mod->units)
		{
			c_traverse_unit(c, unit, visitor, userdata);
		}
	}
	FOREACH(Decl *, ext, compiler.context.method_extension_list)
	{
		c_traverse_decl(c, ext, visitor, userdata);
	}
	FOREACH(Decl *, ug, compiler.context.unregistered_generic_decls)
	{
		c_traverse_decl(c, ug, visitor, userdata);
	}
	FOREACH(Decl *, ums, compiler.context.unregistered_method_specializations)
	{
		c_traverse_decl(c, ums, visitor, userdata);
	}
}

static bool c_module_has_live_code(Module *module)
{
	if (!strip_unused())
	{
		return true;
	}
	FOREACH(CompilationUnit *, unit, module->units)
	{
		if (!unit)
		{
			continue;
		}
		if (unit->main_function && unit->main_function->is_synthetic)
		{
			return true;
		}
		FOREACH(Decl *, d, unit->functions)
		{
			if (d && !d->is_extern && !d->replacement && d->func_decl.body && d->is_live)
			{
				return true;
			}
		}
		FOREACH(Decl *, d, unit->methods)
		{
			if (d && !d->is_extern && !d->replacement && d->func_decl.body && d->is_live)
			{
				return true;
			}
		}
		FOREACH(Decl *, d, unit->lambdas)
		{
			if (d && !d->is_extern && !d->replacement && d->func_decl.body && d->is_live)
			{
				return true;
			}
		}
		FOREACH(Decl *, d, unit->vars)
		{
			if (d && !d->is_extern && !d->replacement && c_is_file_global(d) && d->is_live)
			{
				return true;
			}
		}
		FOREACH(Decl *, d, unit->enums)
		{
			if (d && !d->is_extern && !d->replacement && d->enums.parameters && d->enums.values && (!strip_unused() || d->is_live))
			{
				return true;
			}
		}
	}
	return false;
}

// Libraries named by @link on used declarations.
static void visit_links(GenContext *c, Decl *d, void *ud)
{
	(void)c;
	(void)ud;
	decl_append_links_to_global_during_codegen(d);
}

static void visit_global_def(GenContext *c, Decl *d, void *ud)
{
	(void)ud;
	bool is_current = (!c->current_module || (d->unit && d->unit->module == c->current_module));
	if (!is_current)
	{
		return;
	}
	if (d->decl_kind == DECL_VAR && c_is_file_global(d) && !d->is_extern)
	{
		c_emit_global_def(c, d);
		return;
	}
	if (d->decl_kind == DECL_ENUM && d->enums.parameters && d->enums.values && !d->is_extern)
	{
		int num_values = vec_size(d->enums.values);
		if (num_values == 0)
		{
			return;
		}
		FOREACH(Decl *, p, d->enums.parameters)
		{
			if (!p || !p->name)
			{
				continue;
			}
			const char *arr_name = c_get_enum_assoc_name(d, p);
			if (htable_get(&c->emitted_global_defs, (void *)arr_name))
			{
				continue;
			}
			htable_set(&c->emitted_global_defs, (void *)arr_name, (void *)1);
			ASSERT(p->type);
			Type *ptype = c_safe_type_lower(p->type);
			c_emit_type_forward_decl(c, ptype);
			PRINTF("const %s %s[%d] = {\n", c_type_name(c, ptype), arr_name, num_values);
			for (int i = 0; i < num_values; i++)
			{
				Decl *ev         = d->enums.values[i];
				Expr *assoc_expr = (ev && ev->enum_constant.associated && p->var.index < vec_size(ev->enum_constant.associated))
				                       ? ev->enum_constant.associated[p->var.index]
				                       : NULL;
				PRINT("\t");
				c_emit_const_init_expr(c, assoc_expr, ptype);
				PRINT(",\n");
			}
			PRINT("};\n");
		}
	}
}

static bool c_function_def_is_emitted(Decl *decl, bool only_used)
{
	if (!decl || !decl->func_decl.body || decl->is_extern || decl->replacement) return false;
	if (decl_skip_benchtest_function(decl)) return false;
	return !only_used || decl->is_live;
}

// C allows one prototype per symbol. The file's definitions are declared first, so a symbol defined here gets its
// definition's signature; a call through an extern declaration of it with another signature casts to that one (as
// LLVM calls each declaration's own function type).
static void c_try_declare_function_def(GenContext *c, Decl *decl, bool only_used)
{
	if (c_function_def_is_emitted(decl, only_used)) c_emit_function_decl(c, decl, true);
}

static void c_try_emit_function_def(GenContext *c, Decl *decl, bool only_used)
{
	if (!c_function_def_is_emitted(decl, only_used)) return;
	const char *fname = c_get_decl_name(decl);
	if (htable_get(&c->emitted_func_defs, (void *)fname))
	{
		return;
	}
	htable_set(&c->emitted_func_defs, (void *)fname, (void *)1);
	c_emit_function(c, decl);
}

static Decl **c_collect_and_sort_init_functions(void)
{
	Decl **init_funcs = NULL;
	FOREACH(Module *, mod, compiler.context.module_list)
	{
		if (!mod)
		{
			continue;
		}
		FOREACH(CompilationUnit *, unit, mod->units)
		{
			if (!unit)
			{
				continue;
			}
			FOREACH(Decl *, d, unit->functions)
			{
				if (d && d->decl_kind == DECL_FUNC && d->func_decl.attr_init && !d->is_extern)
				{
					vec_add(init_funcs, d);
				}
			}
		}
	}

	int init_count = vec_size(init_funcs);
	for (int i = 0; i < init_count - 1; i++)
	{
		for (int j = i + 1; j < init_count; j++)
		{
			uint32_t p_i = init_funcs[i]->func_decl.priority ? init_funcs[i]->func_decl.priority : MAX_PRIORITY;
			uint32_t p_j = init_funcs[j]->func_decl.priority ? init_funcs[j]->func_decl.priority : MAX_PRIORITY;
			if (p_j < p_i)
			{
				Decl *tmp     = init_funcs[i];
				init_funcs[i] = init_funcs[j];
				init_funcs[j] = tmp;
			}
		}
	}
	return init_funcs;
}

// The functions a file defines, in the order they are emitted.
static void c_visit_function_defs(GenContext *c, Module **modules, int mod_count, bool only_used,
                                  void (*visit)(GenContext *, Decl *, bool))
{
	for (int m = 0; m < mod_count; m++)
	{
		Module *mod = modules[m];
		if (!mod || !vec_size(mod->units) || (compiler.build.emit_stdlib == EMIT_STDLIB_OFF && module_is_stdlib(mod)))
		{
			continue;
		}
		FOREACH(CompilationUnit *, unit, mod->units)
		{
			FOREACH(Decl *, decl, unit->functions) visit(c, decl, only_used);
			FOREACH(Decl *, func, unit->lambdas) visit(c, func, only_used);
			if (unit->main_function && unit->main_function->is_synthetic) visit(c, unit->main_function, only_used);
			FOREACH(Decl *, decl, unit->methods) visit(c, decl, only_used);
		}
	}
}

// Globals of `modules` whose initializer isn't a C constant: they are assigned at startup, before @init.
static Decl **c_collect_runtime_inits(Module **modules, int mod_count, bool only_used)
{
	Decl **runtime_inits = NULL;
	for (int m = 0; m < mod_count; m++)
	{
		Module *mod = modules[m];
		if (!mod || !vec_size(mod->units) || (compiler.build.emit_stdlib == EMIT_STDLIB_OFF && module_is_stdlib(mod))) continue;
		FOREACH(CompilationUnit *, unit, mod->units)
		{
			FOREACH(Decl *, var, unit->vars)
			{
				if (!var || var->replacement || var->is_template) continue;
				var = decl_raw(var);
				if (!var || var->decl_kind != DECL_VAR || !c_is_file_global(var) || var->is_extern) continue;
				if (only_used && !var->is_live) continue;
				if (var->var.init_expr && !c_can_emit_static_initializer(var->var.init_expr)) vec_add(runtime_inits, var);
			}
		}
	}
	return runtime_inits;
}

// The startup initializer of a module's globals, or NULL when it has none.
static const char *c_module_init_globals_name(Module *module)
{
	if (!c_module_has_live_code(module) || !c_collect_runtime_inits(&module, 1, strip_unused())) return NULL;
	return str_printf("__c3_init_globals_%s", c_sanitize_name(module->name->module));
}

static GenContext *c_gen_module(Module *module, int num)
{
	(void)num;
	if (module)
	{
		if (!vec_size(module->units))
		{
			return NULL;
		}
		if (compiler.build.emit_stdlib == EMIT_STDLIB_OFF && module_is_stdlib(module))
		{
			return NULL;
		}
		if (!c_module_has_live_code(module))
		{
			return NULL;
		}
	}

	const char *base_name       = NULL;
	const char *ir_filename     = NULL;
	const char *asm_filename    = NULL;
	const char *object_filename = NULL;
	if (module)
	{
		codegen_setup_object_names(module, &base_name, &ir_filename, &asm_filename, &object_filename);
	}
	else
	{
		base_name       = filename(build_base_name());
		object_filename = file_append_path(compiler.build.object_file_dir ? compiler.build.object_file_dir : ".", str_printf("%s%s", base_name, get_object_extension()));
	}

	const char *c_filename = file_append_path(compiler.build.object_file_dir ? compiler.build.object_file_dir : ".", str_printf("%s.c", base_name));

	FILE *f = fopen(c_filename, "wb");
	if (!f)
	{
		error_exit("Failed to open output C file '%s'.", c_filename);
	}

	GenContext *c = cmalloc(sizeof(GenContext));
	*c            = (GenContext){
	    .file                 = f,
	    .base_name            = base_name,
	    .c_filename           = c_filename,
	    .object_filename      = object_filename,
	    .current_module       = module,
	    .current_block_live   = true,
	    .record_types         = true,
	};
	htable_init(&c->local_vars, 4096);
	htable_init(&c->local_fault_vars, 4096);
	htable_init(&c->declared_vars, 4096);
	htable_init(&c->var_names, 4096);
	htable_init(&c->used_var_names, 4096);
	htable_init(&c->used_static_names, 1024);
	c->name_scope = 1; // 0 is an unused name.
	htable_init(&c->emitted_func_defs, 8192);
	htable_init(&c->emitted_global_defs, 8192);
	htable_init(&c->gen_decl, 4096);
	htable_init(&c->gen_def, 4096);
	htable_init(&c->decl_names, 4096);
	htable_init(&c->declared_typeids, 1024);
	htable_init(&c->declared_faults, 256);
	htable_init(&c->declared_bitstructs, 64);
	htable_init(&c->emitted_global_decls, 4096);
	htable_init(&c->used_type_set, 4096);

	ByteBuffer type_decls, type_defs, decls, dispatchers, code;
	byte_buffer_init(&type_decls, 0);
	byte_buffer_init(&decls, 0);
	byte_buffer_init(&type_defs, 0);
	byte_buffer_init(&dispatchers, 0);
	byte_buffer_init(&code, 0);
	c->type_decl_buffer = &type_decls;
	c->decl_buffer      = &decls;
	c->dispatch_buffer  = &dispatchers;
	c->buffer           = &code;

	bool only_used   = strip_unused();
	Module **modules = module ? &module : compiler.context.module_list;
	int mod_count    = module ? 1 : vec_size(compiler.context.module_list);
	c_visit_function_defs(c, modules, mod_count, only_used, c_try_declare_function_def);

	PRINT("/* GLOBALS */\n");
	if (module)
	{
		FOREACH(CompilationUnit *, unit, module->units)
		{
			c_traverse_unit(c, unit, visit_global_def, NULL);
		}
	}
	else
	{
		c_traverse_all_modules(c, visit_global_def, NULL);
	}
	PRINT("\n/* FUNCTION BODIES */\n");

	Decl **runtime_inits = c_collect_runtime_inits(modules, mod_count, only_used);

	const char *init_globals_name = module ? c_module_init_globals_name(module) : "__c3_init_globals_all";
	const char *init_linkage      = !module ? "static " : (module_should_weaken(module) ? "__c3_weak_fn " : "");
	if (vec_size(runtime_inits) > 0)
	{
		ByteBuffer body_buf;
		byte_buffer_init(&body_buf, 0);
		ByteBuffer *prev_buf = c->buffer;
		c->buffer            = &body_buf;

		c->cur_ret                  = (CRet){0};
		c->catch                    = NO_CATCH;
		c->defer_error_var          = NULL;
		c->current_break_label      = 0;
		c->current_continue_label   = 0;
		c->current_block_live       = true;
		c->function_locals          = NULL;
		memset(c->local_vars.entries, 0, (c->local_vars.mask + 1) * sizeof(HTEntry *));
		memset(c->local_fault_vars.entries, 0, (c->local_fault_vars.mask + 1) * sizeof(HTEntry *));
		memset(c->declared_vars.entries, 0, (c->declared_vars.mask + 1) * sizeof(HTEntry *));

		FOREACH(Decl *, var, runtime_inits)
		{
			PRINT("{\n");
			COptionalAssign assign;
			c_optional_assign_begin(c, &assign, c_decl_fault_ref(c, var), var->var.init_expr);
			CValue val = {0};
			c_emit_expr(c, &val, var->var.init_expr);
			c_value_rvalue(c, &val);
			c_emit_store(c, c_get_decl_name(var), c_decl_type(var), &val);
			c_optional_assign_end(c, &assign);
			PRINT("}\n");
		}

		c->buffer = prev_buf;

		PRINT("__C3_INIT(101)\n");
		PRINTF("%svoid %s(void) {\n", init_linkage, init_globals_name);
		PRINT("\tstatic bool initialized = false;\n");
		PRINT("\tif (initialized) return;\n");
		PRINT("\tinitialized = true;\n");

		c_emit_local_var_declarations(c);

		if (body_buf.write_idx > 0)
		{
			c_write_bytes(c, body_buf.bytes.ptr, body_buf.write_idx);
		}
		byte_buffer_free(&body_buf);
		// Before every @init, which starts at .CRT$XCA00001.
		PRINTF("}\n__C3_INIT_ENTRY(%s, \".CRT$XCA00000\")\n\n", init_globals_name);
	}

	Decl *main = compiler.context.main;
	if (!module || (main && main->unit->module == module))
	{
		// Only called by the entry point of this file. Each link unit has its own.
		PRINT("static void __c3_init_runtime(void) {\n");
		PRINT("\tstatic bool initialized = false;\n");
		PRINT("\tif (initialized) return;\n");
		PRINT("\tinitialized = true;\n");
		if (!module)
		{
			if (runtime_inits) PRINTF("\t%s();\n", init_globals_name);
		}
		else
		{
			FOREACH(Module *, m, compiler.context.module_list)
			{
				const char *name = c_module_init_globals_name(m);
				if (name) PRINTF("\textern void %s(void);\n\t%s();\n", name, name);
			}
		}

		Decl **init_funcs = c_collect_and_sort_init_functions();
		int init_count    = vec_size(init_funcs);
		for (int i = 0; i < init_count; i++)
		{
			c_emit_function_decl(c, init_funcs[i], false);
			PRINTF("\t%s();\n", c_get_decl_name(init_funcs[i]));
		}
		PRINT("}\n\n");
	}

	c_visit_function_defs(c, modules, mod_count, only_used, c_try_emit_function_def);

	// The file: type declarations, the types its code names in full, other declarations, dispatchers, code.
	c->record_types = false;
	c->buffer       = &type_defs;
	FOREACH(Type *, t, c->used_types)
	{
		c_emit_type_forward_decl(c, t);
		c_emit_type_decl(c, t);
	}
	c->buffer           = NULL;
	c->type_decl_buffer = NULL;
	c->decl_buffer      = NULL;
	c->dispatch_buffer  = NULL;
	fputs("/* Generated by C3 Compiler */\n#include \"__c3_runtime.h\"\n\n", f);
	ByteBuffer *parts[5] = { &type_decls, &type_defs, &decls, &dispatchers, &code };
	uint32_t line         = 3;
	for (int i = 0; i < 5; i++)
	{
		c_write_mapped_lines(f, c, (const char *)parts[i]->bytes.ptr, parts[i]->write_idx, &line);
		byte_buffer_free(parts[i]);
	}
	c->c_file_size = (size_t)ftell(f);
	fclose(f);
	return c;
}

static const char *c_backend_cc     = NULL;
static const char *c_backend_cflags = NULL;


static const char *c_backend_build_cflags(const char *cc)
{
	bool is_cl    = cc_is_msvc(cc);
	bool is_tcc   = cc_is_tcc(cc);
	bool is_clang = strstr(cc, "clang") != NULL || strstr(cc, "zig") != NULL;

	scratch_buffer_clear();
	bool has_warn_flags = compiler.build.cflags && (strstr(compiler.build.cflags, "-W") || strstr(compiler.build.cflags, "/W"));
	if (is_cl)
	{
		scratch_buffer_append(has_warn_flags ? "/nologo " : "/nologo /w ");
		if (!compiler.build.cflags || !strstr(compiler.build.cflags, "/std:"))
		{
			scratch_buffer_append("/std:c11 ");
		}
	}
	else if (is_tcc)
	{
		if (!has_warn_flags)
		{
			scratch_buffer_append("-w ");
		}
	}
	else
	{
		if (!has_warn_flags)
		{
			scratch_buffer_append("-w -Wno-incompatible-pointer-types ");
		}
		scratch_buffer_append("-fno-builtin -fno-strict-aliasing ");

		if (is_clang && compiler.build.arch_os_target != default_target)
		{
			scratch_buffer_append("--target=");
			scratch_buffer_append(compiler.platform.target_triple);
			scratch_buffer_append(" ");
		}
		if (compiler.platform.os == OS_TYPE_IOS && compiler.build.ios.sysroot)
		{
			scratch_buffer_append("-isysroot \"");
			scratch_buffer_append(compiler.build.ios.sysroot);
			scratch_buffer_append("\" ");
		}
		if (compiler.platform.os != OS_TYPE_WIN32 && compiler.platform.reloc_model != RELOC_NONE)
		{
			scratch_buffer_append("-fPIC ");
		}
	}

	switch (compiler.build.optsize)
	{
		case SIZE_OPTIMIZATION_SMALL:
			scratch_buffer_append(is_cl ? "/O1 " : "-Os ");
			break;
		case SIZE_OPTIMIZATION_TINY:
			scratch_buffer_append(is_cl ? "/O1 " : "-Oz ");
			break;
		default:
			switch (compiler.build.optlevel)
			{
				case OPTIMIZATION_NONE:
				case OPTIMIZATION_NOT_SET:
					scratch_buffer_append(is_cl ? "/Od " : "-O0 ");
					break;
				case OPTIMIZATION_LESS:
					scratch_buffer_append(is_cl ? "/O1 " : "-O1 ");
					break;
				case OPTIMIZATION_MORE:
					scratch_buffer_append(is_cl ? "/O2 " : "-O2 ");
					break;
				case OPTIMIZATION_AGGRESSIVE:
					scratch_buffer_append(is_cl ? "/O2 " : "-O3 ");
					break;
			}
			break;
	}

	// The CPU the user chose, as LLVM gets it. Without a choice the C compiler keeps its own default target.
	BuildTarget *build = &compiler.build;
	bool is_x86        = compiler.platform.arch == ARCH_TYPE_X86_64 || compiler.platform.arch == ARCH_TYPE_X86;
	if (is_x86 && build->feature.x86_cpu_set != X86CPU_DEFAULT && !is_tcc)
	{
		static const char *gnu_x86[] = {
			[X86CPU_BASELINE] = "-march=x86-64 ", [X86CPU_SSSE3] = "-march=x86-64 -mssse3 ",
			[X86CPU_SSE4] = "-march=x86-64-v2 ", [X86CPU_AVX1] = "-march=x86-64-v2 -mavx ",
			[X86CPU_AVX2_V1] = "-march=x86-64-v3 ", [X86CPU_AVX2_V2] = "-march=x86-64-v3 ",
			[X86CPU_AVX512] = "-march=x86-64-v4 ", [X86CPU_NATIVE] = "-march=native " };
		// cl.exe's default is SSE2, and it has no SSSE3 or native level.
		static const char *cl_x86[] = {
			[X86CPU_SSE4] = "/arch:SSE4.2 ", [X86CPU_AVX1] = "/arch:AVX ", [X86CPU_AVX2_V1] = "/arch:AVX2 ",
			[X86CPU_AVX2_V2] = "/arch:AVX2 ", [X86CPU_AVX512] = "/arch:AVX512 " };
		const char *arch = (is_cl ? cl_x86 : gnu_x86)[build->feature.x86_cpu_set];
		if (arch) scratch_buffer_append(arch);
	}
	if (is_x86 && build->cpu_flags && !is_cl && !is_tcc)
	{
		// LLVM's x86 feature names are gcc's: +avx2,-sse4.2 is -mavx2 -mno-sse4.2.
		for (const char *f = build->cpu_flags; *f;)
		{
			const char *end = strchr(f, ',');
			size_t len      = end ? (size_t)(end - f) : strlen(f);
			if (len > 1 && (f[0] == '+' || f[0] == '-'))
			{
				scratch_buffer_printf("-m%s%.*s ", f[0] == '-' ? "no-" : "", (int)len - 1, f + 1);
			}
			f += len + (end ? 1 : 0);
		}
	}
	bool is_riscv = compiler.platform.arch == ARCH_TYPE_RISCV32 || compiler.platform.arch == ARCH_TYPE_RISCV64;
	if (is_riscv && !is_cl && !is_tcc)
	{
		const char *xlen = compiler.platform.arch == ARCH_TYPE_RISCV64 ? "64" : "32";
		if (build->feature.riscv_cpu_set != RISCV_CPU_DEFAULT)
		{
			static const char *isa[] = { [RISCV_CPU_RVI] = "i", [RISCV_CPU_RVIMAC] = "imac", [RISCV_CPU_RVIMAFC] = "imafc",
			                             [RISCV_CPU_RVGC] = "gc", [RISCV_CPU_RVGCV] = "gcv" };
			scratch_buffer_printf("-march=rv%s%s ", xlen, isa[build->feature.riscv_cpu_set]);
		}
		if (build->feature.riscv_cpu_set != RISCV_CPU_DEFAULT || build->feature.riscv_abi != RISCV_ABI_DEFAULT)
		{
			int flen = compiler.platform.riscv.flen;
			scratch_buffer_printf("-mabi=%s%s ", xlen[0] == '6' ? "lp64" : "ilp32", flen == 8 ? "d" : flen == 4 ? "f" : "");
		}
	}

	if (compiler.build.debug_info == DEBUG_INFO_FULL)
	{
		scratch_buffer_append(is_cl ? "/Z7 " : (is_clang ? "-g -fno-standalone-debug " : "-g "));
	}
	else if (compiler.build.debug_info == DEBUG_INFO_LINE_TABLES)
	{
		scratch_buffer_append(is_cl ? "/Z7 " : (is_clang ? "-gline-tables-only " : "-g1 "));
	}
	else if (!is_cl && compiler.build.debug_info == DEBUG_INFO_NONE)
	{
		scratch_buffer_append("-g0 ");
	}

	switch (compiler.build.feature.fp_math)
	{
		case FP_FAST:
			scratch_buffer_append(is_cl ? "/fp:fast " : "-ffast-math ");
			break;
		case FP_RELAXED:
			if (!is_cl)
			{
				scratch_buffer_append("-fno-trapping-math ");
			}
			break;
		case FP_STRICT:
		default:
			scratch_buffer_append(is_cl ? "/fp:precise " : "-fno-fast-math ");
			break;
	}

	if (compiler.build.feature.sanitize_address)
	{
		scratch_buffer_append(is_cl ? "/fsanitize=address " : "-fsanitize=address ");
	}
	if (!is_cl)
	{
		if (compiler.build.feature.sanitize_memory)
		{
			scratch_buffer_append("-fsanitize=memory ");
		}
		if (compiler.build.feature.sanitize_thread)
		{
			scratch_buffer_append("-fsanitize=thread ");
		}
		if (compiler.build.unroll_loops == UNROLL_LOOPS_ON)
		{
			scratch_buffer_append("-funroll-loops ");
		}
		if (compiler.build.loop_vectorization == VECTORIZATION_ON)
		{
			scratch_buffer_append("-ftree-vectorize ");
		}
		else if (compiler.build.loop_vectorization == VECTORIZATION_OFF)
		{
			scratch_buffer_append("-fno-tree-vectorize ");
		}
	}

	FOREACH(const char *, inc, compiler.build.cinclude_dirs)
	{
		scratch_buffer_append(is_cl ? "/I \"" : "-I \"");
		scratch_buffer_append(inc);
		scratch_buffer_append("\" ");
	}

	if (is_cl)
	{
		if (compiler.build.win.vs_dirs)
		{
			const char *semi = strchr(compiler.build.win.vs_dirs, ';');
			if (semi)
			{
				int sdk_len = (int)(semi - compiler.build.win.vs_dirs);
				const char *vs_dir = semi + 1;
				scratch_buffer_append(str_printf("/I \"%.*s\\include\\ucrt\" ", sdk_len, compiler.build.win.vs_dirs));
				scratch_buffer_append(str_printf("/I \"%.*s\\include\\um\" ", sdk_len, compiler.build.win.vs_dirs));
				scratch_buffer_append(str_printf("/I \"%.*s\\include\\shared\" ", sdk_len, compiler.build.win.vs_dirs));
				scratch_buffer_append(str_printf("/I \"%s\\include\" ", vs_dir));
			}
		}
		else
		{
			scratch_buffer_push();
			WindowsSDK *sdk = windows_get_sdk();
			scratch_buffer_pop();
			if (sdk && sdk->cl_include_env && !getenv("INCLUDE"))
			{
				StringSlice env = slice_from_string(sdk->cl_include_env);
				while (env.len)
				{
					StringSlice slice = slice_next_token(&env, ';');
					if (slice.len > 0)
					{
						scratch_buffer_append("/I \"");
						scratch_buffer_append_slice(slice);
						scratch_buffer_append("\" ");
					}
				}
			}
		}
	}

	if (compiler.build.cflags)
	{
		scratch_buffer_append(compiler.build.cflags);
		scratch_buffer_append(" ");
	}

	return scratch_buffer_copy();
}

// The C compiler for the target. Only clang takes the target on the command line, any other compiler would silently
// compile for the host.
static const char *c_find_target_c_compiler(void)
{
	const char *cc = find_c_compiler();
	if (cc_is_msvc(cc))
	{
		// From any host, through msvc-wine.
		if (compiler.platform.os == OS_TYPE_WIN32) return cc;
		error_exit("'%s' only compiles for Windows, but the target is '%s'. Use '--target windows-x64' or '--target windows-aarch64'.",
		           cc, compiler.platform.target_triple);
	}
	return cc;
}

// --test: the C counterpart of LLVM's IR verification. The generated C is checked by the C compiler that a build
// would use, with the same flags, without producing objects.
#define C_MAX_COMMAND_LINE 8000

static void c_syntax_check(GenContext **gen_contexts)
{
	const char *cc = c_find_target_c_compiler();
	bool is_cl     = cc_is_msvc(cc);
	const char *flags = c_backend_build_cflags(cc);
	const char *dir   = compiler.build.object_file_dir ? compiler.build.object_file_dir : ".";
	if (cc_is_tcc(cc))
	{
		// tcc has no syntax-only mode (it ignores -fsyntax-only and links): each file is compiled instead.
		const char *obj = file_append_path(dir, "__c3_syntax_check.o");
		FOREACH(GenContext *, c, gen_contexts)
		{
			const char *cmd = str_printf("\"%s\" %s -c \"%s\" -o \"%s\"", cc, flags, c->c_filename, obj);
			if (compiler.build.print_linking) puts(cmd);
			if (system(cmd) != 0) error_exit("The generated C failed the syntax check: %s", cmd);
		}
		file_delete_file(obj);
		return;
	}
	const char *cmd   = str_printf("\"%s\" %s %s", cc, is_cl ? "/Zs" : "-fsyntax-only", flags);
	const char *files = "";
	FOREACH(GenContext *, c, gen_contexts)
	{
		files = str_printf("%s \"%s\"", files, c->c_filename);
	}
	// cmd.exe takes at most 8191 characters, so a longer list goes in a response file, which every supported C
	// compiler reads. Its paths use '/': gcc reads '\\' there as an escape.
	if (strlen(cmd) + strlen(files) > C_MAX_COMMAND_LINE)
	{
		const char *rsp = file_append_path(dir, "__c3_syntax_check.rsp");
		FILE *f         = fopen(rsp, "wb");
		if (!f) error_exit("Failed to write '%s'.", rsp);
		FOREACH(GenContext *, c, gen_contexts)
		{
			fputc('"', f);
			for (const char *p = c->c_filename; *p; p++) fputc(*p == '\\' ? '/' : *p, f);
			fputs("\"\n", f);
		}
		fclose(f);
		files = str_printf(" \"@%s\"", rsp);
	}
	cmd = str_printf("%s%s", cmd, files);
	if (compiler.build.print_linking) puts(cmd);
	if (system(cmd) != 0)
	{
		error_exit("The generated C failed the syntax check: %s", cmd);
	}
}

size_t c_codegen_cost(void *context)
{
	return context ? ((GenContext *)context)->c_file_size : 0;
}

bool c_codegen(void *context, const char **out_name)
{
	*out_name = NULL;
	GenContext *c = (GenContext *)context;
	if (!c || (c->current_module && !compiler_should_output_file(c->base_name)))
	{
		return true;
	}

	if (c_backend_cc)
	{
		*out_name = cc_compiler(c_backend_cc, c->c_filename, c->object_filename, c_backend_cflags, NULL, NULL);
		return *out_name != NULL;
	}
	return true;
}

void **c_gen(Module **modules, int module_count)
{
	if (!module_count)
	{
		return NULL;
	}
	htable_init(&c_decl_name_cache, 16384);
	htable_init(&c_positional_name_owner, 1024);
	c_type_names_init();
	htable_init(&c_needed_typeids, 1024);
	c_needed_typeids_vec = NULL;
	c_recording_typeids = true;

	const char *dir = compiler.build.object_file_dir ? compiler.build.object_file_dir : ".";
	scratch_buffer_clear();
	scratch_buffer_append(dir);
	dir_make_recursive(scratch_buffer_to_string());
	c_traverse_all_modules(&(GenContext){0}, visit_links, NULL);

	bool will_compile = compiler.build.emit_object_files
		|| compiler.build.type == TARGET_TYPE_EXECUTABLE
		|| compiler.build.type == TARGET_TYPE_STATIC_LIB
		|| compiler.build.type == TARGET_TYPE_DYNAMIC_LIB
		|| compiler.build.type == TARGET_TYPE_TEST
		|| compiler.build.type == TARGET_TYPE_BENCHMARK;
	if (will_compile)
	{
		c_backend_cc     = c_find_target_c_compiler();
		c_backend_cflags = c_backend_build_cflags(c_backend_cc);
	}
	else
	{
		c_backend_cc     = NULL;
		c_backend_cflags = NULL;
	}

	GenContext **gen_contexts = NULL;

	if (compiler.build.single_module == SINGLE_MODULE_ON)
	{
		GenContext *c = c_gen_module(NULL, 0);
		if (c)
		{
			vec_add(gen_contexts, c);
		}
	}
	else
	{
		for (int i = 0; i < module_count; i++)
		{
			GenContext *c = c_gen_module(modules[i], i);
			if (c)
			{
				vec_add(gen_contexts, c);
			}
		}
	}

	c_recording_typeids = false;
	c_emit_runtime_header(dir);
	GenContext *rt = c_emit_runtime_c(dir);
	if (rt)
	{
		vec_add(gen_contexts, rt);
	}
	if (compiler.build.test_output) c_syntax_check(gen_contexts);

	return (void **)gen_contexts;
}