#include "c_codegen_internal.h"

/*

Design:

- Faults jump to c->catch as soon as they appear, as in LLVM. An optional function returns its fault and writes the
  value through a leading pointer parameter (the LLVM ABI, c_abi_func_create).
- Sema makes every conversion an expression, so stores (c_emit_store) only copy and assert equal C types. Conversions
  live in their expressions: EXPR_RVALUE (pointer casts), EXPR_ADDR_CONVERSION, and per cast kind
  c_emit_scalar_convert / c_emit_reinterpret / c_emit_vec_recast.
- Under --test the generated C is checked with the C compiler's -fsyntax-only, as LLVM verifies its IR.

Known issues:

1. float16, bfloat and float128 lower to float/double. struct { float16 a, b; } is 4 bytes to c3c but 8 in C, so
   mem::new_array(Pair, 4) overflows the heap (ASan-confirmed). Half-precision rounding is also lost.

2. @section and @noinline are dropped (verified in the emitted C). @inline, @naked and @noreturn look unhandled.

3. A zero sized optional value (`Empty? f()`) gets no out pointer in C, while LLVM still passes one, so such a function
   is not ABI compatible between the backends.

4. The "No method" panic of a dynamic call reports the interface method, not the call site: the dispatcher is a shared
   function without access to the caller's location.

5. Code after an operand that always fails is still emitted; LLVM drops the dead block. Only the sema shape of a
   rethrow of a wildcard optional is handled (EXPR_RETHROW).

6. Remaining type fallbacks: c_ensure_cvalue_var (fallback_type, then int), c_emit_binary_expr (void expression
   type), c_emit_bitstruct_container (uint container).

7. The --test syntax check passes every generated file on one command line; untested against Windows length limits.

8. cl.exe: no weak functions (__c3_weak_fn is empty), so std symbols of a static library clash with those of the
   program. Runtime global initializers rely on __attribute__((constructor)); without it only the entry point's module
   is initialized. Inline assembly is emitted as GNU extended asm, which cl.exe rejects (#error).

9. TCC and cl.exe lower int128 to 64 bits. The TCC-only runtime code has bugs, by inspection: unsigned narrow
   __c3_tcc_sub_overflow never writes *res, and "+r"((uint8_t)desired) isn't an lvalue.
*/

static HTable c_decl_name_cache;

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

static bool c_decl_has_asm_name(Decl *decl)
{
	if (!decl) return false;
	decl = c_decl_unwrap(decl);
	return decl->has_extname && c_extname_is_asm(decl->extname);
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

bool c_decl_needs_asm_label(Decl *decl)
{
	const char *asm_name = c_get_decl_asm_name(decl);
	return asm_name && (c_decl_has_asm_name(decl) || c_is_builtin_symbol_collision(asm_name));
}

static const char *c_compute_decl_name(Decl *decl)
{
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
		return c_intern(str_printf("__c3_anon_%u", (unsigned)declid(decl)));
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
	scratch_buffer_set_extern_decl_name(decl, true);
	char *str = scratch_buffer_copy();
	scratch_buffer_pop();
	return c_check_collision(c_sanitize_name(str));
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

void c_print(GenContext *c, const char *str)
{
	if (!str)
	{
		return;
	}
	c_write_bytes(c, (const uint8_t *)str, strlen(str));
}

void c_print_byte(GenContext *c, uint8_t b)
{
	c_write_bytes(c, &b, 1);
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
					c_print_byte(c, b);
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

// Panic with a user message, optionally formatted with `args`.
void c_emit_panic(GenContext *c, const char *message, SourceLocId loc, Expr **args)
{
	if (no_panic() || !compiler.context.panic_var)
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
		       arr_var, i, arg_val.var, c_typeid_name(type_no_optional(args[i]->type)));
	}
	c_emit_panic_call(c, panic_fn_desc[PANIC_FN_ASSERT_VIOLATION].format, loc, message, str_printf("___var_%d", arr_var), num_args);
}

// Integers are widened to 64 bits when passed to a panic, as the LLVM backend does.
static Type *c_panic_value_type(Type *type)
{
	type = c_safe_type_lower(type);
	if (!type_is_integer(type) || type_bit_size(type) > 64) return type;
	return type_is_unsigned(type) ? type_ulong : type_long;
}

// Emit a compiler generated panic. `texts` and `values` hold the counts given by `panic_fn_desc[id]`.
void c_emit_panic_fn(GenContext *c, PanicFunction id, SourceLocId loc, const char **texts, CPanicValue *values)
{
	if (no_panic() || !compiler.context.panic_var)
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
				typeids[count++] = c_typeid_name(type_string);
			}
		}
		Type *type = c_panic_value_type(values[i].type);
		PRINTF("%s __c3_panic_arg%d = (%s)(%s);\n", c_type_name(c, type), count, c_type_name(c, type), values[i].expr);
		typeids[count++] = c_typeid_name(type);
	}
	if (value_count <= 1)
	{
		for (int j = 0; j < text_count; j++)
		{
			PRINTF("%s __c3_panic_arg%d = ", c_type_name(c, type_get_slice(type_char)), count);
			c_emit_string_slice(c, texts[j]);
			PRINT(";\n");
			typeids[count++] = c_typeid_name(type_string);
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
	if (!c_decl_is_live(d) && !(c->traverse_unused_interfaces && d->decl_kind == DECL_INTERFACE))
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
			Type *ptype = p->type ? c_safe_type_lower(p->type) : type_int;
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

static bool c_has_main_function(Module **modules, int mod_count)
{
	for (int m = 0; m < mod_count; m++)
	{
		Module *mod = modules[m];
		if (!mod)
		{
			continue;
		}
		FOREACH(CompilationUnit *, unit, mod->units)
		{
			if (unit->main_function)
			{
				return true;
			}
			FOREACH(Decl *, decl, unit->functions)
			{
				if (decl && decl->name && strcmp(decl->name, "main") == 0)
				{
					return true;
				}
			}
		}
	}
	return false;
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
	htable_init(&c->emitted_func_defs, 8192);
	htable_init(&c->emitted_global_defs, 8192);
	htable_init(&c->gen_decl, 4096);
	htable_init(&c->gen_def, 4096);
	htable_init(&c->decl_names, 4096);
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

	Decl **runtime_inits = NULL;
	for (int m = 0; m < mod_count; m++)
	{
		Module *mod = modules[m];
		if (!mod || !vec_size(mod->units) || (compiler.build.emit_stdlib == EMIT_STDLIB_OFF && module_is_stdlib(mod)))
		{
			continue;
		}

		FOREACH(CompilationUnit *, unit, mod->units)
		{
			FOREACH(Decl *, var, unit->vars)
			{
				if (!var || var->replacement || var->is_template)
				{
					continue;
				}
				var = decl_raw(var);
				if (!var || var->decl_kind != DECL_VAR || !c_is_file_global(var) || var->is_extern)
				{
					continue;
				}
				if (only_used && !var->is_live)
				{
					continue;
				}
				if (var->var.init_expr && !c_can_emit_static_initializer(var->var.init_expr))
				{
					vec_add(runtime_inits, var);
				}
			}
		}
	}

	const char *init_globals_name = str_printf("__c3_init_globals_%s", module ? c_sanitize_name(module->name->module) : "all");
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

		PRINT("#if defined(__GNUC__) || defined(__clang__)\n__attribute__((constructor(101)))\n#endif\n");
		PRINTF("static void %s(void) {\n", init_globals_name);
		PRINT("\tstatic bool initialized = false;\n");
		PRINT("\tif (initialized) return;\n");
		PRINT("\tinitialized = true;\n");

		c_emit_local_var_declarations(c);

		if (body_buf.write_idx > 0)
		{
			c_write_bytes(c, body_buf.bytes.ptr, body_buf.write_idx);
		}
		byte_buffer_free(&body_buf);
		PRINT("}\n\n");
	}
	else
	{
		PRINTF("static void %s(void) {}\n\n", init_globals_name);
	}

	if (c_has_main_function(modules, mod_count) || compiler.build.single_module == SINGLE_MODULE_ON)
	{
		// Only called by the entry point of this file. Each link unit has its own.
		PRINT("static void __c3_init_runtime(void) {\n");
		PRINT("\tstatic bool initialized = false;\n");
		PRINT("\tif (initialized) return;\n");
		PRINT("\tinitialized = true;\n");
		PRINTF("\t%s();\n", init_globals_name);

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
	for (int i = 0; i < 5; i++)
	{
		fwrite(parts[i]->bytes.ptr, 1, parts[i]->write_idx, f);
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
	bool is_cl    = str_ends_with(cc, "cl.exe");
	bool is_tcc   = str_ends_with(cc, "tcc");
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
		scratch_buffer_append("-fno-builtin -fwrapv -fno-strict-aliasing ");

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
	if (compiler.build.arch_os_target != default_target && !strstr(cc, "clang") && !strstr(cc, "zig") &&
	    compiler.platform.os != OS_TYPE_EMSCRIPTEN)
	{
		error_exit("Cross compiling with the C backend requires clang, but the C compiler is '%s'. Use '--cc clang' or set C3C_CC.", cc);
	}
	return cc;
}

// --test: the C counterpart of LLVM's IR verification. The generated C is checked by the C compiler that a build
// would use, with the same flags, without producing objects.
static void c_syntax_check(GenContext **gen_contexts)
{
	const char *cc = c_find_target_c_compiler();
	bool is_cl     = str_ends_with(cc, "cl.exe");
	const char *flags = c_backend_build_cflags(cc);
	const char *cmd   = str_printf("\"%s\" %s %s", cc, is_cl ? "/Zs" : "-fsyntax-only", flags);
	FOREACH(GenContext *, c, gen_contexts)
	{
		cmd = str_printf("%s \"%s\"", cmd, c->c_filename);
	}
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