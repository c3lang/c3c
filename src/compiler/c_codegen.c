#include "c_codegen_internal.h"

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

const char *c_sanitize_name(const char *name)
{
	if (!name)
	{
		return "_anon";
	}
	char *res = str_dup(name);
	for (char *p = res; *p; p++)
	{
		if (*p == '*')
		{
			*p = 'p';
		}
		else if (!((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_'))
		{
			*p = '_';
		}
	}
	return c_intern(res);
}

static inline bool c_is_builtin_symbol_collision(const char *name)
{
	return strncmp(name, "__atomic_", 9) == 0 || strncmp(name, "__builtin_", 10) == 0;
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
		return decl->extname;
	}
	if (decl->is_extern && decl->name)
	{
		return decl->name;
	}
	return NULL;
}

const char *c_get_decl_name(Decl *decl)
{
	if (!decl)
	{
		return "unknown";
	}
	decl = c_decl_unwrap(decl);

	if ((decl == compiler.context.main && !decl->func_decl.type_parent) ||
	    (decl->is_synthetic && decl->name && strcmp(decl->name, "main") == 0))
	{
		if (compiler.platform.os == OS_TYPE_WIN32 && decl->decl_kind == DECL_FUNC && vec_size(decl->func_decl.signature.params) == 4)
		{
			Decl *param2 = decl->func_decl.signature.params[2];
			Type *t2 = c_decl_type(param2);
			if (t2 && t2->type_kind == TYPE_POINTER && t2->pointer && t2->pointer->type_kind == TYPE_U16)
			{
				return "wWinMain";
			}
			return "WinMain";
		}
		if (!decl->unit || !decl->unit->main_function || !decl->unit->main_function->is_synthetic || decl == decl->unit->main_function)
		{
			return "main";
		}
	}
	if (decl->is_extern)
	{
		const char *name = NULL;
		if (decl->has_extname && decl->extname)
		{
			name = c_sanitize_name(decl->extname);
		}
		else if (decl->name)
		{
			name = c_sanitize_name(decl->name);
		}
		if (name)
		{
			if (c_is_builtin_symbol_collision(name))
			{
				return c_intern(str_printf("__c3_ext_%s", name));
			}
			return name;
		}
	}

	if (!decl->name || decl->name[0] == '$')
	{
		return c_intern(str_printf("__c3_anon_%u", (unsigned)declid(decl)));
	}

	if (decl->has_extname && decl->extname)
	{
		const char *name = c_sanitize_name(decl->extname);
		if (c_is_builtin_symbol_collision(name))
		{
			return c_intern(str_printf("__c3_ext_%s", name));
		}
		return name;
	}

	decl->is_export = true;
	scratch_buffer_set_extern_decl_name(decl, true);
	char *str        = scratch_buffer_copy();
	const char *name = c_sanitize_name(str);
	if (c_is_builtin_symbol_collision(name))
	{
		return c_intern(str_printf("__c3_ext_%s", name));
	}
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

static void c_traverse_decl(GenContext *c, Decl *d, CDeclVisitor visitor, void *userdata)
{
	if (!d || d->decl_kind == DECL_POISONED || d->decl_kind == DECL_ERASED || d->replacement)
	{
		return;
	}
	if (d->decl_kind == DECL_MACRO || d->decl_kind == DECL_IMPORT || d->decl_kind == DECL_ATTRIBUTE || d->decl_kind == DECL_CONTRACT)
	{
		return;
	}
	if (d->decl_kind == DECL_GENERIC)
	{
		FOREACH(Decl *, inst, d->generic_decl.instances)
		c_traverse_decl(c, inst, visitor, userdata);
		return;
	}
	if (d->decl_kind == DECL_GENERIC_INSTANCE)
	{
		FOREACH(Decl *, gd, d->instance_decl.generated_decls)
		c_traverse_decl(c, gd, visitor, userdata);
		return;
	}
	if (d->decl_kind == DECL_ALIAS)
	{
		Decl *raw = c_decl_unwrap(d);
		if (raw && raw != d)
		{
			c_traverse_decl(c, raw, visitor, userdata);
		}
		return;
	}
	if (d->decl_kind == DECL_GROUP)
	{
		FOREACH(Decl *, gd, d->decl_list)
		c_traverse_decl(c, gd, visitor, userdata);
		return;
	}
	if (d->is_template)
	{
		return;
	}
	decl_append_links_to_global_during_codegen(d);
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
		c_traverse_decl(c, im, visitor, userdata);
	}
	if (decl_has_interface(d) && d->method_table && d->method_table->methods)
	{
		FOREACH(Decl *, m, d->method_table->methods)
		c_traverse_decl(c, m, visitor, userdata);
	}
}

static void c_traverse_unit(GenContext *c, CompilationUnit *unit, CDeclVisitor visitor, void *userdata)
{
	if (!unit)
	{
		return;
	}
	FOREACH(Decl *, t, unit->types)
	c_traverse_decl(c, t, visitor, userdata);
	FOREACH(Decl *, v, unit->vars)
	c_traverse_decl(c, v, visitor, userdata);
	FOREACH(Decl *, f, unit->functions)
	c_traverse_decl(c, f, visitor, userdata);
	FOREACH(Decl *, m, unit->methods)
	c_traverse_decl(c, m, visitor, userdata);
	FOREACH(Decl *, l, unit->lambdas)
	c_traverse_decl(c, l, visitor, userdata);
	FOREACH(Decl *, e, unit->enums)
	c_traverse_decl(c, e, visitor, userdata);
	FOREACH(Decl *, g, unit->generic_decls)
	c_traverse_decl(c, g, visitor, userdata);
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
		c_traverse_unit(c, unit, visitor, userdata);
	}
	FOREACH(Decl *, ext, compiler.context.method_extension_list)
	c_traverse_decl(c, ext, visitor, userdata);
	FOREACH(Decl *, ug, compiler.context.unregistered_generic_decls)
	c_traverse_decl(c, ug, visitor, userdata);
	FOREACH(Decl *, ums, compiler.context.unregistered_method_specializations)
	c_traverse_decl(c, ums, visitor, userdata);
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
	}
	return false;
}

static void visit_forward_decl(GenContext *c, Decl *d, void *ud)
{
	(void)ud;
	Type *dt = c_decl_type(d);
	if (is_valid_type_ptr(dt) && dt->type_kind != TYPE_POISONED)
	{
		c_emit_type_forward_decl(c, dt);
	}
	if (d->decl_kind == DECL_FUNC)
	{
		Type *rtype = typeget(d->func_decl.signature.rtype);
		if (is_valid_type_ptr(rtype) && rtype->type_kind != TYPE_POISONED)
		{
			c_emit_type_forward_decl(c, rtype);
		}
		FOREACH(Decl *, p, d->func_decl.signature.params)
		{
			Type *ptype = c_decl_type(p);
			if (is_valid_type_ptr(ptype) && ptype->type_kind != TYPE_POISONED)
			{
				c_emit_type_forward_decl(c, ptype);
			}
		}
	}
}

static void visit_type_decl(GenContext *c, Decl *d, void *ud)
{
	(void)ud;
	Type *dt = c_decl_type(d);
	if (is_valid_type_ptr(dt) && dt->type_kind != TYPE_POISONED)
	{
		c_emit_type_decl(c, dt);
	}
	if (d->decl_kind == DECL_FUNC)
	{
		Type *rtype = typeget(d->func_decl.signature.rtype);
		if (rtype)
		{
			c_emit_type_decl(c, rtype);
		}
		FOREACH(Decl *, p, d->func_decl.signature.params)
		{
			Type *ptype = c_decl_type(p);
			if (is_valid_type_ptr(ptype) && ptype->type_kind != TYPE_POISONED)
			{
				c_emit_type_decl(c, ptype);
			}
		}
	}
}

static void visit_fn_prototype(GenContext *c, Decl *d, void *ud)
{
	(void)ud;
	d = c_decl_unwrap(d);
	if (d && d->decl_kind == DECL_FUNC)
	{
		if (d->func_decl.attr_test)
		{
			if (!compiler.build.build_test)
			{
				return;
			}
		}
		else if (d->func_decl.attr_benchmark)
		{
			if (!compiler.build.build_benchmark)
			{
				return;
			}
		}
		else if (strip_unused() && !d->is_live)
		{
			return;
		}
		bool is_current = (!c->current_module || (d->unit && d->unit->module == c->current_module));
		c_emit_function_decl(c, d, is_current);
	}
}

static void visit_mark_declared(GenContext *c, Decl *d, void *ud)
{
	(void)ud;
	d = c_decl_unwrap(d);
	if (d && d->decl_kind == DECL_FUNC)
	{
		const char *fn_name = c_get_decl_name(d);
		htable_set(&c->decl_names, (void *)fn_name, (void *)1);
	}
	if (d && d->decl_kind == DECL_VAR && c_is_file_global(d))
	{
		const char *vname = c_get_decl_name(d);
		htable_set(&c->emitted_global_decls, (void *)vname, (void *)1);
	}
}

static void visit_dynamic_dispatcher(GenContext *c, Decl *d, void *ud)
{
	(void)ud;
	if (d->decl_kind == DECL_INTERFACE && d->interface_methods)
	{
		FOREACH(Decl *, im, d->interface_methods)
		c_emit_dynamic_dispatcher(c, im);
	}
}

static void visit_global_forward_decl(GenContext *c, Decl *d, void *ud)
{
	(void)ud;
	d = c_decl_unwrap(d);
	if (!d)
	{
		return;
	}
	if (strip_unused() && !d->is_live)
	{
		return;
	}
	if (d->decl_kind == DECL_VAR && c_is_file_global(d))
	{
		c_emit_global_decl(c, d);
		return;
	}
	if (d->decl_kind == DECL_ENUM && d->enums.parameters && d->enums.values)
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
			if (htable_get(&c->emitted_global_decls, (void *)arr_name))
			{
				continue;
			}
			htable_set(&c->emitted_global_decls, (void *)arr_name, (void *)1);
			Type *ptype = p->type ? c_safe_type_lower(p->type) : type_int;
			c_emit_type_forward_decl(c, ptype);
			PRINTF("extern const %s %s[%d];\n", c_type_name(c, ptype), arr_name, num_values);
		}
	}
}

static void visit_global_def(GenContext *c, Decl *d, void *ud)
{
	(void)ud;
	d = c_decl_unwrap(d);
	if (!d)
	{
		return;
	}
	if (strip_unused() && !d->is_live)
	{
		return;
	}
	// In single-module mode (current_module == NULL), emit all globals; otherwise only this module's.
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

static void c_emit_shared_header(const char *dir)
{
	const char *h_filename = file_append_path(dir, "__c3_shared.h");
	FILE *f                = fopen(h_filename, "wb");
	if (!f)
	{
		error_exit("Failed to open output shared header '%s'.", h_filename);
	}

	GenContext *c = cmalloc(sizeof(GenContext));
	*c            = (GenContext){
	    .file           = f,
	    .current_module = NULL,
	};
	htable_init(&c->gen_decl, 16384);
	htable_init(&c->gen_def, 16384);
	htable_init(&c->decl_names, 8192);
	htable_init(&c->emitted_global_decls, 8192);

	fputs("/* Generated by C3 Compiler (Shared Header) */\n", f);
	fputs("#ifndef __C3_SHARED_H__\n", f);
	fputs("#define __C3_SHARED_H__\n\n", f);
	fputs("#include \"__c3_runtime.h\"\n\n", f);

	fputs("/* TYPE FORWARD DECLARATIONS */\n", f);
	int type_count = vec_size(compiler.context.type);
	for (int i = 0; i < type_count; i++)
	{
		Type *t = compiler.context.type[i];
		if (c_type_needs_emission(t))
		{
			c_emit_type_forward_decl(c, t);
		}
	}
	c_traverse_all_modules(c, visit_forward_decl, NULL);
	fputs("\n", f);

	fputs("/* TYPE DEFINITIONS */\n", f);
	for (int i = 0; i < type_count; i++)
	{
		Type *t = compiler.context.type[i];
		if (c_type_needs_emission(t))
		{
			c_emit_type_decl(c, t);
		}
	}
	c_traverse_all_modules(c, visit_type_decl, NULL);
	fputs("\n", f);

	fputs("/* FUNCTION PROTOTYPES */\n", f);
	c_traverse_all_modules(c, visit_fn_prototype, NULL);
	fputs("\n", f);

	fputs("/* DYNAMIC DISPATCHERS */\n", f);
	c_traverse_all_modules(c, visit_dynamic_dispatcher, NULL);
	fputs("\n", f);

	fputs("/* GLOBAL FORWARD DECLARATIONS */\n", f);
	c_traverse_all_modules(c, visit_global_forward_decl, NULL);
	fputs("\n", f);

	fputs("#endif /* __C3_SHARED_H__ */\n", f);
	fclose(f);
}

static void c_mark_all_types_declared(GenContext *c)
{
	int type_count = vec_size(compiler.context.type);
	for (int i = 0; i < type_count; i++)
	{
		Type *t = compiler.context.type[i];
		if (c_type_needs_emission(t))
		{
			const char *tname = c_type_name(c, t);
			htable_set(&c->gen_decl, (void *)tname, (void *)1);
			htable_set(&c->gen_def, (void *)tname, (void *)1);
		}
	}
	c_traverse_all_modules(c, visit_mark_declared, NULL);
}

static void c_try_emit_function_def(GenContext *c, Decl *decl, bool only_used)
{
	if (!decl || !decl->func_decl.body || decl->is_extern || decl->replacement)
	{
		return;
	}
	if (decl->func_decl.attr_test && !compiler.build.build_test)
	{
		return;
	}
	if (decl->func_decl.attr_benchmark && !compiler.build.build_benchmark)
	{
		return;
	}
	if (only_used && !decl->is_live)
	{
		return;
	}
	const char *fname = c_get_decl_name(decl);
	if (htable_get(&c->emitted_func_defs, (void *)fname))
	{
		return;
	}
	htable_set(&c->emitted_func_defs, (void *)fname, (void *)1);
	c_emit_function(c, decl);
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
		base_name       = build_base_name();
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
	    .file               = f,
	    .base_name          = base_name,
	    .c_filename         = c_filename,
	    .object_filename    = object_filename,
	    .current_module     = module,
	    .current_block_live = true,
	};
	htable_init(&c->gen_decl, 16384);
	htable_init(&c->gen_def, 16384);
	htable_init(&c->local_vars, 4096);
	htable_init(&c->local_fault_vars, 4096);
	htable_init(&c->declared_vars, 4096);
	htable_init(&c->decl_names, 8192);
	htable_init(&c->emitted_func_defs, 8192);
	htable_init(&c->emitted_global_decls, 8192);
	htable_init(&c->emitted_global_defs, 8192);

	// Include the shared header with all types, prototypes, and dispatchers
	fputs("/* Generated by C3 Compiler */\n", f);
	fputs("#include \"__c3_shared.h\"\n\n", f);

	// Mark all types as generated so inline functions don't attempt to re-declare them
	c_mark_all_types_declared(c);

	fputs("/* GLOBALS */\n", f);
	c_traverse_all_modules(c, visit_global_def, NULL);
	fputs("\n", f);

	fputs("/* FUNCTION BODIES */\n", f);
	bool only_used = strip_unused();

	Module **modules = module ? &module : compiler.context.module_list;
	int mod_count    = module ? 1 : vec_size(compiler.context.module_list);

	Decl **runtime_inits = NULL;
	for (int m = 0; m < mod_count; m++)
	{
		Module *mod = modules[m];
		if (!mod || !vec_size(mod->units))
		{
			continue;
		}
		if (compiler.build.emit_stdlib == EMIT_STDLIB_OFF && module_is_stdlib(mod))
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

		c->current_return_type      = type_void;
		c->current_macro_ret_var    = 0;
		c->current_macro_ret_type   = NULL;
		c->current_macro_fault_var  = 0;
		c->current_macro_exit_label = 0;
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
			CValue val = {0};
			c_emit_expr(c, &val, var->var.init_expr);
			c_value_rvalue(c, &val);
			const char *gname = c_get_decl_name(var);
			Type *gt          = c_decl_type(var);
			if (c_type_is_aggregate(gt))
			{
				if (val.kind == CV_ADDRESS)
				{
					PRINTF("__c3_memcpy(&%s, (void*)___var_%d, sizeof(%s));\n", gname, val.var, gname);
				}
				else
				{
					PRINTF("__c3_memcpy(&%s, &___var_%d, sizeof(%s));\n", gname, val.var, gname);
				}
			}
			else
			{
				PRINTF("%s = (%s)___var_%d;\n", gname, c_type_name(c, gt), val.var);
			}
			PRINT("}\n");
		}

		c->buffer = prev_buf;

		PRINT("#if defined(__GNUC__) || defined(__clang__)\n__attribute__((constructor(101)))\n#endif\n");
		PRINTF("void %s(void) {\n", init_globals_name);
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
		PRINTF("void %s(void) {}\n\n", init_globals_name);
	}

	bool has_main = false;
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
				has_main = true;
				break;
			}
			FOREACH(Decl *, decl, unit->functions)
			{
				if (decl && decl->name && strcmp(decl->name, "main") == 0)
				{
					has_main = true;
					break;
				}
			}
			if (has_main)
			{
				break;
			}
		}
		if (has_main)
		{
			break;
		}
	}

	if (has_main || compiler.build.single_module == SINGLE_MODULE_ON)
	{
		PRINT("void __c3_init_runtime(void) {\n");
		PRINT("\tstatic bool initialized = false;\n");
		PRINT("\tif (initialized) return;\n");
		PRINT("\tinitialized = true;\n");
		PRINTF("\t%s();\n", init_globals_name);

		// Collect all @init functions
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

		// Sort @init functions by priority (lower number runs first)
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

		for (int i = 0; i < init_count; i++)
		{
			c_emit_function_decl(c, init_funcs[i], false);
			PRINTF("\t%s();\n", c_get_decl_name(init_funcs[i]));
		}
		PRINT("}\n\n");
	}

	for (int m = 0; m < mod_count; m++)
	{
		Module *mod = modules[m];
		if (!mod || !vec_size(mod->units))
		{
			continue;
		}
		if (compiler.build.emit_stdlib == EMIT_STDLIB_OFF && module_is_stdlib(mod))
		{
			continue;
		}

		FOREACH(CompilationUnit *, unit, mod->units)
		{
			FOREACH(Decl *, decl, unit->functions)
			{
				c_try_emit_function_def(c, decl, only_used);
			}
			FOREACH(Decl *, func, unit->lambdas)
			{
				c_try_emit_function_def(c, func, only_used);
			}
			if (unit->main_function && unit->main_function->is_synthetic)
			{
				c_try_emit_function_def(c, unit->main_function, only_used);
			}
			FOREACH(Decl *, decl, unit->methods)
			{
				c_try_emit_function_def(c, decl, only_used);
			}
		}
	}

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
	char buf[4096];
	buf[0] = '\0';

	// 1. Base flags
	bool has_warn_flags = compiler.build.cflags && (strstr(compiler.build.cflags, "-W") || strstr(compiler.build.cflags, "/W"));
	if (is_cl)
	{
		strncat(buf, "/nologo ", sizeof(buf) - strlen(buf) - 1);
		if (!has_warn_flags)
		{
			strncat(buf, "/w ", sizeof(buf) - strlen(buf) - 1);
		}
	}
	else if (is_tcc)
	{
		if (!has_warn_flags)
		{
			strncat(buf, "-w ", sizeof(buf) - strlen(buf) - 1);
		}
	}
	else
	{
		if (!has_warn_flags)
		{
			// -Wno-incompatible-pointer-types prevents Clang 16+ from treating 
			// cross-module C struct pointers with identical layouts as fatal errors
			strncat(buf, "-w -Wno-incompatible-pointer-types ", sizeof(buf) - strlen(buf) - 1);
		}
		strncat(buf, "-fno-builtin -fwrapv -fno-strict-aliasing ", sizeof(buf) - strlen(buf) - 1);

		// Clang requires --target when cross-compiling; GCC uses target-prefixed binaries instead
		if (is_clang && compiler.build.arch_os_target != default_target)
		{
			strncat(buf, "--target=", sizeof(buf) - strlen(buf) - 1);
			strncat(buf, compiler.platform.target_triple, sizeof(buf) - strlen(buf) - 1);
			strncat(buf, " ", sizeof(buf) - strlen(buf) - 1);
		}

		if (compiler.platform.os == OS_TYPE_IOS && compiler.build.ios.sysroot)
		{
			strncat(buf, "-isysroot \"", sizeof(buf) - strlen(buf) - 1);
			strncat(buf, compiler.build.ios.sysroot, sizeof(buf) - strlen(buf) - 1);
			strncat(buf, "\" ", sizeof(buf) - strlen(buf) - 1);
		}
		if (compiler.platform.os != OS_TYPE_WIN32 && compiler.platform.reloc_model != RELOC_NONE)
		{
			strncat(buf, "-fPIC ", sizeof(buf) - strlen(buf) - 1);
		}
	}

	// 2. Optimization level & size
	switch (compiler.build.optsize)
	{
		case SIZE_OPTIMIZATION_SMALL:
			strncat(buf, is_cl ? "/O1 " : "-Os ", sizeof(buf) - strlen(buf) - 1);
			break;
		case SIZE_OPTIMIZATION_TINY:
			strncat(buf, is_cl ? "/O1 " : "-Oz ", sizeof(buf) - strlen(buf) - 1);
			break;
		default:
			switch (compiler.build.optlevel)
			{
				case OPTIMIZATION_NONE:
				case OPTIMIZATION_NOT_SET:
					strncat(buf, is_cl ? "/Od " : "-O0 ", sizeof(buf) - strlen(buf) - 1);
					break;
				case OPTIMIZATION_LESS:
					strncat(buf, is_cl ? "/O1 " : "-O1 ", sizeof(buf) - strlen(buf) - 1);
					break;
				case OPTIMIZATION_MORE:
					strncat(buf, is_cl ? "/O2 " : "-O2 ", sizeof(buf) - strlen(buf) - 1);
					break;
				case OPTIMIZATION_AGGRESSIVE:
					strncat(buf, is_cl ? "/O2 " : "-O3 ", sizeof(buf) - strlen(buf) - 1);
					break;
			}
			break;
	}

	// 3. Debug info
	switch (compiler.build.debug_info)
	{
		case DEBUG_INFO_FULL:
			strncat(buf, is_cl ? "/Z7 " : "-g ", sizeof(buf) - strlen(buf) - 1);
			break;
		case DEBUG_INFO_LINE_TABLES:
			strncat(buf, is_cl ? "/Z7 " : "-g1 ", sizeof(buf) - strlen(buf) - 1);
			break;
		case DEBUG_INFO_NONE:
		default:
			if (!is_cl && compiler.build.debug_info == DEBUG_INFO_NONE)
			{
				strncat(buf, "-g0 ", sizeof(buf) - strlen(buf) - 1);
			}
			break;
	}

	// 4. Floating-point math
	switch (compiler.build.feature.fp_math)
	{
		case FP_FAST:
			strncat(buf, is_cl ? "/fp:fast " : "-ffast-math ", sizeof(buf) - strlen(buf) - 1);
			break;
		case FP_RELAXED:
			if (!is_cl)
			{
				strncat(buf, "-fno-trapping-math ", sizeof(buf) - strlen(buf) - 1);
			}
			break;
		case FP_STRICT:
		default:
			if (is_cl)
			{
				strncat(buf, "/fp:precise ", sizeof(buf) - strlen(buf) - 1);
			}
			else
			{
				strncat(buf, "-fno-fast-math ", sizeof(buf) - strlen(buf) - 1);
			}
			break;
	}

	// 5. Sanitizers & Loop optimizations
	if (compiler.build.feature.sanitize_address)
	{
		strncat(buf, is_cl ? "/fsanitize=address " : "-fsanitize=address ", sizeof(buf) - strlen(buf) - 1);
	}
	if (!is_cl)
	{
		if (compiler.build.feature.sanitize_memory)
		{
			strncat(buf, "-fsanitize=memory ", sizeof(buf) - strlen(buf) - 1);
		}
		if (compiler.build.feature.sanitize_thread)
		{
			strncat(buf, "-fsanitize=thread ", sizeof(buf) - strlen(buf) - 1);
		}
		if (compiler.build.unroll_loops == UNROLL_LOOPS_ON)
		{
			strncat(buf, "-funroll-loops ", sizeof(buf) - strlen(buf) - 1);
		}
		if (compiler.build.loop_vectorization == VECTORIZATION_ON)
		{
			strncat(buf, "-ftree-vectorize ", sizeof(buf) - strlen(buf) - 1);
		}
		else if (compiler.build.loop_vectorization == VECTORIZATION_OFF)
		{
			strncat(buf, "-fno-tree-vectorize ", sizeof(buf) - strlen(buf) - 1);
		}
	}

	// 6. Include directories
	FOREACH(const char *, inc, compiler.build.cinclude_dirs)
	{
		if (is_cl)
		{
			strncat(buf, "/I \"", sizeof(buf) - strlen(buf) - 1);
			strncat(buf, inc, sizeof(buf) - strlen(buf) - 1);
			strncat(buf, "\" ", sizeof(buf) - strlen(buf) - 1);
		}
		else
		{
			strncat(buf, "-I \"", sizeof(buf) - strlen(buf) - 1);
			strncat(buf, inc, sizeof(buf) - strlen(buf) - 1);
			strncat(buf, "\" ", sizeof(buf) - strlen(buf) - 1);
		}
	}

	// 7. User-provided cflags placed LAST so they can override any defaults
	if (compiler.build.cflags)
	{
		strncat(buf, compiler.build.cflags, sizeof(buf) - strlen(buf) - 1);
		strncat(buf, " ", sizeof(buf) - strlen(buf) - 1);
	}

	return str_dup(buf);
}

const char *c_codegen(void *context)
{
	GenContext *c = (GenContext *)context;
	if (!c)
	{
		return NULL;
	}
	if (c->current_module && !compiler_should_output_file(c->base_name))
	{
		return NULL;
	}

	if (compiler.build.emit_object_files || compiler.build.type == TARGET_TYPE_EXECUTABLE || compiler.build.type == TARGET_TYPE_STATIC_LIB || compiler.build.type == TARGET_TYPE_DYNAMIC_LIB || compiler.build.type == TARGET_TYPE_TEST || compiler.build.type == TARGET_TYPE_BENCHMARK)
	{
		const char *cc     = c_backend_cc ? c_backend_cc : (compiler.build.cc ? compiler.build.cc : find_c_compiler());
		const char *cflags = c_backend_cflags ? c_backend_cflags : "";
		bool is_cl         = str_ends_with(cc, "cl.exe");
		char cmd[4096];
		if (is_cl)
		{
			snprintf(cmd, sizeof(cmd), "\"%s\" %s/c \"%s\" /Fo:\"%s\"", cc, cflags, c->c_filename, c->object_filename);
		}
		else
		{
			snprintf(cmd, sizeof(cmd), "\"%s\" %s-c \"%s\" -o \"%s\"", cc, cflags, c->c_filename, c->object_filename);
		}
		if (compiler.build.print_linking)
		{
			puts(cmd);
		}
		DEBUG_LOG("Compiling c sources using '%s'", cmd);
		if (system(cmd) != 0)
		{
			fprintf(stderr, "Failed to compile '%s' using command '%s'.\n", c->c_filename, cmd);
			exit(EXIT_FAILURE);
		}
		return c->object_filename;
	}
	return NULL;
}

void **c_gen(Module **modules, int module_count)
{
	if (!module_count)
	{
		return NULL;
	}
	const char *dir = compiler.build.object_file_dir ? compiler.build.object_file_dir : ".";
	c_emit_runtime_header(dir);
	c_emit_shared_header(dir);

	// Pre-calculate on the main thread before starting worker threads
	c_backend_cc     = compiler.build.cc ? compiler.build.cc : find_c_compiler();
	c_backend_cflags = c_backend_build_cflags(c_backend_cc);

	GenContext **gen_contexts = NULL;
	GenContext *rt            = c_emit_runtime_c(dir);
	if (rt)
	{
		vec_add(gen_contexts, rt);
	}

	if (compiler.build.single_module == SINGLE_MODULE_ON)
	{
		GenContext *c = c_gen_module(NULL, 0);
		if (c)
		{
			vec_add(gen_contexts, c);
		}
		return (void **)gen_contexts;
	}

	for (int i = 0; i < module_count; i++)
	{
		GenContext *c = c_gen_module(modules[i], i);
		if (c)
		{
			vec_add(gen_contexts, c);
		}
	}
	return (void **)gen_contexts;
}