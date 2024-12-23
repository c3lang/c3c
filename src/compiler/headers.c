// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

#define PRINTF(x, ...) fprintf(c->file, x, ## __VA_ARGS__) /* NOLINT */
#define INDENT() indent_line(c->file, indent)

typedef enum
{
	GEN_DONE,
	GEN_FULL,
	GEN_POINTER,
	GEN_DEFINITION,
} GenType;

typedef struct HeaderContext__
{
	FILE *file;
	HTable *gen_decl;
	HTable *gen_def;
	Decl **type_queue;
} HeaderContext;

static void header_ensure_member_types_exist(HeaderContext *c, Decl **members);
static void header_gen_struct_union(HeaderContext *c, int indent, Decl *decl);
static void header_gen_maybe_generate_type(HeaderContext *c, Type *type, bool is_pointer);
static GenType try_gen(HeaderContext *c, Decl *decl, GenType gen);

INLINE bool header_try_gen_definition(HeaderContext *c, Type *type)
{
	if (htable_get(c->gen_def, type) != NULL) return false;
	htable_set(c->gen_def, type, type);
	return true;
}

INLINE bool header_try_gen_decl(HeaderContext *c, Type *type)
{
	if (htable_get(c->gen_decl, type) != NULL) return false;
	htable_set(c->gen_decl, type, type);
	return true;
}
INLINE bool header_try_gen_both(HeaderContext *c, Type *type)
{
	if (header_try_gen_definition(c, type))
	{
		bool success = header_try_gen_decl(c, type);
		ASSERT0(success);
		return true;
	}
	return false;
}


INLINE const char *decl_get_extname(Decl *decl)
{
	if (!decl->extname)
	{
		decl->is_export = true;
		scratch_buffer_set_extern_decl_name(decl, true);
		decl->extname = scratch_buffer_copy();
	}
	return decl->extname;
}

static bool type_is_func_pointer(Type *type)
{
	if (type->type_kind != TYPE_DISTINCT && type->type_kind != TYPE_TYPEDEF) return false;
	type = type_flatten(type);
	return type->type_kind == TYPE_FUNC_PTR;
}

static void indent_line(FILE *file, int indent)
{
	for (int i = 0; i < indent; i++)
	{
		fputc('\t', file);
	}
}

static const char *struct_union_str(Decl *decl)
{
	return decl->decl_kind == DECL_UNION ? "union" : "struct";
}

static void header_print_type(HeaderContext *c, Type *type)
{
	if (type_is_func_pointer(type))
	{
		PRINTF("%s", decl_get_extname(type->decl));
		return;
	}
	ASSERT0(!type_is_optional(type));
	switch (type->type_kind)
	{
		case CT_TYPES:
		case TYPE_OPTIONAL:
			UNREACHABLE
		case TYPE_VOID:
			PRINTF("void");
			return;
		case TYPE_BOOL:
			PRINTF("bool");
			return;
		case TYPE_I8:
			PRINTF("int8_t");
			return;
		case TYPE_I16:
			PRINTF("int16_t");
			return;
		case TYPE_I32:
			PRINTF("int32_t");
			return;
		case TYPE_I64:
			PRINTF("int64_t");
			return;
		case TYPE_I128:
			PRINTF("__int128");
			return;
		case TYPE_U8:
			PRINTF("uint8_t");
			return;
		case TYPE_U16:
			PRINTF("uint16_t");
			return;
		case TYPE_U32:
			PRINTF("uint32_t");
			return;
		case TYPE_U64:
			PRINTF("uint64_t");
			return;
		case TYPE_U128:
			PRINTF("unsigned __int128");
			return;
		case TYPE_BF16:
			PRINTF("__bf16");
			return;
		case TYPE_F16:
			PRINTF("__fp16");
			return;
		case TYPE_F32:
			PRINTF("float");
			return;
		case TYPE_F64:
			PRINTF("double");
			return;
		case TYPE_F128:
			PRINTF("__float128");
			return;
		case TYPE_TYPEID:
			PRINTF("c3typeid_t");
			return;
		case TYPE_POINTER:
			header_print_type(c, type->pointer);
			PRINTF("*");
			return;
		case TYPE_FUNC_PTR:
			type = type->pointer;
			FALLTHROUGH;
		case TYPE_FUNC_RAW:
			PRINTF("%s", decl_get_extname(type->function.decl));
			return;
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ENUM:
			PRINTF("%s", decl_get_extname(type->decl));
			return;
		case TYPE_BITSTRUCT:
			if (type->decl->is_export)
			{
				PRINTF("%s", decl_get_extname(type->decl));
				return;
			}
			header_print_type(c, type->decl->strukt.container_type->type);
			return;
		case TYPE_ANYFAULT:
		case TYPE_FAULTTYPE:
			PRINTF("c3fault_t");
			return;
		case TYPE_DISTINCT:
			if (type->decl->is_export)
			{
				PRINTF("%s", decl_get_extname(type->decl));
				return;
			}
			header_print_type(c, type->decl->distinct->type);
			return;
		case TYPE_TYPEDEF:
			if (type == type_usz) { PRINTF("size_t"); return; }
			if (type == type_isz) { PRINTF("ptrdiff_t"); return; }
			if (type == type_iptr) { PRINTF("intptr_t"); return; }
			if (type == type_uptr) { PRINTF("uintptr_t"); return; }
			if (type->decl->is_export)
			{
				PRINTF("%s", decl_get_extname(type->decl));
				return;
			}
			header_print_type(c, type->canonical);
			return;
		case TYPE_FLEXIBLE_ARRAY:
			UNREACHABLE
		case TYPE_ARRAY:
			PRINTF("struct { ");
			header_print_type(c, type->array.base);
			PRINTF(" arr[%d]; }", type->array.len);
			return;
		case TYPE_ANY:
		case TYPE_INTERFACE:
			PRINTF("c3any_t");
			return;
		case TYPE_SLICE:
			PRINTF("c3slice_t");
			return;
		case TYPE_VECTOR:
			switch (type_flatten(type->array.base)->type_kind)
			{
				case ALL_SIGNED_INTS:
					PRINTF("int");
					break;
				case ALL_UNSIGNED_INTS:
					PRINTF("uint");
					break;
				case ALL_FLOATS:
					PRINTF("float");
					break;
				default:
					UNREACHABLE;
			}
			PRINTF("%dx%d", (int)type_bit_size(type->array.base), type->array.len);
			return;
	}
}

static void header_gen_function_ptr(HeaderContext *c, Type *type)
{
	TypeFunction *fun = &type_flatten(type)->pointer->function;
	Signature *sig = fun->signature;
	Type *rtype = typeget(sig->rtype);
	Type *extra_ret = NULL;
	if (type_is_optional(rtype))
	{
		extra_ret = rtype->optional;
		header_gen_maybe_generate_type(c, extra_ret, false);
		rtype = type_anyfault;
	}
	header_gen_maybe_generate_type(c, rtype, false);
	FOREACH(Decl *, param, sig->params)
	{
		header_gen_maybe_generate_type(c, param->type, false);
	}

	PRINTF("typedef ");
	header_print_type(c, rtype);
	PRINTF("(*%s)(", decl_get_extname(type->decl));
	if (!vec_size(sig->params) && !extra_ret)
	{
		PRINTF("void);\n");
		return;
	}
	if (extra_ret)
	{
		header_print_type(c, type_get_ptr(extra_ret));
		PRINTF(" return_ref");
	}
	FOREACH_IDX(i, Decl *, param, sig->params)
	{
		if (i || extra_ret) PRINTF(", ");
		header_print_type(c, param->type);
		if (param->name) PRINTF(" %s", param->name);
	}
	PRINTF(");\n");
}

static void header_gen_function(HeaderContext *c, Decl *decl, bool print_fn, bool* fn_found)
{
	if (!decl->is_export) return;
	const char *ext_name = decl_get_extname(decl);
	if (ext_name[0] == '_' && ext_name[1] == '_') return;
	if (print_fn && !*fn_found)
	{
		*fn_found = true;
		if (decl->func_decl.type_parent)
		{
			PRINTF("\n/* METHODS */\n");
		}
		else
		{
			PRINTF("\n/* FUNCTIONS */\n");
		}
	}
	Signature *sig = &decl->func_decl.signature;
	Type *rtype = typeget(sig->rtype);
	Type *extra_ret = NULL;
	if (type_is_optional(rtype))
	{
		extra_ret = rtype->optional;
		if (!print_fn) header_gen_maybe_generate_type(c, extra_ret, false);
		rtype = type_anyfault;
	}
	if (!print_fn) header_gen_maybe_generate_type(c, rtype, false);
	if (print_fn)
	{
		PRINTF("extern ");
		header_print_type(c, rtype);
		PRINTF(" %s(", decl_get_extname(decl));
		if (!vec_size(sig->params) && !extra_ret)
		{
			PRINTF("void);\n");
			return;
		}
		if (extra_ret)
		{
			header_print_type(c, type_get_ptr(extra_ret));
			PRINTF(" return_ref");
		}
	}
	FOREACH_IDX(i, Decl *, param, sig->params)
	{
		if (print_fn)
		{
			if (i || extra_ret) PRINTF(", ");
			header_print_type(c, param->type);
			if (param->name) PRINTF(" %s", param->name);
		}
		else
		{
			header_gen_maybe_generate_type(c, param->type, false);
		}
	}
	if (print_fn) PRINTF(");\n");
}

static void header_gen_members(HeaderContext *c, int indent, Decl **members)
{
	int i = 0;
	FOREACH(Decl *,member, members)
	{
		Type *type = type_flatten_no_export(member->type);
		switch (member->decl_kind)
		{
			case DECL_VAR:
				INDENT();
				switch (type->type_kind)
				{
					case TYPE_ARRAY:
						header_print_type(c, type->array.base);
						PRINTF(" %s[%d];\n", member->name, type->array.len);
						break;
					case TYPE_FLEXIBLE_ARRAY:
						header_print_type(c, type->array.base);
						PRINTF(" %s[];\n", member->name);
						break;
					default:
						header_print_type(c, member->type);
						PRINTF(" %s;\n", member->name);
						break;
				}
				break;
			case DECL_STRUCT:
			case DECL_UNION:
				header_gen_struct_union(c, indent, member);
				break;
			case DECL_BITSTRUCT:
				INDENT();
				header_print_type(c, member->strukt.container_type->type->canonical);
				if (member->name)
				{
					PRINTF(" %s;\n", member->name);
					break;
				}
				PRINTF(" __bits%d;\n", ++i);
				break;
			default:
				UNREACHABLE
		}
	}
}

static void header_gen_struct_union_top(HeaderContext *c, Decl *decl, GenType gen_type)
{
	gen_type = try_gen(c, decl, gen_type);
	if (gen_type == GEN_DONE) return;
	if (gen_type != GEN_DEFINITION)
	{
		PRINTF("typedef %s %s__ %s;\n", struct_union_str(decl), decl_get_extname(decl), decl_get_extname(decl));
	}
	if (gen_type == GEN_POINTER)
	{
		vec_add(c->type_queue, decl);
		return;
	}
	header_ensure_member_types_exist(c, decl->strukt.members);
	PRINTF("%s %s__\n", struct_union_str(decl), decl->extname);
	PRINTF("{\n");
	header_gen_members(c, 1, decl->strukt.members);
	PRINTF("};\n");
}

static void header_gen_struct_union(HeaderContext *c, int indent, Decl *decl)
{
	if (!indent)
	{
		PRINTF("typedef %s %s__ %s;\n", struct_union_str(decl), decl->extname, decl->extname);
	}
	INDENT();
	if (decl->name)
	{
		PRINTF("%s %s__\n", struct_union_str(decl), decl->extname);
	}
	else
	{
		PRINTF("%s\n", struct_union_str(decl));
	}
	INDENT();
	PRINTF("{\n");
	header_gen_members(c, indent + 1, decl->strukt.members);
	INDENT();
	PRINTF("};\n");
}


static GenType try_gen(HeaderContext *c, Decl *decl, GenType gen)
{
	Type *type = decl->type;
	switch (gen)
	{
		case GEN_FULL:
			if (header_try_gen_decl(c, type))
			{
				header_try_gen_definition(c, type);
				return GEN_FULL;
			}
			FALLTHROUGH;
		case GEN_DEFINITION:
			if (!header_try_gen_definition(c, type)) return GEN_DONE;
			return GEN_DEFINITION;
		case GEN_DONE:
			UNREACHABLE
		case GEN_POINTER:
			if (!header_try_gen_decl(c, type)) return GEN_DONE;
			return GEN_POINTER;
	}
	UNREACHABLE
}
static void header_gen_enum(HeaderContext *c, int indent, Decl *decl)
{
	if (!header_try_gen_both(c, decl->type)) return;
	Type *underlying_type = decl->enums.type_info->type->canonical;
	if (underlying_type != type_cint->canonical)
	{
		PRINTF("typedef ");
		header_print_type(c, underlying_type);
		PRINTF(" %s;\n", decl_get_extname(decl));
		FOREACH_IDX(i, Decl *, enum_member, decl->enums.values)
		{
			PRINTF("%s %s_%s = %d;\n", decl_get_extname(decl), decl_get_extname(decl), enum_member->name, i);
		}
		return;
	}
	PRINTF("typedef enum %s__\n{\n", decl_get_extname(decl));
	FOREACH(Decl *, enum_member, decl->enums.values)
	{
		PRINTF("\t %s_%s,\n", decl_get_extname(decl), enum_member->name);
	}
	PRINTF("} %s;\n", decl_get_extname(decl));
}

static void header_ensure_member_types_exist(HeaderContext *c, Decl **members)
{
	FOREACH(Decl *, member, members)
	{
		switch (member->decl_kind)
		{
			case DECL_VAR:
				header_gen_maybe_generate_type(c, member->type, false);
				break;
			case DECL_STRUCT:
			case DECL_UNION:
				header_ensure_member_types_exist(c, member->strukt.members);
				break;
			case DECL_BITSTRUCT:
				break;
			default:
				UNREACHABLE
		}
	}
}
static void header_gen_maybe_generate_type(HeaderContext *c, Type *type, bool is_pointer)
{
	if (type_is_func_pointer(type))
	{
		if (!header_try_gen_definition(c, type)) return;
		header_gen_function_ptr(c, type);
		return;
	}
RETRY:
	if (type_is_optional(type)) return;
	switch (type->type_kind)
	{
		case CT_TYPES:
		case TYPE_OPTIONAL:
			UNREACHABLE
		case TYPE_VOID:
		case TYPE_BOOL:
		case ALL_FLOATS:
		case ALL_INTS:
		case TYPE_ANYFAULT:
		case TYPE_TYPEID:
		case TYPE_FAULTTYPE:
		case TYPE_SLICE:
		case TYPE_ANY:
		case TYPE_INTERFACE:
			return;
		case TYPE_DISTINCT:
		{
			if (!header_try_gen_both(c, type)) return;
			Type *underlying_type = type->decl->distinct->type;
			header_gen_maybe_generate_type(c, underlying_type, is_pointer);
			PRINTF("typedef ");
			header_print_type(c, underlying_type);
			PRINTF(" %s;\n", decl_get_extname(type->decl));
			return;
		}
		case TYPE_TYPEDEF:
		{
			if (!header_try_gen_both(c, type)) return;
			Type *underlying_type = type->canonical;
			header_gen_maybe_generate_type(c, underlying_type, is_pointer);
			PRINTF("typedef ");
			header_print_type(c, underlying_type);
			PRINTF(" %s;\n", decl_get_extname(type->decl));
			return;
		}
		case TYPE_BITSTRUCT:
			{
				if (!header_try_gen_both(c, type)) return;
				Type *underlying_type = type->decl->strukt.container_type->type;
				header_gen_maybe_generate_type(c, underlying_type, is_pointer);
				PRINTF("typedef ");
				header_print_type(c, underlying_type);
				PRINTF(" %s;\n", decl_get_extname(type->decl));
				return;
			}
		case TYPE_POINTER:
			type = type->pointer;
			if (htable_get(c->gen_decl, type)) return;
			is_pointer = true;
			goto RETRY;
		case TYPE_FUNC_PTR:
			type = type->pointer;
			goto RETRY;
		case TYPE_ENUM:
			header_gen_enum(c, 0, type->decl);
			return;
		case TYPE_FUNC_RAW:
			UNREACHABLE
			return;
		case TYPE_STRUCT:
		case TYPE_UNION:
			header_gen_struct_union_top(c, type->decl, is_pointer ? GEN_POINTER : GEN_FULL);
			return;
		case TYPE_ARRAY:
			type = type->array.base;
			goto RETRY;
		case TYPE_FLEXIBLE_ARRAY:
			type = type->array.base;
			goto RETRY;
		case TYPE_VECTOR:
		{
			if (!header_try_gen_both(c, type)) return;
			PRINTF("typedef ");
			Type *flat_type = type_flatten(type->array.base);
			header_print_type(c, flat_type);
			PRINTF(" ");
			header_print_type(c, type);
			PRINTF(" __attribute__((vector_size(%d)));\n", (int)type_size(flat_type) * type->array.len);
			return;
		}
	}
}

static void header_gen_global_var(HeaderContext *c, Decl *decl, bool fn_globals, bool *globals_found)
{
	ASSERT0(decl->decl_kind == DECL_VAR);
	// Only exports.
	if (!decl->is_export) return;
	Type *type = decl->type->canonical;
	// Optionals are ignored.
	if (type_is_optional(type)) return;
	type = type_flatten_no_export(type);
	if (fn_globals && !*globals_found)
	{
		*globals_found = true;
		if (decl->var.kind == VARDECL_CONST)
		{
			PRINTF("\n/* CONSTANTS */\n");
		}
		else
		{
			PRINTF("\n/* GLOBALS */\n");
		}
	}
	// Flatten bitstructs.
	if (type->type_kind == TYPE_BITSTRUCT) type = type->decl->strukt.container_type->type->canonical;
	// We will lower some consts to defines, if they are:
	// 1. Not an address
	// 2. Not user defined (i.e. struct or union)
	// 3. Has an init method
	if (decl->var.kind == VARDECL_CONST && !decl->var.is_addr)
	{
		if (!fn_globals) return;
		Expr *init = decl->var.init_expr;
		Type *flat = type_flatten(type);
		if (type_is_arraylike(flat) || type_is_user_defined(flat) || !init) return;
		PRINTF("#define %s ", decl_get_extname(decl));
		ASSERT0(expr_is_const(init));
		switch (init->const_expr.const_kind)
		{
			case CONST_INTEGER:
				PRINTF("%s\n", int_to_str(init->const_expr.ixx, 10, false));
				return;
			case CONST_FLOAT:
				PRINTF("%.15g\n", init->const_expr.fxx.f);
				return;
			case CONST_BOOL:
				PRINTF("%s\n", init->const_expr.b ? "true" : "false");
				return;
			case CONST_REF:
				PRINTF("&%s\n", decl_get_extname(init->const_expr.global_ref));
				return;
			case CONST_POINTER:
				if (!init->const_expr.ptr)
				{
					PRINTF("(void*)0\n");
					return;
				}
				PRINTF("(void*)0x%llx\n", (unsigned long long)init->const_expr.ptr);
				return;
			case CONST_STRING:
				putc('\"', c->file);
				for (unsigned i = 0; i < init->const_expr.bytes.len; i++)
				{
					char ch = init->const_expr.bytes.ptr[i];
					if (ch >= ' ' && ch <= 127 && ch != '"')
					{
						fputc(ch, c->file);
						continue;
					}
					PRINTF("\\x%02x", ch);
				}
				PRINTF("\"\n");
				return;
			case CONST_ENUM:
			case CONST_ERR:
				PRINTF("%s\n", decl_get_extname(init->const_expr.enum_err_val));
				return;
			case CONST_SLICE:
			case CONST_TYPEID:
			case CONST_MEMBER:
			case CONST_INITIALIZER:
			case CONST_UNTYPED_LIST:
			case CONST_BYTES:
				UNREACHABLE
		}
	}
	if (!fn_globals)
	{
		header_gen_maybe_generate_type(c, decl->type, false);
		return;
	}
	header_print_type(c, decl->type);
	ASSERT0(decl->var.kind == VARDECL_GLOBAL || decl->var.kind == VARDECL_CONST);
	PRINTF("extern ");
	if (decl->var.kind == VARDECL_CONST) PRINTF("const ");
	PRINTF(" %s;\n", decl_get_extname(decl));
}

static void header_gen_global_decls(HeaderContext *c, Module **modules, unsigned module_count, bool fn_globals)
{
	bool constants_found = false;
	for (unsigned i = 0; i < module_count; i++)
	{
		Module *module = modules[i];
		// Produce all constants.
		FOREACH(CompilationUnit *, unit, module->units)
		{
			FOREACH(Decl *, var, unit->vars)
			{
				if (var->var.kind != VARDECL_CONST) continue;
				header_gen_global_var(c, var, fn_globals, &constants_found);
			}
		}
	}

	bool globals_found = false;
	for (unsigned i = 0; i < module_count; i++)
	{
		Module *module = modules[i];

		// Generate all globals
		FOREACH(CompilationUnit *, unit, module->units)
		{
			FOREACH(Decl *, var, unit->vars)
			{
				if (var->var.kind != VARDECL_GLOBAL) continue;
				header_gen_global_var(c, var, fn_globals, &globals_found);
			}
		}
	}

	bool functions_found = false;
	for (unsigned i = 0; i < module_count; i++)
	{
		Module *module = modules[i];

		// Generate all functions
		FOREACH(CompilationUnit *, unit, module->units)
		{
			FOREACH(Decl *, fn, unit->functions)
			{
				header_gen_function(c, fn, fn_globals, &functions_found);
			}
		}
	}

	bool methods_found = false;
	for (unsigned i = 0; i < module_count; i++)
	{
		// Ignore stdlib modules
		Module *module = modules[i];
		Module *top_module = module;
		while (top_module->parent_module) top_module = top_module->parent_module;
		if (top_module->name->module == kw_std) continue;
		// Generate all functions
		FOREACH(CompilationUnit *, unit, module->units)
		{
			FOREACH(Decl *, method, unit->methods)
			{
				header_gen_function(c, method, fn_globals, &methods_found);
			}
		}
	}

}
static void process_queue(HeaderContext *c)
{
	for (uint32_t i = 0; i < vec_size(c->type_queue); i++)
	{
		Decl *decl = c->type_queue[i];
		switch (decl->decl_kind)
		{
			case DECL_STRUCT:
			case DECL_UNION:
				header_gen_struct_union_top(c, decl, GEN_DEFINITION);
				break;
			default:
				UNREACHABLE
		}
	}
	vec_resize(c->type_queue, 0);
}
void header_gen(Module **modules, unsigned module_count)
{
	HTable table1, table2;
	htable_init(&table1, 1024);
	htable_init(&table2, 1024);
	const char *name = build_base_name();
	const char *filename = str_printf("%s.h", name);
	FILE *file = fopen(filename, "w");
	HeaderContext context = { .file = file, .gen_def = &table1, .gen_decl = &table2 };
	HeaderContext *c = &context;
	PRINTF("#include <stdint.h>\n");
	PRINTF("#include <stddef.h>\n");
	PRINTF("#include <stdbool.h>\n");
	PRINTF("#ifndef __c3__\n");
	PRINTF("#define __c3__\n\n");
	PRINTF("typedef void* c3typeid_t;\n");
	PRINTF("typedef void* c3fault_t;\n");
	PRINTF("typedef struct { void* ptr; size_t len; } c3slice_t;\n");
	PRINTF("typedef struct { void* ptr; c3typeid_t type; } c3any_t;\n");
	PRINTF("\n#endif\n\n");

	PRINTF("/* TYPES */\n");
	for (unsigned i = 0; i < module_count; i++)
	{
		Module *module = modules[i];
		// Produce all constants.
		FOREACH(CompilationUnit *, unit, module->units)
		{
			FOREACH(Decl *, type, unit->types)
			{
				if (!type->is_export) continue;
				header_gen_maybe_generate_type(c, type->type, false);
			}
		}
		process_queue(c);
	}
	process_queue(c);
	header_gen_global_decls(c, modules, module_count, false);
	process_queue(c);
	header_gen_global_decls(c, modules, module_count, true);
	fclose(file);

}

