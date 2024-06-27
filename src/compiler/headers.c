// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

#define PRINTF(x, ...) fprintf(file, x, ## __VA_ARGS__) /* NOLINT */
#define INDENT() indent_line(file, indent)
#define OUT(file, x, ...) fprintf(file, x, ## __VA_ARGS__) /* NOLINT */

static void header_gen_struct_union(FILE *file, int indent, Decl *decl);
static void header_gen_maybe_generate_type(FILE *file, HTable *table, Type *type);

INLINE const char *decl_get_extname(Decl *decl)
{
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

static void header_gen_type_decl(FILE *file, int indent, Decl *decl);

static void header_print_type(FILE *file, Type *type)
{
	if (type_is_func_pointer(type))
	{
		PRINTF("%s", decl_get_extname(type->decl));
		return;
	}
	assert(!type_is_optional(type));
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
			header_print_type(file, type->pointer);
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
			header_print_type(file, type->decl->bitstruct.base_type->type);
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
			header_print_type(file, type->decl->distinct->type);
			return;
		case TYPE_TYPEDEF:
			if (!type->decl)
			{
				if (type == type_usz) { PRINTF("size_t"); return; }
				if (type == type_isz) { PRINTF("ptrdiff_t"); return; }
				if (type == type_iptr) { PRINTF("intptr_t"); return; }
				if (type == type_uptr) { PRINTF("uintptr_t"); return; }
				header_print_type(file, type->canonical);
				return;
			}
			if (type->decl->is_export)
			{
				PRINTF("%s", decl_get_extname(type->decl));
				return;
			}
			header_print_type(file, type->canonical);
			return;
		case TYPE_FLEXIBLE_ARRAY:
			UNREACHABLE
		case TYPE_ARRAY:
			PRINTF("struct { ");
			header_print_type(file, type->array.base);
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

static void header_gen_function_ptr(FILE *file, HTable *table, Type *type)
{
	TypeFunction *fun = &type_flatten(type)->pointer->function;
	Signature *sig = fun->signature;
	Type *rtype = typeget(sig->rtype);
	Type *extra_ret = NULL;
	if (type_is_optional(rtype))
	{
		extra_ret = rtype->optional;
		header_gen_maybe_generate_type(file, table, extra_ret);
		rtype = type_anyfault;
	}
	header_gen_maybe_generate_type(file, table, rtype);
	FOREACH_BEGIN_IDX(i, Decl *param, sig->params)
		header_gen_maybe_generate_type(file, table, param->type);
	FOREACH_END();

	PRINTF("typedef ");
	header_print_type(file, rtype);
	PRINTF("(*%s)(", decl_get_extname(type->decl));
	if (!vec_size(sig->params) && !extra_ret)
	{
		PRINTF("void);\n");
		return;
	}
	if (extra_ret)
	{
		header_print_type(file, type_get_ptr(extra_ret));
		PRINTF(" return_ref");
	}
	FOREACH_BEGIN_IDX(i, Decl *param, sig->params)
		if (i || extra_ret) PRINTF(", ");
		header_print_type(file, param->type);
		if (param->name) PRINTF(" %s", param->name);
	FOREACH_END();
	PRINTF(");\n");
}

static void header_gen_function(FILE *file, HTable *table, Decl *decl, bool print_fn, bool* fn_found)
{
	if (!decl->is_export) return;
	if (decl->extname[0] == '_' && decl->extname[1] == '_') return;
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
		if (!print_fn) header_gen_maybe_generate_type(file, table, extra_ret);
		rtype = type_anyfault;
	}
	if (!print_fn) header_gen_maybe_generate_type(file, table, rtype);
	if (print_fn)
	{
		PRINTF("extern ");
		header_print_type(file, rtype);
		PRINTF(" %s(", decl_get_extname(decl));
		if (!vec_size(sig->params) && !extra_ret)
		{
			PRINTF("void);\n");
			return;
		}
		if (extra_ret)
		{
			header_print_type(file, type_get_ptr(extra_ret));
			PRINTF(" return_ref");
		}
	}
	FOREACH_BEGIN_IDX(i, Decl *param, sig->params)
		if (print_fn)
		{
			if (i || extra_ret) PRINTF(", ");
			header_print_type(file, param->type);
			if (param->name) PRINTF(" %s", param->name);
		}
		else
		{
			header_gen_maybe_generate_type(file, table, param->type);
		}
	FOREACH_END();
	if (print_fn) PRINTF(");\n");
}

static void header_gen_members(FILE *file, int indent, Decl **members)
{
	VECEACH(members, i)
	{
		Decl *member = members[i];
		Type *type = type_flatten_no_export(member->type);
		switch (member->decl_kind)
		{
			case DECL_VAR:
				INDENT();
				switch (type->type_kind)
				{
					case TYPE_ARRAY:
						header_print_type(file, type->array.base);
						PRINTF(" %s[%d];\n", member->name, type->array.len);
						break;
					case TYPE_FLEXIBLE_ARRAY:
						header_print_type(file, type->array.base);
						PRINTF(" %s[];\n", member->name);
						break;
					default:
						header_print_type(file, member->type);
						PRINTF(" %s;\n", member->name);
						break;
				}
				break;
			case DECL_STRUCT:
			case DECL_UNION:
				header_gen_struct_union(file, indent, member);
				break;
			case DECL_BITSTRUCT:
				INDENT();
				header_print_type(file, member->bitstruct.base_type->type->canonical);
				if (member->name)
				{
					PRINTF(" %s;\n", member->name);
					break;
				}
				PRINTF(" __bits%d;\n", i);
				break;
			default:
				UNREACHABLE
		}
	}
}
static void header_gen_struct_union(FILE *file, int indent, Decl *decl)
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
	header_gen_members(file, indent + 1, decl->strukt.members);
	INDENT();
	PRINTF("};\n");
}


static void header_gen_enum(FILE *file, int indent, Decl *decl)
{
	TODO
}

static void header_gen_err(FILE *file, int indent, Decl *decl)
{
	PRINTF("typedef struct %s_error__ %s_error;\n", decl_get_extname(decl), decl_get_extname(decl));
	PRINTF("struct %s_error__\n{\n", decl_get_extname(decl));
	header_gen_members(file, indent, decl->strukt.members);
	PRINTF("};\n");
}


static void header_gen_type_decl(FILE *file, int indent, Decl *decl)
{
	switch (decl->decl_kind)
	{
		case NON_TYPE_DECLS:
		case DECL_FUNC:
		case DECL_FNTYPE:
			UNREACHABLE
		case DECL_ERASED:
			return;
		case DECL_BITSTRUCT:
			header_print_type(file, decl->bitstruct.base_type->type);
			return;
		case DECL_TYPEDEF:
		case DECL_DISTINCT:
		case DECL_INTERFACE:
			// Ignore
			return;
		case DECL_STRUCT:
		case DECL_UNION:
			header_gen_struct_union(file, indent, decl);
			return;
		case DECL_ENUM:
			header_gen_enum(file, indent, decl);
			return;
		case DECL_FAULT:
			header_gen_err(file, indent, decl);
			return;
	}
	UNREACHABLE
}

void header_ensure_member_types_exist(FILE *file, HTable *table, Decl **members)
{
	FOREACH_BEGIN(Decl *member, members)
		switch (member->decl_kind)
		{
			case DECL_VAR:
				header_gen_maybe_generate_type(file, table, member->type);
				break;
			case DECL_STRUCT:
			case DECL_UNION:
				header_ensure_member_types_exist(file, table, member->strukt.members);
				break;
			case DECL_BITSTRUCT:
				break;
			default:
				UNREACHABLE
		}
	FOREACH_END();
}
static void header_gen_maybe_generate_type(FILE *file, HTable *table, Type *type)
{
	if (type_is_func_pointer(type))
	{
		if (htable_get(table, type)) return;
		htable_set(table, type, type);
		header_gen_function_ptr(file, table, type);
		return;
	}
RETRY:
	if (type_is_optional(type)) return;
	switch (type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_INFERRED_ARRAY:
		case TYPE_UNTYPED_LIST:
		case TYPE_TYPEINFO:
		case TYPE_MEMBER:
		case TYPE_INFERRED_VECTOR:
		case TYPE_WILDCARD:
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
			if (htable_get(table, type)) return;
			Type *underlying_type = type->decl->distinct->type;
			htable_set(table, type, type);
			header_gen_maybe_generate_type(file, table, underlying_type);
			PRINTF("typedef ");
			header_print_type(file, underlying_type);
			PRINTF(" %s;\n", decl_get_extname(type->decl));
			return;
		}
		case TYPE_TYPEDEF:
		{
			if (!type->decl)
			{
				type = type->canonical;
				goto RETRY;
			}
			if (htable_get(table, type)) return;
			htable_set(table, type, type);
			Type *underlying_type = type->canonical;
			header_gen_maybe_generate_type(file, table, underlying_type);
			PRINTF("typedef ");
			header_print_type(file, underlying_type);
			PRINTF(" %s;\n", decl_get_extname(type->decl));
			return;
		}
		case TYPE_BITSTRUCT:
			{
				if (htable_get(table, type)) return;
				htable_set(table, type, type);
				Type *underlying_type = type->decl->bitstruct.base_type->type;
				header_gen_maybe_generate_type(file, table, underlying_type);
				PRINTF("typedef ");
				header_print_type(file, underlying_type);
				PRINTF(" %s;\n", decl_get_extname(type->decl));
				return;
			}
		case TYPE_POINTER:
		case TYPE_FUNC_PTR:
			type = type->pointer;
			goto RETRY;
		case TYPE_ENUM:
			if (htable_get(table, type)) return;
			{
				Decl *decl = type->decl;
				htable_set(table, type, type);
				Type *underlying_type = decl->enums.type_info->type->canonical;
				if (underlying_type == type_cint->canonical)
				{
					PRINTF("typedef enum %s__\n{\n", decl_get_extname(decl));
					FOREACH_BEGIN(Decl *enum_member, decl->enums.values)
						PRINTF("\t %s_%s,\n", decl_get_extname(decl), enum_member->name);
					FOREACH_END();
					PRINTF("} %s;\n", decl_get_extname(decl));
					return;
				}
				PRINTF("typedef ");
				header_print_type(file, underlying_type);
				PRINTF(" %s;\n", decl_get_extname(decl));
				FOREACH_BEGIN_IDX(i, Decl *enum_member, decl->enums.values)
					PRINTF("%s %s_%s = %d;\n", decl_get_extname(decl), decl_get_extname(decl), enum_member->name, i);
				FOREACH_END();
				return;
			}
		case TYPE_FUNC_RAW:
			UNREACHABLE
			return;
		case TYPE_STRUCT:
		case TYPE_UNION:
			if (htable_get(table, type)) return;
			{
				Decl *decl = type->decl;
				PRINTF("typedef %s %s__ %s;\n", struct_union_str(decl), decl_get_extname(decl), decl_get_extname(decl));
				htable_set(table, type, type);
				header_ensure_member_types_exist(file, table, decl->strukt.members);
				PRINTF("%s %s__\n", struct_union_str(decl), decl->extname);
				PRINTF("{\n");
				header_gen_members(file, 1, decl->strukt.members);
				PRINTF("};\n");
				return;
			}
		case TYPE_ARRAY:
			type = type->array.base;
			goto RETRY;
		case TYPE_FLEXIBLE_ARRAY:
			type = type->array.base;
			goto RETRY;
		case TYPE_VECTOR:
		{
			if (htable_get(table, type)) return;
			PRINTF("typedef ");
			Type *flat_type = type_flatten(type->array.base);
			header_print_type(file, flat_type);
			htable_set(table, type, type);
			PRINTF(" ");
			header_print_type(file, type);
			PRINTF(" __attribute__((vector_size(%d)));\n", (int)type_size(flat_type) * type->array.len);
			return;
		}
	}
}

static void header_gen_global_var(FILE *file, HTable *table, Decl *decl, bool fn_globals, bool *globals_found)
{
	assert(decl->decl_kind == DECL_VAR);
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
	if (type->type_kind == TYPE_BITSTRUCT) type = type->decl->bitstruct.base_type->type->canonical;
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
		assert(expr_is_const(init));
		switch (init->const_expr.const_kind)
		{
			case CONST_INTEGER:
				PRINTF("%s\n", int_to_str(init->const_expr.ixx, 10));
				return;
			case CONST_FLOAT:
				PRINTF("%.15g\n", init->const_expr.fxx.f);
				return;
			case CONST_BOOL:
				PRINTF("%s\n", init->const_expr.b ? "true" : "false");
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
				putc('\"', file);
				for (unsigned i = 0; i < init->const_expr.bytes.len; i++)
				{
					char ch = init->const_expr.bytes.ptr[i];
					if (ch >= ' ' && ch <= 127 && ch != '"')
					{
						fputc(ch, file);
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
		header_gen_maybe_generate_type(file, table, decl->type);
		return;
	}
	header_print_type(file, decl->type);
	assert(decl->var.kind == VARDECL_GLOBAL || decl->var.kind == VARDECL_CONST);
	PRINTF("extern ");
	if (decl->var.kind == VARDECL_CONST) PRINTF("const ");
	PRINTF(" %s;\n", decl_get_extname(decl));
}

static void header_gen_global_decls(FILE *file, HTable *table, Module **modules, unsigned module_count, bool fn_globals)
{
	bool constants_found = false;
	for (unsigned i = 0; i < module_count; i++)
	{
		Module *module = modules[i];
		// Produce all constants.
		FOREACH_BEGIN(CompilationUnit *unit, module->units)
			FOREACH_BEGIN(Decl *var, unit->vars)
				if (var->var.kind != VARDECL_CONST) continue;
				header_gen_global_var(file, table, var, fn_globals, &constants_found);
			FOREACH_END();
		FOREACH_END();
	}

	bool globals_found = false;
	for (unsigned i = 0; i < module_count; i++)
	{
		Module *module = modules[i];

		// Generate all globals
		FOREACH_BEGIN(CompilationUnit *unit, module->units)
			FOREACH_BEGIN(Decl *var, unit->vars)
				if (var->var.kind != VARDECL_GLOBAL) continue;
				header_gen_global_var(file, table, var, fn_globals, &globals_found);
			FOREACH_END();
		FOREACH_END();
	}

	bool functions_found = false;
	for (unsigned i = 0; i < module_count; i++)
	{
		Module *module = modules[i];

		// Generate all functions
		FOREACH_BEGIN(CompilationUnit *unit, module->units)
			FOREACH_BEGIN(Decl *fn, unit->functions)
				header_gen_function(file, table, fn, fn_globals, &functions_found);
			FOREACH_END();
		FOREACH_END();
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
		FOREACH_BEGIN(CompilationUnit *unit, module->units)
			FOREACH_BEGIN(Decl *method, unit->methods)
				header_gen_function(file, table, method, fn_globals, &methods_found);
			FOREACH_END();
		FOREACH_END();
	}

}
void header_gen(Module **modules, unsigned module_count)
{
	HTable table;
	htable_init(&table, 1024);
	const char *name = build_base_name();
	const char *filename = str_printf("%s.h", name);
	FILE *file = fopen(filename, "w");
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
		FOREACH_BEGIN(CompilationUnit *unit, module->units)
			FOREACH_BEGIN(Decl *type, unit->types)
				if (!type->is_export) continue;
				header_gen_maybe_generate_type(file, &table, type->type);
			FOREACH_END();
		FOREACH_END();
	}
	header_gen_global_decls(file, &table, modules, module_count, false);
	header_gen_global_decls(file, &table, modules, module_count, true);
	fclose(file);

}

