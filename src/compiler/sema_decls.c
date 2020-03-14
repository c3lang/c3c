// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"


static inline bool sema_analyse_error(Context *context __unused, Decl *decl)
{
	Decl **constants = decl->error.error_constants;
	unsigned size = vec_size(constants);
	if (size > MAX_ERRORS)
	{
		SEMA_ERROR(decl, "More than %d errors declared in a single error type.", MAX_ERRORS);
		return false;
	}
	bool success = true;
	for (unsigned i = 0; i < size; i++)
	{
		Decl *constant = constants[i];
		for (unsigned j = 0; j < i; j++)
		{
			if (constant->name == constants[j]->name)
			{
				SEMA_ERROR(constant, "Duplicate error names, please remove one of them.");
				SEMA_PREV(constants[j], "The previous declaration was here.");
				decl_poison(constant);
				decl_poison(constants[j]);
				success = false;
				break;
			}
		}
		constant->error_constant.value = i;
		constant->resolve_status = RESOLVE_DONE;
	}
	return success;
}


static inline bool sema_analyse_struct_member(Context *context, Decl *decl)
{
	if (decl->decl_kind == DECL_STRUCT || decl->decl_kind == DECL_UNION)
	{
		DEBUG_LOG("Beginning analysis of inner struct/union");
		VECEACH(decl->strukt.members, i)
		{
			Decl *member = decl->strukt.members[i];
			if (!decl_ok(member))
			{
				decl_poison(decl);
				continue;
			}
			if (!sema_analyse_struct_member(context, decl->strukt.members[i]))
			{
				if (decl_ok(decl))
				{
					decl_poison(decl);
					continue;
				}
				decl_poison(decl);
			}
		}
		DEBUG_LOG("Analysis complete.");
		return decl_ok(decl);
	}
	assert(decl->decl_kind == DECL_VAR);
	assert(decl->var.kind == VARDECL_MEMBER);
	assert(!decl->var.init_expr);
	if (!sema_resolve_type_info(context, decl->var.type_info))
	{
		decl_poison(decl);
		return false;
	}
	decl->type = decl->var.type_info->type;
	assert(decl->var.type_info->type);
	return true;
}

static inline bool sema_analyse_struct_union(Context *context, Decl *decl)
{
	DEBUG_LOG("Beginning analysis of %s.", decl->name);
	assert(decl->decl_kind == DECL_STRUCT || decl->decl_kind == DECL_UNION);
	VECEACH(decl->strukt.members, i)
	{
		Decl *member = decl->strukt.members[i];
		if (!decl_ok(member))
		{
			decl_poison(decl);
			continue;
		}
		if (!sema_analyse_struct_member(context, decl->strukt.members[i]))
		{
			if (decl_ok(decl))
			{
				decl_poison(decl);
				continue;
			}
			decl_poison(decl);
		}
	}
	DEBUG_LOG("Analysis complete.");
	return decl_ok(decl);
}


static inline bool sema_analyse_function_param(Context *context, Decl *param, bool is_function)
{
	assert(param->decl_kind == DECL_VAR);
	assert(param->var.kind == VARDECL_PARAM);
	if (!sema_resolve_type_info(context, param->var.type_info))
	{
		return false;
	}
	param->type = param->var.type_info->type;
	if (param->var.init_expr && !is_function)
	{
		SEMA_ERROR(param->var.init_expr, "Function types may not have default arguments.");
		return false;
	}
	if (param->var.init_expr)
	{
		Expr *expr = param->var.init_expr;
		if (!sema_analyse_expr(context, param->type, expr)) return false;
		if (expr->expr_kind != EXPR_CONST)
		{
			SEMA_ERROR(expr, "Only constant expressions may be used as default values.");
			return false;
		}
	}
	return true;
}

static inline Type *sema_analyse_function_signature(Context *context, FunctionSignature *signature, bool is_function)
{
	char buffer[MAX_FUNCTION_SIGNATURE_SIZE + 200];
	size_t buffer_write_offset = 0;
	bool all_ok = true;
	all_ok = sema_resolve_type_info(context, signature->rtype) && all_ok;
	if (all_ok)
	{
		type_append_signature_name(signature->rtype->type, buffer, &buffer_write_offset);
		buffer[buffer_write_offset++] = '(';
	}
	if (vec_size(signature->params) > MAX_PARAMS)
	{
		SEMA_ERROR(signature->params[MAX_PARAMS], "Number of params exceeds %d which is unsupported.", MAX_PARAMS);
		return false;
	}
	STable *names = &context->scratch_table;
	stable_clear(names);

	VECEACH(signature->params, i)
	{
		Decl *param = signature->params[i];
		assert(param->resolve_status == RESOLVE_NOT_DONE);
		param->resolve_status = RESOLVE_RUNNING;
		if (!sema_analyse_function_param(context, param, is_function))
		{
			decl_poison(param);
			all_ok = false;
			continue;
		}
		param->resolve_status = RESOLVE_DONE;
		if (i > 0 && all_ok)
		{
			buffer[buffer_write_offset++] = ',';
		}
		type_append_signature_name(param->var.type_info->type, buffer, &buffer_write_offset);
		if (param->name)
		{
			Decl *prev = stable_set(names, param->name, param);
			if (prev)
			{
				SEMA_ERROR(param, "Duplicate parameter name %s.", param->name);
				SEMA_PREV(prev, "Previous use of the name was here.");
				decl_poison(prev);
				decl_poison(param);
				all_ok = false;
			}
		}
	}
	if (signature->variadic)
	{
		buffer[buffer_write_offset++] = ',';
		buffer[buffer_write_offset++] = '.';
		buffer[buffer_write_offset++] = '.';
		buffer[buffer_write_offset++] = '.';
	}
	buffer[buffer_write_offset++] = ')';
	if (signature->throw_any)
	{
		assert(!signature->throws);
		buffer[buffer_write_offset++] = '!';
	}
	if (vec_size(signature->throws))
	{
		buffer[buffer_write_offset++] = '!';
		VECEACH(signature->throws, i)
		{
			Decl *err_decl = signature->throws[i];
			if (!sema_analyse_decl(context, err_decl))
			{
				continue;
			}
			if (i > 0 && all_ok)
			{
				buffer[buffer_write_offset++] = '|';
			}
			type_append_signature_name(err_decl->type, buffer, &buffer_write_offset);
		}
	}
	if (!all_ok) return NULL;
	TokenType type = TOKEN_INVALID_TOKEN;
	signature->mangled_signature = symtab_add(buffer, buffer_write_offset, fnv1a(buffer, buffer_write_offset), &type);
	Type *func_type = stable_get(&context->local_symbols, signature->mangled_signature);
	if (!func_type)
	{
		func_type = type_new(TYPE_FUNC, signature->mangled_signature);
		func_type->canonical = func_type;
		func_type->func.signature = signature;
		stable_set(&context->local_symbols, signature->mangled_signature, func_type);
	}
	return func_type;

}

static inline bool sema_analyse_typedef(Context *context, Decl *decl)
{
	if (decl->typedef_decl.is_func)
	{
		Type *func_type = sema_analyse_function_signature(context, &decl->typedef_decl.function_signature, false);
		if (!func_type) return false;
		decl->type->canonical = func_type;
		return true;
	}
	if (!sema_resolve_type_info(context, decl->typedef_decl.type_info)) return false;
	decl->type->canonical = decl->typedef_decl.type_info->type;
	// Do we need anything else?
	return true;
}

static inline bool sema_analyse_enum(Context *context, Decl *decl)
{
	if (!sema_resolve_type_info(context, decl->enums.type_info)) return false;
	uint64_t value = 0;
	Type *type = decl->enums.type_info->type;
	bool success = true;
	VECEACH(decl->enums.values, i)
	{
		Decl *enum_value = decl->enums.values[i];
		enum_value->enum_constant.ordinal = i;
		assert(enum_value->resolve_status == RESOLVE_NOT_DONE);
		assert(enum_value->decl_kind == DECL_ENUM_CONSTANT);
		enum_value->resolve_status = RESOLVE_RUNNING;
		Expr *expr = enum_value->enum_constant.expr;
		if (!expr)
		{
			expr = expr_new(EXPR_CONST, INVALID_RANGE);
			expr->type = type;
			expr->resolve_status = RESOLVE_DONE;
			expr->const_expr.type = CONST_INT;
			expr->const_expr.i = value;
			enum_value->enum_constant.expr = expr;
		}
		if (!sema_analyse_expr(context, type, expr))
		{
			success = false;
			enum_value->resolve_status = RESOLVE_DONE;
			continue;
		}
		assert(type_is_integer(expr->type->canonical));
		if (expr->expr_kind != EXPR_CONST)
		{
			SEMA_ERROR(expr, "Expected a constant expression for enum");
			success = false;
		}
		enum_value->resolve_status = RESOLVE_DONE;
		enum_value->type = decl->type;
	}
	return success;
}

static inline bool sema_analyse_throws(Context *context, Decl *decl)
{
	if (!sema_resolve_type_info(context, decl->throws)) return false;
	decl->type = decl->throws->type;
	return true;
}


static inline bool sema_analyse_method_function(Context *context, Decl *decl)
{
	TypeInfo *parent_type = decl->func.type_parent;
	if (!sema_resolve_type_info(context, parent_type)) return false;
	if (!type_may_have_method_functions(parent_type->type))
	{
		SEMA_ERROR(decl,
		           "Method functions can not be associated with '%s'",
		           type_to_error_string(decl->func.type_parent->type));
		return false;
	}
	Decl *parent = parent_type->type->decl;
	VECEACH(parent->method_functions, i)
	{
		Decl *function = parent->method_functions[i];
		if (function->name == decl->name)
		{
			SEMA_ERROR(decl, "Duplicate name '%s' for method function.", function->name);
			SEMA_PREV(function, "Previous definition here.");
			return false;
		}
	}
	DEBUG_LOG("Method function '%s.%s' analysed.", parent->name, decl->name);
	vec_add(parent->method_functions, decl);
	return true;
}



static inline bool sema_analyse_func(Context *context, Decl *decl)
{
	DEBUG_LOG("----Analysing function %s", decl->name);
	Type *func_type = sema_analyse_function_signature(context, &decl->func.function_signature, true);
	decl->type = func_type;
	if (!func_type) return decl_poison(decl);
	if (decl->func.type_parent)
	{
		if (!sema_analyse_method_function(context, decl)) return decl_poison(decl);
	}
	if (decl->func.body && !sema_analyse_function_body(context, decl)) return decl_poison(decl);
	if (decl->name == main_name)
	{
		if (decl->visibility == VISIBLE_LOCAL)
		{
			SEMA_ERROR(decl, "'main' cannot have local visibility.");
			return false;
		}
		decl->visibility = VISIBLE_EXTERN;
	}
	DEBUG_LOG("Function analysis done.");
	return true;
}

static inline bool sema_analyse_macro(Context *context, Decl *decl)
{
	TypeInfo *rtype = decl->macro_decl.rtype;
	if (decl->macro_decl.rtype && !sema_resolve_type_info(context, rtype)) return false;
	VECEACH(decl->macro_decl.parameters, i)
	{
		Decl *param = decl->macro_decl.parameters[i];
		assert(param->decl_kind == DECL_VAR);
		assert(param->var.kind == VARDECL_PARAM);
		if (param->var.type_info && !sema_resolve_type_info(context, param->var.type_info)) return false;
	}
	return true;
}




static inline bool sema_analyse_global(Context *context, Decl *decl)
{
	if (!sema_resolve_type_info(context, decl->var.type_info)) return false;
	if (decl->var.init_expr)
	{
		if (!sema_analyse_expr(context, decl->type, decl->var.init_expr)) return false;
		if (decl->var.init_expr->expr_kind != EXPR_CONST)
		{
			SEMA_ERROR(decl->var.init_expr, "The expression must be a constant value.");
			return false;
		}
	}
	switch (decl->var.kind)
	{
		case VARDECL_CONST:
			assert(decl->var.init_expr);
			return true;
		case VARDECL_GLOBAL:
			return true;
		default:
			UNREACHABLE
			break;
	}
}

static inline bool sema_analyse_generic(Context *context, Decl *decl)
{
	TODO
	return true;
}

bool sema_analyse_decl(Context *context, Decl *decl)
{
	if (decl->resolve_status == RESOLVE_DONE) return decl_ok(decl);

	DEBUG_LOG("Analyse %s", decl->name);
	if (decl->resolve_status == RESOLVE_RUNNING)
	{
		SEMA_ERROR(decl, "Recursive dependency on %s.", decl->name);
		decl_poison(decl);
		return false;
	}

	decl->resolve_status = RESOLVE_RUNNING;
	decl->module = context->module;
	switch (decl->decl_kind)
	{
		case DECL_THROWS:
			if (!sema_analyse_throws(context, decl)) return decl_poison(decl);
			break;
		case DECL_STRUCT:
		case DECL_UNION:
			if (!sema_analyse_struct_union(context, decl)) return decl_poison(decl);
			llvm_set_struct_size_alignment(decl);
			decl_set_external_name(decl);
			break;
		case DECL_FUNC:
			if (!sema_analyse_func(context, decl)) return decl_poison(decl);
			decl_set_external_name(decl);
			break;
		case DECL_MACRO:
			if (!sema_analyse_macro(context, decl)) return decl_poison(decl);
			break;
		case DECL_VAR:
			if (!sema_analyse_global(context, decl)) return decl_poison(decl);
			decl_set_external_name(decl);
			break;
		case DECL_TYPEDEF:
			if (!sema_analyse_typedef(context, decl)) return decl_poison(decl);
			break;
		case DECL_ENUM:
			if (!sema_analyse_enum(context, decl)) return decl_poison(decl);
			decl_set_external_name(decl);
			break;
		case DECL_ERROR:
			if (!sema_analyse_error(context, decl)) return decl_poison(decl);
			decl_set_external_name(decl);
			break;
		case DECL_GENERIC:
			if (!sema_analyse_generic(context, decl)) return decl_poison(decl);
			break;
		case DECL_ATTRIBUTE:
			TODO
		case DECL_POISONED:
		case DECL_IMPORT:
		case DECL_ENUM_CONSTANT:
		case DECL_ERROR_CONSTANT:
		case DECL_ARRAY_VALUE:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
			UNREACHABLE
		case DECL_CT_IF:
			// Handled elsewhere
			UNREACHABLE
	}
	decl->resolve_status = RESOLVE_DONE;
	return true;
}
