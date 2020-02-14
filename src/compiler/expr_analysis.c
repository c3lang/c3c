// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"


#define CONST_PROCESS(_op) \
if (both_const(left, right)) { \
switch (left->const_expr.type) { \
case CONST_INT: expr->const_expr.i = left->const_expr.i _op right->const_expr.i; break; \
case CONST_FLOAT: expr->const_expr.f = left->const_expr.f _op right->const_expr.f; break; \
default: UNREACHABLE } \
expr->expr_kind = EXPR_CONST; \
expr->const_expr.type = left->const_expr.type; \
}

static bool expr_is_ltype(Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_IDENTIFIER:
			return expr->identifier_expr.decl->decl_kind == DECL_VAR && (expr->identifier_expr.decl->var.kind == VARDECL_LOCAL
				|| expr->identifier_expr.decl->var.kind == VARDECL_GLOBAL
				|| expr->identifier_expr.decl->var.kind == VARDECL_PARAM);
		case EXPR_UNARY:
			return expr->unary_expr.operator == UNARYOP_DEREF;
		case EXPR_ACCESS:
			return expr_is_ltype(expr->access_expr.parent);
		case EXPR_SUBSCRIPT:
			return true;
		default:
			return false;
	}
}

static inline bool sema_type_error_on_binop(Expr *expr)
{
	const char *c = token_type_to_string(binaryop_to_token(expr->binary_expr.operator));
	SEMA_ERROR(expr,
	           "Cannot perform '%s' %s '%s'.",
	           type_to_error_string(expr->binary_expr.left->type),
	           c,
	           type_to_error_string(expr->binary_expr.right->type));
	return false;
}


static inline bool sema_expr_analyse_ternary(Context *context, Type *to, Expr *expr)
{
	Expr *left = expr->ternary_expr.then_expr;
	Expr *cond = expr->ternary_expr.cond;
	// Normal
	if (left)
	{
		if (!sema_analyse_expr(context, type_bool, cond)) return expr_poison(expr);
		if (!sema_analyse_expr(context, to, left)) return expr_poison(expr);
	}
	else
	{
		// Elvis
		if (!sema_analyse_expr(context, to, cond)) return expr_poison(expr);
		Type *type = cond->type->canonical;
		if (type->type_kind != TYPE_BOOL && cast_to_bool_kind(type) == CAST_ERROR)
		{
			SEMA_ERROR(cond, "Cannot convert expression to boolean.");
			return false;
		}
		left = cond;
	}

	Expr *right = expr->ternary_expr.else_expr;
	if (!sema_analyse_expr(context, to, right)) return expr_poison(expr);

	Type *left_canonical = left->type->canonical;
	Type *right_canonical = right->type->canonical;
	if (left_canonical != right_canonical)
	{
		Type *max = type_find_max_type(left_canonical, right_canonical);
		if (!max)
		{
			SEMA_ERROR(expr, "Cannot find a common parent type of '%s' and '%s'",
			           type_to_error_string(left_canonical), type_to_error_string(right_canonical));
			return false;
		}
		if (!cast_implicit(left, max) || !cast_implicit(right, max)) return false;
	}

	expr->type = left->type;
	return true;
}

static inline bool sema_expr_analyse_identifier(Context *context, Type *to, Expr *expr)
{
	// TODO what about struct functions
	Decl *ambiguous_decl;
	Decl *decl = sema_resolve_symbol(context, expr->identifier_expr.identifier, expr->identifier_expr.path, &ambiguous_decl);

	if (!decl)
	{
		SEMA_ERROR(expr, "Unknown symbol '%s'.", expr->identifier_expr.identifier);
		return false;
	}

	// Already handled
	if (!decl_ok(decl)) return false;

	if (ambiguous_decl)
	{
		SEMA_ERROR(expr,
		           "Ambiguous symbol '%s' – both defined in %s and %s, please add the module name to resolve the ambiguity",
		           expr->identifier_expr.identifier,
		           decl->module->name->module,
		           ambiguous_decl->module->name->module);
		return false;
	}

	if (decl->decl_kind == DECL_FUNC && !expr->identifier_expr.path && decl->module != context->module)
	{
		SEMA_ERROR(expr, "Functions from other modules, must be prefixed with the module name");
		return false;
	}
	if (decl->decl_kind == DECL_MACRO)
	{
		SEMA_ERROR(expr, "Macro expansions must be prefixed with '@', try using '@%s(...)' instead.", decl->name);
		return false;
	}
	assert(decl->type);
	expr->identifier_expr.decl = decl;
	expr->type = decl->type;
	return true;
}

static inline bool sema_expr_analyse_binary_sub_expr(Context *context, Expr *left, Expr *right)
{
	return sema_analyse_expr(context, NULL, left) & sema_analyse_expr(context, NULL, right);
}

static inline bool sema_expr_analyse_var_call(Context *context, Type *to, Expr *expr) { TODO }
static inline bool sema_expr_analyse_generic_call(Context *context, Type *to, Expr *expr) { TODO };

static inline bool sema_expr_analyse_func_call(Context *context, Type *to, Expr *expr, Decl *decl)
{
	Expr **args =expr->call_expr.arguments;
	Decl **func_params = decl->func.function_signature.params;
	unsigned error_params = decl->func.function_signature.throw_any || decl->func.function_signature.throws;
	if (error_params)
	{
		TODO
	}
	unsigned num_args = vec_size(args);
	// unsigned num_params = vec_size(func_params);
	// TODO handle named parameters, handle default parameters, varargs etc
	for (unsigned i = 0; i < num_args; i++)
	{
		Expr *arg = args[i];
		if (!sema_analyse_expr(context, func_params[i]->type, arg)) return false;
	}
	expr->type = decl->func.function_signature.rtype->type;
	return true;
}

static inline bool sema_expr_analyse_call(Context *context, Type *to, Expr *expr)
{
	// TODO
	Expr *func_expr = expr->call_expr.function;
	if (!sema_analyse_expr(context, to, func_expr)) return false;
	Decl *decl;
	switch (func_expr->expr_kind)
	{
		case EXPR_TYPE_ACCESS:
			decl = func_expr->type_access.method;
			break;
		case EXPR_IDENTIFIER:
			decl = func_expr->identifier_expr.decl;
			break;
		default:
			TODO
	}
	switch (decl->decl_kind)
	{
		case DECL_VAR:
			return sema_expr_analyse_var_call(context, to, expr);
		case DECL_FUNC:
			return sema_expr_analyse_func_call(context, to, expr, decl);
		case DECL_MACRO:
			SEMA_ERROR(expr, "Macro calls must be preceeded by '@'.");
			return false;
		case DECL_GENERIC:
			return sema_expr_analyse_generic_call(context, to, expr);
		case DECL_POISONED:
			return false;
		default:
			SEMA_ERROR(expr, "The expression cannot be called.");
			return false;
	}
}

static inline bool sema_expr_analyse_struct_value(Context *context, Type *to, Expr *expr)
{
	TODO
}

static inline bool sema_expr_analyse_struct_init_values(Context *context, Type *to, Expr *expr)
{
	TODO
}

static inline bool sema_expr_analyse_subscript(Context *context, Type *to, Expr *expr)
{
	if (!sema_analyse_expr(context, NULL, expr->subscript_expr.expr)) return false;

	Type *type = expr->subscript_expr.expr->type->canonical;
	Type *inner_type;
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			inner_type = type->pointer;
			break;
		case TYPE_VARARRAY:
		case TYPE_ARRAY:
			inner_type = type->array.base;
			break;
		case TYPE_SUBARRAY:
			TODO
		case TYPE_STRING:
			inner_type = type_char;
			break;
		default:
			SEMA_ERROR(expr->subscript_expr.expr, "Cannot index '%s'.", type_to_error_string(type));
			return false;
	}

	if (!sema_analyse_expr(context, type_isize, expr->subscript_expr.index)) return false;

	expr->type = inner_type;
	return true;
}

static inline bool sema_expr_analyse_method_function(Context *context, Expr *expr, Decl *decl, bool is_pointer)
{
	const char *name = expr->access_expr.sub_element.string;
	VECEACH(decl->method_functions, i)
	{
		Decl *function = decl->method_functions[i];
		if (function->name == name)
		{
			// TODO
			return true;
		}
	}
	SEMA_ERROR(expr, "Cannot find method function '%s.%s'", decl->name, name);
	return false;
}

static inline bool sema_expr_analyse_enum_constant(Context *context, Expr *expr, Decl *decl)
{
	const char *name = expr->type_access.name.string;
	VECEACH(decl->enums.values, i)
	{
		Decl *enum_constant = decl->enums.values[i];
		if (enum_constant->name == name)
		{
			assert(enum_constant->resolve_status == RESOLVE_DONE);
			expr_replace(expr, enum_constant->enum_constant.expr);
			return true;
		}
	}
	SEMA_ERROR(expr, "'%s' has no enumeration value '%s'.", decl->name, name);
	return false;
}

static inline bool sema_expr_analyse_error_constant(Context *context, Expr *expr, Decl *decl)
{
	const char *name = expr->type_access.name.string;
	VECEACH(decl->error.error_constants, i)
	{
		Decl *error_constant = decl->error.error_constants[i];
		if (error_constant->name == name)
		{
			assert(error_constant->resolve_status == RESOLVE_DONE);
			expr->type = decl->type;
			expr->expr_kind = EXPR_CONST;
			expr->const_expr.type = CONST_INT;
			expr->const_expr.i = decl->error_constant.value;
			return true;
		}
	}
	SEMA_ERROR(expr, "'%s' has no error type '%s'.", decl->name, name);
	return false;
}

static Decl *strukt_recursive_search_member(Decl *strukt, const char *name, int *index)
{
	VECEACH(strukt->strukt.members, i)
	{
		(*index)++;
		Decl *member = strukt->strukt.members[i];
		if (member->name == name) return member;
		if (!member->name && decl_is_struct_type(member))
		{
			Decl *result = strukt_recursive_search_member(member, name, index);
			if (result) return result;
		}
	}
	return NULL;
}

static inline bool sema_expr_analyse_access(Context *context, Type *to, Expr *expr)
{
	if (!sema_analyse_expr(context, NULL, expr->access_expr.parent)) return false;
	Type *parent_type = expr->access_expr.parent->type;

	Type *type = parent_type->canonical;
	bool is_pointer = type->type_kind == TYPE_POINTER;
	if (is_pointer)
	{
		type = type->pointer;
	}
	if (!type_may_have_method_functions(type))
	{
		SEMA_ERROR(expr, "Cannot access '%s' on '%s'", expr->access_expr.sub_element.string, type_to_error_string(parent_type));
		return false;
	}
	Decl *decl = type->decl;
	switch (decl->decl_kind)
	{
		case DECL_ENUM:
		case DECL_ERROR:
			return sema_expr_analyse_method_function(context, expr, decl, is_pointer);
		case DECL_STRUCT:
		case DECL_UNION:
			break;
		default:
			UNREACHABLE
	}
	int index = -1;
	Decl *member = strukt_recursive_search_member(decl, expr->access_expr.sub_element.string, &index);
	if (!member)
	{
		SEMA_TOKEN_ERROR(expr->access_expr.sub_element, "There is no element '%s.%s'.", decl->name, expr->access_expr.sub_element.string);
		return false;
	}
	if (is_pointer)
	{
		Expr *deref = expr_new(EXPR_UNARY, expr->span);
		deref->unary_expr.operator = UNARYOP_DEREF;
		deref->unary_expr.expr = expr->access_expr.parent;
		deref->resolve_status = RESOLVE_DONE;
		deref->type = type;
		expr->access_expr.parent = deref;
	}
	expr->type = member->type;
	expr->access_expr.index = index;
	return true;
}

static inline bool sema_expr_analyse_type_access(Context *context, Type *to, Expr *expr)
{
	TypeInfo *type_info = expr->type_access.type;
	if (!sema_resolve_type_info(context, type_info)) return false;
	if (!type_may_have_method_functions(type_info->type))
	{
		SEMA_ERROR(expr, "'%s' does not have method functions.", type_to_error_string(type_info->type));
		return false;
	}
	Decl *decl = type_info->type->decl;
	// TODO add more constants that can be inspected?
	// e.g. SomeEnum.values, MyUnion.x.offset etc?
	switch (decl->decl_kind)
	{
		case DECL_ENUM:
			if (expr->type_access.name.type == TOKEN_CONST_IDENT) return sema_expr_analyse_enum_constant(context, expr, decl);
			break;
		case DECL_ERROR:
			if (expr->type_access.name.type == TOKEN_CONST_IDENT) return sema_expr_analyse_error_constant(context, expr, decl);
			break;
		case DECL_UNION:
		case DECL_STRUCT:
			break;
		default:
			UNREACHABLE
	}
	VECEACH(type_info->type->decl->method_functions, i)
	{
		Decl *function = type_info->type->decl->method_functions[i];
		if (expr->type_access.name.string == function->name)
		{
			expr->type_access.method = function;
			expr->type = function->func.function_signature.rtype->type;
			return true;
		}
	}
	SEMA_ERROR(expr, "No function '%s.%s' found.", type_to_error_string(type_info->type), expr->type_access.name.string);
	return false;
}

static Decl *sema_analyse_init_path(Context *context, Decl *strukt, Expr *expr);

static Decl *sema_analyse_init_identifier_string(Context *context, Decl *strukt, const char *string)
{
	assert(decl_is_struct_type(strukt));
	Decl **members = strukt->strukt.members;
	VECEACH(members, i)
	{
		Decl *member = members[i];
		if (member->name == string) return member;
		if (!member->name)
		{
			Decl *anonymous_member = sema_analyse_init_identifier_string(context, member->type->decl, string);
			if (anonymous_member) return anonymous_member;
		}
	}
	return NULL;
}

static Decl *sema_analyse_init_identifier(Context *context, Decl *strukt, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_NOT_DONE);
	expr->resolve_status = RESOLVE_RUNNING;
	expr->identifier_expr.decl = sema_analyse_init_identifier_string(context, strukt, expr->identifier_expr.identifier);
	expr->resolve_status = RESOLVE_DONE;
	return expr->identifier_expr.decl;
}
static Decl *sema_analyse_init_access(Context *context, Decl *strukt, Expr *access_expr)
{
	assert(access_expr->resolve_status == RESOLVE_NOT_DONE);
	access_expr->resolve_status = RESOLVE_RUNNING;
	Decl *decl = sema_analyse_init_path(context, strukt, access_expr->access_expr.parent);
	if (!decl || !decl_is_struct_type(decl->type->decl))
	{
		access_expr->resolve_status = RESOLVE_DONE;
		return NULL;
	}
	decl = access_expr->access_expr.ref = sema_analyse_init_identifier_string(context, decl->type->decl, access_expr->access_expr.sub_element.string);
	access_expr->resolve_status = RESOLVE_DONE;
	return decl;
}

static Decl *sema_analyse_init_subscript(Context *context, Decl *array, Expr *subscript)
{
	TODO
	if (array->type->type_kind != TYPE_ARRAY)
	{

	}
}

static Decl *sema_analyse_init_path(Context *context, Decl *strukt, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_ACCESS:
			return sema_analyse_init_access(context, strukt, expr);
		case EXPR_IDENTIFIER:
			return sema_analyse_init_identifier(context, strukt, expr);
		case EXPR_SUBSCRIPT:
			return sema_analyse_init_subscript(context, strukt, expr);
		default:
			return NULL;
	}
}


typedef enum
{
	INIT_SEMA_ERROR,
	INIT_SEMA_NOT_FOUND,
	INIT_SEMA_OK
} InitSemaResult;

static InitSemaResult sema_expr_analyse_struct_named_initializer_list(Context *context, Decl *assigned, Expr *expr_list)
{
	VECEACH(expr_list->initializer_expr, i)
	{
		Expr *expr = expr_list->initializer_expr[i];
		if (expr->expr_kind != EXPR_BINARY && expr->binary_expr.operator != BINARYOP_ASSIGN)
		{
			if (i != 0)
			{
				SEMA_ERROR(expr, "Named and non-named initializers are not allowed together, please choose one or the other.");
				return INIT_SEMA_ERROR;
			}
			// If there is an unexpected expression and no previous element then this is a normal initializer list.
			return INIT_SEMA_NOT_FOUND;
		}
		Expr *path = expr->binary_expr.left;
		Expr *value = expr->binary_expr.right;
		Decl *result = sema_analyse_init_path(context, assigned, path);
		if (!result)
		{
			if (i != 0)
			{
				SEMA_ERROR(path, "Unexpected element when initializing '%s', did you get the name right?", assigned->name);
				return INIT_SEMA_ERROR;
			}
			return INIT_SEMA_NOT_FOUND;
		}
		if (!sema_analyse_expr(context, result->type, value)) return INIT_SEMA_ERROR;
	}
	return INIT_SEMA_OK;
}

static inline bool sema_expr_analyse_struct_initializer_list(Context *context, Type *assigned, Expr *expr)
{
	Decl **members = assigned->decl->strukt.members;
	unsigned size = vec_size(members);
	// Zero size init will initialize to empty.
	if (size == 0) return true;

	InitSemaResult result = sema_expr_analyse_struct_named_initializer_list(context, assigned->decl, expr);
	if (result == INIT_SEMA_ERROR) return false;
	if (result == INIT_SEMA_OK)
	{
		TODO
	}
	if (assigned->type_kind == TYPE_UNION)
	{
		SEMA_ERROR(expr->initializer_expr[0], "Initializer list for unions must use named initializers, e.g. { a = 4 }");
		return false;
	}
	if (size < vec_size(expr->initializer_expr))
	{
		SEMA_ERROR(expr->initializer_expr[size], "Too many elements in initializer, expected only %d.", size);
		return false;
	}
	VECEACH(expr->initializer_expr, i)
	{
		if (!sema_analyse_expr(context, members[i]->type, expr->initializer_expr[i])) return false;
	}
	expr->type = assigned;
	return true;
}

static inline bool sema_expr_analyse_initializer_list(Context *context, Type *to, Expr *expr)
{
	assert(to);
	Type *assigned = to->canonical;
	assert(assigned);
	switch (assigned->type_kind)
	{
		case TYPE_STRUCT:
		case TYPE_UNION:
			if (decl_is_struct_type(assigned->decl)) return sema_expr_analyse_struct_initializer_list(context, assigned, expr);
			break;
		case TYPE_ARRAY:
			TODO
		case TYPE_VARARRAY:
			TODO
		default:
			break;
	}
	SEMA_ERROR(expr, "Cannot assign expression to '%s'.", type_to_error_string(to));
	return false;
}

static inline bool sema_expr_analyse_sizeof(Context *context, Type *to, Expr *expr)
{
	TODO
}

static inline bool sema_expr_analyse_expr_list(Context *context, Type *to, Expr *expr)
{
	bool success = true;
	size_t last = vec_size(expr->expression_list) - 1;
	VECEACH(expr->expression_list, i)
	{
		success &= sema_analyse_expr(context, i == last ? to : NULL, expr->expression_list[i]);
	}
	return success;
}

static inline bool sema_expr_analyse_cast(Context *context, Type *to, Expr *expr)
{
	Expr *inner = expr->cast_expr.expr;
	if (!sema_resolve_type_info(context, expr->cast_expr.type_info)) return false;
	if (!sema_analyse_expr(context, NULL, inner)) return false;

	if (!cast(inner, expr->cast_expr.type_info->type, CAST_TYPE_EXPLICIT)) return false;

	// TODO above is probably not right, cast type not set.
	// Overwrite cast.
	SourceRange loc = expr->span;
	*expr = *inner;
	expr->span = loc;

	return true;
}


static bool sema_expr_analyse_assign(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_analyse_expr(context, NULL, left)) return false;

	if (!expr_is_ltype(left))
	{
		SEMA_ERROR(left, "Expression is not assignable.");
		return false;
	}
	if (!sema_analyse_expr(context, left->type, right)) return false;
	expr->type = right->type;
	return true;
}

static inline bool both_const(Expr *left, Expr *right)
{
	return left->expr_kind == EXPR_CONST && right->expr_kind == EXPR_CONST;
}

static bool sema_expr_analyse_bit_and_assign(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_analyse_expr(context, NULL, left)) return false;

	if (!type_is_number(left->type))
	{
		SEMA_ERROR(left, "Expected a numeric type here.");
		return false;
	}

	if (!sema_analyse_expr(context, left->type->canonical, right)) return false;

	if (!type_is_number(right->type))
	{
		SEMA_ERROR(right, "Expected a numeric type here.");
		return false;
	}

	expr->type = left->type;
	return cast(right, left->type, CAST_TYPE_IMPLICIT_ASSIGN_ADD);
}

static bool sema_expr_analyse_bit_or_assign(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_analyse_expr(context, NULL, left)) return false;

	if (!type_is_number(left->type))
	{
		SEMA_ERROR(left, "Expected a numeric type here.");
		return false;
	}

	if (!sema_analyse_expr(context, left->type->canonical, right)) return false;

	if (!type_is_number(right->type))
	{
		SEMA_ERROR(right, "Expected a numeric type here.");
		return false;
	}

	expr->type = left->type;
	return cast(right, left->type, CAST_TYPE_IMPLICIT_ASSIGN_ADD);
}

static bool sema_expr_analyse_bit_xor_assign(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_analyse_expr(context, NULL, left)) return false;

	if (!type_is_number(left->type))
	{
		SEMA_ERROR(left, "Expected a numeric type here.");
		return false;
	}

	if (!sema_analyse_expr(context, left->type->canonical, right)) return false;

	if (!type_is_number(right->type))
	{
		SEMA_ERROR(right, "Expected a numeric type here.");
		return false;
	}

	expr->type = left->type;
	return cast(right, left->type, CAST_TYPE_IMPLICIT_ASSIGN_ADD);

}

static bool sema_expr_analyse_div_assign(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_analyse_expr(context, NULL, left)) return false;

	if (!type_is_number(left->type))
	{
		SEMA_ERROR(left, "Expected a numeric type here.");
		return false;
	}

	if (!sema_analyse_expr(context, left->type->canonical, right)) return false;

	if (!type_is_number(right->type))
	{
		SEMA_ERROR(right, "Expected a numeric type here.");
		return false;
	}

	expr->type = left->type;
	return cast(right, left->type, CAST_TYPE_IMPLICIT_ASSIGN_ADD);
}

static bool sema_expr_analyse_mult_assign(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_analyse_expr(context, NULL, left)) return false;

	if (!type_is_number(left->type))
	{
		SEMA_ERROR(left, "Expected a numeric type here.");
		return false;
	}

	if (!sema_analyse_expr(context, left->type->canonical, right)) return false;

	if (!type_is_number(right->type))
	{
		SEMA_ERROR(right, "Expected a numeric type here.");
		return false;
	}

	expr->type = left->type;
	return cast(right, left->type, CAST_TYPE_IMPLICIT_ASSIGN_ADD);
}


static bool sema_expr_analyse_sub_assign(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_analyse_expr(context, NULL, left)) return false;

	Type *left_type_canonical = left->type->canonical;

	if (left_type_canonical->type_kind == TYPE_POINTER)
	{
		if (!sema_analyse_expr(context, NULL, right)) return false;
		// Improve check if this should be usize.
		if (!cast_to_runtime(right)) return false;
		Type *right_type = right->type->canonical;
		if (!type_is_integer(right_type))
		{
			SEMA_ERROR(right, "Expected an integer type instead.");
			return false;
		}
		expr->type = left->type;
		return true;
	}

	if (!sema_analyse_expr(context, left->type->canonical, right)) return false;

	if (!type_is_number(left->type))
	{
		SEMA_ERROR(left, "Expected a numeric type here.");
		return false;
	}

	if (!type_is_number(right->type))
	{
		SEMA_ERROR(right, "Expected a numeric type here.");
		return false;
	}

	expr->type = left->type;

	return cast(right, left->type, CAST_TYPE_IMPLICIT_ASSIGN_ADD);
}

static bool sema_expr_analyse_add_assign(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_analyse_expr(context, NULL, left)) return false;

	Type *left_type_canonical = left->type->canonical;

	if (left_type_canonical->type_kind == TYPE_POINTER)
	{
		if (!sema_analyse_expr(context, NULL, right)) return false;
		// Improve check if this should be usize.
		if (!cast_to_runtime(right)) return false;
		Type *right_type = right->type->canonical;
		if (!type_is_integer(right_type))
		{
			SEMA_ERROR(right, "Expected an integer type instead.");
			return false;
		}
		expr->type = left->type;
		return true;
	}

	if (!sema_analyse_expr(context, left->type->canonical, right)) return false;

	if (!type_is_number(left->type))
	{
		SEMA_ERROR(left, "Expected a numeric type here.");
		return false;
	}

	if (!type_is_number(right->type))
	{
		SEMA_ERROR(right, "Expected a numeric type here.");
		return false;
	}

	expr->type = left->type;

	return cast(right, left->type, CAST_TYPE_IMPLICIT_ASSIGN_ADD);
}

static bool binary_arithmetic_promotion(Expr *left, Expr *right, Type *left_type, Type *right_type)
{
	Type *max = type_find_max_type(left_type, right_type);
	return max && type_is_number(max) && cast_implicit(left, max) && cast_implicit(right, max);
}

static bool sema_expr_analyse_sub(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_expr_analyse_binary_sub_expr(context, left, right)) return false;

	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;

	if (left_type->type_kind == TYPE_POINTER)
	{
		if (right_type->type_kind == TYPE_POINTER)
		{
			Type *max = type_find_max_type(left_type, right_type);
			bool success = max && cast_implicit(left, max) && cast_implicit(right, max);
			if (!success) goto ERR;
			expr->type = type_isize;
			return true;
		}
		// No need to cast this, we just ensure it is an integer.
		if (!type_is_integer(right_type) || !cast_to_runtime(right)) goto ERR;
		expr->type = left->type;
		return true;
	}

	if (!binary_arithmetic_promotion(left, right, left_type, right_type)) goto ERR;

	CONST_PROCESS(-);

	expr->type = left->type;
	return true;

	ERR:
	SEMA_ERROR(expr, "Cannot subtract '%s' from '%s'", type_to_error_string(left_type), type_to_error_string(right_type));
	return false;
}

static bool sema_expr_analyse_add(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_expr_analyse_binary_sub_expr(context, left, right)) return false;

	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;

	// Reorder if needed
	if (right_type->type_kind == TYPE_POINTER && left_type->type_kind != TYPE_POINTER)
	{
		Expr *temp = right;
		right = left;
		left = temp;
		right_type = left_type;
		left_type = left->type->canonical;
	}

	if (left_type->type_kind == TYPE_POINTER)
	{
		// No need to cast this, we just ensure it is an integer.
		if (!type_is_integer(right_type) || !cast_to_runtime(right)) goto ERR;
		expr->type = left->type;
		return true;
	}

	if (!binary_arithmetic_promotion(left, right, left_type, right_type)) goto ERR;

	CONST_PROCESS(+);

	expr->type = left->type;
	return true;

	ERR:
	SEMA_ERROR(expr, "Cannot add '%s' to '%s'", type_to_error_string(left_type), type_to_error_string(right_type));
	return false;
}

static bool sema_expr_analyse_mult(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_expr_analyse_binary_sub_expr(context, left, right)) return false;

	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;

	if (!binary_arithmetic_promotion(left, right, left_type, right_type)) goto ERR;

	CONST_PROCESS(*)

	expr->type = left->type;
	return true;

	ERR:
	SEMA_ERROR(expr, "Cannot multiply '%s' and '%s'", type_to_error_string(left_type), type_to_error_string(right_type));
	return false;
}

static bool sema_expr_analyse_div(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_expr_analyse_binary_sub_expr(context, left, right)) return false;

	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;

	if (!binary_arithmetic_promotion(left, right, left_type, right_type)) goto ERR;

	// Check null
	if (right->expr_kind == EXPR_CONST)
	{
		switch (right->const_expr.type)
		{
			case CONST_INT:
				if (right->const_expr.i == 0)
				{
					SEMA_ERROR(right, "Division by zero not allowed.");
					return false;
				}
				break;
			case CONST_FLOAT:
				if (right->const_expr.f == 0)
				{
					SEMA_ERROR(right, "Division by zero not allowed.");
					return false;
				}
				break;
			default:
				UNREACHABLE
		}
	}

	CONST_PROCESS(/)

	expr->type = left->type;
	return true;

	ERR:
	SEMA_ERROR(expr, "Cannot divide '%s' by '%s'", type_to_error_string(left_type), type_to_error_string(right_type));
	return false;

}

static bool sema_expr_analyse_mod(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_expr_analyse_binary_sub_expr(context, left, right)) return false;

	if (!type_is_integer(right->type->canonical) || !type_is_integer(left->type->canonical)) return sema_type_error_on_binop(expr);

	if (right->expr_kind == EXPR_CONST && right->const_expr.i == 0)
	{
		SEMA_ERROR(expr->binary_expr.right, "Cannot perform mod by zero.");
		return false;
	}
	// TODO Insert trap on negative right.
	if (left->expr_kind == EXPR_CONST && right->expr_kind == EXPR_CONST)
	{
		// TODO negative
		expr_replace(expr, left);
		expr->const_expr.i %= right->const_expr.i;
		return expr;
	}

	if (!cast_implicit(left, to)) return false;

	if (!cast_to_runtime(left) || !cast_to_runtime(right)) return false;

	expr->type = left->type;

	return true;
}

static bool sema_expr_analyse_mod_assign(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{

	TODO }


static bool sema_expr_analyse_bit(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_expr_analyse_binary_sub_expr(context, left, right)) return false;

	if (!type_is_integer(right->type->canonical) || !type_is_integer(left->type->canonical)) goto ERR;

	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;

	if (!binary_arithmetic_promotion(left, right, left_type, right_type)) goto ERR;

	if (left->expr_kind == EXPR_CONST && right->expr_kind == EXPR_CONST)
	{
		expr_replace(expr, left);
		switch (expr->binary_expr.operator)
		{
			case TOKEN_AMP:
				expr->const_expr.i &= right->const_expr.i;
				break;
			case TOKEN_BIT_XOR:
				expr->const_expr.i ^= right->const_expr.i;
				break;
			case TOKEN_BIT_OR:
				expr->const_expr.i |= right->const_expr.i;
				break;
			default:
				UNREACHABLE;
		}
		return expr;
	}

	expr->type = left->type;
	return true;

	ERR:
	return sema_type_error_on_binop(expr);

}


static bool sema_expr_analyse_shr(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{

	if (!sema_expr_analyse_binary_sub_expr(context, left, right)) return false;

	if (!type_is_integer(right->type->canonical) || !type_is_integer(left->type->canonical)) goto ERR;

	// First, try to do assign type promotion.
	if (!cast_implicit(left, to)) goto ERR;

	// Next, cast to runtime types, this might be needed for runtime constants, e.g. x >> 2
	if (!cast_to_runtime(left) || !cast_to_runtime(right)) goto ERR;

	if (right->expr_kind == EXPR_CONST)
	{
		if (right->const_expr.i > left->type->canonical->builtin.bitsize)
		{
			SEMA_ERROR(right, "Rightshift exceeds bitsize of '%s'", type_to_error_string(left->type));
			return false;
		}
		if (left->expr_kind == EXPR_CONST)
		{
			expr_replace(expr, left);
			expr->const_expr.i >>= right->const_expr.i;
			return true;
		}
	}

	expr->type = left->type;
	return true;

	ERR:
	return sema_type_error_on_binop(expr);
}

static bool sema_expr_analyse_shr_assign(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_expr_analyse_binary_sub_expr(context, left, right)) return false;

	if (!expr_is_ltype(left))
	{
		SEMA_ERROR(left, "Expression is not assignable.");
		return false;
	}

	// Check that right side is integer and cast to a runtime type if needed.
	if (!type_is_integer(right->type->canonical) || !cast_to_runtime(right)) return false;

	if (right->expr_kind == EXPR_CONST)
	{
		if (right->const_expr.i > left->type->canonical->builtin.bitsize)
		{
			SEMA_ERROR(right, "Rightshift exceeds bitsize of '%s'", type_to_error_string(left->type));
			return false;
		}
	}

	expr->type = left->type;
	return true;
}

static bool sema_expr_analyse_shl(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{

	if (!sema_expr_analyse_binary_sub_expr(context, left, right)) return false;

	if (!type_is_integer(right->type->canonical) || !type_is_integer(left->type->canonical)) goto ERR;

	// First, try to do assign type promotion.
	if (!cast_implicit(left, to)) goto ERR;

	// Next, cast to runtime types, this might be needed for runtime constants, e.g. x << 2
	if (!cast_to_runtime(left) || !cast_to_runtime(right)) return false;

	if (right->expr_kind == EXPR_CONST)
	{
		if (right->const_expr.i > left->type->canonical->builtin.bitsize)
		{
			SEMA_ERROR(right, "Leftshift exceeds bitsize of '%s'", type_to_error_string(left->type));
			return false;
		}
		if (left->expr_kind == EXPR_CONST)
		{
			expr_replace(expr, left);
			expr->const_expr.i <<= right->const_expr.i;
			return true;
		}
	}

	expr->type = left->type;
	return true;

	ERR:
	return sema_type_error_on_binop(expr);
}

static bool sema_expr_analyse_shl_assign(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_expr_analyse_binary_sub_expr(context, left, right)) return false;

	if (!expr_is_ltype(left))
	{
		SEMA_ERROR(left, "Expression is not assignable.");
		return false;
	}

	// Check that right side is integer and cast to a runtime type if needed.
	if (!type_is_integer(right->type->canonical)) return sema_type_error_on_binop(expr);

	if (!cast_to_runtime(right)) return false;

	if (right->expr_kind == EXPR_CONST)
	{
		if (right->const_expr.i > left->type->canonical->builtin.bitsize)
		{
			SEMA_ERROR(right, "Leftshift exceeds bitsize of '%s'", type_to_error_string(left->type));
			return false;
		}
	}

	expr->type = left->type;
	return true;
}


static bool sema_expr_analyse_and(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_expr_analyse_binary_sub_expr(context, left, right)) return false;
	if (!cast_implicit(left, type_bool) || !cast_implicit(right, type_bool)) return false;

	expr->type = type_bool;
	if (both_const(left, right))
	{
		expr_replace(expr, left);
		expr->const_expr.b &= right->const_expr.b;
	}
	return true;
}

static bool sema_expr_analyse_or(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_expr_analyse_binary_sub_expr(context, left, right)) return false;
	if (!cast_implicit(left, type_bool) || !cast_implicit(right, type_bool)) return false;

	if (both_const(left, right))
	{
		expr_replace(expr, left);
		expr->const_expr.b |= right->const_expr.b;
	}
	expr->type = type_bool;
	return true;
}

static bool sema_expr_analyse_and_assign(Context *context, Type *to, Expr *expr, Expr *left, Expr *right) { TODO }
static bool sema_expr_analyse_or_assign(Context *context, Type *to, Expr *expr, Expr *left, Expr *right) { TODO }


static bool sema_expr_analyse_comp(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_expr_analyse_binary_sub_expr(context, left, right)) return false;
	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;
	Type *max = type_find_max_type(left_type, right_type);
	bool success = max && cast_implicit(left, max) && cast_implicit(right, max);
	if (!success)
	{
		SEMA_ERROR(expr, "Cannot implicitly convert types to evaluate '%s' %s '%s'", type_to_error_string(left_type), token_type_to_string(binaryop_to_token(expr->binary_expr.operator)), type_to_error_string(right_type));
		return false;
	}
	if (both_const(left, right))
	{
#define COMP(_op_) \
switch (left->const_expr.type) { \
case CONST_FLOAT: expr->const_expr.b = left->const_expr.f _op_ right->const_expr.f; break; \
case CONST_BOOL: expr->const_expr.b = left->const_expr.b _op_ right->const_expr.b; break; \
case CONST_INT: expr->const_expr.b = left->const_expr.i _op_ right->const_expr.i; break; \
default: UNREACHABLE } break;
		switch (expr->binary_expr.operator)
		{
			case BINARYOP_GT:
				COMP(>)
			case BINARYOP_GE:
				COMP(>=)
			case BINARYOP_LT:
				COMP(<)
			case BINARYOP_LE:
				COMP(<=)
			case BINARYOP_EQ:
				// TODO elsewhere
				COMP(==)
			case BINARYOP_NE:
				// TODO elsewhere
				COMP(!=)
			default:
				UNREACHABLE
		}
#undef COMP
		expr->const_expr.type = CONST_BOOL;
		expr->expr_kind = EXPR_CONST;
	}
	expr->type = type_bool;
	return true;
}

#define SEMA_ANALYSE_CMP(op) { \
if (!cast_arithmetic(left, right, #op)) return false;\
if (!cast_arithmetic(right, left, #op)) return false;\
if (both_const(left, right)) { \
switch (left->const_expr.type) { \
case CONST_FLOAT: expr->const_expr.b = left->const_expr.f op right->const_expr.f; break; \
case CONST_BOOL: expr->const_expr.b = left->const_expr.b op right->const_expr.b; break; \
case CONST_INT: expr->const_expr.b = left->const_expr.i op right->const_expr.i; break; \
default: UNREACHABLE }\
expr->const_expr.type = CONST_BOOL;\
expr->expr_kind = EXPR_CONST;\
}\
if (!cast_to_runtime(left) || !cast_to_runtime(right)) return false;\
expr->type = type_bool;\
return true; }


static bool sema_expr_analyse_elvis(Context *context, Type *to, Expr *expr, Expr *left, Expr *right) { TODO }

static bool sema_expr_analyse_deref(Context *context, Type *to, Expr *expr, Expr *inner)
{
	Type *canonical = inner->type->canonical;
	if (canonical->type_kind != TYPE_POINTER)
	{
		SEMA_ERROR(inner, "Cannot take the dereference of a value of type '%s'", type_to_error_string(inner->type));
		return false;
	}
	if (inner->expr_kind == EXPR_CONST)
	{
		SEMA_ERROR(inner, "Dereferencing nil is not allowed.");
		return false;
	}
	Type *deref_type = inner->type->type_kind != TYPE_POINTER ? inner->type : canonical;
	expr->type = deref_type->pointer;
	return true;
}

static bool sema_expr_analyse_addr(Context *context, Type *to, Expr *expr, Expr *inner)
{
	if (!expr_is_ltype(inner))
	{
		SEMA_ERROR(inner, "Cannot take the address of a value of type '%s'", type_to_error_string(inner->type));
		return false;
	}
	expr->type = type_get_ptr(inner->type);
	return true;
}

static bool sema_expr_analyse_neg(Context *context, Type *to, Expr *expr, Expr *inner)
{
	Type *canonical = inner->type->canonical;
	if (!builtin_may_negate(canonical))
	{
		SEMA_ERROR(expr, "Cannot negate %s.", type_to_error_string(inner->type));
		return false;
	}
	if (inner->expr_kind != EXPR_CONST)
	{
		expr->type = inner->type;
		return true;
	}
	// TODO UXX CAP
	expr_replace(expr, inner);
	switch (expr->const_expr.type)
	{
		case CONST_INT:
			expr->const_expr.i = ~expr->const_expr.i;
			break;
		case CONST_FLOAT:
			expr->const_expr.f = -expr->const_expr.i;
			break;
		default:
			UNREACHABLE
	}
	return true;
}
static bool sema_expr_analyse_bit_not(Context *context, Type *to, Expr *expr, Expr *inner)
{
	Type *canonical = inner->type->canonical;
	if (!type_is_integer(canonical) && canonical != type_bool)
	{
		SEMA_ERROR(expr, "Cannot bit negate %s.", type_to_error_string(inner->type));
	}
	if (inner->expr_kind != EXPR_CONST)
	{
		expr->type = inner->type;
		return true;
	}
	expr_replace(expr, inner);
	// TODO UXX CAP
	switch (expr->const_expr.type)
	{
		case CONST_INT:
			expr->const_expr.i = ~expr->const_expr.i;
			break;
		case CONST_BOOL:
			expr->const_expr.b = !expr->const_expr.b;
			break;
		default:
			UNREACHABLE
	}
	return true;
}
static bool sema_expr_analyse_not(Context *context, Type *to, Expr *expr, Expr *inner)
{
	expr->type = type_bool;
	if (inner->expr_kind == EXPR_CONST)
	{
		switch (expr->const_expr.type)
		{
			case CONST_NIL:
				expr->const_expr.b = true;
				break;
			case CONST_BOOL:
				expr->const_expr.b = !inner->const_expr.b;
				break;
			case CONST_INT:
				expr->const_expr.b = inner->const_expr.i == 0;
				break;
			case CONST_FLOAT:
				expr->const_expr.b = inner->const_expr.f == 0;
				break;
			case CONST_STRING:
				expr->const_expr.b = !inner->const_expr.string.len;
				break;
		}
		expr->const_expr.type = CONST_BOOL;
		expr->expr_kind = EXPR_CONST;
		return true;
	}
	Type *canonical = inner->type->canonical;
	switch (canonical->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_IXX:
		case TYPE_UXX:
		case TYPE_FXX:
		case TYPE_TYPEDEF:
		case TYPE_ERROR_UNION:
			UNREACHABLE
		case TYPE_FUNC:
		case TYPE_ARRAY:
		case TYPE_POINTER:
		case TYPE_VARARRAY:
		case TYPE_SUBARRAY:
		case TYPE_BOOL:
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
		case TYPE_F32:
		case TYPE_F64:
			return true;
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_VOID:
		case TYPE_STRING:
		case TYPE_ENUM:
		case TYPE_ERROR:
		case TYPE_META_TYPE:
			SEMA_ERROR(expr, "Cannot use 'not' on %s", type_to_error_string(inner->type));
			return false;
	}
	UNREACHABLE
}

static inline bool sema_expr_analyse_incdec(Context *context, Type *to, Expr *expr, Expr *inner)
{
	if (!expr_is_ltype(inner))
	{
		SEMA_ERROR(inner, "Expression cannot be assigned to");
		return false;
	}
	if (!type_is_number(inner->type->canonical) && inner->type->canonical->type_kind != TYPE_POINTER)
	{
		SEMA_ERROR(inner, "Expression must be a number or a pointer");
		return false;
	}
	expr->type = inner->type;
	return true;
}



static inline bool sema_expr_analyse_binary(Context *context, Type *to, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_RUNNING);
	Expr *left = expr->binary_expr.left;
	Expr *right = expr->binary_expr.right;
	switch (expr->binary_expr.operator)
	{
		case BINARYOP_ASSIGN:
			return sema_expr_analyse_assign(context, to, expr, left, right);
		case BINARYOP_MULT:
		case BINARYOP_MULT_MOD:
			// Todo treat mod differently
			return sema_expr_analyse_mult(context, to, expr, left, right);
		case BINARYOP_MULT_ASSIGN:
		case BINARYOP_MULT_MOD_ASSIGN:
			return sema_expr_analyse_mult_assign(context, to, expr, left, right);
		case BINARYOP_ADD:
		case BINARYOP_ADD_MOD:
			// TODO tread mod differently
			return sema_expr_analyse_add(context, to, expr, left, right);
		case BINARYOP_ADD_MOD_ASSIGN:
		case BINARYOP_ADD_ASSIGN:
			return sema_expr_analyse_add_assign(context, to, expr, left, right);
		case BINARYOP_SUB:
		case BINARYOP_SUB_MOD:
			return sema_expr_analyse_sub(context, to, expr, left, right);
		case BINARYOP_SUB_ASSIGN:
		case BINARYOP_SUB_MOD_ASSIGN:
			return sema_expr_analyse_sub_assign(context, to, expr, left, right);
		case BINARYOP_DIV:
			return sema_expr_analyse_div(context, to, expr, left, right);
		case BINARYOP_DIV_ASSIGN:
			return sema_expr_analyse_div_assign(context, to, expr, left, right);
		case BINARYOP_MOD:
			return sema_expr_analyse_mod(context, to, expr, left, right);
		case BINARYOP_MOD_ASSIGN:
			return sema_expr_analyse_mod_assign(context, to, expr, left, right);
		case BINARYOP_AND:
			return sema_expr_analyse_and(context, to, expr, left, right);
		case BINARYOP_AND_ASSIGN:
			return sema_expr_analyse_and_assign(context, to, expr, left, right);
		case BINARYOP_OR:
			return sema_expr_analyse_or(context, to, expr, left, right);
		case BINARYOP_OR_ASSIGN:
			return sema_expr_analyse_or_assign(context, to, expr, left, right);
		case BINARYOP_BIT_AND_ASSIGN:
			return sema_expr_analyse_bit_and_assign(context, to, expr, left, right);
		case BINARYOP_BIT_OR:
		case BINARYOP_BIT_XOR:
		case BINARYOP_BIT_AND:
			return sema_expr_analyse_bit(context, to, expr, left, right);
		case BINARYOP_BIT_OR_ASSIGN:
			return sema_expr_analyse_bit_or_assign(context, to, expr, left, right);
		case BINARYOP_BIT_XOR_ASSIGN:
			return sema_expr_analyse_bit_xor_assign(context, to, expr, left, right);
		case BINARYOP_NE:
			// TODO special handling
			return sema_expr_analyse_comp(context, to, expr, left, right);
		case BINARYOP_EQ:
			// TODO special handling
			return sema_expr_analyse_comp(context, to, expr, left, right);
		case BINARYOP_GT:
		case BINARYOP_GE:
		case BINARYOP_LT:
		case BINARYOP_LE:
			return sema_expr_analyse_comp(context, to, expr, left, right);
		case BINARYOP_SHR:
			return sema_expr_analyse_shr(context, to, expr, left, right);
		case BINARYOP_SHR_ASSIGN:
			return sema_expr_analyse_shr_assign(context, to, expr, left, right);
		case BINARYOP_SHL:
			return sema_expr_analyse_shl(context, to, expr, left, right);
		case BINARYOP_SHL_ASSIGN:
			return sema_expr_analyse_shl_assign(context, to, expr, left, right);
		default:
			UNREACHABLE
	}
}

static inline bool sema_expr_analyse_unary(Context *context, Type *to, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_RUNNING);
	Expr *inner = expr->unary_expr.expr;

	if (!sema_analyse_expr(context, NULL, inner)) return false;

	switch (expr->unary_expr.operator)
	{
		case UNARYOP_DEREF:
			return sema_expr_analyse_deref(context, to, expr, inner);
		case UNARYOP_ADDR:
			return sema_expr_analyse_addr(context, to, expr, inner);
		case UNARYOP_NEG:
			return sema_expr_analyse_neg(context, to, expr, inner);
		case UNARYOP_BITNEG:
			return sema_expr_analyse_bit_not(context, to, expr, inner);
		case UNARYOP_NOT:
			return sema_expr_analyse_not(context, to, expr, inner);
		case UNARYOP_DEC:
		case UNARYOP_INC:
			return sema_expr_analyse_incdec(context, to, expr, inner);
		case UNARYOP_ERROR:
			return false;
	}
}

static inline bool sema_expr_analyse_post_unary(Context *context, Type *to, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_RUNNING);
	Expr *inner = expr->post_expr.expr;

	if (!sema_analyse_expr(context, NULL, inner)) return false;

	return sema_expr_analyse_incdec(context, to, expr, inner);
}


static inline bool sema_expr_analyse_try(Context *context, Type *to, Expr *expr)
{
	if (!sema_analyse_expr(context, to, expr->try_expr.expr)) return false;
	expr->type = expr->try_expr.expr->type;
	if (expr->try_expr.else_expr)
	{
		if (!sema_analyse_expr(context, to, expr->try_expr.else_expr)) return false;
	}
	// Check errors!
	TODO
	return true;
}

static Ast *ast_shallow_copy(Ast *source)
{
	Ast *copy = malloc_arena(sizeof(Ast));
	memcpy(copy, source, sizeof(Ast));
	return copy;
}

static Expr *expr_shallow_copy(Expr *source)
{
	Expr *copy = malloc_arena(sizeof(Expr));
	memcpy(copy, source, sizeof(Expr));
	return copy;
}

static Expr **expr_copy_expr_list_from_macro(Context *context, Expr *macro, Expr **expr_list);
static Expr *expr_copy_from_macro(Context *context, Expr *macro, Expr *source_expr);
static Ast *ast_copy_from_macro(Context *context, Expr *macro, Ast *source);
static void ast_copy_list_from_macro(Context *context, Expr *macro, Ast ***to_convert);

static TypeInfo *type_info_copy_from_macro(Context *context, Expr *macro, TypeInfo *source)
{
	if (!source) return NULL;
	TypeInfo *copy = malloc_arena(sizeof(TypeInfo));
	memcpy(copy, source, sizeof(TypeInfo));
	switch (source->kind)
	{
		case TYPE_INFO_POISON:
			return copy;
		case TYPE_INFO_IDENTIFIER:
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			TODO
			break;
		case TYPE_INFO_EXPRESSION:
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			copy->unresolved_type_expr = expr_copy_from_macro(context, macro, source->unresolved_type_expr);
			return copy;
		case TYPE_INFO_ARRAY:
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			copy->array.len = expr_copy_from_macro(context, macro, source->array.len);
			copy->array.base = type_info_copy_from_macro(context, macro, source->array.base);
			return copy;
		case TYPE_INFO_INC_ARRAY:
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			copy->array.base = type_info_copy_from_macro(context, macro, source->array.base);
			return copy;
		case TYPE_INFO_POINTER:
			assert(source->resolve_status == RESOLVE_NOT_DONE);
			copy->pointer = type_info_copy_from_macro(context, macro, source->pointer);
			return copy;
	}
}



static Expr *expr_copy_from_macro(Context *context, Expr *macro, Expr *source_expr)
{
#define EXPR_COPY(x) x = expr_copy_from_macro(context, macro, x)
	if (!source_expr) return NULL;
	Expr *expr = expr_shallow_copy(source_expr);
	switch (source_expr->expr_kind)
	{
		case EXPR_POISONED:
			return source_expr;
		case EXPR_TRY:
			EXPR_COPY(expr->try_expr.expr);
			EXPR_COPY(expr->try_expr.else_expr);
			return expr;
		case EXPR_CONST:
			return expr;
		case EXPR_BINARY:
			EXPR_COPY(expr->binary_expr.left);
			EXPR_COPY(expr->binary_expr.right);
			return expr;
		case EXPR_TERNARY:
			EXPR_COPY(expr->ternary_expr.cond);
			EXPR_COPY(expr->ternary_expr.then_expr);
			EXPR_COPY(expr->ternary_expr.else_expr);
			return expr;
		case EXPR_UNARY:
			EXPR_COPY(expr->unary_expr.expr);
			return expr;
		case EXPR_POST_UNARY:
			EXPR_COPY(expr->post_expr.expr);
			return expr;
		case EXPR_TYPE:
			expr->type_expr.type = type_info_copy_from_macro(context, macro, expr->type_expr.type);
			return expr;
		case EXPR_IDENTIFIER:
			TODO
			break;
		case EXPR_TYPE_ACCESS:
			expr->type_access.type = type_info_copy_from_macro(context, macro, expr->type_expr.type);
			return expr;
		case EXPR_CALL:
			EXPR_COPY(expr->call_expr.function);
			expr->call_expr.arguments = expr_copy_expr_list_from_macro(context, macro, expr->call_expr.arguments);
			return expr;
		case EXPR_SIZEOF:
			TODO
			break;
		case EXPR_SUBSCRIPT:
			EXPR_COPY(expr->subscript_expr.expr);
			EXPR_COPY(expr->subscript_expr.index);
			return expr;
		case EXPR_ACCESS:
			EXPR_COPY(expr->access_expr.parent);
			return expr;
		case EXPR_STRUCT_VALUE:
			expr->struct_value_expr.type = type_info_copy_from_macro(context, macro, expr->struct_value_expr.type);
			EXPR_COPY(expr->struct_value_expr.init_expr);
			return expr;
		case EXPR_STRUCT_INIT_VALUES:
			TODO
			return expr;
		case EXPR_INITIALIZER_LIST:
			expr->initializer_expr = expr_copy_expr_list_from_macro(context, macro, expr->initializer_expr);
			return expr;
		case EXPR_EXPRESSION_LIST:
			expr->expression_list = expr_copy_expr_list_from_macro(context, macro, expr->expression_list);
			return expr;
		case EXPR_CAST:
			EXPR_COPY(expr->cast_expr.expr);
			expr->cast_expr.type_info = expr->cast_expr.type_info = type_info_copy_from_macro(context, macro, expr->cast_expr.type_info);
			return expr;
		case EXPR_SCOPED_EXPR:
			EXPR_COPY(expr->expr_scope.expr);
			return expr;
		case EXPR_MACRO_EXPR:
			EXPR_COPY(expr->macro_expr);
			return expr;
	}
#undef EXPR_COPY
}

static Expr **expr_copy_expr_list_from_macro(Context *context, Expr *macro, Expr **expr_list)
{
	Expr **result = NULL;
	VECEACH(expr_list, i)
	{
		vec_add(result, expr_copy_from_macro(context, macro, expr_list[i]));
	}
	return result;
}

static void ast_copy_list_from_macro(Context *context, Expr *macro, Ast ***to_convert)
{
	Ast **result = NULL;
	Ast **list = *to_convert;
	VECEACH(list, i)
	{
		vec_add(result, ast_copy_from_macro(context, macro, list[i]));
	}
	*to_convert = result;
}

static void type_info_copy_list_from_macro(Context *context, Expr *macro, TypeInfo ***to_convert)
{
	TypeInfo **result = NULL;
	TypeInfo **list = *to_convert;
	VECEACH(list, i)
	{
		vec_add(result, type_info_copy_from_macro(context, macro, list[i]));
	}
	*to_convert = result;
}

static Ast *ast_copy_from_macro(Context *context, Expr *macro, Ast *source)
{
#define EXPR_COPY(x) x = expr_copy_from_macro(context, macro, x)
#define AST_COPY(x) x = ast_copy_from_macro(context, macro, x)
	Ast *ast = ast_shallow_copy(source);
	switch (source->ast_kind)
	{
		case AST_POISONED:
			return ast;
		case AST_ASM_STMT:
			TODO
		case AST_ATTRIBUTE:
			UNREACHABLE
		case AST_BREAK_STMT:
			return ast;
		case AST_CASE_STMT:
			AST_COPY(ast->case_stmt.body);
			EXPR_COPY(ast->case_stmt.expr);
			return ast;
			break;
		case AST_CATCH_STMT:
			AST_COPY(ast->catch_stmt.body);
			return ast;
		case AST_COMPOUND_STMT:
			ast_copy_list_from_macro(context, macro, &ast->compound_stmt.stmts);
			return ast;
		case AST_CONTINUE_STMT:
			return ast;
		case AST_CT_IF_STMT:
			EXPR_COPY(ast->ct_if_stmt.expr);
			AST_COPY(ast->ct_if_stmt.elif);
			AST_COPY(ast->ct_if_stmt.then);
			return ast;
		case AST_CT_ELIF_STMT:
			EXPR_COPY(ast->ct_elif_stmt.expr);
			AST_COPY(ast->ct_elif_stmt.then);
			AST_COPY(ast->ct_elif_stmt.elif);
			return ast;
		case AST_CT_ELSE_STMT:
			AST_COPY(ast->ct_else_stmt);
			return ast;
		case AST_CT_FOR_STMT:
			AST_COPY(ast->ct_for_stmt.body);
			EXPR_COPY(ast->ct_for_stmt.expr);
			return ast;
		case AST_CT_SWITCH_STMT:
			EXPR_COPY(ast->ct_switch_stmt.cond);
			ast_copy_list_from_macro(context, macro, &ast->ct_switch_stmt.body);
			return ast;
		case AST_CT_DEFAULT_STMT:
			AST_COPY(ast->ct_default_stmt);
			return ast;
		case AST_CT_CASE_STMT:
			AST_COPY(ast->ct_case_stmt.body);
			type_info_copy_list_from_macro(context, macro, &ast->ct_case_stmt.types);
			return ast;
		case AST_DECLARE_STMT:
			TODO
			return ast;
		case AST_DEFAULT_STMT:
			AST_COPY(ast->case_stmt.body);
			return ast;
		case AST_DEFER_STMT:
			assert(!ast->defer_stmt.prev_defer);
			AST_COPY(ast->defer_stmt.body);
			return ast;
		case AST_DO_STMT:
			AST_COPY(ast->do_stmt.body);
			EXPR_COPY(ast->do_stmt.expr);
			return ast;
		case AST_EXPR_STMT:
			EXPR_COPY(ast->expr_stmt);
			return ast;
		case AST_FOR_STMT:
			EXPR_COPY(ast->for_stmt.cond);
			EXPR_COPY(ast->for_stmt.incr);
			AST_COPY(ast->for_stmt.body);
			AST_COPY(ast->for_stmt.init);
			return ast;
		case AST_FUNCTION_BLOCK_STMT:
			ast_copy_list_from_macro(context, macro, &ast->function_block_stmt.stmts);
			return ast;
		case AST_GENERIC_CASE_STMT:
			AST_COPY(ast->generic_case_stmt.body);
			// ast->generic_case_stmt.types = ...
			TODO
			return ast;
		case AST_GENERIC_DEFAULT_STMT:
			AST_COPY(ast->generic_default_stmt);
			return ast;
		case AST_GOTO_STMT:
			AST_COPY(ast->goto_stmt.label);
			// TODO fixup name, which needs to be macro local.
			TODO
			return ast;
		case AST_IF_STMT:
			AST_COPY(ast->if_stmt.cond);
			AST_COPY(ast->if_stmt.decl);
			AST_COPY(ast->if_stmt.else_body);
			AST_COPY(ast->if_stmt.then_body);
			return ast;
		case AST_LABEL:
			assert(!ast->label_stmt.defer);
			assert(!ast->label_stmt.in_defer);
			// TODO fixup name which needs to be macro local.
			TODO
			return ast;
		case AST_NOP_STMT:
			return ast;
		case AST_RETURN_STMT:
			EXPR_COPY(ast->return_stmt.expr);
			// TODO handle conversions?
			TODO
			return ast;
		case AST_DECL_EXPR_LIST:
			ast_copy_list_from_macro(context, macro, &ast->decl_expr_stmt);
			return ast;
		case AST_SWITCH_STMT:
			AST_COPY(ast->switch_stmt.decl);
			AST_COPY(ast->switch_stmt.cond);
			ast_copy_list_from_macro(context, macro, &ast->switch_stmt.cases);
			return ast;
		case AST_THROW_STMT:
			EXPR_COPY(ast->throw_stmt.throw_value);
			return ast;
		case AST_TRY_STMT:
			AST_COPY(ast->try_stmt);
			return ast;
		case AST_NEXT_STMT:
			TODO
			return ast;
		case AST_VOLATILE_STMT:
			TODO
			return ast;
		case AST_WHILE_STMT:
			AST_COPY(ast->while_stmt.cond);
			AST_COPY(ast->while_stmt.decl);
			AST_COPY(ast->while_stmt.body);
			return ast;
		case AST_SCOPED_STMT:
			AST_COPY(ast->scoped_stmt.stmt);
			return ast;
	}

#undef EXPR_COPY
#undef AST_COPY
}
static inline bool sema_expr_analyse_macro_call(Context *context, Type *to, Expr *macro, Expr *inner)
{
	Expr *func_expr = inner->call_expr.function;

	if (!sema_analyse_expr(context, NULL, func_expr)) return false;

	Decl *decl;
	switch (func_expr->expr_kind)
	{
		case EXPR_TYPE_ACCESS:
			TODO
		case EXPR_IDENTIFIER:
			decl = func_expr->identifier_expr.decl;
			break;
		default:
			TODO
	}
	if (decl->decl_kind != DECL_MACRO)
	{
		SEMA_ERROR(macro, "A macro was expected here.");
		return false;
	}
	Expr **args =func_expr->call_expr.arguments;
	Decl **func_params = decl->macro_decl.parameters;
	// TODO handle bare macros.
	// TODO handle $ args and # args
	unsigned num_args = vec_size(args);
	// unsigned num_params = vec_size(func_params);
	for (unsigned i = 0; i < num_args; i++)
	{
		Expr *arg = args[i];
		Decl *param = func_params[i];
		if (!sema_analyse_expr(context, param->type, arg)) return false;
	}
	Ast *body = ast_copy_from_macro(context, inner, decl->macro_decl.body);
	TODO
}

static inline bool sema_expr_analyse_macro_call2(Context *context, Type *to, Expr *expr, Decl *macro)
{
	Ast *macro_parent;
	// TODO handle loops
	Decl *stored_macro = context->evaluating_macro;
	Type *stored_rtype = context->rtype;
	context->evaluating_macro = macro;
	context->rtype = macro->macro_decl.rtype->type;
	// Handle escaping macro
	bool success = sema_analyse_statement(context, macro->macro_decl.body);
	context->evaluating_macro = stored_macro;
	context->rtype = stored_rtype;
	if (!success) return false;

	TODO
	return success;
};

static inline bool sema_expr_analyse_macro_expr(Context *context, Type *to, Expr *expr)
{
	Expr *inner = expr->macro_expr;
	switch (inner->expr_kind)
	{
		case EXPR_CALL:
			return sema_expr_analyse_macro_call(context, to, expr, inner);
		case EXPR_ACCESS:
		case EXPR_IDENTIFIER:
			// Allow @f unrolling?
		default:
			SEMA_ERROR(expr, "Expected a macro name after '@'");
			return false;
	}
}

static inline bool sema_expr_analyse_type(Context *context, Type *to, Expr *expr)
{
	if (!sema_resolve_type_info(context, expr->type_expr.type))
	{
		return expr_poison(expr);
	}
	expr->type = type_get_meta(expr->type_expr.type->type);
	return true;
}






static inline bool sema_analyse_expr_dispatch(Context *context, Type *to, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_POISONED:
			return false;
		case EXPR_SCOPED_EXPR:
			UNREACHABLE
		case EXPR_MACRO_EXPR:
			return sema_expr_analyse_macro_expr(context, to, expr);
		case EXPR_TRY:
			return sema_expr_analyse_try(context, to, expr);
		case EXPR_CONST:
			return true;
		case EXPR_BINARY:
			return sema_expr_analyse_binary(context, to, expr);
		case EXPR_TERNARY:
			return sema_expr_analyse_ternary(context, to, expr);
		case EXPR_UNARY:
			return sema_expr_analyse_unary(context, to, expr);
		case EXPR_POST_UNARY:
			return sema_expr_analyse_post_unary(context, to, expr);
		case EXPR_TYPE:
			return sema_expr_analyse_type(context, to, expr);
		case EXPR_IDENTIFIER:
			return sema_expr_analyse_identifier(context, to, expr);
		case EXPR_TYPE_ACCESS:
			return sema_expr_analyse_type_access(context, to, expr);
		case EXPR_CALL:
			return sema_expr_analyse_call(context, to, expr);
		case EXPR_SIZEOF:
			return sema_expr_analyse_sizeof(context, to, expr);
		case EXPR_SUBSCRIPT:
			return sema_expr_analyse_subscript(context, to, expr);
		case EXPR_ACCESS:
			return sema_expr_analyse_access(context, to, expr);
		case EXPR_STRUCT_VALUE:
			return sema_expr_analyse_struct_value(context, to, expr);
		case EXPR_STRUCT_INIT_VALUES:
			return sema_expr_analyse_struct_init_values(context, to, expr);
		case EXPR_INITIALIZER_LIST:
			return sema_expr_analyse_initializer_list(context, to, expr);
		case EXPR_CAST:
			return sema_expr_analyse_cast(context, to, expr);
		case EXPR_EXPRESSION_LIST:
			return sema_expr_analyse_expr_list(context, to, expr);
	}
	UNREACHABLE
}


bool sema_analyse_expr(Context *context, Type *to, Expr *expr)
{
	switch (expr->resolve_status)
	{
		case RESOLVE_NOT_DONE:
			expr->resolve_status = RESOLVE_RUNNING;
			break;
		case RESOLVE_RUNNING:
			SEMA_ERROR(expr, "Recursive resolution of expression");
			return expr_poison(expr);
		case RESOLVE_DONE:
			return expr_ok(expr);
	}
	if (!sema_analyse_expr_dispatch(context, to, expr)) return expr_poison(expr);
	expr->resolve_status = RESOLVE_DONE;
	return cast_implicit(expr, to);
}