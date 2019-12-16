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
			return expr->unary_expr.operator == TOKEN_STAR;
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
	SEMA_ERROR(expr->loc, "Cannot perform '%s' %s '%s'.",
			type_to_error_string(expr->binary_expr.left->type), c, type_to_error_string(expr->binary_expr.right->type));
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
			SEMA_ERROR(cond->loc, "Cannot convert expression to boolean.");
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
			SEMA_ERROR(expr->loc, "Cannot find a common parent type of '%s' and '%s'",
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
	Decl *decl = sema_resolve_symbol(context, expr->identifier_expr.identifier.string, expr->identifier_expr.path, &ambiguous_decl);

	if (!decl)
	{
		SEMA_ERROR(expr->identifier_expr.identifier, "Unknown symbol '%s'.", expr->identifier_expr.identifier.string);
		return false;
	}

	if (ambiguous_decl)
	{
		SEMA_ERROR(expr->identifier_expr.identifier, "Ambiguous symbol '%s' – both defined in %s and %s, please add the module name to resolve the ambiguity",
		           expr->identifier_expr.identifier.string,
		           decl->module->name->module, ambiguous_decl->module->name->module);
		return false;
	}

	if (decl->decl_kind == DECL_FUNC && !expr->identifier_expr.path && decl->module != context->module)
	{
		SEMA_ERROR(expr->identifier_expr.identifier, "Functions from other modules, must be prefixed with the module name");
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
static inline bool sema_expr_analyse_macro_call(Context *context, Type *to, Expr *expr, Decl *macro)
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
static inline bool sema_expr_analyse_generic_call(Context *context, Type *to, Expr *expr) { TODO };

static inline bool sema_expr_analyse_func_call(Context *context, Type *to, Expr *expr, Decl *decl)
{
	Expr **args =expr->call_expr.arguments;
	Decl **func_params = decl->func.function_signature.params;
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
			return sema_expr_analyse_macro_call(context, to, expr, decl);
		case DECL_GENERIC:
			return sema_expr_analyse_generic_call(context, to, expr);
		case DECL_POISONED:
			return false;
		default:
			SEMA_ERROR(expr->loc, "The expression cannot be called.");
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
			SEMA_ERROR(expr->subscript_expr.expr->loc, "Cannot index '%s'.", type_to_error_string(type));
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
		if (function->name.string == name)
		{
			// TODO
			return true;
		}
	}
	SEMA_ERROR(expr->loc, "Cannot find method function '%s.%s'", decl->name.string, name);
	return false;
}

static inline bool sema_expr_analyse_enum_constant(Context *context, Expr *expr, Decl *decl)
{
	const char *name = expr->type_access.name.string;
	VECEACH(decl->enums.values, i)
	{
		Decl *enum_constant = decl->enums.values[i];
		if (enum_constant->name.string == name)
		{
			assert(enum_constant->resolve_status == RESOLVE_DONE);
			expr_replace(expr, enum_constant->enum_constant.expr);
			return true;
		}
	}
	SEMA_ERROR(expr->loc, "'%s' has no enumeration value '%s'.", decl->name.string, name);
	return false;
}

static inline bool sema_expr_analyse_error_constant(Context *context, Expr *expr, Decl *decl)
{
	const char *name = expr->type_access.name.string;
	VECEACH(decl->error.error_constants, i)
	{
		Decl *error_constant = decl->error.error_constants[i];
		if (error_constant->name.string == name)
		{
			assert(error_constant->resolve_status == RESOLVE_DONE);
			expr->type = decl->type;
			expr->expr_kind = EXPR_CONST;
			expr->const_expr.type = CONST_INT;
			expr->const_expr.i = decl->error_constant.value;
			return true;
		}
	}
	SEMA_ERROR(expr->loc, "'%s' has no error type '%s'.", decl->name.string, name);
	return false;
}

static Decl *strukt_recursive_search_member(Decl *strukt, const char *name, int *index)
{
	VECEACH(strukt->strukt.members, i)
	{
		(*index)++;
		Decl *member = strukt->strukt.members[i];
		if (member->name.string == name) return member;
		if (!member->name.string && decl_is_struct_type(member))
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
		SEMA_ERROR(expr->loc, "Cannot access '%s' on '%s'", expr->access_expr.sub_element.string, type_to_error_string(parent_type));
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
		SEMA_ERROR(expr->access_expr.sub_element, "There is no element '%s.%s'.", decl->name.string, expr->access_expr.sub_element.string);
		return false;
	}
	if (is_pointer)
	{
		Expr *deref = expr_new(EXPR_UNARY, expr->loc);
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
		SEMA_ERROR(expr->loc, "'%s' does not have method functions.", type_to_error_string(type_info->type));
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
		if (expr->type_access.name.string == function->name.string)
		{
			expr->type_access.method = function;
			expr->type = function->func.function_signature.rtype->type;
			return true;
		}
	}
	SEMA_ERROR(expr->loc, "No function '%s.%s' found.", type_to_error_string(type_info->type), expr->type_access.name.string);
	return false;
}

static inline Decl *decl_find_by_name(Decl** decls, const char *name)
{
	VECEACH(decls, i)
	{
		if (decls[i]->name.string == name) return decls[i];
	}
	return NULL;
}
static inline bool expr_may_be_struct_field_decl(Expr *maybe_binary)
{
	if (maybe_binary->expr_kind != EXPR_BINARY) return false;
	if (maybe_binary->binary_expr.operator != TOKEN_EQ) return false;
	Expr *expr = maybe_binary->binary_expr.left;
	while (1)
	{
		if (expr->expr_kind == EXPR_IDENTIFIER) return true;
		if (expr->expr_kind != EXPR_ACCESS) return false;
		expr = expr->access_expr.parent;
	}
}

static inline bool sema_expr_analyse_struct_initializer_list(Context *context, Type *assigned, Expr *expr)
{
	Decl **members = assigned->decl->strukt.members;
	unsigned size = vec_size(members);
	VECEACH(expr->initializer_expr, i)
	{
		Expr *field = expr->initializer_expr[i];
		Decl *decl;
		if (expr_may_be_struct_field_decl(field))
		{
			if (field->expr_kind == EXPR_IDENTIFIER)
			{
				decl = decl_find_by_name(members, field->identifier_expr.identifier.string);
			}
			TODO
		}
		else
		{
			if (i >= size)
			{
				SEMA_ERROR(field->loc, "Too many elements in initializer");
				return false;
			}
			decl = members[i];
		}
		if (!cast(field, decl->type, CAST_TYPE_IMPLICIT_ASSIGN)) return false;
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
	SEMA_ERROR(expr->loc, "Cannot assign expression to '%s'.", type_to_error_string(to));
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
	Token loc = expr->loc;
	*expr = *inner;
	expr->loc = loc;

	return true;
}


static bool sema_expr_analyse_assign(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_analyse_expr(context, NULL, left)) return false;

	if (!expr_is_ltype(left))
	{
		SEMA_ERROR(left->loc, "Expression is not assignable.");
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
		SEMA_ERROR(left->loc, "Expected a numeric type here.");
		return false;
	}

	if (!sema_analyse_expr(context, left->type->canonical, right)) return false;

	if (!type_is_number(right->type))
	{
		SEMA_ERROR(right->loc, "Expected a numeric type here.");
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
		SEMA_ERROR(left->loc, "Expected a numeric type here.");
		return false;
	}

	if (!sema_analyse_expr(context, left->type->canonical, right)) return false;

	if (!type_is_number(right->type))
	{
		SEMA_ERROR(right->loc, "Expected a numeric type here.");
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
		SEMA_ERROR(left->loc, "Expected a numeric type here.");
		return false;
	}

	if (!sema_analyse_expr(context, left->type->canonical, right)) return false;

	if (!type_is_number(right->type))
	{
		SEMA_ERROR(right->loc, "Expected a numeric type here.");
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
		SEMA_ERROR(left->loc, "Expected a numeric type here.");
		return false;
	}

	if (!sema_analyse_expr(context, left->type->canonical, right)) return false;

	if (!type_is_number(right->type))
	{
		SEMA_ERROR(right->loc, "Expected a numeric type here.");
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
		SEMA_ERROR(left->loc, "Expected a numeric type here.");
		return false;
	}

	if (!sema_analyse_expr(context, left->type->canonical, right)) return false;

	if (!type_is_number(right->type))
	{
		SEMA_ERROR(right->loc, "Expected a numeric type here.");
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
			SEMA_ERROR(right->loc, "Expected an integer type instead.");
			return false;
		}
		expr->type = left->type;
		return true;
	}

	if (!sema_analyse_expr(context, left->type->canonical, right)) return false;

	if (!type_is_number(left->type))
	{
		SEMA_ERROR(left->loc, "Expected a numeric type here.");
		return false;
	}

	if (!type_is_number(right->type))
	{
		SEMA_ERROR(right->loc, "Expected a numeric type here.");
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
			SEMA_ERROR(right->loc, "Expected an integer type instead.");
			return false;
		}
		expr->type = left->type;
		return true;
	}

	if (!sema_analyse_expr(context, left->type->canonical, right)) return false;

	if (!type_is_number(left->type))
	{
		SEMA_ERROR(left->loc, "Expected a numeric type here.");
		return false;
	}

	if (!type_is_number(right->type))
	{
		SEMA_ERROR(right->loc, "Expected a numeric type here.");
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
	SEMA_ERROR(expr->loc, "Cannot subtract '%s' from '%s'", type_to_error_string(left_type), type_to_error_string(right_type));
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
	SEMA_ERROR(expr->loc, "Cannot add '%s' to '%s'", type_to_error_string(left_type), type_to_error_string(right_type));
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
	SEMA_ERROR(expr->loc, "Cannot multiply '%s' and '%s'", type_to_error_string(left_type), type_to_error_string(right_type));
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
					SEMA_ERROR(right->loc, "Division by zero not allowed.");
					return false;
				}
				break;
			case CONST_FLOAT:
				if (right->const_expr.f == 0)
				{
					SEMA_ERROR(right->loc, "Division by zero not allowed.");
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
	SEMA_ERROR(expr->loc, "Cannot divide '%s' by '%s'", type_to_error_string(left_type), type_to_error_string(right_type));
	return false;

}

static bool sema_expr_analyse_mod(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_expr_analyse_binary_sub_expr(context, left, right)) return false;

	if (!type_is_integer(right->type->canonical) || !type_is_integer(left->type->canonical)) return sema_type_error_on_binop(expr);

	if (right->expr_kind == EXPR_CONST && right->const_expr.i == 0)
	{
		SEMA_ERROR(expr->binary_expr.right->loc, "Cannot perform mod by zero.");
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
			SEMA_ERROR(right->loc, "Rightshift exceeds bitsize of '%s'", type_to_error_string(left->type));
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
		SEMA_ERROR(left->loc, "Expression is not assignable.");
		return false;
	}

	// Check that right side is integer and cast to a runtime type if needed.
	if (!type_is_integer(right->type->canonical) || !cast_to_runtime(right)) return false;

	if (right->expr_kind == EXPR_CONST)
	{
		if (right->const_expr.i > left->type->canonical->builtin.bitsize)
		{
			SEMA_ERROR(right->loc, "Rightshift exceeds bitsize of '%s'", type_to_error_string(left->type));
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
			SEMA_ERROR(right->loc, "Leftshift exceeds bitsize of '%s'", type_to_error_string(left->type));
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
		SEMA_ERROR(left->loc, "Expression is not assignable.");
		return false;
	}

	// Check that right side is integer and cast to a runtime type if needed.
	if (!type_is_integer(right->type->canonical)) return sema_type_error_on_binop(expr);

	if (!cast_to_runtime(right)) return false;

	if (right->expr_kind == EXPR_CONST)
	{
		if (right->const_expr.i > left->type->canonical->builtin.bitsize)
		{
			SEMA_ERROR(right->loc, "Leftshift exceeds bitsize of '%s'", type_to_error_string(left->type));
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
		SEMA_ERROR(expr->loc, "Cannot implicitly convert types to evaluate '%s' %s '%s'", type_to_error_string(left_type), token_type_to_string(binaryop_to_token(expr->binary_expr.operator)), type_to_error_string(right_type));
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
		SEMA_ERROR(inner->loc, "Cannot take the dereference of a value of type '%s'", type_to_error_string(inner->type));
		return false;
	}
	if (inner->expr_kind == EXPR_CONST)
	{
		SEMA_ERROR(inner->loc, "Dereferencing nil is not allowed.");
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
		SEMA_ERROR(inner->loc, "Cannot take the address of a value of type '%s'", type_to_error_string(inner->type));
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
		SEMA_ERROR(expr->loc, "Cannot negate %s.", type_to_error_string(inner->type));
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
		SEMA_ERROR(expr->loc, "Cannot bit negate %s.", type_to_error_string(inner->type));
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
			SEMA_ERROR(expr->loc, "Cannot use 'not' on %s", type_to_error_string(inner->type));
			return false;
	}
	UNREACHABLE
}

static inline bool sema_expr_analyse_incdec(Context *context, Type *to, Expr *expr, Expr *inner)
{
	if (!expr_is_ltype(inner))
	{
		SEMA_ERROR(inner->loc, "Expression cannot be assigned to");
		return false;
	}
	if (!type_is_number(inner->type->canonical) && inner->type->canonical->type_kind != TYPE_POINTER)
	{
		SEMA_ERROR(inner->loc, "Expression must be a number or a pointer");
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
		default:
			UNREACHABLE
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

static inline bool sema_expr_analyse_type(Context *context, Type *to, Expr *expr)
{
	TODO
	return true;
}






static inline bool sema_analyse_expr_dispatch(Context *context, Type *to, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_POISONED:
			return false;
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
			SEMA_ERROR(expr->loc, "Recursive resolution of expression");
			return expr_poison(expr);
		case RESOLVE_DONE:
			return expr_ok(expr);
	}
	if (!sema_analyse_expr_dispatch(context, to, expr)) return expr_poison(expr);
	expr->resolve_status = RESOLVE_DONE;
	return cast_implicit(expr, to);
}