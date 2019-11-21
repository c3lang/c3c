// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_internal.h"

typedef bool(*ExprAnalysis)(Context *, Expr*);
typedef bool(*ExprBinopAnalysis)(Context *, Expr*, Expr*, Expr*);
typedef bool(*ExprUnaryAnalysis)(Context *, Expr*, Expr*);

static ExprBinopAnalysis BINOP_ANALYSIS[TOKEN_EOF];
static ExprUnaryAnalysis UNARYOP_ANALYSIS[TOKEN_EOF + 1];
static ExprUnaryAnalysis POSTUNARYOP_ANALYSIS[TOKEN_EOF + 1];

static bool expr_is_ltype(Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_IDENTIFIER:
			return expr->identifier_expr.decl->decl_kind == DECL_VAR && (expr->identifier_expr.decl->var.kind == VARDECL_LOCAL || expr->identifier_expr.decl->var.kind == VARDECL_GLOBAL);
		case EXPR_UNARY:
			return expr->unary_expr.operator == TOKEN_STAR;
		case EXPR_ACCESS:
			return expr_is_ltype(expr->access_expr.parent);
		default:
			return false;
	}
}

static inline bool sema_type_error_on_binop(const char *op, Expr *expr)
{
	SEMA_ERROR(expr->loc, "Cannot perform '%s' %s '%s'.", type_to_error_string(expr->binary_expr.left->type), op, type_to_error_string(expr->binary_expr.right->type));
	return false;
}


static inline bool sema_expr_analyse_conditional(Context *context, Expr *expr)
{
	TODO
}

static inline bool sema_expr_analyse_identifier(Context *context, Expr *expr)
{
	// TODO what about struct functions
	if (expr->identifier_expr.path)
	{
		TODO
	}
	Decl *decl = context_find_ident(context, expr->identifier_expr.identifier.string);
	if (decl == NULL)
	{
		decl = compiler_find_symbol(expr->identifier_expr.identifier);
		if (decl && !decl_ok(decl)) return false;
	}
	if (decl == NULL)
	{
		SEMA_ERROR(expr->loc, "Unknown identifier %s.", expr->identifier_expr.identifier.string);
		return false;
	}

	assert(decl->type);
	expr->identifier_expr.decl = decl;
	expr->type = decl->type;
	return true;
}

static inline bool sema_expr_analyse_var_call(Context *context, Expr *expr) { TODO }
static inline bool sema_expr_analyse_macro_call(Context *context, Expr *expr, Decl *macro)
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
static inline bool sema_expr_analyse_generic_call(Context *context, Expr *expr) { TODO };

static inline bool sema_expr_analyse_func_call(Context *context, Expr *expr, Decl *decl)
{
	if (decl->func.function_signature.throws != NULL) TODO
	Expr **args =expr->call_expr.arguments;
	Decl **func_params = decl->func.function_signature.params;
	unsigned num_args = vec_size(args);
	// unsigned num_params = vec_size(func_params);
	// TODO handle named parameters, handle default parameters, varargs etc
	for (unsigned i = 0; i < num_args; i++)
	{
		Expr *arg = args[i];
		if (!sema_analyse_expr(context, arg)) return false;
		if (!cast(arg, func_params[i]->type, CAST_TYPE_IMPLICIT_ASSIGN)) return false;
	}
	expr->type = decl->func.function_signature.rtype->type;
	return true;
}

static inline bool sema_expr_analyse_call(Context *context, Expr *expr)
{
	Expr *func_expr = expr->call_expr.function;
	if (!sema_analyse_expr(context, func_expr)) return false;
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
			return sema_expr_analyse_var_call(context, expr);
		case DECL_FUNC:
			return sema_expr_analyse_func_call(context, expr, decl);
		case DECL_MACRO:
			return sema_expr_analyse_macro_call(context, expr, decl);
		case DECL_GENERIC:
			return sema_expr_analyse_generic_call(context, expr);
		default:
			SEMA_ERROR(expr->loc, "The expression cannot be called.");
			return false;
			break;
	}
}

static inline bool sema_expr_analyse_struct_value(Context *context, Expr *expr)
{
	TODO
}

static inline bool sema_expr_analyse_struct_init_values(Context *context, Expr *expr)
{
	TODO
}

static inline bool sema_expr_analyse_subscript(Context *context, Expr *expr)
{
	TODO
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
			expr_replace(expr, decl->enum_constant.expr);
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

static inline bool sema_expr_analyse_access(Context *context, Expr *expr)
{
	if (!sema_analyse_expr(context, expr->access_expr.parent)) return false;
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
		deref->unary_expr.operator = TOKEN_STAR;
		deref->unary_expr.expr = expr->access_expr.parent;
		deref->resolve_status = RESOLVE_DONE;
		deref->type = type;
		expr->access_expr.parent = deref;
	}
	expr->type = member->type;
	expr->access_expr.index = index;
	return true;
}

static inline bool sema_expr_analyse_type_access(Context *context, Expr *expr)
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
static inline bool sema_expr_analyse_initializer_list(Context *context, Expr *expr)
{
	assert(context->left_type_in_assignment);
	Type *assigned = context->left_type_in_assignment->canonical;
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
	SEMA_ERROR(expr->loc, "Cannot assign expression to '%s'.", type_to_error_string(context->left_type_in_assignment));
	return false;
}

static inline bool sema_expr_analyse_sizeof(Context *context, Expr *expr)
{
	TODO
}


static inline bool sema_expr_analyse_cast(Context *context, Expr *expr)
{
	Expr *inner = expr->expr_cast.expr;
	if (!sema_resolve_type_info(context, expr->expr_cast.type_info)) return false;
	if (!sema_analyse_expr(context, inner)) return false;

	if (!cast(inner, expr->expr_cast.type_info->type, CAST_TYPE_EXPLICIT)) return false;

	// TODO above is probably not right, cast type not set.
	// Overwrite cast.
	Token loc = expr->loc;
	*expr = *inner;
	expr->loc = loc;

	return true;
}


static bool sema_expr_analyse_assign(Context *context, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_analyse_expr(context, left)) return false;

	if (!expr_is_ltype(left))
	{
		SEMA_ERROR(left->loc, "Expression is not assignable.");
		return false;
	}

	Type *prev_type = context->left_type_in_assignment;
	context->left_type_in_assignment = left->type;

	bool success = sema_analyse_expr(context, right);

	context->left_type_in_assignment = prev_type;

	if (!success) return false;

	if (!cast(right, left->type, CAST_TYPE_IMPLICIT_ASSIGN)) return false;

	return true;
}

static inline bool both_const(Expr *left, Expr *right)
{
	return left->expr_kind == EXPR_CONST && right->expr_kind == EXPR_CONST;
}

static bool sema_expr_analyse_bit_and_assign(Context *context, Expr *expr, Expr *left, Expr *right) { TODO }
static bool sema_expr_analyse_bit_or_assign(Context *context, Expr *expr, Expr *left, Expr *right) { TODO }
static bool sema_expr_analyse_bit_xor_assign(Context *context, Expr *expr, Expr *left, Expr *right) { TODO }
static bool sema_expr_analyse_div_assign(Context *context, Expr *expr, Expr *left, Expr *right) { TODO }
static bool sema_expr_analyse_add_assign(Context *context, Expr *expr, Expr *left, Expr *right) { TODO }
static bool sema_expr_analyse_sub_assign(Context *context, Expr *expr, Expr *left, Expr *right) { TODO }
static bool sema_expr_analyse_mult_assign(Context *context, Expr *expr, Expr *left, Expr *right) { TODO }

static bool sema_expr_analyse_add(Context *context, Expr *expr, Expr *left, Expr *right)
{
	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;
	if (left_type->type_kind == TYPE_POINTER)
	{
		if (!cast_arithmetic(right, left, "+")) return false;
		expr->type = left_type;
		return true;
	}
	if (right_type->type_kind == TYPE_POINTER)
	{
		if (!cast_arithmetic(left, right, "+")) return false;
		expr->type = right_type;
		return true;
	}
	if (!cast_arithmetic(left, right, "+")) return false;
	if (!cast_arithmetic(right, left, "+")) return false;

	Type *canonical = left->type->canonical;
	if (!type_is_number(canonical))
	{
		SEMA_ERROR(expr->loc, "Add is not allowed");
		return false;
	}
	if (both_const(left, right))
	{
		switch (left->const_expr.type)
		{
			case CONST_INT:
				expr->const_expr.i = left->const_expr.i + right->const_expr.i;
				break;
			case CONST_FLOAT:
				expr->const_expr.f = left->const_expr.f + right->const_expr.f;
				break;
			default:
				UNREACHABLE
		}
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.type = left->const_expr.type;
	}
	expr->type = left->type;
	return true;
}

static bool sema_expr_analyse_sub(Context *context, Expr *expr, Expr *left, Expr *right)
{
	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;
	if (left_type->type_kind == TYPE_POINTER)
	{
		if (right_type->type_kind == TYPE_POINTER)
		{
			if (!cast(right, left_type, CAST_TYPE_IMPLICIT)) return false;
			expr->type = type_isize;
			return true;
		}
		if (!cast_arithmetic(right, left, "-")) return false;
		expr->type = left->type;
		return true;
	}
	if (!cast_arithmetic(left, right, "-")) return false;
	if (!cast_arithmetic(right, left, "-")) return false;

	Type *canonical = left->type->canonical;
	if (!type_is_number(canonical))
	{
		SEMA_ERROR(expr->loc, "- is not allowed");
		return false;
	}
	if (both_const(left, right))
	{
		switch (left->const_expr.type)
		{
			case CONST_INT:
				expr->const_expr.i = left->const_expr.i - right->const_expr.i;
				break;
			case CONST_FLOAT:
				expr->const_expr.f = left->const_expr.f - right->const_expr.f;
				break;
			default:
				UNREACHABLE
		}
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.type = left->const_expr.type;
	}
	expr->type = left->type;
	return true;
}

static bool sema_expr_analyse_mult(Context *context, Expr *expr, Expr *left, Expr *right)
{
	if (!cast_arithmetic(left, right, "*")) return false;
	if (!cast_arithmetic(right, left, "*")) return false;

	Type *canonical = left->type->canonical;
	if (!type_is_number(canonical))
	{
		SEMA_ERROR(expr->loc, "* is not allowed");
		return false;
	}
	if (both_const(left, right))
	{
		switch (left->const_expr.type)
		{
			case CONST_INT:
				expr->const_expr.i = left->const_expr.i * right->const_expr.i;
				break;
			case CONST_FLOAT:
				expr->const_expr.f = left->const_expr.f * right->const_expr.f;
				break;
			default:
				UNREACHABLE
		}
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.type = left->const_expr.type;
	}
	expr->type = left->type;
	return true;
}

static bool sema_expr_analyse_div(Context *context, Expr *expr, Expr *left, Expr *right)
{
	if (!cast_arithmetic(left, right, "/")) return false;
	if (!cast_arithmetic(right, left, "/")) return false;

	Type *canonical = left->type->canonical;
	if (!type_is_number(canonical))
	{
		SEMA_ERROR(expr->loc, "/ is not allowed");
		return false;
	}
	if (both_const(left, right))
	{
		switch (left->const_expr.type)
		{
			case CONST_INT:
				expr->const_expr.i = left->const_expr.i / right->const_expr.i;
				break;
			case CONST_FLOAT:
				expr->const_expr.f = left->const_expr.f / right->const_expr.f;
				break;
			default:
				UNREACHABLE
		}
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.type = left->const_expr.type;
	}
	expr->type = left->type;
	return true;
}


	/*
	switch (left->type)
	{
		case CONST_NIL:
		case CONST_BOOL:
		case CONST_STRING:
			UNREACHABLE;
		case CONST_INT:
			if (right->i == 0)
			{
				SEMA_ERROR(expr->binary_expr.right->loc, "Division by zero not allowed.");
				return false;
			}
			result->i = left->i / right->i;
			break;
		case CONST_FLOAT:
			if (right->f == 0)
			{
				SEMA_ERROR(expr->binary_expr.right->loc, "Division by zero not allowed.");
				return false;
			}
			expr->const_expr.f = left->f / right->f;
			expr->const_expr.type = CONST_FLOAT;
			break;
	}*/

static bool sema_expr_analyse_mod(Context *context, Expr *expr, Expr *left, Expr *right)
{
	if (!cast_arithmetic(left, right, "%") || !cast_arithmetic(right, left, "%")) return false;
	if (!type_is_integer(right->type->canonical) || !type_is_integer(left->type->canonical)) return sema_type_error_on_binop("%", expr);

	if (right->expr_kind == EXPR_CONST && right->const_expr.i == 0)
	{
		SEMA_ERROR(expr->binary_expr.right->loc, "Cannot perform mod by zero.");
		return false;
	}
	if (left->expr_kind == EXPR_CONST && right->expr_kind == EXPR_CONST)
	{
		// TODO negative
		expr_replace(expr, left);
		expr->const_expr.i %= right->const_expr.i;
		return true;
	}

	expr->type = left->type;
	return true;
}

static bool sema_expr_analyse_mod_assign(Context *context, Expr *expr, Expr *left, Expr *right) { TODO }

#define SEMA_ANALYSE_BIT(op) \
{ if (!cast_arithmetic(left, right, #op) || !cast_arithmetic(right, left, #op)) return false; \
if (left->type->canonical->type_kind != TYPE_BOOL && !type_is_integer(left->type)) sema_type_error_on_binop(#op, expr); \
if (left->expr_kind == EXPR_CONST && right->expr_kind == EXPR_CONST)\
{ expr_replace(expr, left); \
if (left->const_expr.type == CONST_BOOL) \
{ expr->const_expr.b |= right->const_expr.b; } \
else { expr->const_expr.i |= expr->const_expr.i; } \
return true; } \
expr->type = left->type; \
return true; }

static bool sema_expr_analyse_bit_or(Context *context, Expr *expr, Expr *left, Expr *right) SEMA_ANALYSE_BIT(|)
static bool sema_expr_analyse_bit_xor(Context *context, Expr *expr, Expr *left, Expr *right) SEMA_ANALYSE_BIT(^)
static bool sema_expr_analyse_bit_and(Context *context, Expr *expr, Expr *left, Expr *right) SEMA_ANALYSE_BIT(&)

static bool sema_expr_analyse_shr(Context *context, Expr *expr, Expr *left, Expr *right)
{
	// Todo: proper handling of signed / unsigned. Proper reduction of constants.
	if (!type_is_integer(left->type) || !type_is_integer(right->type)) return sema_type_error_on_binop(">>", expr);
	if (type_is_signed(right->type->canonical))
	{
		cast(right, type_long, CAST_TYPE_IMPLICIT);
	}
	else
	{
		cast(left, type_ulong, CAST_TYPE_IMPLICIT);
	}
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
}

static bool sema_expr_analyse_shl(Context *context, Expr *expr, Expr *left, Expr *right)
{
	// Todo: proper handling of signed / unsigned. Proper reduction of constants.
	if (!type_is_integer(left->type) || !type_is_integer(right->type)) return sema_type_error_on_binop("<<", expr);
	if (type_is_signed(right->type->canonical))
	{
		cast(right, type_long, CAST_TYPE_IMPLICIT);
	}
	else
	{
		cast(left, type_ulong, CAST_TYPE_IMPLICIT);
	}
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
}

static bool sema_expr_analyse_shr_assign(Context *context, Expr *expr, Expr *left, Expr *right) { TODO }
static bool sema_expr_analyse_shl_assign(Context *context, Expr *expr, Expr *left, Expr *right) { TODO }

static bool sema_expr_analyse_and(Context *context, Expr *expr, Expr *left, Expr *right)
{
	expr->type = type_bool;
	if (!cast(left, type_bool, CAST_TYPE_IMPLICIT)) return false;
	if (!cast(right, type_bool, CAST_TYPE_IMPLICIT)) return false;
	if (both_const(left, right))
	{
		expr_replace(expr, left);
		expr->const_expr.b &= right->const_expr.b;
	}
	return true;
}

static bool sema_expr_analyse_or(Context *context, Expr *expr, Expr *left, Expr *right)
{
	if (!cast(left, type_bool, CAST_TYPE_IMPLICIT)) return false;
	if (!cast(right, type_bool, CAST_TYPE_IMPLICIT)) return false;
	if (both_const(left, right))
	{
		expr_replace(expr, left);
		expr->const_expr.b |= right->const_expr.b;
	}
	expr->type = type_bool;
	return true;
}

static bool sema_expr_analyse_and_assign(Context *context, Expr *expr, Expr *left, Expr *right) { TODO }
static bool sema_expr_analyse_or_assign(Context *context, Expr *expr, Expr *left, Expr *right) { TODO }

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

static bool sema_expr_analyse_eq(Context *context, Expr *expr, Expr *left, Expr *right) SEMA_ANALYSE_CMP(==)
static bool sema_expr_analyse_ne(Context *context, Expr *expr, Expr *left, Expr *right) SEMA_ANALYSE_CMP(!=)
static bool sema_expr_analyse_ge(Context *context, Expr *expr, Expr *left, Expr *right) SEMA_ANALYSE_CMP(>=)
static bool sema_expr_analyse_gt(Context *context, Expr *expr, Expr *left, Expr *right) SEMA_ANALYSE_CMP(>)
static bool sema_expr_analyse_le(Context *context, Expr *expr, Expr *left, Expr *right) SEMA_ANALYSE_CMP(<=)
static bool sema_expr_analyse_lt(Context *context, Expr *expr, Expr *left, Expr *right) SEMA_ANALYSE_CMP(<)

static bool sema_expr_analyse_elvis(Context *context, Expr *expr, Expr *left, Expr *right) { TODO }

static bool sema_expr_analyse_deref(Context *context, Expr *expr, Expr *inner)
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

static bool sema_expr_analyse_addr(Context *context, Expr *expr, Expr *inner)
{
	if (!expr_is_ltype(inner))
	{
		SEMA_ERROR(inner->loc, "Cannot take the address of a value of type '%s'", type_to_error_string(inner->type));
		return false;
	}
	expr->type = type_get_ptr(inner->type);
	return true;
}

static bool sema_expr_analyse_neg(Context *context, Expr *expr, Expr *inner)
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
static bool sema_expr_analyse_bit_not(Context *context, Expr *expr, Expr *inner)
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
static bool sema_expr_analyse_not(Context *context, Expr *expr, Expr *inner)
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



static inline bool sema_expr_analyse_incdec(Context *context, Expr *expr, Expr *inner)
{
	if (!expr_is_ltype(inner))
	{
		SEMA_ERROR(inner->loc, "Expression cannot be assigned to");
		return false;
	}
	if (!type_is_integer(inner->type->canonical) && inner->type->canonical->type_kind == TYPE_POINTER)
	{
		SEMA_ERROR(inner->loc, "Expression must be an integer or pointer");
		return false;
	}
	expr->type = inner->type;
	return true;
}


static inline bool sema_expr_analyse_binary(Context *context, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_RUNNING);
	Expr *left = expr->binary_expr.left;
	Expr *right = expr->binary_expr.right;

	// Special handling due to initalizer lists
	if (expr->binary_expr.operator == TOKEN_EQ)
	{
		return sema_expr_analyse_assign(context, expr, left, right);
	}

	if (!sema_analyse_expr(context, left)) return false;
	if (!sema_analyse_expr(context, right)) return false;

	assert(left->type);
	assert(right->type);
	return BINOP_ANALYSIS[expr->binary_expr.operator](context, expr, left, right);
}

static inline bool sema_expr_analyse_unary(Context *context, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_RUNNING);
	Expr *inner = expr->unary_expr.expr;

	if (!sema_analyse_expr(context, inner)) return false;

	return UNARYOP_ANALYSIS[expr->unary_expr.operator](context, expr, inner);
}

static inline bool sema_expr_analyse_postunary(Context *context, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_RUNNING);
	Expr *inner = expr->post_expr.expr;

	if (!sema_analyse_expr(context, inner)) return false;

	assert(expr->post_expr.operator == TOKEN_PLUSPLUS || expr->post_expr.operator == TOKEN_MINUSMINUS);
	return sema_expr_analyse_incdec(context, expr, inner);
}

static inline bool sema_expr_analyse_try(Context *context, Expr *expr)
{
	if (!sema_analyse_expr(context, expr->try_expr.expr)) return false;
	expr->type = expr->try_expr.expr->type;
	if (expr->try_expr.else_expr)
	{
		if (!sema_analyse_expr(context, expr->try_expr.else_expr)) return false;
		if (!cast(expr->try_expr.else_expr, expr->type, CAST_TYPE_IMPLICIT)) return false;
	}
	// Check errors!
	TODO
	return true;
}

static inline bool sema_expr_analyse_type(Context *context, Expr *expr)
{
	TODO
	return true;
}


static ExprBinopAnalysis BINOP_ANALYSIS[TOKEN_EOF] = {
		[TOKEN_EQ] = NULL, // Explicitly dispatched
		[TOKEN_STAR] = &sema_expr_analyse_mult,
		[TOKEN_MULT_ASSIGN] = &sema_expr_analyse_mult_assign,
		[TOKEN_PLUS] = &sema_expr_analyse_add,
		[TOKEN_PLUS_ASSIGN] = &sema_expr_analyse_add_assign,
		[TOKEN_MINUS] = &sema_expr_analyse_sub,
		[TOKEN_MINUS_ASSIGN] = &sema_expr_analyse_sub_assign,
		[TOKEN_DIV] = &sema_expr_analyse_div,
		[TOKEN_DIV_ASSIGN] = &sema_expr_analyse_div_assign,
		[TOKEN_MOD] = &sema_expr_analyse_mod,
		[TOKEN_MOD_ASSIGN] = &sema_expr_analyse_mod_assign,
		[TOKEN_AND] = &sema_expr_analyse_and,
		[TOKEN_AND_ASSIGN] = &sema_expr_analyse_and_assign,
		[TOKEN_OR] = &sema_expr_analyse_or,
		[TOKEN_OR_ASSIGN] = &sema_expr_analyse_or_assign,
		[TOKEN_AMP] = &sema_expr_analyse_bit_and,
		[TOKEN_BIT_AND_ASSIGN] = &sema_expr_analyse_bit_and_assign,
		[TOKEN_BIT_OR] = &sema_expr_analyse_bit_or,
		[TOKEN_BIT_OR_ASSIGN] = &sema_expr_analyse_bit_or_assign,
		[TOKEN_BIT_XOR] = &sema_expr_analyse_bit_xor,
		[TOKEN_BIT_XOR_ASSIGN] = &sema_expr_analyse_bit_xor_assign,
		[TOKEN_NOT_EQUAL] = &sema_expr_analyse_ne,
		[TOKEN_EQEQ] = &sema_expr_analyse_eq,
		[TOKEN_GREATER_EQ] = &sema_expr_analyse_ge,
		[TOKEN_GREATER] = &sema_expr_analyse_gt,
		[TOKEN_LESS_EQ] = &sema_expr_analyse_le,
		[TOKEN_LESS] = &sema_expr_analyse_lt,
		[TOKEN_SHR] = &sema_expr_analyse_shr,
		[TOKEN_SHR_ASSIGN] = &sema_expr_analyse_shr_assign,
		[TOKEN_SHL] = &sema_expr_analyse_shl,
		[TOKEN_SHL_ASSIGN] = &sema_expr_analyse_shl_assign,
		[TOKEN_ELVIS] = &sema_expr_analyse_elvis,
};


static ExprUnaryAnalysis UNARYOP_ANALYSIS[TOKEN_EOF + 1] = {
		[TOKEN_STAR] = &sema_expr_analyse_deref,
		[TOKEN_AMP] = &sema_expr_analyse_addr,
		[TOKEN_MINUS] = &sema_expr_analyse_neg,
		[TOKEN_BIT_NOT] = &sema_expr_analyse_bit_not,
		[TOKEN_NOT] = &sema_expr_analyse_not,
		[TOKEN_PLUSPLUS] = &sema_expr_analyse_incdec,
		[TOKEN_MINUSMINUS] = &sema_expr_analyse_incdec,
};

static ExprAnalysis EXPR_ANALYSIS[EXPR_CAST + 1] = {
		[EXPR_TRY] = &sema_expr_analyse_try,
		[EXPR_CONST] = NULL,
		[EXPR_BINARY] = &sema_expr_analyse_binary,
		[EXPR_CONDITIONAL] = &sema_expr_analyse_conditional,
		[EXPR_UNARY] = &sema_expr_analyse_unary,
		[EXPR_POST_UNARY] = &sema_expr_analyse_postunary,
		[EXPR_TYPE] = &sema_expr_analyse_type,
		[EXPR_IDENTIFIER] = &sema_expr_analyse_identifier,
		[EXPR_TYPE_ACCESS] = &sema_expr_analyse_type_access,
		[EXPR_CALL] = &sema_expr_analyse_call,
		[EXPR_SIZEOF] = &sema_expr_analyse_sizeof,
		[EXPR_SUBSCRIPT] = &sema_expr_analyse_subscript,
		[EXPR_ACCESS] = &sema_expr_analyse_access,
		[EXPR_STRUCT_VALUE] = &sema_expr_analyse_struct_value,
		[EXPR_STRUCT_INIT_VALUES] = &sema_expr_analyse_struct_init_values,
		[EXPR_INITIALIZER_LIST] = &sema_expr_analyse_initializer_list,
		[EXPR_CAST] = &sema_expr_analyse_cast,
};

bool sema_analyse_expr(Context *context, Expr *expr)
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
	if (!EXPR_ANALYSIS[expr->expr_kind](context, expr)) return expr_poison(expr);
	expr->resolve_status = RESOLVE_DONE;
	return true;
}