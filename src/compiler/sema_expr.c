// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"
#include <math.h>
/*
 * TODOs
 * - Disallow jumping in and out of an expression block.
 */

static inline bool sema_cast_rvalue(Context *context, Type *to, Expr *expr);

int BINOP_PREC_REQ[BINARYOP_LAST] =
{
		// bitwise operations
		[BINARYOP_BIT_OR] = 1,
		[BINARYOP_BIT_XOR] = 1,
		[BINARYOP_BIT_AND] = 1,

		// comparison operations
		[BINARYOP_GT] = 2,
		[BINARYOP_GE] = 2,
		[BINARYOP_LT] = 2,
		[BINARYOP_LE] = 2,
		[BINARYOP_NE] = 2,
		[BINARYOP_EQ] = 2,

		[BINARYOP_SHR] = 3,
		[BINARYOP_SHL] = 3,


};


static Expr *expr_access_inline_member(Expr *parent, Decl *parent_decl)
{
	Expr *embedded_struct = expr_new(EXPR_ACCESS, parent->span);
	embedded_struct->resolve_status = RESOLVE_DONE;
	embedded_struct->access_expr.parent = parent;
	embedded_struct->access_expr.ref = parent_decl->strukt.members[0];
	expr_set_type(embedded_struct, embedded_struct->access_expr.ref->type);
	return embedded_struct;
}

static inline bool sema_expr_analyse_binary(Context *context, Type *to, Expr *expr);
static inline bool sema_analyse_expr_value(Context *context, Type *to, Expr *expr);
static inline bool expr_const_int_valid(Expr *expr, Type *type)
{
	if (expr_const_int_overflowed(&expr->const_expr))
	{
		SEMA_ERROR(expr, "Cannot fit '%s' into type '%s'.", expr_const_to_error_string(&expr->const_expr), type_to_error_string(type));
		return false;
	}
	if (bigint_cmp_zero(&expr->const_expr.i) == CMP_LT && type_kind_is_unsigned(expr->const_expr.kind))
	{
		SEMA_ERROR(expr, "'%s' underflows type '%s'.", expr_const_to_error_string(&expr->const_expr), type_to_error_string(type));
		return false;
	}
	return true;
}

static inline bool is_const(Expr *expr)
{
	return expr->expr_kind == EXPR_CONST;
}

static inline bool both_const(Expr *left, Expr *right)
{
	return left->expr_kind == EXPR_CONST && right->expr_kind == EXPR_CONST;
}

static inline bool both_any_integer(Expr *left, Expr *right)
{
	return type_is_any_integer(type_flatten(left->type)) && type_is_any_integer(type_flatten(right->type));
}

void expr_copy_properties(Expr *to, Expr *from)
{
	to->failable = from->failable;
	to->pure = from->pure;
	to->constant = from->constant;
}

void expr_copy_types(Expr *to, Expr *from)
{
	to->type = from->type;
	to->original_type = from->original_type;
}

void expr_insert_addr(Expr *original)
{
	assert(original->resolve_status == RESOLVE_DONE);
	if (original->expr_kind == EXPR_UNARY && original->unary_expr.operator == UNARYOP_DEREF)
	{
		*original = *original->unary_expr.expr;
		return;
	}
	Expr *inner = expr_alloc();
	*inner = *original;
	original->expr_kind = EXPR_UNARY;
	expr_set_type(original, type_get_ptr(inner->type));
	original->unary_expr.operator = UNARYOP_ADDR;
	original->unary_expr.expr = inner;
	original->pure = false;
	original->constant = false;
	original->failable = inner->failable;
}

Expr *expr_variable(Decl *decl)
{
	Expr *expr = expr_new(EXPR_IDENTIFIER, decl->span);
	expr->identifier_expr.decl = decl;
	expr->pure = true;
	expr->constant = false;
	expr->resolve_status = RESOLVE_DONE;
	expr_set_type(expr, decl->type);
	return expr;
}

void expr_insert_deref(Expr *original)
{
	assert(original->resolve_status == RESOLVE_DONE);
	assert(original->type->canonical->type_kind == TYPE_POINTER);
	if (original->expr_kind == EXPR_UNARY && original->unary_expr.operator == UNARYOP_ADDR)
	{
		*original = *original->unary_expr.expr;
		return;
	}

	Type *pointee = original->type->type_kind == TYPE_POINTER ? original->type->pointer : original->type->canonical->pointer;
	Expr *inner = expr_alloc();
	*inner = *original;
	original->expr_kind = EXPR_UNARY;
	expr_set_type(original, pointee);
	original->unary_expr.operator = UNARYOP_DEREF;
	original->unary_expr.expr = inner;
	original->pure = false;
	original->constant = false;
	original->failable = inner->failable;
}


static void expr_unify_binary_properties(Expr *expr, Expr *left, Expr *right)
{
	expr->pure = left->pure & right->pure;
	expr->constant = left->constant & right->constant;
	expr->failable = left->failable | right->failable;
}

static void expr_unify_binary(Expr *expr, Expr *left, Expr *right)
{
	expr->type = left->type;
	expr->original_type = type_find_max_type(left->original_type->canonical, right->original_type->canonical);
	expr_unify_binary_properties(expr, left, right);
}

static inline void context_pop_returns(Context *context, Ast **restore)
{
	if (!context->returns_cache && context->returns)
	{
		context->returns_cache = context->returns;
	}
	context->returns = restore;
}
static inline Ast **context_push_returns(Context *context)
{
	Ast** old_returns = context->returns;
	if (context->returns_cache)
	{
		context->returns = context->returns_cache;
		context->returns_cache = NULL;
		vec_resize(context->returns, 0);
	}
	else
	{
		context->returns = NULL;
	}
	return old_returns;
}

int sema_check_comp_time_bool(Context *context, Expr *expr)
{
	if (!sema_analyse_expr_of_required_type(context, type_bool, expr, false)) return -1;
	if (expr->expr_kind != EXPR_CONST)
	{
		SEMA_ERROR(expr, "Compile time evaluation requires a compile time constant value.");
		return -1;
	}
	return expr->const_expr.b;
}

static bool expr_is_ltype(Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_CONST_IDENTIFIER:
			return false;
		case EXPR_CT_IDENT:
			return true;
		case EXPR_IDENTIFIER:
		{
			Decl *decl = expr->identifier_expr.decl;
			if (decl->decl_kind != DECL_VAR) return false;
			decl = decl_raw(decl);
			switch (decl->var.kind)
			{
				case VARDECL_LOCAL_CT:
				case VARDECL_LOCAL_CT_TYPE:
				case VARDECL_LOCAL:
				case VARDECL_GLOBAL:
				case VARDECL_PARAM:
				case VARDECL_PARAM_REF:
					return true;
				case VARDECL_CONST:
				default:
					return false;
			}
		}
		case EXPR_UNARY:
			return expr->unary_expr.operator == UNARYOP_DEREF;
		case EXPR_ACCESS:
			return expr_is_ltype(expr->access_expr.parent);
		case EXPR_GROUP:
			return expr_is_ltype(expr->group_expr);
		case EXPR_SUBSCRIPT:
		case EXPR_SLICE:
			return true;
		default:
			return false;
	}
}

static inline bool sema_cast_ident_rvalue(Context *context, Type *to, Expr *expr)
{
	Decl *decl = expr->identifier_expr.decl;
	decl = decl_flatten(decl);
	expr->identifier_expr.is_rvalue = true;

	switch (decl->decl_kind)
	{
		case DECL_FUNC:
			SEMA_ERROR(expr, "Expected function followed by (...) or prefixed by &.");
			return expr_poison(expr);
		case DECL_MACRO:
			SEMA_ERROR(expr, "Expected macro followed by (...) or prefixed by '&'.");
			return expr_poison(expr);
		case DECL_GENERIC:
			SEMA_ERROR(expr, "Expected generic followed by (...) or prefixed by '&'.");
			return expr_poison(expr);
		case DECL_ENUM_CONSTANT:
			expr_replace(expr, decl->enum_constant.expr);
			return true;
		case DECL_VAR:
			break;
		case DECL_DISTINCT:
		case DECL_TYPEDEF:
			UNREACHABLE
		case DECL_POISONED:
			return expr_poison(expr);
		case DECL_LABEL:
			SEMA_ERROR(expr, "Did you intend to use the label '%s' here?", decl->name);
			return expr_poison(expr);
		case DECL_INTERFACE:
			SEMA_ERROR(expr, "Expected interface followed by '.'.");
			return expr_poison(expr);
		case DECL_STRUCT:
			SEMA_ERROR(expr, "Expected struct followed by (...) or '.'.");
			return expr_poison(expr);
		case DECL_UNION:
			SEMA_ERROR(expr, "Expected union followed by (...) or '.'.");
			return expr_poison(expr);
		case DECL_ENUM:
			SEMA_ERROR(expr, "Expected enum name followed by '.' and an enum value.");
			return expr_poison(expr);
		case DECL_ERR:
			SEMA_ERROR(expr, "Did you forget a '!' after '%s'?", decl->name);
			return expr_poison(expr);
		case DECL_ARRAY_VALUE:
			UNREACHABLE
		case DECL_IMPORT:
		case DECL_CT_IF:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
		case DECL_CT_SWITCH:
		case DECL_CT_CASE:
		case DECL_ATTRIBUTE:
		case DECL_CT_ASSERT:
		case DECL_DEFINE:
			UNREACHABLE
	}
	switch (decl->var.kind)
	{
		case VARDECL_CONST:
			if (!type_is_builtin(decl->type->type_kind)) break;
			expr_replace(expr, copy_expr(decl->var.init_expr));
			return sema_analyse_expr(context, to, expr);
		case VARDECL_PARAM_EXPR:
			expr_replace(expr, copy_expr(decl->var.init_expr));
			assert(decl->var.init_expr->resolve_status == RESOLVE_DONE);
			return true;
		case VARDECL_PARAM_CT_TYPE:
			TODO
		case VARDECL_PARAM_REF:
			expr_replace(expr, copy_expr(decl->var.init_expr));
			return sema_cast_rvalue(context, to, expr);
		case VARDECL_PARAM:
		case VARDECL_GLOBAL:
		case VARDECL_LOCAL:
		case VARDECL_ALIAS:
			break;
		case VARDECL_MEMBER:
			SEMA_ERROR(expr, "Expected '%s' followed by a method call or property.", decl->name);
			return expr_poison(expr);
		case VARDECL_PARAM_CT:
			TODO
			break;
		case VARDECL_LOCAL_CT:
			TODO
			break;
		case VARDECL_LOCAL_CT_TYPE:
			TODO
			break;
	}
	return true;
}

static ExprFailableStatus expr_is_failable(Expr *expr)
{
	if (expr->expr_kind != EXPR_IDENTIFIER) return FAILABLE_NO;
	Decl *decl = expr->identifier_expr.decl;
	if (decl->decl_kind != DECL_VAR) return FAILABLE_NO;
	if (decl->var.kind == VARDECL_ALIAS && decl->var.alias->var.failable) return FAILABLE_UNWRAPPED;
	return decl->var.failable ? FAILABLE_YES : FAILABLE_NO;
}


static inline bool sema_type_error_on_binop(Expr *expr)
{
	const char *c = token_type_to_string(binaryop_to_token(expr->binary_expr.operator));
	SEMA_ERROR(expr, "%s is not defined in the expression %s %s %s.",
	               c, type_quoted_error_string(expr->binary_expr.left->type),
	               c, type_quoted_error_string(expr->binary_expr.right->type));
	return false;
}

static bool expr_cast_to_index(Expr *index)
{
	switch (index->type->canonical->type_kind)
	{
		case TYPE_IXX:
			if (!bigint_fits_in_bits(&index->const_expr.i, 64, true))
			{
				SEMA_ERROR(index, "The index is out of range, it must fit in a signed 64 bit integer.");
				return false;
			}
			return cast(index, type_iptrdiff);
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
			return cast(index, type_iptrdiff);
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
			return cast(index, type_uptrdiff);
		case TYPE_U128:
			SEMA_ERROR(index, "You need to explicitly cast this to a uint or ulong.");
			return false;
		case TYPE_I128:
			SEMA_ERROR(index, "You need to explicitly cast this to a int or long.");
			return false;
		default:
			SEMA_ERROR(index, "Cannot implicitly convert '%s' to an index.", type_to_error_string(index->type));
			return false;
	}
}

static inline bool sema_expr_analyse_ternary(Context *context, Type *to, Expr *expr)
{
	Expr *left = expr->ternary_expr.then_expr;
	Expr *cond = expr->ternary_expr.cond;
	// Normal
	if (left)
	{
		if (!sema_analyse_expr(context, type_bool, cond)) return expr_poison(expr);
		if (!cast_implicit(cond, type_bool)) return expr_poison(expr);
		if (!sema_analyse_expr(context, to, left)) return expr_poison(expr);
		expr->failable = left->failable | cond->failable;
	}
	else
	{
		// Elvis
		if (!sema_analyse_expr(context, to, cond)) return expr_poison(expr);
		expr->failable = cond->failable;
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

	expr->pure = cond->pure & left->pure & right->pure;
	expr->constant = cond->constant & left->constant & right->pure;

	expr->failable |= right->failable;
	Type *left_canonical = left->type->canonical;
	Type *right_canonical = right->type->canonical;
	if (type_is_ct(left_canonical) && type_is_ct(right_canonical))
	{
		if (to)
		{
			if (!cast_implicit(left, to) || !cast_implicit(right, to)) return false;
		}
		else
		{
			if (!cast_implicitly_to_runtime(left) || !cast_implicitly_to_runtime(right)) return false;
		}
		left_canonical = left->type->canonical;
		right_canonical = right->type->canonical;
	}
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

	expr_unify_binary(expr, left, right);

	return true;
}



static inline Decl *decl_find_enum_constant(TokenId token, Decl *decl)
{
	const char *name = TOKSTR(token);
	VECEACH(decl->enums.values, i)
	{
		Decl *enum_constant = decl->enums.values[i];
		if (enum_constant->name == name)
		{
			return enum_constant;
		}
	}
	return NULL;
}

static inline bool sema_expr_analyse_enum_constant(Expr *expr, TokenId name, Decl *decl)
{
	Decl *enum_constant = decl_find_enum_constant(name, decl);
	if (!enum_constant) return false;

	assert(enum_constant->resolve_status == RESOLVE_DONE);
	expr_set_type(expr, decl->type);
	expr->access_expr.ref = enum_constant;
	expr->expr_kind = EXPR_MEMBER_ACCESS;
	return true;
}


static inline bool find_possible_inferred_identifier(Type *to, Expr *expr)
{
	if (to->canonical->type_kind != TYPE_ENUM && to->canonical->type_kind != TYPE_ERRTYPE) return false;
	Decl *parent_decl = to->canonical->decl;
	switch (parent_decl->decl_kind)
	{
		case DECL_ENUM:
			return sema_expr_analyse_enum_constant(expr, expr->identifier_expr.identifier, parent_decl);
		case DECL_UNION:
		case DECL_STRUCT:
			return false;
		default:
			UNREACHABLE
	}

}


bool expr_is_constant_eval(Expr *expr)
{
	// TODO rethink this.
	switch (expr->expr_kind)
	{
		case EXPR_CONST:
			return true;
		case EXPR_COMPOUND_LITERAL:
			return expr_is_constant_eval(expr->expr_compound_literal.initializer);
		case EXPR_INITIALIZER_LIST:
		{
			Expr** init_exprs = expr->initializer_expr.initializer_expr;
			switch (expr->initializer_expr.init_type)
			{
				case INITIALIZER_NORMAL:
				{
					VECEACH(init_exprs, i)
					{
						if (!expr_is_constant_eval(init_exprs[i])) return false;
					}
					return true;
				}
				default:
					return false;
			}
		}
		default:
			return false;
	}
}

static inline bool sema_expr_analyse_identifier(Context *context, Type *to, Expr *expr)
{
	Decl *ambiguous_decl = NULL;
	Decl *private_symbol = NULL;
	expr->pure = true;

	DEBUG_LOG("Now resolving %s", TOKSTR(expr->identifier_expr.identifier));
	Decl *decl = sema_resolve_normal_symbol(context,
	                                        expr->identifier_expr.identifier,
	                                        expr->identifier_expr.path,
	                                        false);
	if (!decl_ok(decl)) return false;
	if (!decl && !expr->identifier_expr.path && to)
	{
		if (find_possible_inferred_identifier(to, expr)) return true;
	}
	if (!decl)
	{
		decl = sema_resolve_normal_symbol(context,
		                                  expr->identifier_expr.identifier,
		                                  expr->identifier_expr.path,
		                                  true);
		assert(!decl_ok(decl));
		return false;
	}

	// Already handled
	if (!decl_ok(decl)) return false;

	if (decl->decl_kind == DECL_FUNC && !expr->identifier_expr.path && decl->module != context->module)
	{
		SEMA_ERROR(expr, "Functions from other modules, must be prefixed with the module name");
		return false;
	}
	if (decl->resolve_status != RESOLVE_DONE)
	{
		if (!sema_analyse_decl(context, decl)) return decl_poison(decl);
	}
	if (decl->decl_kind == DECL_VAR && decl->var.failable)
	{
		expr->failable = true;
	}
	if (decl->decl_kind == DECL_VAR)
	{
		switch (decl->var.kind)
		{
			case VARDECL_CONST:
				if (!decl->type)
				{
					Expr *copy = copy_expr(decl->var.init_expr);
					if (!sema_analyse_expr(context, to, copy)) return false;
					if (!expr_is_constant_eval(copy))
					{
						SEMA_ERROR(expr, "Constant value did not evaluate to a constant.");
						return false;
					}
					expr_replace(expr, copy);
					return true;
				}
				if (decl->var.failable)
				{
					SEMA_ERROR(expr, "Constants may never be 'failable', please remove the '!'.");
					return false;
				}
				break;
			default:
				break;
		}
	}
	if (!decl->type) decl->type = type_void;
	expr->identifier_expr.decl = decl;
	expr_set_type(expr, decl->type);
	expr->pure = true;
	expr->constant = false;
	DEBUG_LOG("Resolution successful of %s.", decl->name);
	return true;
}

static inline bool sema_expr_analyse_macro_expansion(Context *context, Expr *expr)
{
	Expr *inner = expr->macro_expansion_expr.inner;
	if (inner->expr_kind == EXPR_IDENTIFIER)
	{
		if (!inner->identifier_expr.path && TOKSTR(inner->identifier_expr.identifier) == context->macro_scope.body_param)
		{
			expr->expr_kind = EXPR_MACRO_BODY_EXPANSION;
			expr->body_expansion_expr.ast = NULL;
			expr->body_expansion_expr.declarations = NULL;
			expr->resolve_status = RESOLVE_NOT_DONE;
			expr->type = expr->original_type = type_void;
			return true;
		}
	}
	if (!sema_analyse_expr_value(context, NULL, inner)) return false;
	Decl *decl;
	switch (inner->expr_kind)
	{
		case EXPR_IDENTIFIER:
			decl = inner->identifier_expr.decl;
			break;
		case EXPR_ACCESS:
			decl = inner->access_expr.ref;
			break;
		default:
			SEMA_ERROR(expr, "Expected a macro identifier here.");
			return false;
	}
	if (decl->decl_kind != DECL_MACRO)
	{
		SEMA_ERROR(inner, "Expected a macro identifier here.");
		return false;
	}
	expr->macro_expansion_expr.decl = decl;
	expr_copy_properties(expr, inner);
	return true;
}

static inline bool sema_expr_analyse_ct_identifier(Context *context, Expr *expr)
{
	expr->pure = true;

	DEBUG_LOG("Now resolving %s", TOKSTR(expr->ct_ident_expr.identifier));
	Decl *decl = sema_resolve_normal_symbol(context,
	                                        expr->ct_ident_expr.identifier,
	                                        NULL, true);

	// Already handled
	if (!decl_ok(decl))
	{
		return expr_poison(expr);
	}

	DEBUG_LOG("Resolution successful of %s.", decl->name);
	assert(decl->decl_kind == DECL_VAR);
	assert(decl->resolve_status == RESOLVE_DONE);

	expr->ct_ident_expr.decl = decl;
	expr_set_type(expr, decl->type);
	expr->pure = true;
	expr->constant = true;
	return true;
}

static inline bool sema_expr_analyse_hash_identifier(Context *context, Type *to, Expr *expr)
{
	expr->pure = true;

	DEBUG_LOG("Now resolving %s", TOKSTR(expr->hash_ident_expr.identifier));
	Decl *decl = sema_resolve_normal_symbol(context,
	                                        expr->hash_ident_expr.identifier,
	                                        NULL, true);

	// Already handled
	if (!decl_ok(decl))
	{
		return expr_poison(expr);
	}

	DEBUG_LOG("Resolution successful of %s.", decl->name);
	assert(decl->decl_kind == DECL_VAR);
	assert(decl->resolve_status == RESOLVE_DONE);

	assert(decl->var.init_expr->resolve_status == RESOLVE_DONE);
	expr_replace(expr, copy_expr(decl->var.init_expr));
	return sema_analyse_expr(context, to, expr);
}

static inline bool sema_expr_analyse_binary_sub_expr(Context *context, Type *to, Expr *left, Expr *right)
{
	return sema_analyse_expr(context, to, left) & sema_analyse_expr(context, to, right);
}

static inline int find_index_of_named_parameter(Decl **func_params, Expr *expr)
{
	if (vec_size(expr->designator_expr.path) != 1)
	{
		SEMA_ERROR(expr, "Expected the name of a function parameter here, this looks like a member path.");
		return -1;
	}
	DesignatorElement *element = expr->designator_expr.path[0];
	if (element->kind != DESIGNATOR_FIELD)
	{
		SEMA_ERROR(expr, "Expected the name of a function parameter here, this looks like an array path field.");
		return -1;
	}
	const char *name = element->field;
	VECEACH(func_params, i)
	{
		if (func_params[i]->name == name) return (int)i;
	}
	SEMA_ERROR(expr, "There's no parameter with the name '%s'.", name);
	return -1;
}

static inline bool sema_expr_analyse_intrinsic_fp_invocation(Context *context, Expr *expr, Decl *decl, Type *to)
{
	unsigned arguments = vec_size(expr->call_expr.arguments);
	if (arguments != 1)
	{
		SEMA_ERROR(expr, "Expected 1 argument to intrinsic %s.", decl->name);
		return false;
	}
	Expr *arg = expr->call_expr.arguments[0];
	if (!sema_analyse_expr(context, to, arg)) return false;
	// Convert ints to float comptime float.
	if (type_is_any_integer(arg->type->canonical))
	{
		if (!cast_implicit(arg, type_compfloat)) return false;
	}
	// If this is not a float argument => error.
	if (!type_is_float(arg->type->canonical))
	{
		SEMA_ERROR(arg, "Expected a floating point argument.", decl->name);
		return false;
	}
	// We lower to a real float in case we got a compfloat.
	if (!cast_implicitly_to_runtime(arg)) return false;

	// The expression type is the argument type.
	expr_set_type(expr, arg->type);
	return true;

}

static inline bool sema_expr_analyse_intrinsic_invocation(Context *context, Expr *expr, Decl *decl, Type *to)
{
	if (decl->name == kw___ceil || decl->name == kw___trunc || decl->name == kw___round || decl->name == kw___sqrt)
	{
		return sema_expr_analyse_intrinsic_fp_invocation(context, expr, decl, to);
	}
	UNREACHABLE
}

static inline bool expr_may_unpack_as_vararg(Expr *expr, Type *variadic_base_type)
{
	Type *base_type = variadic_base_type->canonical;
	Type *canonical = expr->type->canonical;
	switch (canonical->type_kind)
	{
		case TYPE_ARRAY:
		case TYPE_SUBARRAY:
			return canonical->array.base == base_type;
		case TYPE_POINTER:
			if (canonical->pointer->type_kind == TYPE_ARRAY) return canonical->pointer->array.base == base_type;
			return false;
		default:
			return false;
	}
}

typedef struct
{
	bool macro;
	bool failable;
	TokenId block_parameter;
	Decl **params;
	Expr *struct_var;
	Variadic variadic;
} CalledDecl;

static inline bool expr_promote_vararg(Context *context, Expr *arg)
{
	Type *arg_type = arg->type->canonical;
	// 1. Is it compile time?
	if (type_is_ct(arg_type))
	{
		// TODO Fix the int conversion.
		// 1. Pick double / CInt
		Type *target_type = type_is_any_integer(arg_type) ? type_cint() : type_double;
		return cast_implicit(arg, target_type);
	}

	// 2. Promote any integer or bool to at least CInt
	if (type_is_promotable_integer(arg_type) || arg_type == type_bool)
	{
		return cast(arg, type_cint());
	}
	// 3. Promote any float to at least double
	if (type_is_promotable_float(arg->type))
	{
		return cast(arg, type_double);
	}
	return true;
}

static inline bool sema_check_invalid_body_arguments(Context *context, Expr *call, CalledDecl *callee)
{
	Decl **body_arguments = call->call_expr.body_arguments;
	bool has_body_arguments = vec_size(body_arguments) > 0;

	// 1. Check if there are body arguments but no actual block.
	if (has_body_arguments && !callee->block_parameter.index)
	{
		if (callee->macro)
		{
			SEMA_ERROR(body_arguments[0], "This macro does not support arguments.");
		}
		else
		{
			SEMA_ERROR(body_arguments[0], "Only macro calls may have body arguments for a trailing block.");
		}
		return false;
	}

	// 2. If there is a body then...
	if (call->call_expr.body)
	{
		// 2a. If not a macro then this is an error.
		if (!callee->macro)
		{
			SEMA_ERROR(call, "Only macro calls may take a trailing block.");
			return false;
		}
		// 2b. If we don't have a block parameter, then this is an error as well
		if (!callee->block_parameter.index)
		{
			SEMA_ERROR(call, "This macro does not support trailing statements, please remove it.");
			return false;
		}

		// 2c. This is a macro and it has a block parameter. Everything is fine!
		return true;
	}

	// 3. If we don't have a body, then if it has a block parameter this is an error.
	if (callee->block_parameter.index)
	{
		assert(callee->macro);
		SEMA_ERROR(call, "Expected call to have a trailing statement, did you forget to add it?");
		return false;
	}

	// 4. No body and no block parameter, this is fine.
	return true;
}

static inline bool sema_expand_call_arguments(Context *context, Expr *call, Decl **params, Expr **args, unsigned func_param_count, bool variadic)
{
	unsigned num_args = vec_size(args);

	// 1. We need at least as many function locations as we have parameters.
	unsigned entries_needed = func_param_count > num_args ? func_param_count : num_args;
	Expr **actual_args = VECNEW(Expr*, entries_needed);
	for (unsigned i = 0; i < entries_needed; i++) vec_add(actual_args, NULL);
	// TODO this should not be needed:
	memset(actual_args, 0, entries_needed * sizeof(Expr*));

	// 2. Loop through the parameters.
	bool uses_named_parameters = false;
	for (unsigned i = 0; i < num_args; i++)
	{
		Expr *arg = args[i];

		// 3. Handle named parameters
		if (arg->expr_kind == EXPR_DESIGNATOR)
		{
			// 8a. We have named parameters, that will add some restrictions.
			uses_named_parameters = true;

			// 8b. Find the location of the parameter.
			int index = find_index_of_named_parameter(params, arg);

			// 8c. If it's not found then this is an error.
			if (index < 0) return false;

			// 8d. We might actually be finding the typed vararg at the end,
			//     this is an error.
			if (params[index]->var.vararg)
			{
				SEMA_ERROR(arg, "Vararg parameters may not be named parameters, use normal parameters instead.", params[index]->name);
				return false;
			}

			// 8e. We might have already set this parameter, that is not allowed.
			if (actual_args[index])
			{
				SEMA_ERROR(arg, "The parameter '%s' was already set.", params[index]->name);
				return false;
			}

			// 8g. Set the parameter and update failability.
			actual_args[index] = arg->designator_expr.value;
			call->failable |= arg->designator_expr.value->failable;
			continue;
		}

		// 9. Check for previous use of named parameters, this is not allowed
		//     like foo(.a = 1, 3) => error.
		if (uses_named_parameters)
		{
			SEMA_ERROR(call, "A regular parameter cannot follow a named parameter.");
			return false;
		}

		// 10. If we exceed the function parameter count (remember we reduced this by one
		//     in the case of typed vararg) we're now in a variadic list.
		if (i >= func_param_count)
		{
			// 11. We might have a typed variadic argument.
			if (!variadic)
			{
				// 15. We have too many parameters...
				SEMA_ERROR(arg, "This argument would would exceed the number of parameters, did you add to many arguments?");
				return false;
			}

			// 11a. Look if we did an unsplat
			if (call->call_expr.unsplat_last)
			{
				// 11b. Is this the last argument, or did we get some before the unpack?
				if (i < num_args - 1)
				{
					SEMA_ERROR(arg,
					           "This looks like a variable argument before an unpacked variable which isn't allowed. Did you add too many arguments?");
					return false;
				}
			}
		}
		actual_args[i] = arg;
	}

	// 17. Set default values.
	for (unsigned i = 0; i < entries_needed; i++)
	{
		// 17a. Assigned a value - skip
		if (actual_args[i]) continue;

		// 17b. Set the init expression.
		if (params[i]->var.init_expr)
		{
			assert(params[i]->var.init_expr->resolve_status == RESOLVE_DONE);
			actual_args[i] = params[i]->var.init_expr;
			continue;
		}

		// 17c. Vararg not set? That's fine.
		if (params[i]->var.vararg) continue;

		// 17d. Argument missing, that's bad.
		SEMA_ERROR(call, "The mandatory parameter '%s' was not set, please add it.", params[i]->name);
		return false;
	}
	call->call_expr.arguments = actual_args;
	return true;
}
static inline bool sema_expr_analyse_call_invocation(Context *context, Expr *call, CalledDecl callee)
{
	// 1. Check body arguments.
	if (!sema_check_invalid_body_arguments(context, call, &callee)) return false;

	call->failable = callee.failable;

	// 2. Pick out all the arguments and parameters.
	Expr **args = call->call_expr.arguments;
	Decl **params = callee.params;
	unsigned num_args = vec_size(args);

	// 3. If this is a type call, then we have an implicit first argument.
	if (callee.struct_var)
	{
		// 3a. Insert an argument first, by adding null to the end and then moving all arguments
		//     by one step.
		vec_add(args, NULL);
		for (unsigned i = num_args; i > 0; i--)
		{
			args[i] = args[i - 1];
		}
		// 3b. Then insert the argument.
		args[0] = callee.struct_var;
		num_args++;
		call->call_expr.arguments = args;
		call->call_expr.is_type_method = true;
		assert(!call->call_expr.is_pointer_call);
	}

	// 4. Check for unsplat of the last argument.
	bool unsplat = call->call_expr.unsplat_last;
	if (unsplat)
	{
		// 4a. Is this *not* a variadic function/macro? - Then that's an error.
		if (callee.variadic == VARIADIC_NONE)
		{
			SEMA_ERROR(call->call_expr.arguments[num_args - 1],
					   "Unpacking is only allowed for %s with variable parameters.",
					   callee.macro ? "macros" : "functions");
			return false;
		}
	}

	// 5. Zero out all argument slots.
	unsigned func_param_count = vec_size(params);

	// 6. We might have a typed variadic call e.g. foo(int, double...)
	//    get that type.
	Type *variadic_type = NULL;
	if (callee.variadic == VARIADIC_TYPED)
	{
		// 7a. The parameter type is <type>[], so we get the <type>
		assert(params[func_param_count - 1]->type->type_kind == TYPE_SUBARRAY);
		variadic_type = params[func_param_count - 1]->type->array.base;
		// 7b. The last function parameter is implicit so we will pretend it's not there.
		func_param_count--;
	}

	if (!sema_expand_call_arguments(context, call, params, args, func_param_count, callee.variadic != VARIADIC_NONE)) return false;

	args = call->call_expr.arguments;
	num_args = vec_size(args);

	// 7. Loop through the parameters.
	for (unsigned i = 0; i < num_args; i++)
	{
		Expr *arg = args[i];

		// 10. If we exceed the function parameter count (remember we reduced this by one
		//     in the case of typed vararg) we're now in a variadic list.
		if (i >= func_param_count)
		{
			// 11. We might have a typed variadic argument.
			if (variadic_type)
			{
				// 11a. Look if we did an unsplat
				if (call->call_expr.unsplat_last)
				{
					// 11c. Analyse the expression. We don't use any type inference here since
					//      foo(...{ 1, 2, 3 }) is a fairly worthless thing.
					if (!sema_analyse_expr(context, NULL, arg)) return false;

					// 11d. If it is allowed.
					if (!expr_may_unpack_as_vararg(arg, variadic_type))
					{
						SEMA_ERROR(arg, "It's not possible to unpack %s as vararg of type %s",
						           type_quoted_error_string(arg->type),
						           type_quoted_error_string(variadic_type));
						return false;
					}
				}
				else
				{
					// 11e. A simple variadic value:
					if (!sema_analyse_expr_of_required_type(context, variadic_type, arg, true)) return false;
				}
				// Set the argument at the location.
				call->failable |= arg->failable;
				continue;
			}
			// 12. We might have a naked variadic argument
			if (callee.variadic == VARIADIC_RAW)
			{
				// 12a. Analyse the expression.
				if (!sema_analyse_expr(context, NULL, arg)) return false;

				// 12b. In the case of a compile time variable non macros we cast to c_int / double.
				if (!callee.macro)
				{
					if (!expr_promote_vararg(context, arg)) return false;
				}
				// Set the argument at the location.
				call->failable |= arg->failable;
				continue;
			}
			UNREACHABLE
		}

		Decl *param = params[i];

		// 16. Analyse a regular argument.
		switch (param->var.kind)
		{
			case VARDECL_PARAM_REF:
				// &foo
				if (!sema_analyse_expr_value(context, param->type, arg)) return false;
				if (param->type && param->type->canonical != arg->type->canonical)
				{
					SEMA_ERROR(arg, "'%s' cannot be implicitly cast to '%s'.", type_to_error_string(arg->type), type_to_error_string(param->type));
					return false;
				}
				break;
			case VARDECL_PARAM:
				// foo
				if (!sema_analyse_expr_of_required_type(context, param->type, arg, true)) return false;
				if (!param->type && !cast_implicitly_to_runtime(arg))
				{
					SEMA_ERROR(arg, "Constant cannot implicitly be cast to a real type.");
					return false;
				}
				if (callee.macro)
				{
					param->alignment = type_abi_alignment(param->type ? param->type : arg->type);
				}
				break;
			case VARDECL_PARAM_EXPR:
				// #foo
				// We push a scope here as this will prevent the expression from modifying
				// compile time variables during evaluation:
				assert(callee.macro);
				SCOPE_START
					if (!sema_analyse_expr_of_required_type(context, param->type, arg, true)) return SCOPE_POP_ERROR();
				SCOPE_END;
				break;
			case VARDECL_PARAM_CT:
				// $foo
				assert(callee.macro);
				if (!sema_analyse_expr_of_required_type(context, param->type, arg, false)) return false;
				if (!expr_is_constant_eval(arg))
				{
					SEMA_ERROR(arg, "A compile time parameter must always be a constant, did you mistake it for a normal paramter?");
					return false;
				}
				break;
			case VARDECL_PARAM_CT_TYPE:
				// $Foo
				if (!sema_analyse_expr_value(context, NULL, arg)) return false;
				// TODO check typeof
				if (arg->expr_kind != EXPR_TYPEINFO)
				{
					SEMA_ERROR(arg, "A type, like 'int' or 'double' was expected for the parameter '%s'.", param->name);
					return false;
				}
				break;
			case VARDECL_CONST:
			case VARDECL_GLOBAL:
			case VARDECL_LOCAL:
			case VARDECL_MEMBER:
			case VARDECL_LOCAL_CT:
			case VARDECL_LOCAL_CT_TYPE:
			case VARDECL_ALIAS:
				UNREACHABLE
		}
		if (param->type)
		{
			if (!cast_implicit(arg, param->type)) return false;
		}
		else
		{
			param->type = arg->type;
		}
		call->failable |= arg->failable;
	}


	return true;
}
static inline bool sema_expr_analyse_func_invocation(Context *context, FunctionSignature *signature, Expr *expr, Decl *decl, Type *to, Expr *struct_var)
{
	CalledDecl callee = {
			.macro = false,
			.block_parameter = NO_TOKEN_ID,
			.struct_var = struct_var,
			.params = signature->params,
			.variadic = signature->variadic,
			.failable = signature->failable,
	};
	if (!sema_expr_analyse_call_invocation(context, expr, callee)) return false;

	// 2. Builtin? We handle that elsewhere.
	if (decl->func_decl.is_builtin)
	{
		assert(!struct_var);
		return sema_expr_analyse_intrinsic_invocation(context, expr, decl, to);
	}

	expr_set_type(expr, signature->rtype->type);

	return true;
}

static inline bool sema_expr_analyse_var_call(Context *context, Type *to, Expr *expr, Decl *var_decl)
{
	Type *func_ptr_type = var_decl->type->canonical;
	expr->failable |= var_decl->var.failable;
	if (func_ptr_type->type_kind != TYPE_POINTER || func_ptr_type->pointer->type_kind != TYPE_FUNC)
	{
		SEMA_ERROR(expr, "Only macros, functions and function pointers maybe invoked, this is of type '%s'.", type_to_error_string(var_decl->type));
		return false;
	}
	expr->call_expr.is_pointer_call = true;
	return sema_expr_analyse_func_invocation(context,
	                                         func_ptr_type->pointer->func.signature,
	                                         expr,
	                                         func_ptr_type->decl,
	                                         to, NULL);

}





static inline Type *unify_returns(Context *context, Type *to)
{
	bool all_returns_need_casts = false;
	// Let's unify the return statements.
	VECEACH(context->returns, i)
	{
		Ast *return_stmt = context->returns[i];
		Expr *ret_expr = return_stmt->return_stmt.expr;
		bool last_expr_was_void = to == type_void;
		Type *right_canonical = ret_expr ? ret_expr->type->canonical : type_void;
		bool current_expr_was_void = right_canonical == type_void;
		if (i > 0 && last_expr_was_void != current_expr_was_void)
		{
			SEMA_ERROR(return_stmt, "You can't combine empty returns with value returns.");
			SEMA_PREV(context->returns[i - 1], "Previous return was here.");
			return NULL;
		}
		if (to != type_void)
		{
			if (current_expr_was_void)
			{
				SEMA_ERROR(return_stmt, "The return must be a value of type '%s'.", type_to_error_string(to));
				return NULL;
			}
			if (!cast_implicit(ret_expr, to))
			{
				return NULL;
			}
			continue;
		}

		// The simple case.
		if (to == right_canonical) continue;

		// Try to find a common type:
		Type *max = type_find_max_type(to, right_canonical);
		if (!max)
		{
			SEMA_ERROR(return_stmt, "Cannot find a common parent type of '%s' and '%s'",
			               type_to_error_string(to), type_to_error_string(right_canonical));
			SEMA_PREV(context->returns[i - 1], "The previous return was here.");
			return false;
		}
		to = max;
		all_returns_need_casts = true;
	}
	if (all_returns_need_casts)
	{
		VECEACH(context->returns, i)
		{
			Ast *return_stmt = context->returns[i];
			Expr *ret_expr = return_stmt->return_stmt.expr;
			if (!cast_implicit(ret_expr, to))
			{
				return NULL;
			}
		}
	}
	return to;
}

static inline bool sema_expr_analyse_func_call(Context *context, Type *to, Expr *expr, Decl *decl, Expr *struct_var)
{
	expr->call_expr.is_pointer_call = false;
	return sema_expr_analyse_func_invocation(context, &decl->func_decl.function_signature, expr, decl, to, struct_var);
}

static bool sema_check_stmt_compile_time(Context *context, Ast *ast);

static bool sema_check_expr_compile_time(Context *context, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_CONST:
			return true;
		case EXPR_MACRO_BLOCK:
			VECEACH(expr->macro_block.stmts, i)
			{
				if (!sema_check_stmt_compile_time(context, expr->macro_block.stmts[i])) return false;
			}
			return true;
		default:
			return false;
	}
	UNREACHABLE
}

static bool sema_check_stmt_compile_time(Context *context, Ast *ast)
{
	switch (ast->ast_kind)
	{
		case AST_NOP_STMT:
			return true;
		case AST_RETURN_STMT:
			if (!ast->return_stmt.expr) return true;
			return expr_is_constant_eval(ast->return_stmt.expr);
		case AST_EXPR_STMT:
			return sema_check_expr_compile_time(context, ast->expr_stmt);
		case AST_CT_COMPOUND_STMT:
		case AST_COMPOUND_STMT:
			VECEACH(ast->compound_stmt.stmts, i)
			{
				if (!sema_check_stmt_compile_time(context, ast->ct_compound_stmt[i])) return false;
			}
			return true;
		default:
			return false;
	}
}

static bool sema_expr_analyse_macro_call(Context *context, Type *to, Expr *call_expr, Expr *struct_var, Decl *decl)
{
	assert(decl->decl_kind == DECL_MACRO);

	// TODO failable
	if (context->macro_scope.depth >= MAX_MACRO_NESTING)
	{
		SEMA_ERROR(call_expr, "Too deep nesting (more than %d levels) when evaluating this macro.", MAX_MACRO_NESTING);
		return false;
	}

	Decl **params = copy_decl_list(decl->macro_decl.parameters);
	CalledDecl callee = {
			.macro = true,
			.block_parameter = decl->macro_decl.block_parameter,
			.params = params,
			.struct_var = struct_var
	};

	if (!sema_expr_analyse_call_invocation(context, call_expr, callee)) return false;
	Decl **func_params = decl->macro_decl.parameters;
	Expr **args = call_expr->call_expr.arguments;
	VECEACH(params, i)
	{
		Decl *param = params[i];
		param->var.init_expr = args[i];
	}

	unsigned body_params = vec_size(call_expr->call_expr.body_arguments);
	unsigned expected_body_params = vec_size(decl->macro_decl.body_parameters);
	if (expected_body_params > body_params)
	{
		SEMA_ERROR(call_expr, "Not enough parameters for the macro body, expected %d.", expected_body_params);
		return false;
	}
	if (expected_body_params < body_params)
	{
		SEMA_ERROR(call_expr, "Too many parameters for the macro body, expected %d.", expected_body_params);
		return false;
	}
	for (unsigned i = 0; i < expected_body_params; i++)
	{
		Decl *body_param = decl->macro_decl.body_parameters[i];
		assert(body_param->resolve_status == RESOLVE_DONE);
		Decl *body_arg = call_expr->call_expr.body_arguments[i];
		if (!body_arg->var.type_info)
		{
			SEMA_ERROR(body_arg, "Expected a type parameter before this variable name.");
			return false;
		}
		if (!sema_resolve_type_info(context, body_arg->var.type_info)) return false;
		body_arg->type = body_arg->var.type_info->type;
		if (body_param->var.type_info)
		{
			Type *declare_type = body_param->var.type_info->type->canonical;
			if (declare_type != body_arg->type)
			{
				SEMA_ERROR(body_arg->var.type_info, "This parameter should be '%s' but was '%s'",
				           type_to_error_string(declare_type),
				           type_quoted_error_string(body_arg->type));
				return false;
			}
		}
		if (!body_arg->alignment) body_arg->alignment = type_alloca_alignment(body_arg->type);
	}
	Decl **first_local = context->macro_scope.macro ? context->macro_scope.locals_start : context->locals;

	MacroScope old_macro_scope = context->macro_scope;

	bool ok = true;

	SCOPE_OUTER_START

		for (unsigned i = 0; i < expected_body_params; i++)
		{
			Decl *body_arg = call_expr->call_expr.body_arguments[i];
			sema_add_local(context, body_arg);
		}

		context->macro_scope = (MacroScope){
				.body_param = decl->macro_decl.block_parameter.index ? TOKSTR(decl->macro_decl.block_parameter) : NULL,
				.macro = decl,
				.inline_line = TOKLOC(call_expr->span.loc)->line,
				.original_inline_line = old_macro_scope.depth ? old_macro_scope.original_inline_line : TOKLOC(call_expr->span.loc)->line,
				.locals_start = context->active_scope.current_local,
				.depth = old_macro_scope.depth + 1,
				.yield_symbol_start = first_local,
				.yield_body = call_expr->call_expr.body,
				.yield_symbol_end = context->active_scope.current_local,
				.yield_args = call_expr->call_expr.body_arguments,
		};


		Ast *body = copy_ast(decl->macro_decl.body);
		TypeInfo *foo = decl->macro_decl.rtype;

		Ast **saved_returns = context_push_returns(context);
		context->expected_block_type = foo ? foo->type : to;
		SCOPE_START_WITH_FLAGS(SCOPE_MACRO);


			VECEACH(params, i)
			{
				Decl *param = params[i];
				sema_add_local(context, param);
			}

			VECEACH(body->compound_stmt.stmts, i)
			{
				if (!sema_analyse_statement(context, body->compound_stmt.stmts[i]))
				{
					ok = false;
					goto EXIT;
				}
			}

			if (!vec_size(context->returns))
			{
				if (to)
				{
					SEMA_ERROR(decl, "Missing return in macro that evaluates to %s.", type_to_error_string(to));
					ok = false;
					goto EXIT;
				}
			}

			Expr *first_return_expr = vec_size(context->returns) ? context->returns[0]->return_stmt.expr : NULL;
			Type *left_canonical = first_return_expr ? first_return_expr->type->canonical : type_void;
			// Let's unify the return statements.
			left_canonical = unify_returns(context, left_canonical);
			if (!left_canonical)
			{
				ok = false;
				goto EXIT;
			}
			expr_set_type(call_expr, left_canonical);
			if (vec_size(context->returns) == 1)
			{
				Expr *result = context->returns[0]->return_stmt.expr;
				if (result && expr_is_constant_eval(result))
				{
					if (sema_check_stmt_compile_time(context, body))
					{
						expr_replace(call_expr, result);
						goto EXIT;
					}
				}
			}

			call_expr->expr_kind = EXPR_MACRO_BLOCK;
			call_expr->macro_block.stmts = body->compound_stmt.stmts;
			call_expr->macro_block.params = params;
			call_expr->macro_block.args = args;

			EXIT:
		SCOPE_END;
		context_pop_returns(context, saved_returns);

	SCOPE_OUTER_END;
	context->macro_scope = old_macro_scope;
	return ok;
}
static inline Decl *sema_generate_generic_function(Context *context, Expr *call_expr, Expr *struct_var, Decl *decl, const char *mangled_name)
{
	TODO
	return NULL;
}

static inline bool sema_expr_analyse_generic_call(Context *context, Type *to, Expr *call_expr, Expr *struct_var, Decl *decl)
{
	assert(decl->decl_kind == DECL_GENERIC);

	Expr **args = call_expr->call_expr.arguments;
	Decl **func_params = decl->macro_decl.parameters;

	unsigned explicit_args = vec_size(args);
	unsigned total_args = explicit_args;

	scratch_buffer_clear();
	scratch_buffer_append(decl->external_name);

	// TODO revisit name mangling
	if (struct_var)
	{
		total_args++;
		scratch_buffer_append_char('@');
		scratch_buffer_append(struct_var->type->canonical->name);
	}

	if (total_args != vec_size(func_params))
	{
		// TODO HANDLE VARARGS
		SEMA_ERROR(call_expr, "Mismatch on number of arguments.");
		return false;
	}

	int offset = total_args - explicit_args;
	for (unsigned i = 0; i < explicit_args; i++)
	{
		Expr *arg = args[i];
		Decl *param = func_params[i + offset];
		if (param->var.kind == VARDECL_PARAM_CT_TYPE)
		{
			if (!sema_analyse_expr_value(context, NULL, arg)) return false;
			if (arg->expr_kind != EXPR_TYPEINFO)
			{
				SEMA_ERROR(arg, "A type, like 'int' or 'double' was expected for the parameter '%s'.", param->name);
				return false;
			}
			scratch_buffer_append_char(',');
			scratch_buffer_append(arg->type_expr->type->canonical->name);
			continue;
		}
		if (param->var.type_info)
		{
			if (!sema_analyse_expr_of_required_type(context, param->var.type_info->type, arg, true)) return false;
		}
		else
		{
			if (!sema_analyse_expr(context, NULL, arg)) return false;
			if (!cast_implicitly_to_runtime(arg)) return false;
		}
		scratch_buffer_append_char(',');
		scratch_buffer_append(arg->type->canonical->name);
	}
	const char *mangled_name = scratch_buffer_interned();
	Decl **generic_cache = decl->module->generic_cache;
	Decl *found = NULL;
	VECEACH(generic_cache, i)
	{
		if (generic_cache[i]->external_name == mangled_name)
		{
			found = generic_cache[i];
			break;
		}
	}
	if (!found)
	{
		found = sema_generate_generic_function(context, call_expr, struct_var, decl, mangled_name);
	}
	// Remove type parameters from call.
	for (unsigned i = 0; i < explicit_args; i++)
	{
		Decl *param = func_params[i + offset];
		if (param->var.kind == VARDECL_PARAM_CT_TYPE)
		{
			for (unsigned j = i + 1; j < explicit_args; j++)
			{
				args[j - 1] = args[j];
			}
			explicit_args--;
			i--;
		}
	}
	vec_resize(args, explicit_args);
	// Perform the normal func call on the found declaration.
	return sema_expr_analyse_func_call(context, to, call_expr, found, struct_var);
}

static bool sema_analyse_body_expansion(Context *context, Expr *call)
{
	Decl *macro = context->macro_scope.macro;
	assert(macro);
	assert(macro->macro_decl.block_parameter.index);

	ExprCall *call_expr = &call->call_expr;
	if (vec_size(call_expr->body_arguments))
	{
		SEMA_ERROR(call, "Nested expansion is not possible.");
		return false;
	}
	if (call_expr->unsplat_last)
	{
		SEMA_ERROR(call, "Expanding parameters is not allowed for macro invocations.");
		return false;
	}
	unsigned expressions = vec_size(call_expr->arguments);
	if (expressions != vec_size(macro->macro_decl.body_parameters))
	{
		SEMA_ERROR(call, "Expected %d parameter(s).", vec_size(macro->macro_decl.body_parameters));
		return false;
	}
	Expr **args = call_expr->arguments;
	for (unsigned i = 0; i < expressions; i++)
	{
		Expr *expr = args[i];
		Decl *param = context->macro_scope.yield_args[i];
		if (!sema_analyse_expr(context, param->type, expr)) return false;
	}
	assert(call_expr->function->expr_kind == EXPR_MACRO_BODY_EXPANSION);
	expr_replace(call, call_expr->function);
	call->body_expansion_expr.values = args;
	call->body_expansion_expr.declarations = context->macro_scope.yield_args;
	bool in_yield = context->macro_scope.in_yield;
	context->macro_scope.in_yield = true;
	call->body_expansion_expr.ast = copy_ast(context->macro_scope.yield_body);
	bool success = sema_analyse_statement(context, call->body_expansion_expr.ast);
	context->macro_scope.in_yield = in_yield;
	return success;
}

bool sema_expr_analyse_general_call(Context *context, Type *to, Expr *expr, Decl *decl, Expr *struct_var, bool is_macro)
{
	int force_inline = -1;
	VECEACH(expr->call_expr.attributes, i)
	{
		Attr *attr = expr->call_expr.attributes[i];
		AttributeType attribute = sema_analyse_attribute(context, attr, ATTR_CALL);
		if (attribute == ATTRIBUTE_NONE) return expr_poison(expr);

		bool had = false;
		switch (attribute)
		{
			case ATTRIBUTE_INLINE:
			case ATTRIBUTE_NOINLINE:
				if (decl->decl_kind != DECL_FUNC)
				{
					SEMA_TOKID_ERROR(attr->name, "Inline / noinline attribute is only allowed for direct function/method calls");
					return expr_poison(expr);
				}
				force_inline = attribute == ATTRIBUTE_INLINE ? 1 : 0;
				break;
			default:
				UNREACHABLE;
		}
	}
	expr->call_expr.is_type_method = struct_var != NULL;
	switch (decl->decl_kind)
	{
		case DECL_MACRO:
			if (!is_macro)
			{
				SEMA_ERROR(expr, "A macro neeeds to be called with a '@' prefix, please add it.");
				return false;
			}
			expr->call_expr.func_ref = decl;
			return sema_expr_analyse_macro_call(context, to, expr, struct_var, decl);
		case DECL_VAR:
			if (is_macro)
			{
				SEMA_ERROR(expr, "A function cannot be called with a '@' prefix, please remove it.");
				return false;
			}
			assert(struct_var == NULL);
			return sema_expr_analyse_var_call(context, to, expr, decl);
		case DECL_FUNC:
			if (is_macro)
			{
				SEMA_ERROR(expr, "A function cannot be called with a '@' prefix, please remove it.");
				return false;
			}
			expr->call_expr.func_ref = decl;
			expr->call_expr.force_inline = force_inline == 1;
			expr->call_expr.force_noinline = force_inline == 0;
			return sema_expr_analyse_func_call(context, to, expr, decl, struct_var);
		case DECL_GENERIC:
			if (is_macro)
			{
				SEMA_ERROR(expr, "A generic function cannot be called with a '@' prefix, please remove it.");
				return false;
			}
			expr->call_expr.func_ref = decl;
			return sema_expr_analyse_generic_call(context, to, expr, struct_var, decl);
		case DECL_POISONED:
			return false;
		default:
			SEMA_ERROR(expr, "This expression cannot be called.");
			return false;
	}
}


static inline bool sema_expr_analyse_call(Context *context, Type *to, Expr *expr)
{
	expr->constant = false;
	expr->pure = false;
	Expr *func_expr = expr->call_expr.function;

	if (!sema_analyse_expr_value(context, NULL, func_expr)) return false;
	if (func_expr->expr_kind == EXPR_MACRO_BODY_EXPANSION)
	{
		return sema_analyse_body_expansion(context, expr);
	}
	expr->failable = func_expr->failable;
	Decl *decl;
	Expr *struct_var = NULL;
	bool macro = false;
	switch (func_expr->expr_kind)
	{
		case EXPR_IDENTIFIER:
			decl = func_expr->identifier_expr.decl;
			break;
		case EXPR_ACCESS:
			decl = func_expr->access_expr.ref;
			if (decl->decl_kind == DECL_FUNC || decl->decl_kind == DECL_MACRO)
			{
				expr_insert_addr(func_expr->access_expr.parent);
				struct_var = func_expr->access_expr.parent;
				macro = decl->decl_kind == DECL_MACRO;
			}
			break;
		case EXPR_MACRO_EXPANSION:
			decl = func_expr->macro_expansion_expr.decl;
			macro = true;
			break;
		case EXPR_LEN:
			if (func_expr->type == type_void)
			{
				expr_replace(expr, func_expr);
				expr_set_type(expr, type_usize);
				return true;
			}
			FALLTHROUGH;
		case EXPR_TYPEINFO:
			if (func_expr->type_expr->resolve_status == RESOLVE_DONE)
			{
				SEMA_ERROR(expr, "A type cannot be followed by (), if you intended a cast, use (type)(expression).");
			}
			else
			{
				SEMA_ERROR(expr, "A type cannot be followed by (), did you mean to use ({})?");
			}
			return false;
		default:
			SEMA_ERROR(expr, "This value cannot be invoked, did you accidentally add ()?");
			return false;
	}
	decl = decl_flatten(decl);
	return sema_expr_analyse_general_call(context, to, expr, decl, struct_var, macro);
}


static bool expr_check_index_in_range(Context *context, Type *type, Expr *index_expr, bool end_index, bool from_end)
{
	assert(type == type->canonical);
	if (index_expr->expr_kind != EXPR_CONST) return true;
	if (!bigint_fits_in_bits(&index_expr->const_expr.i, 64, true))
	{
		SEMA_ERROR(index_expr, "Index does not fit into an 64-signed integer.");
		return false;
	}
	int64_t index = bigint_as_signed(&index_expr->const_expr.i);
	if (from_end && index < 0)
	{
		SEMA_ERROR(index_expr, "Negative numbers are not allowed when indexing from the end.");
		return false;
	}
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			assert(!from_end);
			return true;
		case TYPE_ARRAY:
		{
			int64_t len = (int64_t)type->array.len;
			if (from_end)
			{
				index = len - index;
			}
			// Checking end can only be done for arrays.
			if (end_index && index >= len)
			{
				SEMA_ERROR(index_expr, "Array end index out of bounds, was %lld, exceeding array length %lld.", (long long)index, (long long)len);
				return false;
			}
			if (!end_index && index >= len)
			{
				SEMA_ERROR(index_expr, "Array index out of bounds, was %lld, exceeding max array index %lld.", (long long)index, (long long)len - 1);
				return false;
			}
			break;
		}
		case TYPE_STRLIT:
		case TYPE_SUBARRAY:
			// If not from end, just check the negative values.
			if (!from_end) break;
			// From end we can only do sanity checks ^0 is invalid for non-end index. ^-1 and less is invalid for all.
			if (index == 0 && !end_index)
			{
				SEMA_ERROR(index_expr,
				           "Array index out of bounds, index from end (%lld) must be greater than zero or it will exceed the max array index.",
				           (long long) index);
				return false;
			}
			return true;
		default:
			UNREACHABLE
	}
	if (index < 0)
	{
		SEMA_ERROR(index_expr, "Array index out of bounds, using a negative array index is only allowed with pointers.");
		return false;
	}
	return true;
}

static Type *sema_expr_find_indexable_type_recursively(Type **type, Expr **parent)
{
	while (1)
	{
		Type *inner_type = type_get_indexed_type(*type);
		if (!inner_type && type_is_substruct(*type))
		{
			Expr *embedded_struct = expr_access_inline_member(*parent, (*type)->decl);
			*type = embedded_struct->type->canonical;
			*parent = embedded_struct;
			continue;
		}
		return inner_type;
	}
}
static inline bool sema_expr_analyse_subscript(Context *context, Expr *expr)
{
	if (!sema_analyse_expr(context, NULL, expr->subscript_expr.expr)) return false;
	expr->failable = expr->subscript_expr.expr->failable;
	assert(expr->expr_kind == EXPR_SUBSCRIPT);
	Expr *subscripted = expr->subscript_expr.expr;
	Type *type = type_flatten(subscripted->type);
	Expr *index = expr->subscript_expr.index;
	Type *current_type = type;
	Expr *current_expr = subscripted;

	Type *inner_type = sema_expr_find_indexable_type_recursively(&current_type, &current_expr);
	if (!inner_type)
	{
		SEMA_ERROR(subscripted, "Cannot index '%s'.", type_to_error_string(type));
		return false;
	}
	if (!sema_analyse_expr(context, NULL, index)) return false;

	expr->constant = index->constant & subscripted->constant;
	expr->pure = index->pure & subscripted->pure;

	// Cast to an appropriate type for index.
	if (!expr_cast_to_index(index)) return false;

	// Check range
	if (!expr_check_index_in_range(context, current_type, index, false, expr->subscript_expr.from_back)) return false;

	expr->subscript_expr.expr = current_expr;
	expr->failable |= index->failable;
	expr_set_type(expr, inner_type);
	return true;
}

static inline bool sema_expr_analyse_slice(Context *context, Expr *expr)
{
	if (!sema_analyse_expr(context, NULL, expr->slice_expr.expr)) return false;
	expr->failable = expr->slice_expr.expr->failable;
	assert(expr->expr_kind == EXPR_SLICE);
	Expr *subscripted = expr->slice_expr.expr;
	expr->pure = subscripted->pure;
	expr->constant = subscripted->constant;
	Type *type = type_flatten(subscripted->type);
	Expr *start = expr->slice_expr.start;
	Expr *end = expr->slice_expr.end;

	Expr *current_expr = subscripted;

	Type *inner_type = sema_expr_find_indexable_type_recursively(&type, &current_expr);
	if (!inner_type)
	{
		SEMA_ERROR(subscripted, "Cannot index '%s'.", type_to_error_string(subscripted->type));
		return false;
	}
	expr->slice_expr.expr = current_expr;

	if (!sema_analyse_expr(context, type_int, start)) return false;
	expr->pure &= start->pure;
	expr->constant &= start->constant;
	if (end && !sema_analyse_expr(context, type_int, end)) return false;
	expr->pure &= !end || end->pure;
	expr->constant &= !end || end->constant;

	// Fix index sizes
	if (!expr_cast_to_index(start)) return false;
	if (end && !expr_cast_to_index(end)) return false;

	// Check range
	if (type->type_kind == TYPE_POINTER)
	{
		if (expr->slice_expr.start_from_back)
		{
			SEMA_ERROR(expr->slice_expr.start, "Indexing from the end is not allowed for pointers.");
			return false;
		}
		if (!end)
		{
			SEMA_ERROR(expr, "Omitting end index is not allowed for pointers.");
			return false;
		}
		if (end && expr->slice_expr.end_from_back)
		{
			SEMA_ERROR(expr->slice_expr.end, "Indexing from the end is not allowed for pointers.");
			return false;
		}
	}
	if (!expr_check_index_in_range(context, type, start, false, expr->slice_expr.start_from_back)) return false;
	if (end && !expr_check_index_in_range(context, type, end, true, expr->slice_expr.end_from_back)) return false;

	if (start && end && start->expr_kind == EXPR_CONST && end->expr_kind == EXPR_CONST)
	{
		if (type->type_kind == TYPE_ARRAY)
		{
			BigInt len;
			bigint_init_unsigned(&len, type->array.len);
			BigInt result;
			if (expr->slice_expr.start_from_back)
			{
				bigint_sub(&result, &len, &start->const_expr.i);
				start->const_expr.i = result;
				expr->slice_expr.start_from_back = false;
			}
			if (expr->slice_expr.end_from_back)
			{
				bigint_sub(&result, &len, &end->const_expr.i);
				end->const_expr.i = result;
				expr->slice_expr.end_from_back = false;
			}
		}
		if (expr->slice_expr.start_from_back && expr->slice_expr.end_from_back)
		{
			if (expr_const_compare(&start->const_expr, &end->const_expr, BINARYOP_LT))
			{
				SEMA_ERROR(start, "Start index greater than end index.");
				return false;
			}
		}
		else if (!expr->slice_expr.start_from_back && !expr->slice_expr.end_from_back)
		{
			if (expr_const_compare(&start->const_expr, &end->const_expr, BINARYOP_GT))
			{
				SEMA_ERROR(start, "Start index greater than end index.");
				return false;
			}
		}
	}

	expr->failable |= start->failable;
	expr_set_type(expr, type_get_subarray(inner_type));
	return true;
}


static inline bool sema_expr_analyse_group(Context *context, Type *to, Expr *expr)
{
	if (!sema_analyse_expr(context, NULL, expr->group_expr)) return false;
	*expr = *expr->group_expr;
	return true;
}


static inline void expr_rewrite_to_int_const(Expr *expr_to_rewrite, Type *type, uint64_t value)
{
	expr_to_rewrite->expr_kind = EXPR_CONST;
	expr_const_set_int(&expr_to_rewrite->const_expr, value, type->canonical->type_kind);
	expr_set_type(expr_to_rewrite, type);
	expr_to_rewrite->constant = true;
	expr_to_rewrite->pure = true;
	expr_to_rewrite->resolve_status = RESOLVE_DONE;
}


static inline void expr_rewrite_to_string(Expr *expr_to_rewrite, const char *string)
{
	expr_to_rewrite->expr_kind = EXPR_CONST;
	expr_to_rewrite->constant = true;
	expr_to_rewrite->const_expr.kind = TYPE_STRLIT;
	expr_to_rewrite->const_expr.string.chars = (char *)string;
	expr_to_rewrite->const_expr.string.len = (int)strlen(string);
	expr_to_rewrite->pure = true;
	expr_to_rewrite->resolve_status = RESOLVE_DONE;
	expr_set_type(expr_to_rewrite, type_compstr);
}


static bool sema_expr_analyse_typeinfo(Context *context, Expr *expr)
{
	expr->constant = true;
	expr->pure = true;

	TypeInfo *type_info = expr->type_expr;
	if (!sema_resolve_type_info(context, type_info)) return false;
	expr_set_type(expr, type_typeinfo);
	return true;
}

/*
static inline bool sema_expr_analyse_member_access(Context *context, Expr *expr)
{
	Type *type = expr->access_expr.parent->type->decl->member_decl.type_info->type;
	Type *canonical = type->canonical;
	const char *sub_element = TOKSTR(expr->access_expr.sub_element);
	if (sub_element == kw_sizeof)
	{
		expr_rewrite_to_int_const(expr, type_usize, type_size(canonical));
		return true;
	}
	if (sub_element == kw_offsetof)
	{
		TODO // calculate offset.
	}
	// Possibly alignof
	if (!type_may_have_sub_elements(type))
	{
		SEMA_ERROR(expr, "'%s' does not have a member '%s'.", type_to_error_string(type), sub_element);
		return false;
	}
	Decl *decl = canonical->decl;

	switch (decl->decl_kind)
	{
		case DECL_ENUM:
			if (TOKTYPE(expr->access_expr.sub_element) == TOKEN_CONST_IDENT)
			{
				if (!sema_expr_analyse_enum_constant(expr, sub_element, decl))
				{
					SEMA_ERROR(expr,
					               "'%s' has no enumeration value '%s'.",
					               decl->name,
					               sub_element);
					return false;
				}
				return true;
			}
			break;
		case DECL_ERR:
		case DECL_UNION:
		case DECL_STRUCT:
			break;
		default:
			UNREACHABLE
	}

	VECEACH(decl->methods, i)
	{
		Decl *function = decl->methods[i];
		if (sub_element == function->name)
		{
			expr->access_expr.ref = function;
			expr->type = function->type;
			return true;
		}
	}

	if (decl_is_struct_type(decl))
	{
		VECEACH(decl->strukt.members, i)
		{
			Decl *member = decl->strukt.members[i];
			if (sub_element == member->name)
			{
				expr->access_expr.ref = member;
				expr->type = member->member_decl.reference_type;
				return true;
			}
		}
	}
	SEMA_ERROR(expr,
	               "No function or member '%s.%s' found.",
	               type_to_error_string(type),
	               sub_element);
	return false;
}
*/


static void add_members_to_context(Context *context, Decl *decl)
{
	VECEACH(decl->methods, i)
	{
		Decl *func = decl->methods[i];
		sema_add_member(context, func);
	}
	while (decl->decl_kind == DECL_DISTINCT)
	{
		Type *type = decl->distinct_decl.base_type->canonical;
		if (!type_is_user_defined(type)) break;
		decl = type->decl;
	}
	if (decl_is_struct_type(decl))
	{
		Decl **members = decl->strukt.members;
		VECEACH(members, i)
		{
			Decl *member = members[i];
			if (member->name == NULL)
			{
				add_members_to_context(context, member);
				continue;
			}
			sema_add_member(context, member);
		}
	}
}

static Expr *enum_minmax_value(Decl *decl, BinaryOp comparison)
{
	assert(decl->decl_kind == DECL_ENUM);
	bool is_signed = type_is_signed(decl->enums.type_info->type->canonical);
	Expr *expr = NULL;
	VECEACH(decl->enums.values, i)
	{
		Decl *enum_constant = decl->enums.values[i];
		Expr *candidate = enum_constant->enum_constant.expr;
		assert(candidate->expr_kind == EXPR_CONST);
		if (!expr)
		{
			expr = candidate;
			continue;
		}
		if (expr_const_compare(&candidate->const_expr, &expr->const_expr, comparison))
		{
			expr = candidate;
		}
	}
	return expr;
}

/**
 * 1. .A -> It is an enum constant.
 * 2. .foo -> It is a function.
 * 3. .@foo -> It is a macro.
 * 4. .#bar -> It is an identifier to resolve as a member or a function
 * 5. .@#bar -> It is an identifier to resolve as a macro
 */
static TokenId sema_expr_resolve_access_child(Expr *child)
{
	switch (child->expr_kind)
	{
		case EXPR_IDENTIFIER:
		case EXPR_CONST_IDENTIFIER:
			// Not allowed obviously.
			if (child->identifier_expr.path) break;
			return child->identifier_expr.identifier;
		case EXPR_HASH_IDENT:
			TODO
		case EXPR_MACRO_EXPANSION:
			child = child->macro_expansion_expr.inner;
			switch (child->expr_kind)
			{
				case EXPR_IDENTIFIER:
				case EXPR_HASH_IDENT:
					return sema_expr_resolve_access_child(child);
				default:
					SEMA_ERROR(child, "Expected a macro name.");
					return INVALID_TOKEN_ID;
			}
		default:
			break;

	}
	SEMA_ERROR(child, "Expected an identifier here.");
	return INVALID_TOKEN_ID;
}

static inline bool sema_expr_analyse_type_access(Expr *expr, TypeInfo *parent, bool was_group, bool is_macro, TokenId identifier_token)
{
	// 1. Foo*.sizeof is not allowed, it must be (Foo*).sizeof
	if (!was_group && type_kind_is_derived(parent->type->type_kind))
	{
		SEMA_ERROR(expr->access_expr.parent, "Array and pointer types must be enclosed in (), did you forget it?");
		return false;
	}

	expr->constant = true;
	expr->pure = true;
	TokenType type = TOKTYPE(identifier_token);

	// 2. Handle Foo.typeid => return a typeid expression.
	if (type == TOKEN_TYPEID)
	{
		expr_set_type(expr, type_typeid);
		expr->expr_kind = EXPR_TYPEID;
		expr->typeid_expr = parent;
		expr->resolve_status = RESOLVE_DONE;
		return true;
	}

	Type *canonical = parent->type->canonical;
	const char *name = TOKSTR(identifier_token);

	// 3. Handle float.nan, double.inf etc
	if (type_is_float(canonical))
	{
		if (name == kw_nan)
		{
			expr->expr_kind = EXPR_CONST;
			expr->const_expr.kind = canonical->type_kind;
#if LONG_DOUBLE
			expr->const_expr.f = nanl("");
#else
			expr->const_expr.f = nan("");
#endif
			expr_set_type(expr, parent->type);
			expr->constant = true;
			expr->pure = true;
			expr->resolve_status = RESOLVE_DONE;
			return true;
		}
		if (name == kw_inf)
		{
			expr->expr_kind = EXPR_CONST;
			expr->const_expr.kind = parent->type->canonical->type_kind;
			expr->const_expr.f = INFINITY;
			expr_set_type(expr, parent->type->canonical);
			expr->constant = true;
			expr->pure = true;
			expr->resolve_status = RESOLVE_DONE;
			return true;
		}
	}

	if (!type_may_have_sub_elements(canonical))
	{
		SEMA_ERROR(expr, "'%s' does not have a property '%s'.", type_to_error_string(parent->type), name);
		return false;
	}
	Decl *decl = canonical->decl;
	// TODO add more constants that can be inspected?
	// e.g. SomeEnum.values, MyUnion.x.offset etc?
	switch (decl->decl_kind)
	{
		case DECL_ENUM:
			if (type == TOKEN_CONST_IDENT)
			{
				if (!sema_expr_analyse_enum_constant(expr, identifier_token, decl))
				{
					SEMA_ERROR(expr, "'%s' has no enumeration value '%s'.", decl->name, name);
					return false;
				}
				return true;
			}
			if (name == kw_elements)
			{
				expr_rewrite_to_int_const(expr, type_compint, vec_size(decl->enums.values));
				return true;
			}
			if (name == kw_max)
			{
				Expr *max = enum_minmax_value(decl, BINARYOP_GT);
				if (!max)
				{
					expr_rewrite_to_int_const(expr, decl->enums.type_info->type->canonical, 0);
					return true;
				}
				expr_replace(expr, max);
				return true;
			}
			if (name == kw_min)
			{
				Expr *min = enum_minmax_value(decl, BINARYOP_LT);
				if (!min)
				{
					expr_rewrite_to_int_const(expr, decl->enums.type_info->type->canonical, 0);
					return true;
				}
				expr_replace(expr, min);
				return true;
			}
			break;
		case DECL_ERR:
		case DECL_UNION:
		case DECL_STRUCT:
		case DECL_DISTINCT:
			break;
		default:
			UNREACHABLE
	}


	VECEACH(decl->methods, i)
	{
		Decl *function = decl->methods[i];
		if (name == function->name)
		{
			expr->access_expr.ref = function;
			expr_set_type(expr, function->type);
			return true;
		}
	}
	SEMA_ERROR(expr, "No function '%s.%s' found.", type_to_error_string(parent->type), name);
	return false;
}

static inline bool sema_expr_analyse_member_access(Context *context, Expr *expr, bool is_macro, TokenId identifier_token)
{
	Expr *parent = expr->access_expr.parent;

	expr->constant = true;
	expr->pure = true;

	Expr *child = expr->access_expr.child;

	TokenType type = TOKTYPE(identifier_token);
	const char *name = TOKSTR(identifier_token);

	Decl *ref = parent->access_expr.ref;

	bool is_leaf_member_ref = ref->decl_kind == DECL_VAR;
	if (type == TOKEN_TYPEID && !is_macro)
	{
		expr_set_type(expr, type_typeid);
		expr->expr_kind = EXPR_TYPEID;
		if (is_leaf_member_ref)
		{
			expr->typeid_expr = ref->var.type_info;
		}
		else
		{
			expr->typeid_expr = type_info_new_base(ref->type, parent->span);
		}
		expr->resolve_status = RESOLVE_DONE;
		return true;
	}
	if (name == kw_ordinal && !is_macro)
	{
		if (ref->decl_kind == DECL_ENUM_CONSTANT)
		{
			expr_rewrite_to_int_const(expr, type_compint, ref->enum_constant.ordinal);
			return true;
		}
	}

	// If we have something like struct Foo { Bar b; int c; struct d { int e; } }
	// If we are inspecting Foo.c we're done here. Otherwise handle struct d / Bar b case
	// The same way.
	if (is_leaf_member_ref)
	{
		if (!type_is_structlike(ref->type))
		{
			if (is_macro)
			{
				SEMA_ERROR(expr, "'%s' does not have a macro '%s'.", type_to_error_string(ref->type), name);
				return false;
			}
			SEMA_ERROR(expr, "'%s' does not have a member or property '%s'.", type_to_error_string(ref->type), name);
			return false;
		}
		// Pretend to be an inline struct.
		ref = ref->type->decl;
	}

	VECEACH(ref->methods, i)
	{
		Decl *decl = ref->methods[i];
		bool is_macro_decl = decl->decl_kind == DECL_MACRO;
		if (name == decl->name)
		{
			if (is_macro != is_macro_decl)
			{
				if (is_macro)
				{
					SEMA_ERROR(child, "Method '%s' should not be prefixed with '@'.", name);
				}
				else
				{
					SEMA_ERROR(child, "Macro method '%s' must be prefixed with '@'.", name);
				}
				return false;
			}
			expr->access_expr.ref = decl;
			expr_set_type(expr, decl->type);
			return true;
		}
	}
	// We want a distinct to be able to access members.
	Decl *flat_ref = ref;
	while (flat_ref->decl_kind == DECL_DISTINCT)
	{
		Type *flat_type = flat_ref->distinct_decl.base_type->canonical;
		if (!type_is_user_defined(flat_type)) break;
		flat_ref = flat_type->decl;
	}

	switch (flat_ref->decl_kind)
	{
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_ERR:
			VECEACH(flat_ref->strukt.members, i)
			{
				Decl *member = ref->strukt.members[i];
				if (name == member->name)
				{
					if (is_macro)
					{
						SEMA_ERROR(child, "member '%s' should not be prefixed with '@'.", name);
						return false;
					}
					expr->expr_kind = EXPR_MEMBER_ACCESS;
					expr->access_expr.ref = member;
					expr_set_type(expr, member->type);
					return true;
				}
			}
		default:
			break;
	}
	SEMA_ERROR(expr, "No function or member '%s.%s' found.", "todo", name);
	TODO
	return false;
}

/**
 * Analyse "x.y"
 */
static inline bool sema_expr_analyse_access(Context *context, Expr *expr)
{
	Expr *parent = expr->access_expr.parent;
	bool was_group = parent->expr_kind == EXPR_GROUP;

	// 1. Resolve the left hand
	if (!sema_analyse_expr_value(context, NULL, parent)) return false;

	// 2. The right hand side may be a @ident or ident
	Expr *child = expr->access_expr.child;
	bool is_macro = child->expr_kind == EXPR_MACRO_EXPANSION;

	// 3. Find the actual token.
	TokenId identifier_token = sema_expr_resolve_access_child(child);
	if (TOKEN_IS_INVALID(identifier_token)) return false;

	// 2. If our left hand side is a type, e.g. MyInt.abc, handle this here.
	if (parent->expr_kind == EXPR_TYPEINFO)
	{
		return sema_expr_analyse_type_access(expr, parent->type_expr, was_group, is_macro, identifier_token);
	}

	// 3. The left hand side may already be indexing into a type:
	//    x.y.z. In this case "x.y" is the parent.
	if (parent->expr_kind == EXPR_MEMBER_ACCESS)
	{
		return sema_expr_analyse_member_access(context, expr, is_macro, identifier_token);
	}


	// 6. Copy failability
	expr->failable = parent->failable;

	assert(expr->expr_kind == EXPR_ACCESS);
	assert(parent->resolve_status == RESOLVE_DONE);

	// 7. Is this a pointer? If so we insert a deref.
	bool is_pointer = parent->type->canonical->type_kind == TYPE_POINTER;
	if (is_pointer)
	{
		if (!sema_cast_rvalue(context, NULL, parent)) return false;
		expr_insert_deref(expr->access_expr.parent);
		parent = expr->access_expr.parent;
	}

	// 8. Depending on parent type, we have some hard coded types
	const char *kw = TOKSTR(identifier_token);
	Expr *current_parent = parent;

	Type *type = parent->type->canonical;
	Type *flat_type = type_flatten(type);
CHECK_DEEPER:

	// 9. Fix hard coded function `len` on subarrays and arrays
	if (!is_macro && kw == kw_len)
	{
		if (flat_type->type_kind == TYPE_SUBARRAY)
		{
			expr->expr_kind = EXPR_LEN;
			expr->len_expr.inner = parent;
			expr_set_type(expr, type_void);
			expr->resolve_status = RESOLVE_DONE;
			return true;
		}
		if (flat_type->type_kind == TYPE_ARRAY)
		{
			expr_rewrite_to_int_const(expr, type_compint, flat_type->array.len);
			return true;
		}
	}

	// 9. At this point we may only have distinct, struct, union, error, enum
	if (!type_may_have_sub_elements(type))
	{
		SEMA_ERROR(expr, "There is no member or method '%s' on '%s'", kw, type_to_error_string(parent->type));
		return false;
	}

	// 10. Dump all members and methods into the scope.
	Decl *decl = type->decl;
	Decl *member;
	SCOPE_START
		add_members_to_context(context, decl);
		member = sema_resolve_symbol_in_current_dynamic_scope(context, kw);
	SCOPE_END;

	if (!member)
	{
		Decl *ambiguous = NULL;
		Decl *private = NULL;
		member = sema_resolve_method(context, decl, kw, &ambiguous, &private);
		if (member)
		{
			context_register_external_symbol(context, member);
		}
	}
	// 11. If we didn't find a match...
	if (!member)
	{
		// 11a. We have a potential embedded struct check:
		if (type_is_substruct(type))
		{
			Expr *embedded_struct = expr_access_inline_member(parent, type->decl);
			current_parent = embedded_struct;
			type = embedded_struct->type->canonical;
			goto CHECK_DEEPER;
		}

		// 11b. Otherwise we give up.
		SEMA_ERROR(expr, "There is no field or method '%s.%s'.", decl->name, kw);
		return false;
	}

	// 12a. If the member was a macro and it isn't prefixed with `@` then that's an error.
	if (member->decl_kind == DECL_MACRO && !is_macro)
	{
		SEMA_ERROR(expr, "Expected '@' before the macro name.");
		return false;
	}
	// 12b. If the member was *not* a macro but was prefixed with `@` then that's an error.
	if (member->decl_kind != DECL_MACRO && is_macro)
	{
		SEMA_ERROR(expr, "'@' should only be placed in front of macro names.");
		return false;
	}

	// 13. Copy properties.
	expr->access_expr.parent = current_parent;
	expr->constant = expr->access_expr.parent->constant;
	expr->pure = expr->access_expr.parent->pure;
	expr_set_type(expr, member->type);
	expr->access_expr.ref = member;
	return true;
}

static Decl *sema_resolve_element_for_name(Decl** decls, DesignatorElement **elements, unsigned *index)
{

	DesignatorElement *element = elements[*index];
	const char *name = element->field;
	unsigned old_index = *index;
	VECEACH(decls, i)
	{
		Decl *decl = decls[i];
		// The simple case, we have a match.
		if (decl->name == name)
		{
			element->index = i;
			return decl;
		}
		if (!decl->name)
		{
			assert(type_is_structlike(decl->type));
			// Anonymous struct
			Decl *found = sema_resolve_element_for_name(decl->strukt.members, elements, index);
			// No match, continue...
			if (!found) continue;

			// Special handling, we now need to patch the elements
			unsigned current_size = vec_size(elements);
			// Add an element at the end.
			vec_add(elements, NULL);
			// Shift all elements
			for (unsigned j = current_size; j > old_index; j--)
			{
				elements[j] = elements[j - 1];
			}
			// Create our anon field.
			DesignatorElement *anon_element = CALLOCS(DesignatorElement);
			anon_element->kind = DESIGNATOR_FIELD;
			anon_element->index = i;
			elements[old_index] = anon_element;
			// Advance
			(*index)++;
			return found;
		}
	}
	return NULL;
}

static int64_t sema_analyse_designator_index(Context *context, Expr *index)
{
	if (!sema_analyse_expr_value(context, type_uint, index))
	{
		return -1;
	}

	// Unless we already have type_usize, cast to type_isize;
	if (!expr_cast_to_index(index))
	{
		return -1;
	}
	if (index->expr_kind != EXPR_CONST)
	{
		SEMA_ERROR(index, "The index must be a constant value.");
		return -1;
	}
	if (!bigint_fits_in_bits(&index->const_expr.i, 64, true))
	{
		SEMA_ERROR(index, "The value of the index does not fit in a long.");
		return -1;
	}
	int64_t index_val = bigint_as_signed(&index->const_expr.i);
	if (index_val < 0)
	{
		SEMA_ERROR(index, "Negative index values is not allowed.");
		return -1;
	}
	return index_val;
}

static Type *sema_find_type_of_element(Context *context, Type *type, DesignatorElement **elements, unsigned *curr_index, bool *is_constant, bool *did_report_error, ArrayIndex *max_index)
{
	Type *type_lowered = type_lowering(type);
	DesignatorElement *element = elements[*curr_index];
	if (element->kind == DESIGNATOR_ARRAY || element->kind == DESIGNATOR_RANGE)
	{
		if (type_lowered->type_kind != TYPE_ARRAY && type_lowered->type_kind != TYPE_INFERRED_ARRAY)
		{
			return NULL;
		}
		ByteSize len = type_lowered->type_kind == TYPE_INFERRED_ARRAY ? MAX_ARRAYINDEX : type_lowered->array.len;
		ArrayIndex index = sema_analyse_designator_index(context, element->index_expr);
		if (index < 0)
		{
			*did_report_error = true;
			return NULL;
		}
		if (index >= (ArrayIndex)len)
		{
			SEMA_ERROR(element->index_expr, "The index may must be less than the array length (which was %llu).", (unsigned long long)len);
			*did_report_error = true;
			return NULL;
		}

		element->index = index;
		if (max_index && *max_index < index) *max_index = index;
		if (element->kind == DESIGNATOR_RANGE)
		{
			int64_t end_index = sema_analyse_designator_index(context, element->index_end_expr);
			if (end_index < 0)
			{
				*did_report_error = true;
				return NULL;
			}
			if (index > end_index)
			{
				SEMA_ERROR(element->index_end_expr, "End index must be greater than start index.");
				*did_report_error = true;
				return NULL;
			}
			if (end_index > (ArrayIndex)len)
			{
				*did_report_error = true;
				SEMA_ERROR(element->index_expr, "The index may must be less than the array length (which was %llu).", (unsigned long long)len);
				return NULL;
			}
			element->index_end = end_index;
			if (max_index && *max_index < end_index) *max_index = end_index;
		}
		return type_lowered->array.base;
	}
	assert(element->kind == DESIGNATOR_FIELD);
	if (!type_is_structlike(type_lowered))
	{
		return NULL;
	}
	Decl *member = sema_resolve_element_for_name(type_lowered->decl->strukt.members, elements, curr_index);
	if (!member) return NULL;
	return member->type;
}

static Type *sema_expr_analyse_designator(Context *context, Type *current, Expr *expr, ArrayIndex *max_index)
{
	DesignatorElement **path = expr->designator_expr.path;

	// Walk down into this path
	bool is_constant = true;
	bool did_report_error = false;
	for (unsigned i = 0; i < vec_size(path); i++)
	{
		Type *new_current = sema_find_type_of_element(context, current, path, &i, &is_constant, &did_report_error, i == 0 ? max_index : NULL);
		if (!new_current)
		{
			if (!did_report_error) SEMA_ERROR(expr, "This is not a valid member of '%s'.", type_to_error_string(current));
			return NULL;
		}
		current = new_current;
	}
	expr->constant = is_constant;
	expr->pure = is_constant;
	return current;
}

static void sema_update_const_initializer_with_designator(ConstInitializer *const_init,
														  DesignatorElement **curr,
														  DesignatorElement **end,
														  Expr *value);

static void sema_create_const_initializer_value(ConstInitializer *const_init, Expr *value)
{
	// Possibly this is already a const initializers, in that case
	// overwrite what is inside, eg [1] = { .a = 1 }
	if (value->expr_kind == EXPR_INITIALIZER_LIST && value->initializer_expr.init_type == INITIALIZER_CONST)
	{
		*const_init = *value->initializer_expr.initializer;
		value->initializer_expr.initializer = const_init;
		return;
	}
	const_init->init_value = value;
	const_init->type = type_flatten(value->type);
	const_init->kind = CONST_INIT_VALUE;
}

static bool is_empty_initializer_list(Expr *value)
{
	return value->initializer_expr.init_type == INITIALIZER_CONST && value->initializer_expr.initializer->kind == CONST_INIT_ZERO;
}
static void sema_create_const_initializer(ConstInitializer *const_init, Expr *initializer);

/**
 * Update a struct element, e.g. { .a = 1 } or { .a[12] = { .b } }
 */
static inline void sema_update_const_initializer_with_designator_struct(ConstInitializer *const_init,
																		DesignatorElement **curr,
																		DesignatorElement **end,
																		Expr *value)
{
	// Get the current path element that we're processing
	DesignatorElement *element = curr[0];
	assert(element->kind == DESIGNATOR_FIELD);
	DesignatorElement **next_element = curr + 1;
	bool is_last_path_element = next_element == end;

	// Optimize in case this is a zero, e.g. [12].b = {}
	if (is_last_path_element && is_empty_initializer_list(value))
	{
		const_init->kind = CONST_INIT_ZERO;
		return;
	}
	Decl **elements = const_init->type->decl->strukt.members;

	// Convert a zero struct and expand it into all its parts.
	if (const_init->kind == CONST_INIT_ZERO)
	{
		// Allocate array containing all elements { a, b, c ... }
		ConstInitializer **const_inits = MALLOC(sizeof(ConstInitializer *) * vec_size(elements));
		VECEACH(elements, i)
		{
			// Create zero initializers for each of those { a: zeroinit, b: zeroinit, ... }
			ConstInitializer *element_init = MALLOC(sizeof(ConstInitializer));
			element_init->type = type_flatten(elements[i]->type);
			element_init->kind = CONST_INIT_ZERO;
			const_inits[i] = element_init;
		}
		// Change type to CONST_INIT_STRUCT since we expanded.
		const_init->init_struct = const_inits;
		const_init->kind = CONST_INIT_STRUCT;
	}

	// We should always have expanded the struct at this point.
	assert(const_init->kind == CONST_INIT_STRUCT);

	// Find the ConstInitializer to change
	ConstInitializer *sub_element = const_init->init_struct[element->index];

	// If this isn't the last element, we recurse.
	if (!is_last_path_element)
	{
		sema_update_const_initializer_with_designator(sub_element, next_element, end, value);
		return;
	}

	// Otherwise we update the value in that particular element.
	sema_create_const_initializer_value(sub_element, value);
}

/**
 * Update a union element, which is different from structs, since we're here
 * only keeping a single value.
 * Note that if we have two fields "a" and "b", then in this case { .b = 2, .a = 1 },
 * we will completely discard the information in ".b = 2", even if there had been
 * an overlap. In essence we're implicitly clearing the memory when we assign to .a, meaning
 * we are allowed to completely overwrite the "memory" of .b
 */
static inline void sema_update_const_initializer_with_designator_union(ConstInitializer *const_init,
                                                                        DesignatorElement **curr,
                                                                        DesignatorElement **end,
                                                                        Expr *value)
{
	DesignatorElement *element = curr[0];
	assert(element->kind == DESIGNATOR_FIELD);
	ConstInitializer *sub_element = const_init->init_union.element;

	// If it's an empty initializer, just clear everything back to CONST_INIT_ZERO
	// That is we get for example: { .b = { 1, 3 }, .a = { } }. This would then simply
	// become { }
	DesignatorElement **next_element = curr + 1;
	bool is_at_last_path_element = next_element == end;
	if (is_at_last_path_element && is_empty_initializer_list(value))
	{
		const_init->kind = CONST_INIT_ZERO;
		return;
	}

	// If we currently have a zero, then we create a sub element that is zero.
	if (const_init->kind == CONST_INIT_ZERO)
	{
		sub_element = const_init->init_union.element = CALLOCS(ConstInitializer);
		sub_element->kind = CONST_INIT_ZERO;
		const_init->init_union.element = sub_element;
	}
	else if (element->index != const_init->init_union.index)
	{
		// We will zero out the sub element if it wasn't a union
		sub_element->kind = CONST_INIT_ZERO;
	}

	// Update of the sub element.
	sub_element->type = type_flatten(const_init->type->decl->strukt.members[element->index]->type);

	// And the index
	const_init->init_union.index = element->index;

	// And the type
	const_init->kind = CONST_INIT_UNION;

	// If we're not at the last element in the path, descend further.
	if (!is_at_last_path_element)
	{
		sema_update_const_initializer_with_designator(sub_element, next_element, end, value);
		return;
	}

	// Otherwise just set the current type.
	sema_create_const_initializer_value(sub_element, value);
}




/**
 * Update an array { [2] = 1 }
 */
static inline void sema_update_const_initializer_with_designator_array(ConstInitializer *const_init,
                                                                       DesignatorElement **curr,
                                                                       DesignatorElement **end,
                                                                       Expr *value)
{
	DesignatorElement *element = curr[0];
	ArrayIndex low_index = element->index;
	ArrayIndex high_index = element->kind == DESIGNATOR_RANGE ? element->index_end : element->index;
	assert(element->kind == DESIGNATOR_ARRAY || element->kind == DESIGNATOR_RANGE);

	// Expand zero into array.
	if (const_init->kind == CONST_INIT_ZERO)
	{
		const_init->kind = CONST_INIT_ARRAY;
		const_init->init_array.elements = NULL;
	}

	Type *element_type = type_flatten(const_init->type->array.base);
	DesignatorElement **next_element = curr + 1;
	bool is_last_path_element = next_element == end;

	// Get all the elements in the array
	ConstInitializer **array_elements = const_init->init_array.elements;

	unsigned array_count = vec_size(array_elements);

	ArrayIndex insert_index = 0;

	for (ArrayIndex index = low_index; index <= high_index; index++)
	{
		// Walk to the insert point or until we reached the end of the array.
		while (insert_index < array_count && array_elements[insert_index]->init_array_value.index < index)
		{
			insert_index++;
		}
		// Pick up the initializer at the insert point.
		ConstInitializer *initializer = insert_index < array_count ? array_elements[insert_index] : NULL;
		ConstInitializer *inner_value;

		// If we don't have an initializer, the location needs to be at the end.
		// Create and append:
		if (!initializer)
		{
			initializer = MALLOC(sizeof(ConstInitializer));
			initializer->type = element_type;
			initializer->kind = CONST_INIT_ARRAY_VALUE;
			initializer->init_array_value.index = index;
			inner_value = MALLOC(sizeof(ConstInitializer));
			inner_value->type = element_type;
			inner_value->kind = CONST_INIT_ZERO;
			initializer->init_array_value.element = inner_value;
			vec_add(array_elements, initializer);
			array_count++;
		}
		else
		{
			// If we already have an initializer "found"
			// it might be after the index. In this case, we
			// need to do an insert.
			if (initializer->init_array_value.index != insert_index)
			{
				assert(initializer->init_array_value.index > insert_index);

				// First we add a null at the end.
				vec_add(array_elements, NULL);
				// Shift all values one step up:
				for (unsigned i = array_count; i > insert_index; i--)
				{
					array_elements[i] = array_elements[i - 1];
				}
				// Then we create our new entry.
				initializer = MALLOC(sizeof(ConstInitializer));
				initializer->type = element_type;
				initializer->kind = CONST_INIT_ARRAY_VALUE;
				initializer->init_array_value.index = index;
				inner_value = MALLOC(sizeof(ConstInitializer));
				inner_value->type = element_type;
				inner_value->kind = CONST_INIT_ZERO;
				initializer->init_array_value.element = inner_value;
				// And assign it to the location.
				array_elements[insert_index] = initializer;
			}
		}

		const_init->init_array.elements = array_elements;
		inner_value = initializer->init_array_value.element;

		// Update
		if (!is_last_path_element)
		{
			sema_update_const_initializer_with_designator(inner_value, next_element, end, value);
			continue;
		}
		sema_create_const_initializer_value(inner_value, value);
	}
}

static inline void sema_update_const_initializer_with_designator(
		ConstInitializer *const_init,
		DesignatorElement **curr,
		DesignatorElement **end,
		Expr *value)
{
	switch (const_init->type->type_kind)
	{
		case TYPE_STRUCT:
		case TYPE_ERRTYPE:
			sema_update_const_initializer_with_designator_struct(const_init, curr, end, value);
			return;
		case TYPE_UNION:
			sema_update_const_initializer_with_designator_union(const_init, curr, end, value);
			return;
		case TYPE_ARRAY:
			sema_update_const_initializer_with_designator_array(const_init, curr, end, value);
			return;
		case TYPE_VECTOR:
			TODO
		default:
			UNREACHABLE
	}
}


/**
 * Create a const initializer.
 */
static void sema_create_const_initializer(ConstInitializer *const_init, Expr *initializer)
{
	const_init->kind = CONST_INIT_ZERO;
	// Flatten the type since the external type might be typedef or a distinct type.
	const_init->type = type_flatten(initializer->type);

	// Loop through the initializers.
	Expr **init_expressions = initializer->initializer_expr.initializer_expr;
	VECEACH(init_expressions, i)
	{
		Expr *expr = init_expressions[i];
		DesignatorElement **path = expr->designator_expr.path;
		Expr *value = expr->designator_expr.value;
		sema_update_const_initializer_with_designator(const_init, path, path + vec_size(path), value);
	}
}

static void debug_dump_const_initializer(ConstInitializer *init, const char *name, unsigned indent)
{
	for (unsigned i = 0; i < indent; i++) printf("  ");
	if (name)
	{
		printf("%s : %s", name, init->type->name ?: "WTF");
	}
	else
	{
		printf("%s", init->type->name ?: "WTF");
	}
	switch (init->kind)
	{
		case CONST_INIT_ZERO:
			printf(" = 0\n");
			return;
		case CONST_INIT_UNION:
		{
			printf(" ->\n");
			Decl** members = init->type->decl->strukt.members;
			debug_dump_const_initializer(init->init_union.element, members[init->init_union.index]->name, indent + 1);
			return;
		}
		case CONST_INIT_STRUCT:
		{
			printf(" ->\n");
			Decl** members = init->type->decl->strukt.members;
			unsigned member_count = vec_size(members);
			for (unsigned i = 0; i < member_count; i++)
			{
				debug_dump_const_initializer(init->init_struct[i], members[i]->name, indent + 1);
			}
			return;
		}
		case CONST_INIT_ARRAY_VALUE:
		{
			printf(" [%llu] ->\n", (unsigned long long)init->init_array_value.index);
			debug_dump_const_initializer(init->init_array_value.element, "", indent + 1);
			return;
		}
		case CONST_INIT_VALUE:
		{
			assert(init->init_value->expr_kind == EXPR_CONST);
			printf("  = ");
			expr_const_fprint(stdout, &init->init_value->const_expr);
			puts("");
			return;
		}
		case CONST_INIT_ARRAY:
			printf(" [//] ->\n");
			{
				VECEACH(init->init_array.elements, i)
				{
					debug_dump_const_initializer(init->init_array.elements[i], NULL, indent + 1);
				}
			}
			return;
		case CONST_INIT_ARRAY_FULL:
			printf(" [: ->\n");
			{
				VECEACH(init->init_array_full, i)
				{
					debug_dump_const_initializer(init->init_array_full[i], NULL, indent + 1);
				}
			}
			return;

	}
	UNREACHABLE
}

static bool sema_expr_analyse_designated_initializer(Context *context, Type *assigned, Expr *initializer)
{
	Expr **init_expressions = initializer->initializer_expr.initializer_expr;
	Type *original = assigned->canonical;
	bool is_structlike = type_is_structlike(assigned->canonical);

	initializer->pure = true;
	initializer->constant = true;
	ArrayIndex max_index = -1;
	VECEACH(init_expressions, i)
	{
		Expr *expr = init_expressions[i];
		Type *result = sema_expr_analyse_designator(context, original, expr, &max_index);
		if (!result) return false;
		if (!sema_analyse_expr_of_required_type(context, result, expr->designator_expr.value, true)) return false;
		expr->pure &= expr->designator_expr.value->pure;
		expr->constant &= expr->designator_expr.value->constant;
		expr->failable |= expr->designator_expr.value->failable;
		expr->resolve_status = RESOLVE_DONE;
		initializer->pure &= expr->pure;
		initializer->constant &= expr->constant;
	}

	if (!is_structlike && initializer->type->type_kind == TYPE_INFERRED_ARRAY)
	{
		initializer->type = sema_type_lower_by_size(initializer->type, max_index + 1);
	}
	if (initializer->constant)
	{
		ConstInitializer *const_init = MALLOC(sizeof(ConstInitializer));
		sema_create_const_initializer(const_init, initializer);
		initializer->initializer_expr.init_type = INITIALIZER_CONST;
		initializer->initializer_expr.initializer = const_init;
		return true;
	}
	initializer->initializer_expr.init_type = INITIALIZER_DESIGNATED;
	return true;
}

/**
 * Perform analysis for a plain initializer, that is one initializing all fields.
 * @return true if analysis succeeds.
 */
static inline bool sema_expr_analyse_struct_plain_initializer(Context *context, Decl *assigned, Expr *initializer)
{
	initializer->pure = true;
	initializer->constant = true;

	Expr **elements = initializer->initializer_expr.initializer_expr;
	Decl **members = assigned->strukt.members;
	initializer->initializer_expr.init_type = INITIALIZER_NORMAL;
	unsigned size = vec_size(elements);
	unsigned expected_members = vec_size(members);

	// 1. For struct number of members must be the same as the size of the struct.
	//    Since we already handled the case with an empty initializer before going here
	//    zero entries must be an error.
	assert(size > 0 && "We should already have handled the size == 0 case.");
	if (expected_members == 0)
	{
		// Generate a nice error message for zero.
		SEMA_ERROR(elements[0], "Too many elements in initializer, it must be empty.");
		return false;
	}

	// 2. In case of a union, only expect a single entry.
	if (assigned->decl_kind == DECL_UNION) expected_members = 1;


	// 3. Loop through all elements.
	VECEACH(elements, i)
	{
		// 4. Check if we exceeded the list of elements in the struct/union.
		//    This way we can check the other elements which might help the
		//    user pinpoint where they put the double elements.
		Expr *element = elements[i];
		if (i >= expected_members)
		{
			SEMA_ERROR(element, "Too many elements in initializer, expected only %d.", expected_members);
			return false;
		}
		// 5. We know the required type, so resolve the expression.
		if (!sema_analyse_expr_of_required_type(context, members[i]->type, elements[i], 0)) return false;
		initializer->pure &= element->pure;
		initializer->constant &= element->constant;
		initializer->failable |= element->failable;
	}

	// 6. There's the case of too few values as well. Mark the last field as wrong.
	if (expected_members > size)
	{
		SEMA_ERROR(elements[size - 1], "Too few elements in initializer, there should be elements after this one.");
		return false;
	}

	if (initializer->constant)
	{
		ConstInitializer *const_init = CALLOCS(ConstInitializer);
		const_init->kind = CONST_INIT_STRUCT;
		const_init->type = type_flatten(initializer->type);
		ConstInitializer **inits = MALLOC(sizeof(ConstInitializer *) * vec_size(elements));
		VECEACH(elements, i)
		{
			Expr *expr = elements[i];
			if (expr->expr_kind == EXPR_INITIALIZER_LIST)
			{
				assert(expr->constant);
				inits[i] = expr->initializer_expr.initializer;
				continue;
			}
			ConstInitializer *element_init = MALLOC(sizeof(ConstInitializer));
			sema_create_const_initializer_value(element_init, expr);
			inits[i] = element_init;
		}
		const_init->init_struct = inits;
		initializer->initializer_expr.init_type = INITIALIZER_CONST;
		initializer->initializer_expr.initializer = const_init;
	}

	// 7. Done!
	return true;
}


/**
 * Perform analysis for a plain initializer, that is one initializing all fields.
 * @return true if analysis succeeds.
 */
static inline bool sema_expr_analyse_array_plain_initializer(Context *context, Type *assigned, Expr *initializer)
{
	initializer->pure = true;
	initializer->constant = true;

	Expr **elements = initializer->initializer_expr.initializer_expr;

	assert((assigned->type_kind == TYPE_ARRAY) && "The other types are not done yet.");

	Type *inner_type = type_get_indexed_type(assigned);
	assert(inner_type);

	initializer->initializer_expr.init_type = INITIALIZER_NORMAL;
	unsigned size = vec_size(elements);
	unsigned expected_members = assigned->array.len;
	if (assigned->type_kind != TYPE_ARRAY) expected_members = size;

	assert(size > 0 && "We should already have handled the size == 0 case.");
	if (expected_members == 0)
	{
		// Generate a nice error message for zero.
		SEMA_ERROR(elements[0], "Too many elements in initializer, it must be empty.");
		return false;
	}

	VECEACH(elements, i)
	{
		Expr *element = elements[i];
		if (i >= expected_members)
		{
			SEMA_ERROR(element, "Too many elements in initializer, expected only %d.", expected_members);
			return false;
		}
		if (!sema_analyse_expr_of_required_type(context, inner_type, element, true)) return false;
		initializer->failable |= element->failable;
		initializer->pure &= element->pure;
		initializer->constant &= element->constant;
	}

	if (expected_members > size)
	{
		SEMA_ERROR(elements[size - 1], "Too few elements in initializer, %d elements are needed.", expected_members);
		return false;
	}

	if (initializer->constant)
	{
		ConstInitializer *const_init = CALLOCS(ConstInitializer);
		const_init->kind = CONST_INIT_ARRAY_FULL;
		const_init->type = type_flatten(initializer->type);
		ConstInitializer **inits = MALLOC(sizeof(ConstInitializer *) * vec_size(elements));
		VECEACH(elements, i)
		{
			Expr *expr = elements[i];
			if (expr->expr_kind == EXPR_INITIALIZER_LIST)
			{
				assert(expr->constant);
				inits[i] = expr->initializer_expr.initializer;
				continue;
			}
			ConstInitializer *element_init = MALLOC(sizeof(ConstInitializer));
			sema_create_const_initializer_value(element_init, expr);
			inits[i] = element_init;
		}
		const_init->init_array_full = inits;
		initializer->initializer_expr.init_type = INITIALIZER_CONST;
		initializer->initializer_expr.initializer = const_init;
	}

	// 7. Done!
	return true;
}

static inline bool sema_expr_analyse_initializer(Context *context, Type *external_type, Type *assigned, Expr *expr)
{
	Expr **init_expressions = expr->initializer_expr.initializer_expr;

	unsigned init_expression_count = vec_size(init_expressions);

	// 1. Zero size init will initialize to empty.
	if (init_expression_count == 0)
	{
		external_type = sema_type_lower_by_size(external_type, 0);
		assigned = sema_type_lower_by_size(assigned, 0);
		expr_set_type(expr, external_type);
		ConstInitializer *initializer = CALLOCS(ConstInitializer);
		initializer->kind = CONST_INIT_ZERO;
		initializer->type = type_flatten(expr->type);
		expr->initializer_expr.init_type = INITIALIZER_CONST;
		expr->initializer_expr.initializer = initializer;
		expr->constant = true;
		expr->pure = true;
		return true;
	}

	// 2. Check if we might have a designated initializer
	//    this means that in this case we're actually not resolving macros here.
	if (init_expressions[0]->expr_kind == EXPR_DESIGNATOR)
	{
		expr_set_type(expr, external_type);
		return sema_expr_analyse_designated_initializer(context, assigned, expr);
	}

	external_type = sema_type_lower_by_size(external_type, init_expression_count);
	assigned = sema_type_lower_by_size(assigned, init_expression_count);
	expr_set_type(expr, external_type);

	// 3. Otherwise use the plain initializer.
	if (assigned->type_kind == TYPE_ARRAY || assigned->type_kind == TYPE_INFERRED_ARRAY)
	{
		return sema_expr_analyse_array_plain_initializer(context, assigned, expr);
	}

	return sema_expr_analyse_struct_plain_initializer(context, assigned->decl, expr);
}

static inline bool sema_expr_analyse_initializer_list(Context *context, Type *to, Expr *expr)
{
	assert(to);
	Type *assigned = type_flatten(to);
	switch (assigned->type_kind)
	{
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ARRAY:
		case TYPE_ERRTYPE:
		case TYPE_INFERRED_ARRAY:
			return sema_expr_analyse_initializer(context, to, assigned, expr);
		default:
			break;
	}
	// Fix error on compound literals
	SEMA_ERROR(expr, "'%s' cannot use compound literal initialization, did you intend to use a cast?", type_to_error_string(to));
	return false;
}



static inline bool sema_expr_analyse_expr_list(Context *context, Type *to, Expr *expr)
{
	bool success = true;
	ByteSize last = vec_size(expr->expression_list) - 1;
	bool constant = true;
	bool pure = true;
	VECEACH(expr->expression_list, i)
	{
		Expr *checked_expr = expr->expression_list[i];
		success &= sema_analyse_expr_of_required_type(context, i == last ? to : NULL, checked_expr, 0);
		expr->failable |= checked_expr->failable;
		constant &= checked_expr->constant;
		pure &= checked_expr->pure;
	}
	expr->pure = pure;
	expr->constant = constant;
	return success;
}

static inline bool sema_expr_analyse_cast(Context *context, Type *to, Expr *expr)
{
	Expr *inner = expr->cast_expr.expr;
	bool success = sema_resolve_type_info(context, expr->cast_expr.type_info);
	success &= sema_analyse_expr(context, NULL, inner);
	expr->pure = inner->pure;
	expr->constant = inner->constant;
	if (!success) return false;

	Type *target_type = expr->cast_expr.type_info->type;
	if (!cast_may_explicit(inner->type, target_type))
	{
		SEMA_ERROR(expr, "Cannot cast %s to %s.", type_quoted_error_string(inner->type), type_quoted_error_string(target_type));
		return false;
	}

	cast(inner, target_type);

	expr_replace(expr, inner);

	return true;
}

static inline bool sema_expr_analyse_slice_assign(Context *context, Expr *expr, Type *left_type, Expr *right, ExprFailableStatus lhs_is_failable)
{
	// 1. Evaluate right side to required type.
	if (!sema_analyse_expr_of_required_type(context, left_type->array.base, right, lhs_is_failable != FAILABLE_NO)) return false;

	Expr *left = expr->binary_expr.left;
	expr_copy_types(expr, right);
	expr->expr_kind = EXPR_SLICE_ASSIGN;
	expr->slice_assign_expr.left = left;
	expr->slice_assign_expr.right = right;

	return true;
}

bool sema_expr_analyse_assign_right_side(Context *context, Expr *expr, Type *left_type, Expr *right, ExprFailableStatus lhs_is_failable)
{
	if (expr && expr->binary_expr.left->expr_kind == EXPR_SLICE)
	{
		return sema_expr_analyse_slice_assign(context, expr, left_type, right, lhs_is_failable);
	}

	// 1. Evaluate right side to required type.
	if (!sema_analyse_expr(context, left_type, right)) return false;
	if (right->failable && lhs_is_failable != FAILABLE_YES)
	{
		if (lhs_is_failable == FAILABLE_UNWRAPPED)
		{
			SEMA_ERROR(expr->binary_expr.left, "The variable is unwrapped in this context, if you don't want to unwrap it, use () around the variable to suppress unwrapping, like 'catch err = (x)' and 'try (x)'.");
			return false;
		}
		if (!left_type) left_type = right->type;
		SEMA_ERROR(right, "'%s!' cannot be converted into '%s'.", type_to_error_string(right->type), type_to_error_string(left_type));
		return false;
	}

	// 2. Evaluate right hand side, making special concession for inferred arrays.
	do
	{
		if (!left_type) break;
		if (left_type->canonical->type_kind == TYPE_INFERRED_ARRAY && right->type->canonical->type_kind == TYPE_ARRAY)
		{
			if (left_type->canonical->array.base == right->type->canonical->array.base) break;
		}
		assert(right->type->canonical->type_kind != TYPE_INFERRED_ARRAY);
		if (!cast_implicit(right, left_type)) return false;
	} while (0);

	// 3. Set the result to the type on the right side.
	if (expr) expr_copy_types(expr, right);

	return true;
}
static inline bool sema_expr_begin_analyse(Expr *expr)
{
	switch (expr->resolve_status)
	{
		case RESOLVE_NOT_DONE:
			expr->resolve_status = RESOLVE_RUNNING;
			return true;
		case RESOLVE_RUNNING:
			SEMA_ERROR(expr, "Recursive resolution of expression");
			return expr_poison(expr);
		case RESOLVE_DONE:
			return false;
	}
	UNREACHABLE
}


static inline bool sema_expr_analyse_ct_identifier_lvalue(Context *context, Expr *expr)
{
	if (!sema_expr_begin_analyse(expr)) return expr_ok(expr);

	Decl *ambiguous_decl = NULL;
	Decl *private_symbol = NULL;
	DEBUG_LOG("Now resolving %s", TOKSTR(expr->ct_ident_expr.identifier));
	Decl *decl = sema_resolve_normal_symbol(context,
	                                 expr->ct_ident_expr.identifier,
	                                 NULL, false);

	// Skip if poisoned.
	if (!decl_ok(decl)) return false;

	if (!decl)
	{
		SEMA_ERROR(expr, "The compile time variable '%s' was not defined in this scope.", expr->ct_ident_expr.identifier);
		return expr_poison(expr);
	}

	if (decl->var.scope_depth < context->active_scope.depth)
	{
		SEMA_ERROR(expr, "Cannot modify '%s' inside of a runtime scope.", decl->name);
		return false;
	}
	expr->ct_ident_expr.decl = decl;
	expr->resolve_status = RESOLVE_DONE;
	return true;
}


static bool sema_expr_analyse_ct_identifier_assign(Context *context, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_expr_analyse_ct_identifier_lvalue(context, left)) return false;

	// 3. Evaluate right side to required type.
	if (!sema_expr_analyse_assign_right_side(context, expr, left->type, right, true)) return false;

	left->ct_ident_expr.decl->var.init_expr = right;
	expr_replace(expr, right);
	return true;
}

static bool sema_expr_analyse_ct_type_identifier_assign(Context *context, Expr *expr, Expr *left, Expr *right)
{
	TypeInfo *info = left->type_expr;

	if (info->kind != TYPE_INFO_IDENTIFIER || info->unresolved.path || TOKTYPE(info->unresolved.name_loc) != TOKEN_CT_TYPE_IDENT)
	{
		SEMA_ERROR(left, "A type cannot be assigned to.");
		return false;
	}

	TokenId token = info->unresolved.name_loc;

	if (!sema_analyse_expr_value(context, NULL, right)) return false;

	if (right->expr_kind != EXPR_TYPEINFO)
	{
		SEMA_ERROR(right, "Expected a type here.");
		return false;
	}

	Decl *decl = sema_resolve_normal_symbol(context, token, NULL, false);

	if (!decl)
	{
		decl = decl_new(DECL_VAR, token, VISIBLE_LOCAL);
		decl->var.kind = VARDECL_LOCAL_CT_TYPE;
		if (!sema_add_local(context, decl)) return false;
	}
	decl = sema_resolve_normal_symbol(context, token, NULL, true);

	decl->var.init_expr = right;

	expr->expr_kind = EXPR_NOP;
	expr->type = type_void;

	return true;
}

/**
 * Analyse a = b
 * @return true if analysis works
 */
static bool sema_expr_analyse_assign(Context *context, Expr *expr, Expr *left, Expr *right)
{
	expr->constant = false;
	expr->pure = false;


	// 1. Evaluate left side
	if (left->expr_kind == EXPR_CT_IDENT)
	{
		return sema_expr_analyse_ct_identifier_assign(context, expr, left, right);
	}

	if (left->expr_kind == EXPR_TYPEINFO)
	{
		return sema_expr_analyse_ct_type_identifier_assign(context, expr, left, right);
	}

	if (!sema_analyse_expr_value(context, NULL, left)) return false;

	if (left->expr_kind == EXPR_TRY)
	{
		if (expr_is_ltype(left->try_expr.expr))
		{
			SEMA_ERROR(left, "This part will be evaluated to a boolean, so if you want to test an assignment, "
							 "you need to use () around the assignment, like this: '%s (variable = expression)'.",
							 left->expr_kind == EXPR_TRY ? "try" : "catch");
			return false;
		}
	}

	// 2. Check assignability
	if (!expr_is_ltype(left))
	{
		SEMA_ERROR(left, "This expression is not assignable, did you make a mistake?");
		return false;
	}

	ExprFailableStatus failable_status = expr_is_failable(left);

	// 3. Evaluate right side to required type.
	if (!sema_expr_analyse_assign_right_side(context, expr, left->type, right, failable_status)) return false;

	if (failable_status == FAILABLE_UNWRAPPED && right->failable)
	{
		return sema_rewrap_var(context, left->identifier_expr.decl);
	}
	return true;
}


/**
 * Analyse define $foo = ...
 *
 * @return true if analysis worked.
 */
static bool sema_expr_analyse_ct_common_assign(Context *context, Expr *expr, Expr *left)
{

	// 1. Analyse left side.
	if (!sema_expr_analyse_ct_identifier_lvalue(context, left)) return false;

	Decl *left_var = left->ct_ident_expr.decl;

	Expr *left_value = left_var->var.init_expr;
	assert(left_value);
	assert(!left_value->failable);

	expr->binary_expr.left = left_value;

	expr->binary_expr.operator = binaryop_assign_base_op(expr->binary_expr.operator);

	if (!sema_expr_analyse_binary(context, NULL, expr)) return false;

	if (expr->expr_kind != EXPR_CONST)
	{
		SEMA_ERROR(expr->binary_expr.right, "Expected a constant expression.");
		return false;
	}

	left->ct_ident_expr.decl->var.init_expr = expr;

	return true;
}

/**
 * Analyse *= /= %= ^= |= &=
 *
 * @return true if analysis worked.
 */
static bool sema_expr_analyse_common_assign(Context *context, Expr *expr, Expr *left, Expr *right, bool int_only)
{
	expr->pure = false;
	expr->constant = false;

	if (left->expr_kind == EXPR_CT_IDENT)
	{
		return sema_expr_analyse_ct_common_assign(context, expr, left);
	}

	// 1. Analyse left side.
	if (!sema_analyse_expr_value(context, NULL, left)) return false;

	// 2. Verify that the left side is assignable.
	if (!expr_is_ltype(left))
	{
		SEMA_ERROR(left, "Expression is not assignable.");
		return false;
	}

	// 3. If this is only defined for ints (*%, ^= |= &= %=) verify that this is an int.
	if (int_only && !type_is_any_integer(left->type))
	{
		SEMA_ERROR(left, "Expected an integer here.");
		return false;
	}

	// 4. In any case, these ops are only defined on numbers.
	if (!type_is_numeric(left->type))
	{
		SEMA_ERROR(left, "Expected a numeric type here.");
		return false;
	}

	// 5. Cast the right hand side to the one on the left
	if (!sema_analyse_expr_of_required_type(context, left->type->canonical, right, expr_is_failable(left))) return false;

	// 6. Check for zero in case of div or mod.
	if (right->expr_kind == EXPR_CONST)
	{
		if (expr->binary_expr.operator == BINARYOP_DIV_ASSIGN)
		{
			switch (right->const_expr.kind)
			{
				case ALL_INTS:
					if (bigint_cmp_zero(&right->const_expr.i) == CMP_EQ)
					{
						SEMA_ERROR(right, "Division by zero not allowed.");
						return false;
					}
					break;
				case ALL_FLOATS:
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
		else if (expr->binary_expr.operator == BINARYOP_MOD_ASSIGN)
		{
			switch (right->const_expr.kind)
			{
				case ALL_INTS:
					if (bigint_cmp_zero(&right->const_expr.i) == CMP_EQ)
					{
						SEMA_ERROR(right, "% by zero not allowed.");
						return false;
					}
					break;
				default:
					UNREACHABLE
			}
		}
	}

	// 7. Assign type
	expr_copy_types(expr, left);
	return true;
}


/**
 * Handle a += b, a -= b
 * @return true if analysis succeeded.
 */
static bool sema_expr_analyse_add_sub_assign(Context *context, Expr *expr, Expr *left, Expr *right)
{
	if (left->expr_kind == EXPR_CT_IDENT)
	{
		return sema_expr_analyse_ct_common_assign(context, expr, left);
	}

	// 1. Analyse the left hand side
	if (!sema_analyse_expr(context, NULL, left)) return false;

	// 2. Ensure the left hand side is assignable
	if (!expr_is_ltype(left))
	{
		SEMA_ERROR(left, "Expression is not assignable.");
		return false;
	}

	Type *left_type_canonical = left->type->canonical;

	// 4. Attempt to analyse and cast this side to the same type if possible.
	if (!sema_analyse_expr(context, left->type, right)) return false;

	// 3. Copy type & set properties.
	expr_copy_types(expr, left);
	expr->pure = false;
	expr->constant = false;
	expr->failable = left->failable | right->failable;


	// 5. In the pointer case we have to treat this differently.
	if (left_type_canonical->type_kind == TYPE_POINTER)
	{

		// 6. Convert any compile time values to runtime
		if (!cast_implicitly_to_runtime(right)) return false;

		// 7. Finally, check that the right side is indeed an integer.
		if (!type_is_integer(right->type->canonical))
		{
			SEMA_ERROR(right, "The right side was '%s' but only integers are valid on the right side of %s when the left side is a pointer.",
			               type_to_error_string(right->type),
			               token_type_to_string(binaryop_to_token(expr->binary_expr.operator)));
			return false;
		}
		return true;
	}

	// 8. Otherwise we cast rhs to lhs
	if (!cast_implicit(right, left->type)) return false;

	// 9. We expect a numeric type on both left and right
	if (!type_is_numeric(left->type))
	{
		SEMA_ERROR(left, "Expected a numeric type here.");
		return false;
	}

	return true;
}

static Type *numeric_arithmetic_promotion(Type *type)
{
	if (!type) return NULL;
	switch (type->type_kind)
	{
		case ALL_SIGNED_INTS:
			if (type->builtin.bitsize < platform_target.width_c_int) return type_cint();
			return type;
		case ALL_UNSIGNED_INTS:
			if (type->builtin.bitsize < platform_target.width_c_int) return type_cuint();
			return type;
		case TYPE_F16:
			// Promote F16 to a real type.
			return type_float;
		default:
			return type;
	}
}

static bool binary_arithmetic_promotion(Context *context, Expr *left, Expr *right, Type *left_type, Type *right_type, Expr *parent, const char *error_message)
{

	Type *max = numeric_arithmetic_promotion(type_find_max_type(left_type, right_type));
	if (!max || !type_is_numeric(max))
	{
		if (!error_message)
		{
			return sema_type_error_on_binop(parent);
		}
		SEMA_ERROR(parent, error_message, type_quoted_error_string(left->type), type_quoted_error_string(right->type));
		return false;
	}
	return cast_implicit(left, max) & cast_implicit(right, max);
}

static bool sema_check_int_type_fit(Expr *expr, Type *target_type)
{
	if (!target_type) return true;
	Type *type = expr->type->canonical;
	if (!type_is_any_integer(target_type->canonical) || !type_is_any_integer(type)) return true;
	if (type_size(type) > type_size(target_type))
	{
		SEMA_ERROR(expr, "A '%s' cannot implicitly convert into '%s'.", type_to_error_string(expr->type), type_to_error_string(target_type));
		return false;
	}
	return true;
}

static void unify_voidptr(Expr *left, Expr *right, Type **left_type_ref, Type **right_type_ref)
{
	if (*left_type_ref == *right_type_ref) return;
	if (*left_type_ref == type_voidptr)
	{
		cast(left, *right_type_ref);
		*left_type_ref = *right_type_ref;
	}
	if (*right_type_ref == type_voidptr)
	{
		cast(right, *left_type_ref);
		*right_type_ref = *left_type_ref;
	}
}
/**
 * Analyse a - b
 * @return true if analysis succeeded
 */
static bool sema_expr_analyse_sub(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyse a and b.
	if (!sema_expr_analyse_binary_sub_expr(context, to, left, right)) return false;

	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;

	// 2. Handle the ptr - x and ptr - other_pointer
	if (left_type->type_kind == TYPE_POINTER)
	{
		// 3. ptr - other pointer
		if (right_type->type_kind == TYPE_POINTER)
		{
			// 3a. Require that both types are the same.
			unify_voidptr(left, right, &left_type, &right_type);
			if (left_type != right_type)
			{
				SEMA_ERROR(expr, "'%s' - '%s' is not allowed. Subtracting pointers of different types from each other is not possible.", type_to_error_string(left_type), type_to_error_string(right_type));
				return false;
			}

			// 3b. Set the type
			expr_set_type(expr, type_iptrdiff);

			// 3c. Set all other properties
			expr_unify_binary_properties(expr, left, right);

			// 3d. Otherwise check against assigned type so for example short x = &p - &q is an error.
			return sema_check_int_type_fit(expr, to);
		}

		right_type = right->type->canonical;

		// 4. Check that the right hand side is an integer.
		if (!type_is_any_integer(right_type))
		{
			SEMA_ERROR(expr, "Cannot subtract '%s' from '%s'", type_to_error_string(right_type), type_to_error_string(left_type));
			return false;
		}

		// 5. Make sure that the integer does not exceed iptrdiff in size.
		if (type_size(right_type) > type_size(type_iptrdiff))
		{
			SEMA_ERROR(expr, "Cannot subtract a '%s' from a pointer, please first cast it to '%s'.", type_to_error_string(right_type), type_to_error_string(type_iptrdiff));
			return false;
		}

		// 6. Convert to iptrdiff
		if (!cast_implicit(right, type_iptrdiff)) return true;

		// 7. Assign the type of the left side.
		expr_copy_types(expr, left);
		expr_unify_binary_properties(expr, left, right);

		return true;
	}

	// 7. Attempt arithmetic promotion, to promote both to a common type.
	if (!binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, "The subtraction %s - %s is not possible."))
	{
		return false;
	}

	left_type = left->type->canonical;

	expr_unify_binary(expr, left, right);

	// 8. Handle constant folding.
	if (both_const(left, right))
	{
		expr_replace(expr, left);
		switch (left_type->type_kind)
		{
			case ALL_INTS:
				bigint_sub(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
				break;
			case ALL_FLOATS:
				expr->const_expr.f = left->const_expr.f - right->const_expr.f;
				break;
			default:
				UNREACHABLE
		}
	}

	return true;

}

/**
 * Analyse a + b
 * @return true if it succeeds.
 */
static bool sema_expr_analyse_add(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	// 1. Promote everything to the recipient type – if possible
	//    this is safe in the pointer case actually.
	if (!sema_expr_analyse_binary_sub_expr(context, to, left, right)) return false;

	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;

	// 2. To detect pointer additions, reorder if needed
	if (right_type->type_kind == TYPE_POINTER && left_type->type_kind != TYPE_POINTER)
	{
		Expr *temp = right;
		right = left;
		left = temp;
		right_type = left_type;
		left_type = left->type->canonical;
		expr->binary_expr.left = left;
		expr->binary_expr.right = right;
	}

	// 3. The "left" will now always be the pointer.
	//    so check if we want to do the normal pointer add special handling.
	if (left_type->type_kind == TYPE_POINTER)
	{
		// 3a. Check that the other side is an integer of some sort.
		if (!type_is_any_integer(right_type))
		{
			SEMA_ERROR(right, "A value of type '%s' cannot be added to '%s', an integer was expected here.",
			               type_to_error_string(right->type),
			               type_to_error_string(left->type));
			return false;
		}

		// 3b. Cast it to usize or isize depending on underlying type.
		//     Either is fine, but it looks a bit nicer if we actually do this and keep the sign.
		bool success = cast_implicit(right, type_iptrdiff);

		// No need to check the cast we just ensured it was an integer.
		assert(success && "This should always work");
		(void)success;

		// 3c. Set the type and other properties.
		expr_copy_types(expr, left);
		expr_unify_binary_properties(expr, left, right);
		return true;
	}

	// 4. Do an binary arithmetic promotion
	if (!binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, "Cannot do the addition %s + %s."))
	{
		return false;
	}

	// 5. Handle the "both const" case. We should only see ints and floats at this point.
	if (both_const(left, right))
	{
		expr_replace(expr, left);
		switch (left->const_expr.kind)
		{
			case ALL_INTS:
				bigint_add(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
				break;
			case ALL_FLOATS:
				expr->const_expr.f = left->const_expr.f + right->const_expr.f;
				break;
			default:
				UNREACHABLE
		}
	}


	// 6. Set the type & other properties.
	expr_unify_binary(expr, left, right);

	return true;

}

/**
 * Analyse a * b
 *
 * Will analyse a and b and then use arithmetic promotion on each.
 * It will then try to promote both to a common type,
 *
 * @return true if analysis worked.
 */
static bool sema_expr_analyse_mult(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{

	// 1. Analyse the sub expressions.
	if (!sema_expr_analyse_binary_sub_expr(context, to, left, right)) return false;

	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;

	// 2. Perform promotion to a common type.
	if (!binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, "Cannot multiply %s by %s"))
	{
		return false;
	}

	// 3. Handle constant folding.
	if (both_const(left, right))
	{
		expr_replace(expr, left);

		switch (left->const_expr.kind)
		{
			case ALL_INTS:
				bigint_mul(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
				break;
			case ALL_FLOATS:
				expr->const_expr.f = left->const_expr.f * right->const_expr.f;
				break;
			default:
				UNREACHABLE
		}
	}

	// 4. Update failable / pure / type etc.
	expr_unify_binary(expr, left, right);

	return true;
}

/**
 * Analyse a / b
 * @return true if analysis completed ok.
 */
static bool sema_expr_analyse_div(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyse sub expressions.
	if (!sema_expr_analyse_binary_sub_expr(context, to, left, right)) return false;

	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;

	// 2. Perform promotion to a common type.
	if (!binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, "Cannot divide %s by %s."))
	{
		return false;
	}

	// 3. Check for a constant 0 on the right hand side.
	if (is_const(right))
	{
		switch (right->const_expr.kind)
		{
			case ALL_INTS:
				if (bigint_cmp_zero(&right->const_expr.i) == CMP_EQ)
				{
					SEMA_ERROR(right, "This expression evaluates to zero and division by zero is not allowed.");
					return false;
				}
				break;
			case ALL_FLOATS:
				break;
			default:
				UNREACHABLE
		}
	}

	// 4. Perform constant folding.
	if (both_const(left, right))
	{
		expr_replace(expr, left);
		switch (left->const_expr.kind)
		{
			case ALL_INTS:
				bigint_div_floor(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
				break;
			case ALL_FLOATS:
				expr->const_expr.f = left->const_expr.f / right->const_expr.f;
				break;
			default:
				UNREACHABLE
		}
	}

	// 5. Done.
	expr_unify_binary(expr, left, right);

	return true;

}

/**
 * Analyse a % b
 * @return true if analysis succeeds.
 */
static bool sema_expr_analyse_mod(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyse both sides.
	if (!sema_expr_analyse_binary_sub_expr(context, to, left, right)) return false;

	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;

	// 2. Perform promotion to a common type.
	if (!binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, "Cannot divide %s by %s."))
	{
		return false;
	}

	// 3. a % 0 is not valid, so detect it.
	if (is_const(right) && bigint_cmp_zero(&right->const_expr.i) == CMP_EQ)
	{
		SEMA_ERROR(expr->binary_expr.right, "Cannot perform %% with a constant zero.");
		return false;
	}

	// 4. Constant fold
	if (both_const(left, right))
	{
		expr_replace(expr, left);
		// 4a. Remember this is remainder.
		bigint_rem(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
	}

	expr_unify_binary(expr, left, right);

	return true;
}

/**
 * Analyse a ^ b, a | b, a & b
 * @return true if the analysis succeeded.
 */
static bool sema_expr_analyse_bit(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	// 1. Convert to top down type if possible.
	if (!sema_expr_analyse_binary_sub_expr(context, to, left, right)) return false;

	// 2. Check that both are integers.
	if (!both_any_integer(left, right))
	{
		return sema_type_error_on_binop(expr);
	}

	// 3. Promote to the same type.

	Type *left_type = left->type->canonical;
	Type *right_type = right->type->canonical;

	if (!binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, NULL))
	{
		return false;
	}

	// 4. Do constant folding if both sides are constant.
	if (both_const(left, right))
	{
		expr_replace(expr, left);
		switch (expr->binary_expr.operator)
		{
			case BINARYOP_BIT_AND:
				bigint_and(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
				break;
			case BINARYOP_BIT_XOR:
				bigint_xor(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
				break;
			case BINARYOP_BIT_OR:
				bigint_or(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
				break;
			default:
				UNREACHABLE;
		}
	}

	// 5. Assign the type
	expr_unify_binary(expr, left, right);
	return true;
}

/**
 * Analyse >> and << operations.
 * @return true if the analysis succeeded.
 */
static bool sema_expr_analyse_shift(Context *context, Type *to, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyze lhs with target type.
	if (!sema_analyse_expr(context, to, left)) return false;

	// 2. Analyse rhs without target type.
	if (!sema_analyse_expr(context, NULL, right)) return false;

	// 3. Only integers may be shifted.
	if (!both_any_integer(left, right))
	{
		return sema_type_error_on_binop(expr);
	}

	// 4. Promote lhs using the usual numeric promotion.
	if (!cast_implicit(left, numeric_arithmetic_promotion(left->type))) return false;

	expr->pure = left->pure & right->pure;
	expr->constant = left->constant & right->constant;
	expr->failable = left->failable | right->failable;

	// 5. For a constant right hand side we will make a series of checks.
	if (is_const(right))
	{
		// 5a. Make sure the value does not exceed the bitsize of
		//     the left hand side. We ignore this check for lhs being a constant.
		if (left->type->canonical->type_kind != TYPE_IXX)
		{
			BigInt bitsize;
			bigint_init_unsigned(&bitsize, left->type->canonical->builtin.bitsize);
			if (bigint_cmp(&right->const_expr.i, &bitsize) == CMP_GT)
			{
				SEMA_ERROR(right, "The shift exceeds bitsize of '%s'.", type_to_error_string(left->type));
				return false;
			}
		}
		// 5b. Make sure that the right hand side is positive.
		if (bigint_cmp_zero(&right->const_expr.i) == CMP_LT)
		{
			SEMA_ERROR(right, "A shift must be a positive number.");
			return false;
		}

		// 6. Fold constant expressions.
		if (is_const(left))
		{
			// 6a. For >> this is always an arithmetic shift.
			if (expr->binary_expr.operator == BINARYOP_SHR)
			{
				expr_replace(expr, left);
				bigint_shr(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
				return true;
			}
			// 6b. The << case needs to behave differently for bigints and fixed bit integers.
			expr_replace(expr, left);
			if (left->const_expr.kind == TYPE_IXX)
			{
				bigint_shl(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i);
			}
			else
			{
				int bit_count = left->type->canonical->builtin.bitsize;
				bool is_signed = !type_kind_is_unsigned(left->const_expr.kind);
				bigint_shl_trunc(&expr->const_expr.i, &left->const_expr.i, &right->const_expr.i, bit_count, is_signed);
			}
			return true;
		}
	}

	// 7. In the case of 2 >> x, we will want to cast to the target type if available, otherwise we use a ct->runtime promotion.
	if (type_is_ct(left->type))
	{
		if (to)
		{
			// 7a. The target type exists, convert to this type after arithmetic promotion.
			if (!cast_implicit(left, numeric_arithmetic_promotion(to))) return false;
			left->original_type = to;
		}
		else
		{
			// 7b. Otherwise, use runtime promotion.
			if (!cast_implicitly_to_runtime(left)) return false;
		}
	}

	expr_copy_types(expr, left);

	return true;
}

/**
 * Analyse a <<= b a >>= b
 * @return true is the analysis succeeds, false otherwise.
 */
static bool sema_expr_analyse_shift_assign(Context *context, Expr *expr, Expr *left, Expr *right)
{

	// 1. Analyze the two sub lhs & rhs *without coercion*
	if (!sema_expr_analyse_binary_sub_expr(context, NULL, left, right)) return false;

	expr->pure = false;
	expr->constant = false;
	expr->failable = left->failable | right->failable;

	// 2. Ensure the left hand side is assignable
	if (!expr_is_ltype(left))
	{
		SEMA_ERROR(left, "Expression is not assignable.");
		return false;
	}

	// 3. Only integers may be shifted.
	if (!both_any_integer(left, right)) return sema_type_error_on_binop(expr);

	// 4. For a constant right hand side we will make a series of checks.
	if (is_const(right))
	{
		// 4a. Make sure the value does not exceed the bitsize of
		//     the left hand side.
		BigInt bitsize;
		bigint_init_unsigned(&bitsize, left->type->canonical->builtin.bitsize);
		if (bigint_cmp(&right->const_expr.i, &bitsize) == CMP_GT)
		{
			SEMA_ERROR(right, "The shift exceeds bitsize of '%s'.", type_to_error_string(left->type));
			return false;
		}

		// 4b. Make sure that the right hand side is positive.
		if (bigint_cmp_zero(&right->const_expr.i) == CMP_LT)
		{
			SEMA_ERROR(right, "A shift must be a positive number.");
			return false;
		}
	}

	// 5. Set the type using the left hand side.
	expr_copy_types(expr, left);

	return true;
}


static bool sema_expr_analyse_and(Context *context, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_analyse_expr(context, type_bool, left) || !sema_analyse_expr(context, type_bool, right)) return false;
	if (!cast_implicit(left, type_bool) || !cast_implicit(right, type_bool)) return false;

	expr_set_type(expr, type_bool);
	if (both_const(left, right))
	{
		expr_replace(expr, left);
		expr->const_expr.b &= right->const_expr.b;
	}
	expr_unify_binary_properties(expr, left, right);
	return true;
}

static bool sema_expr_analyse_or(Context *context, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_expr_analyse_binary_sub_expr(context, NULL, left, right)) return false;
	if (!cast_implicit(left, type_bool) || !cast_implicit(right, type_bool)) return false;

	if (both_const(left, right))
	{
		expr_replace(expr, left);
		expr->const_expr.b |= right->const_expr.b;
	}
	expr_unify_binary_properties(expr, left, right);
	expr_set_type(expr, type_bool);
	return true;
}



static void cast_to_max_bit_size(Context *context, Expr *left, Expr *right, Type *left_type, Type *right_type)
{
	int bit_size_left = left_type->builtin.bitsize;
	int bit_size_right = right_type->builtin.bitsize;
	assert(bit_size_left && bit_size_right);
	if (bit_size_left == bit_size_right) return;
	if (bit_size_left < bit_size_right)
	{
		Type *to = left->type->type_kind < TYPE_U8
				? type_int_signed_by_bitsize(bit_size_right)
				: type_int_unsigned_by_bitsize(bit_size_right);
		bool success = cast_implicit(left, to);
		assert(success);
		return;
	}
	Type *to = right->type->type_kind < TYPE_U8
	           ? type_int_signed_by_bitsize(bit_size_right)
	           : type_int_unsigned_by_bitsize(bit_size_right);
	bool success = cast_implicit(right, to);
	assert(success);
}

/**
 * Analyze a == b, a != b, a > b, a < b, a >= b, a <= b
 * @return
 */
static bool sema_expr_analyse_comp(Context *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyse left and right side without any conversions.
	if (!sema_expr_analyse_binary_sub_expr(context, NULL, left, right)) return false;

	bool is_equality_type_op = expr->binary_expr.operator == BINARYOP_NE || expr->binary_expr.operator == BINARYOP_EQ;

	Type *left_type = type_flatten(left->type);
	Type *right_type = type_flatten(right->type);

	// 2. Handle the case of signed comparisons.
	//    This happens when either side has a definite integer type
	//    and those are either signed or unsigned.
	//    If either side is compint, then this does not happen.
	if ((type_is_unsigned(left_type) && type_is_signed(right_type))
		|| (type_is_signed(left_type) && type_is_unsigned(right_type)))
	{
		// 2a. Resize so that both sides have the same bit width. This will always work.
		cast_to_max_bit_size(context, left, right, left_type, right_type);
	}
	else
	{
		// 3. In the normal case, treat this as a binary op, finding the max type.
		Type *max = type_find_max_type(left->type->canonical, right->type->canonical);

		// 4. If no common type, then that's an error:
		if (!max)
		{
			SEMA_ERROR(expr, "'%s' and '%s' are different types and cannot be compared.",
			               type_to_error_string(left->type), type_to_error_string(right->type));
		};

		// 5. Most types can do equality, but not all can do comparison,
		//    so we need to check that as well.
		if (!is_equality_type_op)
		{
			Type *effective_type = max;
			if (max->type_kind == TYPE_DISTINCT)
			{
				effective_type = max->decl->distinct_decl.base_type;
			}
			switch (effective_type->type_kind)
			{
				case TYPE_POISONED:
					return false;
				case TYPE_VOID:
				case TYPE_TYPEINFO:
				case TYPE_TYPEDEF:
				case TYPE_DISTINCT:
				case TYPE_INFERRED_ARRAY:
					UNREACHABLE
				case TYPE_BOOL:
				case TYPE_ENUM:
				case TYPE_ERRTYPE:
				case TYPE_FUNC:
				case TYPE_STRUCT:
				case TYPE_UNION:
				case TYPE_ERR_UNION:
				case TYPE_STRLIT:
				case TYPE_ARRAY:
				case TYPE_SUBARRAY:
				case TYPE_TYPEID:
				case TYPE_VIRTUAL_ANY:
				case TYPE_VIRTUAL:
					// Only != and == allowed.
					goto ERR;
				case ALL_INTS:
				case ALL_FLOATS:
					// All comparisons allowed
					break;
				case TYPE_POINTER:
					// Only comparisons between the same type is allowed. Subtypes not allowed.
					if (left_type != right_type && left_type != type_voidptr && right_type != type_voidptr)
					{
						SEMA_ERROR(expr, "Cannot compare pointers of different types.");
						return false;
					}
					break;
				case TYPE_VECTOR:
					TODO
			}
		}

		// 6. Do the implicit cast.
		if (!cast_implicit(left, max)) goto ERR;
		if (!cast_implicit(right, max)) goto ERR;
	}

	// 7. Do constant folding.
	if (both_const(left, right))
	{
		expr->const_expr.b = expr_const_compare(&left->const_expr, &right->const_expr, expr->binary_expr.operator);
		expr->const_expr.kind = TYPE_BOOL;
		expr->expr_kind = EXPR_CONST;
	}

	// 8. Set the type to bool
	expr_unify_binary_properties(expr, left, right);
	expr_set_type(expr, type_bool);
	return true;

	ERR:
	SEMA_ERROR(expr, "Cannot evaluate '%s' %s '%s'", type_to_error_string(left_type), token_type_to_string(binaryop_to_token(expr->binary_expr.operator)), type_to_error_string(right_type));
	return false;
}

/**
 * Analyse *a
 * @return true if analysis succeeds.
 */
static bool sema_expr_analyse_deref(Context *context, Expr *expr, Expr *inner)
{
	Type *canonical = inner->type->canonical;
	// 1. Check that we have a pointer, or dereference is not allowed.
	if (canonical->type_kind != TYPE_POINTER)
	{
		SEMA_ERROR(inner, "Cannot dereference a value of type '%s'", type_to_error_string(inner->type));
		return false;
	}
	// 2. This could be a constant, in which case it is a null which is an error.
	if (inner->expr_kind == EXPR_CONST)
	{
		SEMA_ERROR(inner, "Dereferencing null is not allowed.");
		return false;
	}
	// 3. Now the type might not be a pointer because of a typedef,
	//    otherwise we need to use the the canonical representation.
	Type *deref_type = inner->type->type_kind != TYPE_POINTER ? inner->type : canonical;

	// 4. And... set the type.
	expr_set_type(expr, deref_type->pointer);
	expr_copy_properties(expr, inner);

	return true;
}

static inline bool sema_take_addr_of_var(Expr *expr, Decl *decl, bool *is_constant)
{
	if (decl->decl_kind != DECL_VAR) return false;
	*is_constant = false;
	switch (decl->var.kind)
	{
		case VARDECL_GLOBAL:
			*is_constant = true;
			return true;
		case VARDECL_LOCAL:
		case VARDECL_PARAM:
		case VARDECL_PARAM_REF:
			return true;
		case VARDECL_CONST:
			*is_constant = true;
			if (!decl->var.type_info)
			{
				SEMA_ERROR(expr, "The constant is not typed, either type it or use && to take the reference to a temporary.");
				SEMA_PREV(decl, "The constant was defined here.");
				return false;
			}
			return true;
		case VARDECL_PARAM_EXPR:
			SEMA_ERROR(expr, "It is not possible to take the address of a captured expression, but you can use && to take a reference to the temporary value.");
			return false;
		case VARDECL_PARAM_CT:
		case VARDECL_PARAM_CT_TYPE:
		case VARDECL_LOCAL_CT_TYPE:
		case VARDECL_LOCAL_CT:
			// May not be reached due to EXPR_CT_IDENT being handled elsewhere.
			UNREACHABLE;
		case VARDECL_MEMBER:
		case VARDECL_ALIAS:
			UNREACHABLE
	}
	UNREACHABLE
}

static inline bool sema_take_addr_of_ident(Expr *inner, bool *is_constant)
{
	Decl *decl = inner->identifier_expr.decl;
	decl = decl_raw(decl);
	switch (decl->decl_kind)
	{
		case DECL_ENUM_CONSTANT:
		case DECL_FUNC:
			*is_constant = true;
			return true;
		case DECL_VAR:
			return sema_take_addr_of_var(inner, decl, is_constant);
		default:
			SEMA_ERROR(inner, "It is not possible to take the address of a '%s'.", type_to_error_string(inner->type));
			return false;
	}
}

static bool sema_take_addr_of(Expr *inner, bool *is_constant)
{
	switch (inner->expr_kind)
	{
		case EXPR_CT_IDENT:
			SEMA_ERROR(inner, "It's not possible to take the address of a compile time value.");
			return false;
		case EXPR_MACRO_EXPANSION:
			SEMA_ERROR(inner, "It's not possible to take the address of a macro.");
			return false;
		case EXPR_IDENTIFIER:
		case EXPR_CONST_IDENTIFIER:
			return sema_take_addr_of_ident(inner, is_constant);
		case EXPR_UNARY:
			*is_constant = inner->constant;
			if (inner->unary_expr.operator == UNARYOP_DEREF) return true;
			break;
		case EXPR_ACCESS:
			return sema_take_addr_of(inner->access_expr.parent, is_constant);
		case EXPR_GROUP:
			return sema_take_addr_of(inner->group_expr, is_constant);
		case EXPR_SUBSCRIPT:
		{
			bool index_was_const = false;
			if (!sema_take_addr_of(inner->subscript_expr.expr, &index_was_const)) return false;
			*is_constant = index_was_const && inner->constant;
			return true;
		}
		case EXPR_SLICE:
		{
			*is_constant = false;
			bool dummy;
			return sema_take_addr_of(inner->slice_expr.expr, &dummy);
		}
		default:
			break;
	}
	SEMA_ERROR(inner, "To take the address of a temporary value, use '&&' instead of '&'.", type_to_error_string(inner->type));
	return false;
}

/**
 * Analyse &a
 * @return true if analysis succeeds.
 */
static bool sema_expr_analyse_addr(Context *context, Type *to, Expr *expr, Expr *inner)
{
	// 1. Evaluate the expression
	REDO:
	switch (inner->expr_kind)
	{
		case EXPR_POISONED:
			return false;
		case EXPR_GROUP:
			// We want to collapse any grouping here.
			expr_replace(inner, inner->group_expr);
			goto REDO;
		default:
			if (!sema_analyse_expr_value(context, NULL, inner)) return expr_poison(expr);
	}

	// 2. Take the address.
	bool is_constant = false;
	if (!sema_take_addr_of(inner, &is_constant)) return expr_poison(expr);

	// 3. Get the pointer of the underlying type.
	expr_set_type(expr, type_get_ptr(inner->type));
	expr->constant = is_constant;
	expr->pure = inner->pure;
	expr->failable = inner->failable;

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
	expr_copy_properties(expr, inner);

	if (inner->expr_kind != EXPR_CONST)
	{
		expr_copy_types(expr, inner);
		return true;
	}
	expr_replace(expr, inner);

	switch (expr->const_expr.kind)
	{
		case ALL_INTS:
			bigint_negate(&expr->const_expr.i, &inner->const_expr.i);
			if (expr_const_int_overflowed(&expr->const_expr))
			{
				SEMA_ERROR(expr, "%s does not fit in '%s'.", expr_const_to_error_string(&expr->const_expr), type_to_error_string(expr->type));
				return false;
			}
			return true;
		case ALL_FLOATS:
			expr->const_expr.f = -expr->const_expr.f;
			break;
		default:
			UNREACHABLE
	}
	return true;
}

/**
 * Analyse ~x and ~123
 *
 * @return
 */
static bool sema_expr_analyse_bit_not(Context *context, Type *to, Expr *expr, Expr *inner)
{
	expr_copy_properties(expr, inner);

	Type *canonical = inner->type->canonical;
	if (canonical->type_kind == TYPE_IXX)
	{
		SEMA_ERROR(expr, "Cannot bit negate an untyped integer literal, please first cast it to a concrete type.", type_to_error_string(inner->type));
		return false;
	}
	if (!type_is_any_integer(canonical) && canonical != type_bool)
	{
		SEMA_ERROR(expr, "Cannot bit negate '%s'.", type_to_error_string(inner->type));
		return false;
	}
	// The simple case, non-const.
	if (inner->expr_kind != EXPR_CONST)
	{
		expr_copy_types(expr, inner);
		return true;
	}

	expr_replace(expr, inner);
	switch (expr->const_expr.kind)
	{
		case ALL_SIGNED_INTS:
			bigint_not(&expr->const_expr.i, &inner->const_expr.i, canonical->builtin.bitsize, true);
			break;
		case ALL_UNSIGNED_INTS:
			bigint_not(&expr->const_expr.i, &inner->const_expr.i, canonical->builtin.bitsize, false);
			break;
		case TYPE_BOOL:
			expr->const_expr.b = !expr->const_expr.b;
			break;
		case TYPE_IXX:
		default:
			UNREACHABLE
	}
	return true;
}

static bool sema_expr_analyse_not(Expr *expr, Expr *inner)
{
	expr_copy_properties(expr, inner);
	expr_set_type(expr, type_bool);

	if (inner->expr_kind == EXPR_CONST)
	{
		switch (expr->const_expr.kind)
		{
			case ALL_INTS:
				expr->const_expr.b = bigint_cmp_zero(&inner->const_expr.i) == CMP_EQ;
				break;
			case TYPE_BOOL:
				expr->const_expr.b = !inner->const_expr.b;
				break;
			case ALL_FLOATS:
				expr->const_expr.b = inner->const_expr.f == 0.0;
				break;
			case TYPE_STRLIT:
				expr->const_expr.b = !inner->const_expr.string.len;
				break;
			case TYPE_ERRTYPE:
			case TYPE_ENUM:
				TODO
			default:
				UNREACHABLE
		}
		expr->const_expr.kind = TYPE_BOOL;
		expr->expr_kind = EXPR_CONST;
		return true;
	}
	Type *canonical = inner->type->canonical;
	switch (type_flatten(canonical)->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_IXX:
		case TYPE_FXX:
		case TYPE_TYPEDEF:
		case TYPE_ERR_UNION:
		case TYPE_DISTINCT:
		case TYPE_INFERRED_ARRAY:
			UNREACHABLE
		case TYPE_FUNC:
		case TYPE_ARRAY:
		case TYPE_POINTER:
		case TYPE_VIRTUAL:
		case TYPE_VIRTUAL_ANY:
		case TYPE_SUBARRAY:
		case TYPE_BOOL:
		case TYPE_VECTOR:
		case ALL_REAL_FLOATS:
		case ALL_UNSIGNED_INTS:
		case ALL_SIGNED_INTS:
			return true;
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_VOID:
		case TYPE_STRLIT:
		case TYPE_ENUM:
		case TYPE_ERRTYPE:
		case TYPE_TYPEID:
		case TYPE_TYPEINFO:
			SEMA_ERROR(expr, "Cannot use 'not' on %s", type_to_error_string(inner->type));
			return false;
	}
	UNREACHABLE
}

static inline bool sema_expr_analyse_ct_incdec(Context *context, Expr *expr, Expr *inner)
{
	assert(inner->expr_kind == EXPR_CT_IDENT);

	if (!sema_expr_analyse_ct_identifier_lvalue(context, inner)) return false;

	Decl *var = inner->ct_ident_expr.decl;
	Expr *start_value = var->var.init_expr;
	assert(start_value->expr_kind == EXPR_CONST);

	switch (start_value->const_expr.kind)
	{
		case ALL_INTS:
			break;
		default:
			SEMA_ERROR(expr, "The compile time variable '%s' does not hold an integer.", var->name);
			return false;
	}

	Expr *end_value = expr_copy(start_value);

	// Make the change.
	BigInt change;
	bigint_init_signed(&change, expr->unary_expr.operator == UNARYOP_DEC ? -1 : 1);
	bigint_add(&end_value->const_expr.i, &start_value->const_expr.i, &change);
	if (!expr_const_int_valid(end_value, start_value->type)) return false;

	var->var.init_expr = end_value;

	if (expr->expr_kind == EXPR_POST_UNARY)
	{
		expr_replace(expr, start_value);
	}
	else
	{
		expr_replace(expr, end_value);
	}
	return true;
}

/**
 * Analyse foo++ foo-- --foo ++foo
 * @return false if analysis fails.
 */
static inline bool sema_expr_analyse_incdec(Context *context, Expr *expr, Expr *inner)
{
	expr->constant = false;
	expr->pure = false;
	expr->failable = inner->failable;

	if (!expr_is_ltype(inner))
	{
		SEMA_ERROR(inner, "Expression cannot be assigned to.");
		return false;
	}
	if (inner->expr_kind == EXPR_CT_IDENT)
	{
		return sema_expr_analyse_ct_incdec(context, expr, inner);
	}

	Type *type = type_flatten(inner->type);

	if (!type_is_numeric(type) && type->type_kind != TYPE_POINTER)
	{
		SEMA_ERROR(inner, "Expression must be a number or a pointer.");
		return false;
	}
	expr_copy_types(expr, inner);
	return true;
}

static inline bool sema_expr_analyse_taddr(Context *context, Type *to, Expr *expr, Expr *inner)
{
	Type *inferred_type = NULL;
	if (to->type_kind == TYPE_POINTER)
	{
		inferred_type = to->pointer;
	}

	if (!sema_analyse_expr(context, inferred_type, inner)) return false;

	expr_copy_properties(expr, inner);
	expr_set_type(expr, type_get_ptr(inner->type));
	return true;
}

static bool unclear_op_precedence(Expr *left_side, Expr * main_expr, Expr *right_side)
{
	int precedence_main = BINOP_PREC_REQ[main_expr->binary_expr.operator];
	if (left_side->expr_kind == EXPR_BINARY)
	{
		int precedence_left = BINOP_PREC_REQ[left_side->binary_expr.operator];
		return precedence_left && (precedence_left == precedence_main);
	}
	if (right_side->expr_kind == EXPR_BINARY)
	{
		int precedence_right = BINOP_PREC_REQ[right_side->binary_expr.operator];
		return precedence_right && (precedence_right == precedence_main);
	}
	return false;
}

static inline bool sema_expr_analyse_binary(Context *context, Type *to, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_RUNNING);
	Expr *left = expr->binary_expr.left;
	Expr *right = expr->binary_expr.right;
	// check if both sides have a binary operation where the precedence is unclear. Example: a ^ b | c
	if (unclear_op_precedence(left, expr, right))
	{
		SEMA_ERROR(expr, "You need to add explicit parentheses to clarify precedence.");
		return expr_poison(expr);
	}
	// Don't push down bool conversions for example.
	if (to && !type_is_numeric(to)) to = NULL;
	switch (expr->binary_expr.operator)
	{
		case BINARYOP_ASSIGN:
			return sema_expr_analyse_assign(context, expr, left, right);
		case BINARYOP_MULT:
			return sema_expr_analyse_mult(context, to, expr, left, right);
		case BINARYOP_ADD:
			return sema_expr_analyse_add(context, to, expr, left, right);
		case BINARYOP_ADD_ASSIGN:
		case BINARYOP_SUB_ASSIGN:
			return sema_expr_analyse_add_sub_assign(context, expr, left, right);
		case BINARYOP_SUB:
			return sema_expr_analyse_sub(context, to, expr, left, right);
		case BINARYOP_DIV:
			return sema_expr_analyse_div(context, to, expr, left, right);
		case BINARYOP_MULT_ASSIGN:
		case BINARYOP_DIV_ASSIGN:
			return sema_expr_analyse_common_assign(context, expr, left, right, false);
		case BINARYOP_BIT_AND_ASSIGN:
		case BINARYOP_BIT_OR_ASSIGN:
		case BINARYOP_BIT_XOR_ASSIGN:
		case BINARYOP_MOD_ASSIGN:
			return sema_expr_analyse_common_assign(context, expr, left, right, true);
		case BINARYOP_MOD:
			return sema_expr_analyse_mod(context, to, expr, left, right);
		case BINARYOP_AND:
			return sema_expr_analyse_and(context, expr, left, right);
		case BINARYOP_OR:
			return sema_expr_analyse_or(context, expr, left, right);
		case BINARYOP_BIT_OR:
		case BINARYOP_BIT_XOR:
		case BINARYOP_BIT_AND:
			return sema_expr_analyse_bit(context, to, expr, left, right);
		case BINARYOP_NE:
		case BINARYOP_EQ:
		case BINARYOP_GT:
		case BINARYOP_GE:
		case BINARYOP_LT:
		case BINARYOP_LE:
			return sema_expr_analyse_comp(context, expr, left, right);
		case BINARYOP_SHR:
		case BINARYOP_SHL:
			return sema_expr_analyse_shift(context, to, expr, left, right);
		case BINARYOP_SHR_ASSIGN:
		case BINARYOP_SHL_ASSIGN:
			return sema_expr_analyse_shift_assign(context, expr, left, right);
		case BINARYOP_ERROR:
			break;
	}
	UNREACHABLE
}

static inline bool sema_expr_analyse_unary(Context *context, Type *to, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_RUNNING);
	Expr *inner = expr->unary_expr.expr;
	switch (expr->unary_expr.operator)
	{
		case UNARYOP_DEREF:
			if (!sema_analyse_expr(context, NULL, inner)) return false;
			return sema_expr_analyse_deref(context, expr, inner);
		case UNARYOP_ADDR:
			return sema_expr_analyse_addr(context, to, expr, inner);
		case UNARYOP_NEG:
			if (!sema_analyse_expr(context, to, inner)) return false;
			return sema_expr_analyse_neg(context, to, expr, inner);
		case UNARYOP_BITNEG:
			if (!sema_analyse_expr(context, to, inner)) return false;
			return sema_expr_analyse_bit_not(context, to, expr, inner);
		case UNARYOP_NOT:
			if (!sema_analyse_expr(context, NULL, inner)) return false;
			return sema_expr_analyse_not(expr, inner);
		case UNARYOP_DEC:
		case UNARYOP_INC:
			if (!sema_analyse_expr_value(context, NULL, inner)) return false;
			return sema_expr_analyse_incdec(context, expr, inner);
		case UNARYOP_TADDR:
			return sema_expr_analyse_taddr(context, to, expr, inner);
		case UNARYOP_ERROR:
			return false;
	}
	UNREACHABLE
}

static inline bool sema_expr_analyse_post_unary(Context *context, Type *to, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_RUNNING);
	Expr *inner = expr->post_expr.expr;

	if (!sema_analyse_expr_value(context, NULL, inner)) return false;

	return sema_expr_analyse_incdec(context, expr, inner);
}


static inline bool sema_expr_analyse_try_old(Context *context, Expr *expr)
{
	Expr *inner = expr->trycatch_expr;
	bool success = sema_analyse_expr(context, NULL, inner);
	expr->pure = inner->pure;
	expr->constant = false;
	if (!success) return false;
	if (!inner->failable)
	{
		SEMA_ERROR(expr->trycatch_expr, "Expected a failable expression to 'try'.");
		return false;
	}
	expr_set_type(expr, type_bool);
	return true;
}

static inline bool sema_expr_analyse_try_assign(Context *context, Expr *expr, bool implicitly_create_variable)
{
	expr->constant = false;

	Expr *init = expr->try_assign_expr.init;
	Expr *lhs = expr->try_assign_expr.expr;

	Type *lhs_type;

	// If we have:
	// a. In a context where implicit declaration of variables occur
	// b. The LHS is an identifier without PATH
	// c. And this variable does not exist in the scope.
	// Create it (when we know the type)
	bool create_variable;
	if (implicitly_create_variable && lhs->expr_kind == EXPR_IDENTIFIER && !lhs->identifier_expr.path
		&& !sema_resolve_normal_symbol(context, lhs->identifier_expr.identifier, NULL, false))
	{
		create_variable = true;
		lhs_type = NULL;
	}
	else
	{
		// Otherwise we just analyse it.
		create_variable = false;
		if (expr->try_assign_expr.is_try)
		{
			if (!sema_analyse_expr_value(context, NULL, lhs)) return false;
		}
		else
		{
			if (!sema_analyse_expr_of_required_type(context, type_anyerr, lhs, true)) return false;
		}
		if (!expr_is_ltype(lhs))
		{
			SEMA_ERROR(lhs, "This expression is not assignable, did you make a mistake?");
			return false;
		}
		ExprFailableStatus failable_status = expr_is_failable(lhs);
		if (failable_status == FAILABLE_YES)
		{
			SEMA_ERROR(lhs, "A 'try' assignment is not possible with failable on the left hand side, did you intend 'try (variable = expr)'?");
			return false;
		}
		lhs_type = lhs->type;
	}

	if (expr->try_assign_expr.is_try)
	{
		if (!sema_analyse_expr_of_required_type(context, lhs_type, init, true)) return false;
	}
	else
	{
		if (!sema_analyse_expr_of_required_type(context, NULL, init, true)) return false;
	}

	if (!init->failable)
	{
		SEMA_ERROR(init, "Expected a failable expression to '%s'.", expr->try_assign_expr.is_try ? "try" : "catch");
		return false;
	}

	if (create_variable)
	{
		lhs_type = init->type;
		Decl *decl = decl_new_var(lhs->identifier_expr.identifier, type_info_new_base(lhs_type, lhs->span), VARDECL_LOCAL, VISIBLE_LOCAL);
		TODO
		// try_expr->try_expr.implicit_decl = decl;
		if (!sema_add_local(context, decl)) return false;
		if (!sema_analyse_expr_value(context, NULL, lhs)) return false;
	}

	expr->constant = false;
	expr->pure = init->pure & lhs->pure;
	expr_set_type(expr, type_bool);
	return true;
}
static inline bool sema_expr_analyse_try(Context *context, Expr *expr)
{
	Expr *inner = expr->try_expr.expr;
	if (!sema_analyse_expr(context, NULL, inner)) return false;
	expr->pure = inner->pure;
	expr->constant = false;
	if (!inner->failable)
	{
		SEMA_ERROR(expr->try_expr.expr, "Expected a failable expression to '%s'.", expr->expr_kind == EXPR_TRY ? "try" : "catch");
		return false;
	}
	expr_set_type(expr, type_bool);
	return true;
}

static inline bool sema_expr_analyse_catch_old(Context *context, Expr *expr)
{
	Expr *inner = expr->trycatch_expr;
	bool success = sema_analyse_expr(context, NULL, inner);
	expr->pure = inner->pure;
	expr->constant = false;
	if (!success) return false;
	if (!inner->failable)
	{
		SEMA_ERROR(expr->trycatch_expr, "Expected a failable expression to 'catch'.");
		return false;
	}
	expr_set_type(expr, type_bool);
	return true;
}


static inline bool sema_expr_analyse_else(Context *context, Type *to, Expr *expr)
{
	Expr *inner = expr->else_expr.expr;
	bool success = sema_analyse_expr(context, to, inner);
	expr->pure = inner->pure;
	expr->constant = false;
	if (!success) return false;
	Type *type = inner->type;
	if (!inner->failable)
	{
		SEMA_ERROR(inner, "No failable to 'else' in the expression, please remove the 'else'.");
		return false;
	}
	if (expr->else_expr.is_jump)
	{
		expr->pure = false;
		if (!sema_analyse_statement(context, expr->else_expr.else_stmt)) return false;
		expr_copy_types(expr, inner);
		return true;
	}

	// First we analyse the "else" and try to implictly cast.
	if (!sema_analyse_expr(context, type, expr->else_expr.else_expr)) return false;
	expr->pure &= expr->else_expr.else_expr->pure;
	// Here we might need to insert casts.
	Type *common = type_find_max_type(type, expr->else_expr.else_expr->type);
	if (!cast_implicit(expr->else_expr.else_expr, common)) return false;

	expr->type = common;
	expr->original_type = type_find_max_type(inner->original_type, expr->else_expr.else_expr->original_type);

	return cast_implicit(expr->else_expr.expr, common);
}

static inline bool sema_expr_analyse_guard(Context *context, Type *to, Expr *expr)
{
	Expr *inner = expr->guard_expr.inner;
	bool success = sema_analyse_expr(context, to, inner);
	expr->guard_expr.defer = context->active_scope.defer_last;
	if (!success) return false;
	expr_copy_types(expr, inner);
	expr->pure = false;
	expr->constant = false;
	if (!inner->failable)
	{
		SEMA_ERROR(expr, "No failable to rethrow before '!!' in the expression, please remove '!!'.");
		return false;
	}
	if (!context->failable_return)
	{
		SEMA_ERROR(expr, "This expression implicitly returns with a failable result, but the function does not allow failable results. Did you mean to use 'else' instead?");
		return false;
	}
	return true;
}

static inline bool sema_expr_analyse_const(Type *to, Expr *expr)
{
	expr->constant = true;
	expr->pure = true;
	return true;
}





static inline bool sema_expr_analyse_type(Context *context, Expr *expr)
{
	if (!sema_resolve_type_info(context, expr->typeid_expr))
	{
		return expr_poison(expr);
	}
	expr_set_type(expr, type_typeid);
	return true;
}


static inline bool sema_expr_analyse_expr_block(Context *context, Type *to, Expr *expr)
{
	bool success = true;
	expr_set_type(expr, type_void);
	bool saved_expr_failable_return = context->expr_failable_return;
	Type *prev_expected_block_type = context->expected_block_type;
	Ast **saved_returns = context_push_returns(context);
	context->expected_block_type = to;


	SCOPE_START_WITH_FLAGS(SCOPE_EXPR_BLOCK)

		PUSH_CONTINUE(NULL);
		PUSH_BREAK(NULL);
		PUSH_NEXT(NULL, NULL);

		VECEACH(expr->expr_block.stmts, i)
		{
			if (!sema_analyse_statement(context, expr->expr_block.stmts[i]))
			{
				success = false;
				goto EXIT;
			}
		}

		if (!context->active_scope.jump_end && to)
		{
			SEMA_ERROR(expr, "Expected the block to return with a value of type %s.", type_to_error_string(to));
			success = false;
		}
		if (!vec_size(context->returns)) goto EXIT;

		Expr *first_return_expr = context->returns[0]->return_stmt.expr;
		Type *left_canonical = first_return_expr ? first_return_expr->type->canonical : type_void;
		// Let's unify the return statements.
		left_canonical = unify_returns(context, left_canonical);
		if (!left_canonical)
		{
			success = false;
			goto EXIT;
		}
		expr_set_type(expr, left_canonical);

	EXIT:
		POP_BREAKCONT();
		POP_NEXT();

	SCOPE_END;
	context_pop_returns(context, saved_returns);
	expr->failable = context->expr_failable_return;
	context->expr_failable_return = saved_expr_failable_return;
	context->expected_block_type = prev_expected_block_type;
	expr->constant = false;
	expr->pure = false;
	return success;
}


static inline bool sema_expr_analyse_compound_literal(Context *context, Expr *expr)
{
	if (!sema_resolve_type_info(context, expr->expr_compound_literal.type_info)) return false;
	Type *type = expr->expr_compound_literal.type_info->type;
	if (!sema_expr_analyse_initializer_list(context, type, expr->expr_compound_literal.initializer)) return false;
	expr_replace(expr, expr->expr_compound_literal.initializer);
	return true;
}

static inline bool sema_expr_analyse_typeof(Context *context, Expr *expr)
{
	if (!sema_analyse_expr(context, NULL, expr->typeof_expr)) return false;
	expr->pure = expr->typeof_expr->pure;
	expr->constant = expr->typeof_expr->constant;
	Type *type = expr->typeof_expr->type->canonical;
	expr->expr_kind = EXPR_TYPEID;
	expr->typeid_expr = type_info_new_base(type, expr->typeof_expr->span);
	expr_set_type(expr, type_typeid);
	return true;
}


static inline bool sema_expr_analyse_failable(Context *context, Type *to, Expr *expr)
{
	Expr *inner = expr->failable_expr;

	// Syntactic sugar: Foo! => Foo({})!
	if (inner->expr_kind == EXPR_TYPEINFO)
	{
		TypeInfo *type = inner->type_expr;
		*inner = (Expr) { .expr_kind = EXPR_COMPOUND_LITERAL, .span = inner->span };
		inner->expr_compound_literal.type_info = type;
		Expr *initializer_list = expr_new(EXPR_INITIALIZER_LIST, inner->span);
		initializer_list->initializer_expr.init_type = INITIALIZER_UNKNOWN;
		inner->expr_compound_literal.initializer = initializer_list;
	}

	if (!sema_analyse_expr(context, NULL, inner)) return false;
	expr->pure = inner->pure;
	expr->constant = inner->constant;
	if (inner->failable)
	{
		SEMA_ERROR(inner, "The inner expression is already a failable.");
	}
	Type *type = inner->type->canonical;
	if (inner->expr_kind == EXPR_FAILABLE)
	{
		SEMA_ERROR(inner, "It looks like you added one too many '!' after the error.");
		return false;
	}
	if (type->type_kind != TYPE_ERRTYPE && type->type_kind != TYPE_ERR_UNION)
	{
		SEMA_ERROR(inner, "You cannot use the '!' operator on expressions of type '%s'", type_to_error_string(type));
		return false;
	}
	if (!to)
	{
		expr_set_type(expr, type_void);
		return true;
	}
	expr->failable = true;
	expr_set_type(expr, to);
	return true;
}

static inline bool sema_expr_analyse_placeholder(Context *context, Type *to, Expr *expr)
{
	const char *string = TOKSTR(expr->placeholder_expr.identifier);
	if (string == kw_FILE)
	{
		expr_rewrite_to_string(expr, context->file->name);
		return true;
	}
	if (string == kw_FUNC)
	{
		if (!context->active_function_for_analysis)
		{
			expr_rewrite_to_string(expr, "<GLOBAL>");
			return true;
		}
		expr_rewrite_to_string(expr, context->active_function_for_analysis->name);
		return true;
	}
	if (string == kw_LINEREAL)
	{
		expr_rewrite_to_int_const(expr, type_compint, TOKLOC(expr->placeholder_expr.identifier)->line);
		return true;
	}
	if (string == kw_LINE)
	{
		if (context->macro_scope.depth)
		{
			expr_rewrite_to_int_const(expr, type_compint, context->macro_scope.original_inline_line);
		}
		else
		{
			expr_rewrite_to_int_const(expr, type_compint, TOKLOC(expr->placeholder_expr.identifier)->line);
		}
		return true;
	}
	Expr *value = stable_get(&global_context.compiler_defines, string);
	if (!value)
	{
		SEMA_ERROR(expr, "The placeholder constant '%s' was not defined, did you mistype or forget to add it?", string);
		return false;
	}
	expr_replace(expr, value);
	if (expr->expr_kind == EXPR_CONST && type_kind_is_any_integer(expr->const_expr.kind) && value->const_expr.i.digit_count > 1)
	{
		bigint_init_bigint(&expr->const_expr.i, &value->const_expr.i);
	}
	return true;
}

typedef struct MiniLexer_
{
	const char *chars;
	uint32_t index;
} MiniLexer;

static inline char minilex_next(MiniLexer *lexer)
{
	return lexer->chars[lexer->index++];
}

static inline char minilex_peek(MiniLexer *lexer, int32_t offset)
{
	return lexer->chars[lexer->index + offset];
}

static inline bool minilex_match(MiniLexer *lexer, char c)
{
	if (lexer->chars[lexer->index] != c) return false;
	lexer->index++;
	return true;
}

static inline void minilex_skip_whitespace(MiniLexer *lexer)
{
	char c;
	while (1)
	{
		c = lexer->chars[lexer->index];
		if ((c != ' ') && (c != '\t')) break;
		lexer->index++;
	}
}

uint64_t minilex_parse_number(MiniLexer *lexer, uint64_t max)
{
	assert(max < UINT64_MAX);
	ByteSize value = 0;
	while (is_number(minilex_peek(lexer, 0)))
	{
		ByteSize old_value = value;
		value = value * 10 + minilex_next(lexer) - '0';
		if (old_value > value || value > max)
		{
			return UINT64_MAX;
		}
	}
	return value;
}

static inline bool sema_analyse_idents_string(Context *context, MiniLexer *lexer, ExprFlatElement **elements_ref)
{
	ExprFlatElement *elements = NULL;

	char c;
	ExprFlatElement element;
	while (1)
	{
		minilex_skip_whitespace(lexer);
		if (minilex_match(lexer, '['))
		{
			minilex_skip_whitespace(lexer);
			if (!is_number(minilex_peek(lexer, 0))) return false;
			uint64_t value = minilex_parse_number(lexer, MAX_ARRAYINDEX);
			if (value == UINT64_MAX) return false;
			if (!minilex_match(lexer, ']')) return false;
			element.array = true;
			element.index = (ArrayIndex)value;
		}
		else
		{
			scratch_buffer_clear();
			while (is_alphanum_(minilex_peek(lexer, 0)))
			{
				scratch_buffer_append_char(minilex_next(lexer));
			}
			if (!global_context.scratch_buffer_len) return false;
			TokenType token_type;
			const char *ident = symtab_find(global_context.scratch_buffer,
			                                global_context.scratch_buffer_len,
			                                fnv1a(global_context.scratch_buffer, global_context.scratch_buffer_len),
			                                &token_type);
			if (!ident || token_type != TOKEN_IDENT) return false;
			element.array = false;
			element.ident = ident;
		}
		vec_add(elements, element);
		minilex_skip_whitespace(lexer);
		if (minilex_match(lexer, '\0')) break;
		if (!minilex_match(lexer, '.') && minilex_peek(lexer, 0) != '[') return false;
	}
	*elements_ref = elements;
	return true;
}

static inline bool sema_analyse_identifier_path_string(Context *context, Expr *expr, Decl **decl_ref, Type **type_ref, ExprFlatElement **idents_ref)
{
	char *chars = expr->const_expr.string.chars;
	uint32_t len = expr->const_expr.string.len;
	if (!len)
	{
		SEMA_ERROR(expr, "Expected a name here.");
		return false;
	}

	// TODO HANDLE VIRTUAL!

	MiniLexer lexer = { .chars = chars };

	// Do we parse a path?
	Path *path = NULL;

	// Skip past the start.
	while (is_lower_(minilex_peek(&lexer, 0))) minilex_next(&lexer);

	minilex_skip_whitespace(&lexer);

	// Yes, parse a path.
	if (minilex_match(&lexer, ':'))
	{
		if (!minilex_match(&lexer, ':')) goto FAIL_PARSE;
		lexer.index = 0;
		NEXT_PART:
		scratch_buffer_clear();
		minilex_skip_whitespace(&lexer);
		while (is_lower(minilex_peek(&lexer, 0)))
		{
			scratch_buffer_append_char(minilex_next(&lexer));
		}
		minilex_skip_whitespace(&lexer);

		if (!(minilex_match(&lexer, ':') && minilex_match(&lexer, ':')))
		{
			UNREACHABLE;
		}
		uint32_t start_index = lexer.index;

		minilex_skip_whitespace(&lexer);

		while (is_lower_(minilex_peek(&lexer, 0))) minilex_next(&lexer);

		minilex_skip_whitespace(&lexer);
		if (minilex_match(&lexer, ':'))
		{
			if (!minilex_match(&lexer, ':')) goto FAIL_PARSE;
			scratch_buffer_append_len("::", 2);
			lexer.index = start_index;
			goto NEXT_PART;
		}
		lexer.index = start_index;
		path = path_create_from_string(scratch_buffer_to_string(), global_context.scratch_buffer_len, expr->span);
	}
	else
	{
		lexer.index = 0;
	}

	scratch_buffer_clear();
	minilex_skip_whitespace(&lexer);
	while (is_alphanum_(minilex_peek(&lexer, 0)))
	{
		scratch_buffer_append_char(minilex_next(&lexer));
	}
	if (!global_context.scratch_buffer_len) goto FAIL;
	TokenType token_type;
	const char *symbol = symtab_find(global_context.scratch_buffer,
	                                 global_context.scratch_buffer_len,
	                                 fnv1a(global_context.scratch_buffer, global_context.scratch_buffer_len),
	                                 &token_type);
	if (!symbol)
	{
		SEMA_ERROR(expr, "'%s' could not be found, did you misspell it?", chars);
		return false;
	}
	Type *type = NULL;
	Decl *decl = NULL;
	if (token_is_type(token_type))
	{
		type = type_from_token(token_type);
	}
	else
	{
		decl = TRY_DECL_OR(sema_resolve_string_symbol(context, symbol, expr->span, path), false);
		if (decl->decl_kind == DECL_TYPEDEF)
		{
			type = decl->typedef_decl.type_info->type;
			decl = NULL;
		}
	}
	if (type || decl_is_user_defined_type(decl))
	{
		if (decl)
		{
			type = decl->type;
			decl = NULL;
		}
		while (1)
		{
			minilex_skip_whitespace(&lexer);
			if (minilex_match(&lexer, '*'))
			{
				type = type_get_ptr(type);
				continue;
			}
			if (!minilex_match(&lexer, '[')) break;
			minilex_skip_whitespace(&lexer);
			if (minilex_match(&lexer, ']'))
			{
				type = type_get_subarray(type);
				continue;
			}
			ByteSize array_size = 0;
			while (is_number(minilex_peek(&lexer, 0)))
			{
				ByteSize old_array_size = array_size;
				array_size = array_size * 10 + minilex_next(&lexer) - '0';
				if (old_array_size > array_size || array_size > MAX_ARRAYINDEX)
				{
					SEMA_ERROR(expr, "Array index out of bounds.");
					return false;
				}
			}
			minilex_skip_whitespace(&lexer);
			if (!minilex_match(&lexer, ']')) goto FAIL_PARSE;
			type = type_get_array(type, array_size);
		}
	}

	minilex_skip_whitespace(&lexer);
	if (minilex_match(&lexer, '\0')) goto EXIT;


	minilex_skip_whitespace(&lexer);
	if (!minilex_match(&lexer, '.'))
	{
		SEMA_ERROR(expr, "A '.' was expected after '%s'.", symbol);
		return false;
	}

	if (!sema_analyse_idents_string(context, &lexer, idents_ref))
	{
		SEMA_ERROR(expr, "The path to an existing member was expected after '%s', did you make a mistake?", symbol);
		return false;
	}

EXIT:
	*decl_ref = decl;
	*type_ref = type;
	return true;

FAIL:
	SEMA_ERROR(expr, "'%s' is not a known identifier, did you misspell it?", chars);
	return false;

FAIL_PARSE:
	SEMA_ERROR(expr, "'%s' could not be parsed as an identifier, did you make a mistake?", chars);
	return false;

}


static inline bool sema_analyse_ct_call_parameters(Context *context, Expr *expr)
{
	if (expr->resolve_status == RESOLVE_DONE) return true;

	Expr **elements = expr->ct_call_expr.arguments;
	unsigned count = vec_size(elements);
	if (count > 2)
	{
		SEMA_ERROR(elements[2], "The function can only take one or two arguments.");
		return expr_poison(expr);
	}
	Expr *first_expr = elements[0];
	if (!sema_analyse_expr_value(context, NULL, first_expr)) return false;
	ExprFlatElement *flatpath = NULL;
	Expr *second_expr = NULL;
	if (count == 2)
	{
		second_expr = elements[1];
		if (second_expr->expr_kind != EXPR_FLATPATH)
		{
			if (!sema_analyse_expr_value(context, NULL, second_expr)) return false;
			if (second_expr->expr_kind != EXPR_CONST || second_expr->const_expr.kind != TYPE_STRLIT)
			{
				SEMA_ERROR(second_expr, "Expected a string here.");
				return false;
			}
			MiniLexer lexer = { .chars = second_expr->const_expr.string.chars };
			if (!sema_analyse_idents_string(context, &lexer, &flatpath))
			{
				SEMA_ERROR(expr, "The path to an existing member was expected, did you make a mistake?");
				return false;
			}
		}
		else
		{
			flatpath = second_expr->flatpath_expr;
		}
	}
	Decl *decl = NULL;
	Type *type = NULL;
	if (first_expr->expr_kind == EXPR_CONST && first_expr->const_expr.kind == TYPE_STRLIT)
	{
		ExprFlatElement *path_elements = NULL;
		if (!sema_analyse_identifier_path_string(context, first_expr, &decl, &type, &path_elements)) return false;
		if (path_elements)
		{
			if (flatpath)
			{
				SEMA_ERROR(elements[1], "You cannot combine a string with a member path with a separate path here.");
				return false;
			}
			flatpath = path_elements;
		}
	}
	else
	{
		switch (first_expr->expr_kind)
		{
			case EXPR_IDENTIFIER:
				decl = first_expr->identifier_expr.decl;
				break;
			case EXPR_TYPEINFO:
				type = first_expr->type_expr->type;
				break;
			case EXPR_TYPEOF:
				type = first_expr->typeof_expr->type;
				break;
			default:
				break;
		}
	}
	if (!type && !decl)
	{
		SEMA_ERROR(first_expr, "A type or declaration was expected.");
		return false;
	}
	if (!decl && type_is_user_defined(type)) decl = type->decl;
	if (decl)
	{
		if (!sema_analyse_decl(context, decl)) return false;
		if (decl)
		{
			switch (decl->decl_kind)
			{
				case DECL_DISTINCT:
				case DECL_ENUM:
				case DECL_ERR:
				case DECL_FUNC:
				case DECL_STRUCT:
				case DECL_TYPEDEF:
				case DECL_UNION:
					break;
				case DECL_VAR:
					if (decl->type) break;
					FALLTHROUGH;
				default:
					SEMA_ERROR(first_expr, "A type or typed declaration was expected.");
					return false;
			}
		}
		type = decl->type;
	}
	expr->ct_call_expr.flatpath = flatpath;
	expr->ct_call_expr.decl = decl;
	expr->ct_call_expr.type = type;

	return true;
}

static inline bool sema_expr_analyse_ct_sizeof(Context *context, Type *to, Expr *expr)
{
	Expr *first = expr->ct_call_expr.arguments[0];
	if (!sema_analyse_ct_call_parameters(context, expr)) return false;
	Type *type = expr->ct_call_expr.type->canonical;
	ExprFlatElement *elements = expr->ct_call_expr.flatpath;
	VECEACH(elements, i)
	{
		ExprFlatElement element = elements[i];
		type = type_flatten_distinct(type);
		if (element.array)
		{
			if (type->type_kind != TYPE_ARRAY)
			{
				SEMA_ERROR(first, "%s is not a fixed size array.", type_quoted_error_string(expr->ct_call_expr.type));
				return false;
			}
			type = type->array.base;
			continue;
		}
		if (!type_is_structlike(type))
		{
			if (i == 0)
			{
				SEMA_ERROR(first, "%s has no members.", type_quoted_error_string(expr->ct_call_expr.type));
			}
			else
			{
				SEMA_ERROR(first, "There is no such member in %s.", type_quoted_error_string(expr->ct_call_expr.type));
			}
			return false;
		}
		Decl *decl = type->decl;
		SCOPE_START
			add_members_to_context(context, type->decl);
			decl = sema_resolve_symbol_in_current_dynamic_scope(context, element.ident);
		SCOPE_END;
		if (!decl)
		{
			SEMA_ERROR(first, "There is no such member in %s.", type_quoted_error_string(expr->ct_call_expr.type));
			return false;
		}
		type = decl->type;
	}
	expr_rewrite_to_int_const(expr, type_compint, type_size(type));
	return true;
}

static inline bool decl_is_local(Decl *decl)
{
	if (decl->decl_kind != DECL_VAR) return false;
	VarDeclKind kind = decl->var.kind;
	return kind == VARDECL_PARAM_CT_TYPE
		|| kind == VARDECL_PARAM
		|| kind == VARDECL_PARAM_CT
		|| kind == VARDECL_LOCAL
		|| kind == VARDECL_LOCAL_CT_TYPE
		|| kind == VARDECL_LOCAL_CT
		|| kind == VARDECL_PARAM_REF
		|| kind == VARDECL_PARAM_EXPR
		|| kind == VARDECL_MEMBER;
}

static inline bool sema_expr_analyse_ct_nameof(Context *context, Type *to, Expr *expr)
{
	if (expr->resolve_status == RESOLVE_DONE) return true;

	Expr **elements = expr->ct_call_expr.arguments;
	unsigned count = vec_size(elements);
	if (count > 1)
	{
		SEMA_ERROR(elements[1], "The function only takes one argument.");
		return expr_poison(expr);
	}
	Expr *first_expr = elements[0];
	if (!sema_analyse_expr_value(context, NULL, first_expr)) return false;

	bool qualified = expr->ct_call_expr.token_type == TOKEN_CT_QNAMEOF;

	if (first_expr->expr_kind == EXPR_TYPEINFO)
	{
		Type *type = first_expr->type_expr->type->canonical;
		if (!sema_resolve_type(context, type)) return false;
		// TODO type_is_builtin is wrong also this does not cover virtual.
		if (!qualified || type_is_builtin(type->type_kind))
		{
			expr_rewrite_to_string(expr, type->name);
			return true;
		}
		scratch_buffer_clear();
		scratch_buffer_append(type->decl->module->name->module);
		scratch_buffer_append("::");
		scratch_buffer_append(type->name);
		expr_rewrite_to_string(expr, scratch_buffer_interned());
		return true;
	}
	if (first_expr->expr_kind != EXPR_IDENTIFIER)
	{
		SEMA_ERROR(first_expr, "Expected an identifier.");
		return false;
	}
	Decl *decl = first_expr->identifier_expr.decl;

	if (!decl->module || !qualified || decl_is_local(decl))
	{
		expr_rewrite_to_string(expr, decl->name);
		return true;
	}
	scratch_buffer_clear();
	scratch_buffer_append(decl->module->name->module);
	scratch_buffer_append("::");
	scratch_buffer_append(decl->name);
	expr_rewrite_to_string(expr, scratch_buffer_interned());
	return true;
}


static inline bool sema_expr_analyse_ct_alignof(Context *context, Type *to, Expr *expr)
{
	Expr *first = expr->ct_call_expr.arguments[0];
	if (!sema_analyse_ct_call_parameters(context, expr)) return false;
	Type *type = expr->ct_call_expr.type->canonical;
	ExprFlatElement *elements = expr->ct_call_expr.flatpath;

	Decl *decl = expr->ct_call_expr.decl;
	AlignSize align = decl && !decl_is_user_defined_type(decl) ? expr->ct_call_expr.decl->alignment : type_abi_alignment(type);
	VECEACH(elements, i)
	{
		ExprFlatElement element = elements[i];
		type = type_flatten_distinct(type);
		if (element.array)
		{
			if (type->type_kind != TYPE_ARRAY)
			{
				SEMA_ERROR(first, "%s is not a fixed size array.", type_quoted_error_string(expr->ct_call_expr.type));
				return false;
			}
			type = type->array.base;
			ByteSize size = type_size(type);
			align = (AlignSize)type_min_alignment(size * element.index, align);
			continue;
		}
		if (!type_is_structlike(type))
		{
			if (i == 0)
			{
				SEMA_ERROR(first, "%s has no members.", type_quoted_error_string(expr->ct_call_expr.type));
			}
			else
			{
				SEMA_ERROR(first, "There is no such member in %s.", type_quoted_error_string(expr->ct_call_expr.type));
			}
			return false;
		}
		Decl *decl = type->decl;
		SCOPE_START
			add_members_to_context(context, type->decl);
			decl = sema_resolve_symbol_in_current_dynamic_scope(context, element.ident);
		SCOPE_END;
		if (!decl)
		{
			SEMA_ERROR(first, "There is no such member in %s.", type_quoted_error_string(expr->ct_call_expr.type));
			return false;
		}
		type = decl->type;
		align = type_min_alignment(decl->offset, align);
	}

	expr_rewrite_to_int_const(expr, type_compint, align);

	return true;
}


static inline bool sema_expr_analyse_ct_offsetof(Context *context, Type *to, Expr *expr)
{
	Expr *first = expr->ct_call_expr.arguments[0];
	if (!sema_analyse_ct_call_parameters(context, expr)) return false;
	Type *type = expr->ct_call_expr.type->canonical;
	ExprFlatElement *elements = expr->ct_call_expr.flatpath;
	if (!vec_size(elements))
	{
		SEMA_ERROR(first, "Expected a path to get the offset of.");
		return false;
	}
	ByteSize offset = 0;
	VECEACH(elements, i)
	{
		ExprFlatElement element = elements[i];
		type = type_flatten_distinct(type);
		if (element.array)
		{
			if (type->type_kind != TYPE_ARRAY)
			{
				SEMA_ERROR(first, "%s is not a fixed size array.", type_quoted_error_string(expr->ct_call_expr.type));
				return false;
			}
			type = type->array.base;
			offset += type_size(type) * element.index;
			continue;
		}
		if (!type_is_structlike(type))
		{
			if (i == 0)
			{
				SEMA_ERROR(first, "%s has no members.", type_quoted_error_string(expr->ct_call_expr.type));
			}
			else
			{
				SEMA_ERROR(first, "There is no such member in %s.", type_quoted_error_string(expr->ct_call_expr.type));
			}
			return false;
		}
		Decl *decl = type->decl;
		SCOPE_START
			add_members_to_context(context, type->decl);
			decl = sema_resolve_symbol_in_current_dynamic_scope(context, element.ident);
		SCOPE_END;
		if (!decl)
		{
			SEMA_ERROR(first, "There is no such member in %s.", type_quoted_error_string(expr->ct_call_expr.type));
			return false;
		}
		type = decl->type;
		offset += decl->offset;
	}

	expr_rewrite_to_int_const(expr, type_compint, offset);

	return true;
}

static inline bool sema_expr_analyse_ct_call(Context *context, Type *to, Expr *expr)
{
	switch (expr->ct_call_expr.token_type)
	{
		case TOKEN_CT_SIZEOF:
			return sema_expr_analyse_ct_sizeof(context, to, expr);
		case TOKEN_CT_ALIGNOF:
			return sema_expr_analyse_ct_alignof(context, to, expr);
		case TOKEN_CT_OFFSETOF:
			return sema_expr_analyse_ct_offsetof(context, to, expr);
		case TOKEN_CT_QNAMEOF:
			return sema_expr_analyse_ct_nameof(context, to, expr);
		case TOKEN_CT_NAMEOF:
			return sema_expr_analyse_ct_nameof(context, to, expr);
		default:
			UNREACHABLE
	}
}

static inline bool sema_expr_analyse_decl(Context *context, Type *to, Expr *expr)
{
	if (!sema_analyse_local_decl(context, expr->decl_expr)) return false;
	expr_set_type(expr, expr->decl_expr->type);
	expr->pure = !expr->decl_expr->var.init_expr || expr->decl_expr->var.init_expr->pure;
	expr->constant = expr->decl_expr->var.kind == VARDECL_CONST;
	return true;
}
static inline bool sema_analyse_expr_dispatch(Context *context, Type *to, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_DECL_LIST:
		case EXPR_UNDEF:
		case EXPR_ENUM_CONSTANT:
		case EXPR_MEMBER_ACCESS:
		case EXPR_DESIGNATOR:
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_FLATPATH:
		case EXPR_NOP:
			UNREACHABLE
		case EXPR_DECL:
			return sema_expr_analyse_decl(context, to, expr);
		case EXPR_CT_CALL:
			return sema_expr_analyse_ct_call(context, to, expr);
		case EXPR_HASH_IDENT:
			return sema_expr_analyse_hash_identifier(context, to, expr);
		case EXPR_CT_IDENT:
			return sema_expr_analyse_ct_identifier(context, expr);
		case EXPR_FAILABLE:
			return sema_expr_analyse_failable(context, to, expr);
		case EXPR_PLACEHOLDER:
			return sema_expr_analyse_placeholder(context, to, expr);
		case EXPR_POISONED:
			return false;
		case EXPR_LEN:
		case EXPR_SLICE_ASSIGN:
			// Created during semantic analysis
			UNREACHABLE
		case EXPR_MACRO_BLOCK:
		case EXPR_SCOPED_EXPR:
			UNREACHABLE
		case EXPR_TYPEINFO:
			return sema_expr_analyse_typeinfo(context, expr);
		case EXPR_SLICE:
			return sema_expr_analyse_slice(context, expr);
		case EXPR_TRY:
			return sema_expr_analyse_try(context, expr);
		case EXPR_TRY_ASSIGN:
			return sema_expr_analyse_try_assign(context, expr, false);
		case EXPR_CATCH_OLD:
			return sema_expr_analyse_catch_old(context, expr);
		case EXPR_TRY_OLD:
			return sema_expr_analyse_try_old(context, expr);
		case EXPR_TYPEOF:
			return sema_expr_analyse_typeof(context, expr);
		case EXPR_ELSE:
			return sema_expr_analyse_else(context, to, expr);
		case EXPR_COMPOUND_LITERAL:
			return sema_expr_analyse_compound_literal(context, expr);
		case EXPR_EXPR_BLOCK:
			return sema_expr_analyse_expr_block(context, to, expr);
		case EXPR_GUARD:
			return sema_expr_analyse_guard(context, to, expr);
		case EXPR_CONST:
			return sema_expr_analyse_const(to, expr);
		case EXPR_BINARY:
			if (!sema_expr_analyse_binary(context, to, expr)) return false;
			if (expr->expr_kind == EXPR_BINARY)
			{
				expr->failable = expr->binary_expr.left->failable | expr->binary_expr.right->failable;
			}
			return true;
		case EXPR_TERNARY:
			return sema_expr_analyse_ternary(context, to, expr);
		case EXPR_UNARY:
			if (!sema_expr_analyse_unary(context, to, expr)) return false;
			if (expr->expr_kind == EXPR_UNARY) expr->failable = expr->unary_expr.expr->failable;
			return true;
		case EXPR_POST_UNARY:
			if (!sema_expr_analyse_post_unary(context, to, expr)) return false;
			if (expr->expr_kind == EXPR_UNARY) expr->failable = expr->unary_expr.expr->failable;
			return true;
		case EXPR_TYPEID:
			return sema_expr_analyse_type(context, expr);
		case EXPR_MACRO_EXPANSION:
			return sema_expr_analyse_macro_expansion(context, expr);
		case EXPR_CONST_IDENTIFIER:
		case EXPR_IDENTIFIER:
			return sema_expr_analyse_identifier(context, to, expr);
		case EXPR_CALL:
			return sema_expr_analyse_call(context, to, expr);
		case EXPR_SUBSCRIPT:
			return sema_expr_analyse_subscript(context, expr);
		case EXPR_GROUP:
			return sema_expr_analyse_group(context, to, expr);
		case EXPR_ACCESS:
			return sema_expr_analyse_access(context, expr);
		case EXPR_INITIALIZER_LIST:
			return sema_expr_analyse_initializer_list(context, to, expr);
		case EXPR_CAST:
			return sema_expr_analyse_cast(context, to, expr);
		case EXPR_EXPRESSION_LIST:
			return sema_expr_analyse_expr_list(context, to, expr);
	}
	UNREACHABLE
}


bool sema_analyse_expr_of_required_type(Context *context, Type *to, Expr *expr, bool may_be_failable)
{
	if (!sema_analyse_expr(context, to, expr)) return false;
	if (expr->failable && !may_be_failable)
	{
		if (!to) to = expr->type;
		SEMA_ERROR(expr, "'%s!' cannot be converted into '%s'.", type_to_error_string(expr->type), type_to_error_string(to));
		return false;
	}
	return to ? cast_implicit(expr, to) : true;
}


static inline bool sema_cast_ct_ident_rvalue(Context *context, Type *to, Expr *expr)
{
	Decl *decl = expr->ct_ident_expr.decl;
	Expr *copy = copy_expr(decl->var.init_expr);
	if (!sema_analyse_expr(context, to, copy)) return false;
	expr_replace(expr, copy);
	return true;
}

static inline bool sema_cast_rvalue(Context *context, Type *to, Expr *expr)
{
	if (!expr_ok(expr)) return false;
	switch (expr->expr_kind)
	{
		case EXPR_MACRO_BODY_EXPANSION:
			if (!expr->body_expansion_expr.ast)
			{
				SEMA_ERROR(expr, "'@%s' must be followed by ().", context->macro_scope.body_param);
				return false;
			}
		case EXPR_ACCESS:
			if (expr->access_expr.ref->decl_kind == DECL_FUNC)
			{
				SEMA_ERROR(expr, "A function name must be followed by '(' or preceeded by '&'.");
				return false;
			}
			if (expr->access_expr.ref->decl_kind == DECL_MACRO)
			{
				SEMA_ERROR(expr, "A macro name must be followed by '('.");
				return false;
			}
			break;
		case EXPR_MEMBER_ACCESS:
			if (expr->access_expr.ref->decl_kind == DECL_ENUM_CONSTANT)
			{
				Type *original_type = expr->access_expr.ref->type;
				expr_replace(expr, expr->access_expr.ref->enum_constant.expr);
				expr->original_type = original_type;
				return true;
			}
			SEMA_ERROR(expr, "A member must be followed by '.' plus a property like 'sizeof'.");
			return false;
		case EXPR_LEN:
			if (expr->type != type_void) return true;
			SEMA_ERROR(expr, "Expected () after 'len' for subarrays.");
			return false;
		case EXPR_TYPEINFO:
			SEMA_ERROR(expr, "A type must be followed by either (...) or '.'.");
			return false;
		case EXPR_ENUM_CONSTANT:
			assert(expr->expr_enum->enum_constant.expr->expr_kind == EXPR_CONST);
			expr->const_expr = expr->expr_enum->enum_constant.expr->const_expr;
			expr->expr_kind = EXPR_CONST;
			break;
		case EXPR_MACRO_EXPANSION:
			SEMA_ERROR(expr, "Expected macro followed by (...).", expr->ct_macro_ident_expr.identifier);
			return expr_poison(expr);
		case EXPR_CT_IDENT:
			if (!sema_cast_ct_ident_rvalue(context, to, expr)) return false;
			break;
		case EXPR_IDENTIFIER:
		case EXPR_CONST_IDENTIFIER:
			if (!sema_cast_ident_rvalue(context, to, expr)) return false;
			break;
		default:
			break;
	}
	return true;
}

bool sema_analyse_expr_value(Context *context, Type *to, Expr *expr)
{
	switch (expr->resolve_status)
	{
		case RESOLVE_NOT_DONE:
			expr->resolve_status = RESOLVE_RUNNING;
			if (!sema_analyse_expr_dispatch(context, to, expr)) return expr_poison(expr);
			expr->resolve_status = RESOLVE_DONE;
			return true;
		case RESOLVE_RUNNING:
			SEMA_ERROR(expr, "Recursive resolution of expression");
			return expr_poison(expr);
		case RESOLVE_DONE:
			return expr_ok(expr);
		default:
			UNREACHABLE
	}
}

ArrayIndex sema_get_initializer_const_array_size(Context *context, Expr *initializer, bool *may_be_array, bool *is_const_size)
{
	assert(initializer->expr_kind == EXPR_INITIALIZER_LIST);
	Expr **initializers = initializer->initializer_expr.initializer_expr;
	*may_be_array = true;
	*is_const_size = true;
	unsigned element_count = vec_size(initializers);
	// If it's empty or the first element is not a designator, we assume an array list
	// with that many elements.
	if (!element_count || initializers[0]->expr_kind != EXPR_DESIGNATOR) return element_count;
	ArrayIndex size = 0;
	// Otherwise we assume everything's a designator.
	VECEACH(initializers, i)
	{
		Expr *sub_initializer = initializers[i];
		if (sub_initializer->expr_kind != EXPR_DESIGNATOR)
		{
			// Simply messed up: a mix of designators and regular ones.
			return -1;
		}
		DesignatorElement *element = sub_initializer->designator_expr.path[0];
		switch (element->kind)
		{
			case DESIGNATOR_FIELD:
				// Struct, abandon!
				*may_be_array = false;
				return -1;
			case DESIGNATOR_ARRAY:
			{
				ArrayIndex index = sema_analyse_designator_index(context, element->index_expr);
				if (index < 0 || element->index_expr->expr_kind != EXPR_CONST)
				{
					*is_const_size = false;
					return -1;
				}
				size = MAX(size, index + 1);
				break;
			}
			case DESIGNATOR_RANGE:
			{
				ArrayIndex index = sema_analyse_designator_index(context, element->index_end_expr);
				if (index < 0 || element->index_end_expr->expr_kind != EXPR_CONST)
				{
					*is_const_size = false;
					return -1;
				}
				size = MAX(size, index + 1);
				break;
			}
			default:
				UNREACHABLE
		}
	}
	return size;
}

bool sema_analyse_expr(Context *context, Type *to, Expr *expr)
{
	return sema_analyse_expr_value(context, to, expr) && sema_cast_rvalue(context, to, expr);
}

