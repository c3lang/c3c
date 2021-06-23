// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

/*
 * TODOs
 * - Disallow jumping in and out of an expression block.
 */

static inline bool sema_cast_rvalue(Context *context, Type *to, Expr *expr);


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
		case EXPR_MACRO_CT_IDENTIFIER:
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
		case DECL_GENERIC:
		case DECL_CT_IF:
		case DECL_CT_ELSE:
		case DECL_CT_ELIF:
		case DECL_CT_SWITCH:
		case DECL_CT_CASE:
		case DECL_ATTRIBUTE:
			UNREACHABLE
		case DECL_DEFINE:
			TODO
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
		case VARDECL_ALIAS:
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
	SEMA_ERROR(expr, "%s is not defined in the expression '%s' %s '%s'.",
	               c, type_to_error_string(expr->binary_expr.left->type),
	               c, type_to_error_string(expr->binary_expr.right->type));
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

static inline bool sema_expr_analyse_identifier_resolve(Context *context, Type *to, Expr *expr, ExprIdentifier *id_expr)
{
	Decl *ambiguous_decl = NULL;
	Decl *private_symbol = NULL;
	expr->pure = true;

	DEBUG_LOG("Now resolving %s", TOKSTR(id_expr->identifier));
	Decl *decl = sema_resolve_normal_symbol(context,
	                                        id_expr->identifier,
	                                        id_expr->path, false);
	if (!decl && !id_expr->path && to)
	{
		if (find_possible_inferred_identifier(to, expr)) return true;
	}

	if (!decl)
	{
		decl = sema_resolve_normal_symbol(context, id_expr->identifier, id_expr->path, true);
		assert(!decl_ok(decl) && "Expected a poisoned decl here.");
		return false;
	}

	// Already handled
	if (!decl_ok(decl)) return false;

	if (decl->decl_kind == DECL_FUNC && !id_expr->path && decl->module != context->module)
	{
		SEMA_ERROR(expr, "Functions from other modules must be prefixed with the module name, please add '%s' in front.", decl->module->name->module);
		return false;
	}
	if (decl->decl_kind == DECL_MACRO)
	{
		if (expr->expr_kind != EXPR_MACRO_IDENTIFIER)
		{
			SEMA_ERROR(expr, "Macro expansions must be prefixed with '@', try using '@%s(...)' instead.", decl->name);
			return false;
		}
		id_expr->decl = decl;
		expr_set_type(expr, type_void);
		return true;
	}
	if (expr->expr_kind == EXPR_MACRO_IDENTIFIER)
	{
		SEMA_ERROR(expr, "Only macro expansions can be prefixed with '@', please try to remove it.", decl->name);
		return false;
	}
	if (decl->resolve_status != RESOLVE_DONE)
	{
		if (!sema_analyse_decl(context, decl)) return poisoned_decl;
	}
	if (decl->decl_kind == DECL_VAR && decl->var.failable)
	{
		expr->failable = true;
	}
	if (expr->expr_kind == EXPR_CONST_IDENTIFIER)
	{
		assert(decl->decl_kind == DECL_VAR);
		assert(decl->var.kind == VARDECL_CONST);
		assert(!decl->var.failable);
	}
	assert(decl->type);
	expr->identifier_expr.decl = decl;
	expr_set_type(expr, decl->type);
	expr->pure = true;
	expr->constant = false;
	DEBUG_LOG("Resolution successful of %s.", decl->name);
	return true;
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
	if (decl->decl_kind == DECL_MACRO)
	{
		if (expr->expr_kind != EXPR_MACRO_IDENTIFIER)
		{
			SEMA_ERROR(expr, "Macro expansions must be prefixed with '@', try using '@%s(...)' instead.", decl->name);
			return false;
		}
		expr->identifier_expr.decl = decl;
		expr_set_type(expr, type_void);
		return true;
	}
	if (expr->expr_kind == EXPR_MACRO_IDENTIFIER)
	{
		SEMA_ERROR(expr, "Only macro expansions can be prefixed with '@', please try removing it.", decl->name);
		return false;
	}
	if (decl->resolve_status != RESOLVE_DONE)
	{
		if (!sema_analyse_decl(context, decl)) return poisoned_decl;
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
static inline bool sema_expr_analyse_func_invocation(Context *context, FunctionSignature *signature, Expr *expr, Decl *decl, Type *to, Expr *struct_var)
{
	// 1. Builtin? We handle that elsewhere.
	if (decl->func.is_builtin)
	{
		assert(!struct_var);
		return sema_expr_analyse_intrinsic_invocation(context, expr, decl, to);
	}
	Decl **func_params = signature->params;
	Expr **args = expr->call_expr.arguments;

	// 2. If this is a type call, then we have an implicit first argument.
	unsigned struct_args = struct_var == NULL ? 0 : 1;

	// 3. There might be unpacking for typed varargs.
	unsigned num_args = vec_size(args) + struct_args;
	bool unsplat = expr->call_expr.unsplat_last;
	if (unsplat)
	{
		// 4. Is this *not* a naked vararg or typed vararg? That's an error.
		if (!signature->typed_variadic && !signature->variadic)
		{
			SEMA_ERROR(VECLAST(expr->call_expr.arguments), "Unpacking is only allowed for functions with variable parameters.");
			return false;
		}
	}

	// 4. Zero out all argument slots.
	unsigned func_param_count = vec_size(func_params);
	// 4a. We need at least as many function locations as we have parameters.
	unsigned entries_needed = func_param_count > num_args ? func_param_count : num_args;
	Expr **actual_args = VECNEW(Expr*, entries_needed);
	for (unsigned i = 0; i < entries_needed; i++) vec_add(actual_args, NULL);
	memset(actual_args, 0, entries_needed * sizeof(Expr*));

	// 5. We might have a typed variadic call e.g. foo(int, double...)
	//    get that type.
	Type *variadic_type = NULL;
	if (signature->typed_variadic)
	{
		// 5a. The parameter type is <type>[], so we get the <type>
		assert(func_params[func_param_count - 1]->type->type_kind == TYPE_SUBARRAY);
		variadic_type = func_params[func_param_count - 1]->type->array.base;
		// 5b. The last function parameter is implicit, so remove it.
		func_param_count--;
	}

	// 6. Loop through the parameters.
	bool uses_named_parameters = false;
	for (unsigned i = 0; i < num_args; i++)
	{
		// 7. We might pick up the argument as the type if this is a type
		//    method.
		bool is_implicit = i < struct_args;
		Expr *arg = is_implicit ? struct_var : args[i - struct_args];

		// 8. Handle named parameters
		if (arg->expr_kind == EXPR_DESIGNATOR)
		{
			// 8a. We have named parameters, that will add some restrictions.
			uses_named_parameters = true;

			// 8b. Find the location of the parameter.
			int index = find_index_of_named_parameter(func_params, arg);

			// 8c. If it's not found then this is an error.
			if (index < 0) return false;

			// 8d. We might actually be finding the typed vararg at the end,
			//     this is an error.
			if (func_params[index]->var.vararg)
			{
				SEMA_ERROR(arg, "Vararg parameters may not be named parameters, use normal parameters instead.", func_params[index]->name);
				return false;
			}

			// 8e. We might have already set this parameter, that is not allowed.
			if (actual_args[index])
			{
				SEMA_ERROR(arg, "The parameter '%s' was already set once.", func_params[index]->name);
				return false;
			}

			// 8f. Check the parameter
			if (!sema_analyse_expr_of_required_type(context, func_params[index]->type, arg->designator_expr.value, true)) return false;

			// 8g. Set the parameter and update failability.
			actual_args[index] = arg->designator_expr.value;
			expr->failable |= arg->designator_expr.value->failable;
			continue;
		}

		// 9. Check for previous use of named parameters, this is not allowed
		//     like foo(.a = 1, 3) => error.
		if (uses_named_parameters)
		{
			SEMA_ERROR(expr, "A regular parameter cannot follow a named parameter.");
			return false;
		}

		// 10. If we exceed the function parameter count (remember we reduced this by one
		//     in the case of typed vararg) we're now in a variadic list.
		if (i >= func_param_count)
		{
			// 11. We might have a typed variadic argument.
			if (variadic_type)
			{
				// 11a. Look if we did an unsplat
				if (expr->call_expr.unsplat_last)
				{
					// 11b. Is this the last argument, or did we get some before the unpack?
					if (i < num_args - 1)
					{
						SEMA_ERROR(arg, "This looks like a variable argument before an unpacked variable which isn't allowed. Did you add too many arguments?");
						return false;
					}
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
				actual_args[i] = arg;
				expr->failable |= arg->failable;
				continue;
			}
			// 12. We might have a naked variadic argument
			if (signature->variadic)
			{
				// 12a. Analyse the expression.
				if (!sema_analyse_expr(context, NULL, arg)) return false;

				// 12b. In the case of a compile time variable we cast to c_int / double.
				Type *arg_type = arg->type->canonical;
				if (type_is_ct(arg_type))
				{
					// 12c. Pick double / CInt
					Type *target_type = type_is_any_integer(arg_type) ? type_c_int->canonical : type_double;
					if (!cast_implicit(arg, target_type)) return false;
					arg_type = target_type;
				}
				// 12d. Promote any integer or bool to at least CInt
				if (type_is_promotable_integer(arg_type) || arg_type == type_bool)
				{
					cast(arg, type_c_int->canonical);
					arg_type = type_c_int->canonical;
				}
				// 12e. Promote any float to at least Double
				if (type_is_promotable_float(arg->type))
				{
					cast(arg, type_double);
					arg_type = type_double;
				}
				// Set the argument at the location.
				actual_args[i] = arg;
				expr->failable |= arg->failable;
				continue;
			}
			// 13. We have too many parameters...
			SEMA_ERROR(expr, "Too many parameters for this function.");
			return false;
		}

		// 14. Analyse a regular argument.
		if (!sema_analyse_expr_of_required_type(context, func_params[i]->type, arg, true)) return false;
		expr->failable |= arg->failable;
		actual_args[i] = arg;
	}

	// 15. Set default values.
	for (unsigned i = 0; i < entries_needed; i++)
	{
		// 15a. Assigned a value - skip
		if (actual_args[i]) continue;

		// 15b. Set the init expression.
		if (func_params[i]->var.init_expr)
		{
			assert(func_params[i]->var.init_expr->resolve_status == RESOLVE_DONE);
			actual_args[i] = func_params[i]->var.init_expr;
			continue;
		}

		// 15c. Vararg not set? That's fine.
		if (func_params[i]->var.vararg) continue;

		// 15d. Argument missing, that's bad.
		SEMA_ERROR(expr, "Parameter '%s' was not set.", func_params[i]->name);
		return false;
	}
	// 16. Set the type and failable.
	expr_set_type(expr, signature->rtype->type);
	expr->call_expr.arguments = actual_args;
	expr->failable |= signature->failable;
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

static inline bool sema_expr_analyse_generic_call(Context *context, Type *to, Decl *decl, Expr *expr)
{
	Expr **arguments = expr->call_expr.arguments;
	Decl **parameter_list = decl->generic_decl.parameters;
	if (vec_size(parameter_list) != vec_size(arguments))
	{
		SEMA_ERROR(expr, "Expected %d parameter(s) to the generic function.", vec_size(parameter_list));
		return false;
	}
	VECEACH(arguments, i)
	{
		if (!sema_analyse_expr(context, NULL, arguments[i])) return false;
	}

	TODO
};




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
	return sema_expr_analyse_func_invocation(context, &decl->func.function_signature, expr, decl, to, struct_var);
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

static inline bool sema_expr_analyse_macro_call(Context *context, Type *to, Expr *call_expr, Decl *decl)
{
	// TODO failable
	if (context->macro_scope.depth >= MAX_MACRO_NESTING)
	{
		SEMA_ERROR(call_expr, "Too deep nesting (more than %d levels) when evaluating this macro.", MAX_MACRO_NESTING);
		return false;
	}

	if (decl->has_body_param && !call_expr->call_expr.body)
	{
		SEMA_ERROR(call_expr, "Expected call to have a trailing statement, did you forget to add it?");
		return false;
	}
	if (!decl->has_body_param && call_expr->call_expr.body)
	{
		SEMA_ERROR(call_expr, "This macro does not support trailing statements, please remove it.");
		return false;
	}

	Expr **args = call_expr->call_expr.arguments;
	Decl **func_params = decl->macro_decl.parameters;

	unsigned num_args = vec_size(args);
	if (num_args != vec_size(func_params))
	{
		// TODO
		SEMA_ERROR(call_expr, "Mismatch on number of arguments.");
		return false;
	}
	Decl **params = num_args > 0 ? VECNEW(Decl *, num_args) : NULL;
	for (unsigned i = 0; i < num_args; i++)
	{
		Expr *arg = args[i];
		Decl *param = copy_decl(func_params[i]);
		vec_add(params, param);
		assert(param->decl_kind == DECL_VAR);
		assert(param->resolve_status == RESOLVE_DONE);
		param->resolve_status = RESOLVE_RUNNING;
		// Maybe there's a type, but in general a macro param may be
		// typeless.
		// Maybe we should actually do something like:
		// macro var foo(var a, var $b) to make it more uniform.
		if (param->var.type_info)
		{
			// Resolve it
			if (!sema_resolve_type_info(context, param->var.type_info)) return false;
			// And set the type, we're done.
			param->type = param->var.type_info->type;
		}
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
				if (!sema_analyse_expr_of_required_type(context, param->type, arg, false)) return false;
				param->alignment = type_abi_alignment(param->type ? param->type : arg->type);
				break;
			case VARDECL_PARAM_EXPR:
				// #foo
				// We push a scope here as this will prevent the expression from modifying
				// compile time variables during evaluation:
				SCOPE_START
					if (!sema_analyse_expr_of_required_type(context, param->type, arg, false)) return SCOPE_POP_ERROR();
				SCOPE_END;
				break;
			case VARDECL_PARAM_CT:
				// $foo
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
		param->var.init_expr = arg;
		param->resolve_status = RESOLVE_DONE;
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


			for (unsigned i = 0; i < num_args; i++)
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



static inline bool sema_expr_analyse_call(Context *context, Type *to, Expr *expr)
{
	expr->constant = false;
	expr->pure = false;

	Expr *func_expr = expr->call_expr.function;
	if (!sema_analyse_expr_value(context, NULL, func_expr)) return false;
	expr->failable = func_expr->failable;
	Decl *decl;
	Expr *struct_var = NULL;
	switch (func_expr->expr_kind)
	{
		case EXPR_IDENTIFIER:
			decl = func_expr->identifier_expr.decl;
			break;
		case EXPR_ACCESS:
			decl = func_expr->access_expr.ref;
			if (decl->decl_kind == DECL_FUNC)
			{
				expr->call_expr.is_type_method = true;
				struct_var = expr_new(EXPR_UNARY, func_expr->access_expr.parent->span);
				struct_var->unary_expr.expr = func_expr->access_expr.parent;
				struct_var->unary_expr.operator = UNARYOP_ADDR;
				struct_var->resolve_status = RESOLVE_DONE;
				assert(func_expr->access_expr.parent->resolve_status == RESOLVE_DONE);
				expr_set_type(struct_var, type_get_ptr(struct_var->unary_expr.expr->type));
			}
			break;
		case EXPR_MACRO_IDENTIFIER:
			return sema_expr_analyse_macro_call(context, to, expr, func_expr->identifier_expr.decl);
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
	switch (decl->decl_kind)
	{
		case DECL_VAR:
			return sema_expr_analyse_var_call(context, to, expr, decl);
		case DECL_FUNC:
			return sema_expr_analyse_func_call(context, to, expr, decl, struct_var);
		case DECL_MACRO:
			UNREACHABLE
		case DECL_GENERIC:
			return sema_expr_analyse_generic_call(context, to, decl, expr);
		case DECL_POISONED:
			return false;
		default:
			break;
	}
	SEMA_ERROR(expr, "The expression cannot be called.");
	return false;
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

static inline void insert_access_deref(Expr *expr)
{
	Expr *deref = expr_new(EXPR_UNARY, expr->span);
	deref->unary_expr.operator = UNARYOP_DEREF;
	deref->unary_expr.expr = expr->access_expr.parent;
	deref->resolve_status = RESOLVE_DONE;
	assert(expr->access_expr.parent->type->canonical->type_kind == TYPE_POINTER);
	expr_set_type(deref, expr->access_expr.parent->type->canonical->pointer);
	deref->failable = expr->access_expr.parent->failable;
	expr->access_expr.parent = deref;
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
	VECEACH(decl->methods, i)
	{
		Decl *func = decl->methods[i];
		sema_add_member(context, func);
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
static inline bool sema_expr_analyse_type_access(Expr *expr, TypeInfo *parent, bool was_group)
{
	if (!was_group && type_kind_is_derived(parent->type->type_kind))
	{
		SEMA_ERROR(expr->access_expr.parent, "Array and pointer types must be enclosed in (), did you forget it?");
		return false;
	}

	expr->constant = true;
	expr->pure = true;

	Type *canonical = parent->type->canonical;
	TokenType type = TOKTYPE(expr->access_expr.sub_element);
	const char *name = TOKSTR(expr->access_expr.sub_element);
	if (type == TOKEN_TYPEID)
	{
		expr_set_type(expr, type_typeid);
		expr->expr_kind = EXPR_TYPEID;
		expr->typeid_expr = parent;
		expr->resolve_status = RESOLVE_DONE;
		return true;
	}
	if (name == kw_sizeof)
	{
		expr_rewrite_to_int_const(expr, type_compint, type_size(canonical));
		return true;
	}
	if (name == kw_alignof)
	{
		expr_rewrite_to_int_const(expr, type_compint, type_abi_alignment(canonical));
		return true;
	}
	if (name == kw_nameof)
	{
		expr_rewrite_to_string(expr, canonical->name);
		return true;
	}
	if (name == kw_qnameof)
	{
		expr_rewrite_to_string(expr, type_generate_qname(canonical));
		return true;
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
				if (!sema_expr_analyse_enum_constant(expr, expr->access_expr.sub_element, decl))
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
	VECEACH(decl->strukt.members, i)
	{
		Decl *member = decl->strukt.members[i];
		if (name == member->name)
		{
			expr->expr_kind = EXPR_MEMBER_ACCESS;
			expr->access_expr.ref = member;
			expr_set_type(expr, member->type);
			return true;
		}
	}
	SEMA_ERROR(expr, "No function or member '%s.%s' found.", type_to_error_string(parent->type), name);
	return false;
}

static inline bool sema_expr_analyse_member_access(Context *context, Expr *expr)
{
	Expr *parent = expr->access_expr.parent;

	expr->constant = true;
	expr->pure = true;

	TokenType type = TOKTYPE(expr->access_expr.sub_element);
	const char *name = TOKSTR(expr->access_expr.sub_element);

	Decl *ref = parent->access_expr.ref;

	bool is_plain_member = ref->decl_kind == DECL_VAR;
	if (type == TOKEN_TYPEID)
	{
		expr_set_type(expr, type_typeid);
		expr->expr_kind = EXPR_TYPEID;
		if (is_plain_member)
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
	if (name == kw_sizeof)
	{
		expr_rewrite_to_int_const(expr, type_usize, type_size(ref->type));
		return true;
	}
	if (name == kw_alignof)
	{
		expr_rewrite_to_int_const(expr, type_usize, type_abi_alignment(ref->type));
		return true;
	}
	if (name == kw_ordinal)
	{
		if (ref->decl_kind == DECL_ENUM_CONSTANT)
		{
			expr_rewrite_to_int_const(expr, type_usize, ref->enum_constant.ordinal);
			return true;
		}
	}
	if (name == kw_nameof)
	{
		expr_rewrite_to_string(expr, ref->name);
		return true;
	}
	if (name == kw_qnameof)
	{
		TODO
	}
	if (name == kw_kindof)
	{
		TODO
	}
	// If we have something like struct Foo { Bar b; int c; struct d { int e; } }
	// If we are inspecting Foo.c we're done here. Otherwise handle struct d / Bar b case
	// The same way.
	if (is_plain_member)
	{
		if (!type_is_structlike(ref->type))
		{
			SEMA_ERROR(expr, "'%s' does not have a member or property '%s'", type_to_error_string(ref->type), name);
			return false;
		}
		// Pretend to be an inline struct.
		ref = ref->type->decl;
	}
	VECEACH(ref->methods, i)
	{
		Decl *function = ref->methods[i];
		if (name == function->name)
		{
			expr->access_expr.ref = function;
			expr_set_type(expr, function->type);
			return true;
		}
	}
	VECEACH(ref->strukt.members, i)
	{
		Decl *member = ref->strukt.members[i];
		if (name == member->name)
		{
			expr->expr_kind = EXPR_MEMBER_ACCESS;
			expr->access_expr.ref = member;
			expr_set_type(expr, member->type);
			return true;
		}
	}
	SEMA_ERROR(expr, "No function or member '%s.%s' found.", "todo", name);
	TODO
	return false;
}


static inline bool sema_expr_analyse_access(Context *context, Expr *expr)
{
	Expr *parent = expr->access_expr.parent;
	bool was_group = parent->expr_kind == EXPR_GROUP;
	if (!sema_analyse_expr_value(context, NULL, parent)) return false;

	if (parent->type == type_typeinfo)
	{
		return sema_expr_analyse_type_access(expr, parent->type_expr, was_group);
	}
	if (parent->expr_kind == EXPR_MEMBER_ACCESS)
	{
		return sema_expr_analyse_member_access(context, expr);
	}
	expr->failable = parent->failable;

	assert(expr->expr_kind == EXPR_ACCESS);
	assert(parent->resolve_status == RESOLVE_DONE);

	Type *parent_type = parent->type;
	Type *type = type_flatten(parent_type);

	bool is_pointer = type->type_kind == TYPE_POINTER;
	if (is_pointer)
	{
		type = type_flatten(type->pointer);
		if (!sema_cast_rvalue(context, NULL, parent)) return false;
		insert_access_deref(expr);
		parent = expr->access_expr.parent;
	}
	const char *kw = TOKSTR(expr->access_expr.sub_element);
	Expr *current_parent = parent;
CHECK_DEEPER:

	switch (type->type_kind)
	{
		case TYPE_SUBARRAY:
			if (kw == kw_sizeof)
			{
				expr_rewrite_to_int_const(expr, type_usize, type_size(type));
				return true;
			}
			if (kw == kw_len)
			{
				expr->expr_kind = EXPR_LEN;
				expr->len_expr.inner = parent;
				expr_set_type(expr, type_void);
				expr->resolve_status = RESOLVE_DONE;
				return true;
			}
			goto NO_MATCH;
		case TYPE_ARRAY:
			if (kw == kw_sizeof)
			{
				expr_rewrite_to_int_const(expr, type_usize, type_size(type));
				return true;
			}
			if (kw == kw_len)
			{
				expr_rewrite_to_int_const(expr, type_usize, type->array.len);
				return true;
			}
			goto NO_MATCH;
		case TYPE_ENUM:
		case TYPE_ERRTYPE:
		case TYPE_STRUCT:
		case TYPE_UNION:
			break;
		default:
		NO_MATCH:
			SEMA_ERROR(expr, "There is no member or method '%s' on '%s'", TOKSTR(expr->access_expr.sub_element), type_to_error_string(parent_type));
			return false;
	}

	Decl *decl = type->decl;
	Decl *member;
	SCOPE_START
		add_members_to_context(context, decl);
		member = sema_resolve_symbol_in_current_dynamic_scope(context, kw);
	SCOPE_END;


	if (!member)
	{
		// We have a potential embedded struct check:
		if (type_is_substruct(type))
		{
			Expr *embedded_struct = expr_access_inline_member(parent, type->decl);
			current_parent = embedded_struct;
			type = embedded_struct->type->canonical;
			goto CHECK_DEEPER;
		}
		SEMA_ERROR(expr, "There is no field or method '%s.%s'.", decl->name, kw);
		return false;
	}
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
		debug_dump_const_initializer(const_init, "TOP", 0);
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
	if (!sema_analyse_expr_of_required_type(context, left_type, right, lhs_is_failable != FAILABLE_NO)) return false;

	// 2. Set the result to the type on the right side.
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
	if (!sema_analyse_expr_value(context, NULL, left)) return false;

	// 2. Check assignability
	if (!expr_is_ltype(left))
	{
		SEMA_ERROR(left, "Expression is not assignable.");
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
		case ALL_UNSIGNED_INTS:
			if (type->builtin.bitsize < platform_target.width_c_int) return type_c_int->canonical;
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
		SEMA_ERROR(parent, error_message, type_to_error_string(left_type), type_to_error_string(right_type));
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
	if (!binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, "Cannot subtract '%s' from '%s'"))
	{
		return false;
	}

	left_type = left->type->canonical;

	expr_unify_binary(expr, left, right);

	// 8. Handle constant folding.
	if (both_const(left, right))
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.kind = left_type->type_kind;
		expr_copy_types(expr, left);
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
	if (!binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, "Cannot add '%s' to '%s'"))
	{
		return false;
	}

	// 5. Handle the "both const" case. We should only see ints and floats at this point.
	if (both_const(left, right))
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.kind = left->type->canonical->type_kind;
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
	if (!binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, "Cannot multiply '%s' by '%s'"))
	{
		return false;
	}

	// 3. Handle constant folding.
	if (both_const(left, right))
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.kind = left->type->canonical->type_kind;

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
	if (!binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, "Cannot divide '%s' by '%s'."))
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
		expr->expr_kind = EXPR_CONST;
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
	if (!binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, "Cannot divide '%s' by '%s'."))
	{
		return false;
	}

	// 3. a % 0 is not valid, so detect it.
	if (is_const(right) && bigint_cmp_zero(&right->const_expr.i) == CMP_EQ)
	{
		SEMA_ERROR(expr->binary_expr.right, "Cannot perform % with a constant zero.");
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
	if (!sema_analyse_expr(context, type_bool, left) & sema_analyse_expr(context, type_bool, right)) return false;
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
				case TYPE_MEMBER:
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
		case EXPR_MACRO_CT_IDENTIFIER:
			SEMA_ERROR(inner, "It's not possible to take the address of a compile time value.");
			return false;
		case EXPR_MACRO_IDENTIFIER:
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
		case TYPE_MEMBER:
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


static inline bool sema_expr_analyse_binary(Context *context, Type *to, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_RUNNING);
	Expr *left = expr->binary_expr.left;
	Expr *right = expr->binary_expr.right;
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


static inline bool sema_expr_analyse_try(Context *context, Expr *expr)
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

static inline bool sema_expr_analyse_catch(Context *context, Expr *expr)
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
	if (!sema_analyse_expr(context, NULL, inner)) return false;
	expr->pure = inner->pure;
	expr->constant = inner->constant;
	if (inner->expr_kind == EXPR_TYPEINFO)
	{
		TypeInfo *inner_type_info = inner->type_expr;
		if (inner_type_info->type->type_kind != TYPE_ERRTYPE)
		{
			SEMA_ERROR(inner, "This must be an error type.");
			return false;
		}
		inner->expr_kind = EXPR_COMPOUND_LITERAL;
		inner->expr_compound_literal.type_info = inner_type_info;
		inner->expr_compound_literal.initializer = NULL;
		expr_set_type(inner, inner_type_info->type);
	}
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


static inline bool sema_analyse_expr_dispatch(Context *context, Type *to, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_DECL_LIST:
		case EXPR_UNDEF:
		case EXPR_ENUM_CONSTANT:
		case EXPR_MEMBER_ACCESS:
		case EXPR_DESIGNATOR:
			UNREACHABLE
		case EXPR_HASH_IDENT:
			return sema_expr_analyse_hash_identifier(context, to, expr);
		case EXPR_MACRO_CT_IDENTIFIER:
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
		case EXPR_CATCH:
			return sema_expr_analyse_catch(context, expr);
		case EXPR_TRY:
			return sema_expr_analyse_try(context, expr);
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
		case EXPR_CONST_IDENTIFIER:
		case EXPR_IDENTIFIER:
		case EXPR_MACRO_IDENTIFIER:
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
		case EXPR_MACRO_CT_IDENTIFIER:
			SEMA_ERROR(expr, "Expected compile time macro variable '%s' followed by (...).", expr->ct_macro_ident_expr.identifier);
			return expr_poison(expr);
		case EXPR_MACRO_IDENTIFIER:
			SEMA_ERROR(expr, "Expected macro '%s' followed by (...).", expr->macro_identifier_expr.identifier);
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

