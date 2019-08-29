// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "compiler_internal.h"

static bool sema_resolve_type(Context *context, Type *type);
static bool sema_analyse_expression(Context *context, Expr *expr);




void sema_init(File *file)
{
	LOG_FUNC
}

/**
 * Symmetrically promotes one expression by inserting an implicit cast.
 *
 * @param expr1
 * @param expr2
 * @return false if an implicit cast is not allowed.
 */
static bool type_promote(Expr *expr1, Expr *expr2)
{
	if (expr1->type->canonical == expr2->type->canonical) return true;
	TODO
}

/**
 * Check if a type is contained in another type.
 *
 * @param type
 * @param possible_subtype
 * @return true if it is a subtype
 */
static bool canonical_type_is_subtype(Type *type, Type *possible_subtype)
{
	assert(type == type->canonical && possible_subtype == possible_subtype->canonical);
	if (type == possible_subtype) return true;
	if (type->type_kind != possible_subtype->type_kind) return false;
	if (type->type_kind != TYPE_USER_DEFINED || type->decl->decl_kind != DECL_STRUCT) return false;

	if (!possible_subtype->decl->strukt.members) return false;

	Decl *first_element = possible_subtype->decl->strukt.members[0];

	if (first_element->decl_kind != DECL_VAR) return false;

	return canonical_type_is_subtype(type, first_element->var.type->canonical);
}

static bool cast_const_type_expression(Expr *expr, Type *type)
{
	Type *canonical = type->canonical;
	ExprConst *expr_const = &expr->const_expr;
	switch (canonical->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_USER_DEFINED:
		case TYPE_EXPRESSION:
		case TYPE_INC_ARRAY:
			UNREACHABLE;
			break;
		case TYPE_ARRAY:
			TODO
			UNREACHABLE
		case TYPE_VARARRAY:
			TODO
			UNREACHABLE;
		case TYPE_POINTER:

			TODO
			UNREACHABLE;
		case TYPE_VOID:
		case TYPE_BOOL:
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
		case TYPE_IXX:
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
		case TYPE_UXX:
		case TYPE_F32:
		case TYPE_F64:
		case TYPE_FXX:
		{
			TODO
		//	BuiltinConv conv = BUILTIN_CONVERSION[expr->type->type_kind - TYPE_VOID][canonical->type_kind - TYPE_VOID];
		//	builtin_convert(&expr->const_expr, conv, canonical);
			expr->type = type;
			return true;
		}
		case TYPE_STRING:
			break;
	}

	TODO
}

static bool expr_insert_cast(Expr *expr, Type *canonical)
{
	assert(canonical == canonical->canonical);
	if (expr->expr_kind == EXPR_CONST) return cast_const_type_expression(expr, canonical);
	TODO
}


typedef enum _AssignCast
{
	ASSIGN_CAST_FAILED,
	ASSIGN_CAST_FAILED_ERROR_SHOWN,
	ASSIGN_CAST_SKIP,
	ASSIGN_CAST,
} AssignCast;

/**
 * Pick the cast to do.
 *
 * @param expr1
 * @param expr2
 * @return AssignCast
 */
static inline bool type_assign_cast_type(Expr *expr, Type *target_type, bool is_update)
{
	return cast(expr, target_type, CAST_TYPE_IMPLICIT_ASSIGN);
	/*
	Type *right_type = target_type->canonical;
	Type *left_type = expr->type->canonical;
	if (right_type == left_type) return true;

	// X = Y where Y is subtype, this is truncation.
	if (canonical_type_is_subtype(left_type, right_type))
	{
		insert_cast(expr, CAST_TRUNC, target_type);
		return true;
	}

	// X* = void*
	if (right_type == &type_voidptr)
	{
		if (left_type->type_kind == TYPE_POINTER)
		{
			insert_cast(expr, CAST_PTRPTR, target_type);
			return true;
		}
		return false;
	}

	// void* = X*, void* = X[], void* = X[n], void* = string
	if (left_type == &type_voidptr)
	{
		switch (left_type->type_kind)
		{
			case TYPE_POINTER:
				insert_cast(expr, CAST_PTRPTR, target_type);
				return true;
			case TYPE_VARARRAY:
				insert_cast(expr, CAST_VARRPTR, target_type);
				return true;
			case TYPE_ARRAY:
				insert_cast(expr, CAST_ARRPTR, target_type);
				return true;
			case TYPE_STRING:
				insert_cast(expr, CAST_STRPTR, target_type);
			default:
				return false;
		}
	}

	// X* = X[n], X* = X[], X* = Y[], X* = Y[n], X* = Y* if subtype
	if (left_type->type_kind == TYPE_POINTER)
	{
		switch (right_type->type_kind)
		{
			case TYPE_ARRAY:
				insert_cast(expr, CAST_ARRPTR, target_type);
				return true;
			case TYPE_VARARRAY:
				insert_cast(expr, CAST_VARRPTR, target_type);
				return true;
			case TYPE_POINTER:
				if (canonical_type_is_subtype(left_type->base, right_type->base)) return false;
				insert_cast(expr, CAST_PTRPTR, target_type);
				return true;
			case TYPE_STRING:
				if (left_type->type_kind == TYPE_I8 || left_type->type_kind == TYPE_U8)
				{
					insert_cast(expr, CAST_STRPTR, target_type);
					return true;
				}
				return false;
			default:
				return false;
		}
	}

	if (!type_is_builtin(left_type->type_kind) || !type_is_builtin(right_type->type_kind)) return false;

	return cast(expr, right_type, target_type, CAST_TYPE_IMPLICIT_ASSIGN);*/
}

/**
 * Tries to implicitly cast the expression to the recipient type (return, assignment)
 *
 * @param expr1
 * @param expr2
 * @return false if an implicit cast is not allowed.
 */
static bool type_assign_cast(Expr *expr, Type *type, bool is_update)
{
	assert(type->canonical == type);
	if (!type_assign_cast_type(expr, type, is_update))
	{
		expr_poison(expr);
		return false;
	}
	return true;
}

static bool insert_assign_cast_if_needed(Expr *expr, Type *type)
{
	assert(type->canonical == type);
	if (expr->type->canonical == type) return true;
	if (!type_assign_cast_type(expr, type, false))
	{
		expr_poison(expr);
		return false;
	}
	return true;
}

static bool insert_arith_cast_if_needed(Expr *left, Expr *right, Type *left_type)
{
	Type *right_type = right->type->canonical;
	assert(left_type->canonical == left_type);
	if (right_type == left_type) return true;
	TODO
}

static bool insert_bool_cast_if_needed(Expr *expr)
{
	Type *canonical = expr->type->canonical;
	if (canonical == &type_bool) return true;
	TODO
}

static void show_shadow_error(Decl *decl, Decl *old)
{
	sema_error_range(decl->name.span, "The '%s' would shadow a previous declaration.", decl->name.string);
	sema_prev_at_range(old->name.span, "The previous use of '%s' was here.", decl->name.string);
}

static inline Decl *module_find_symbol(Module *module, const char *symbol)
{
	return stable_get(&module->symbols, symbol);
}


static Decl *context_find_ident(Context *context, const char *symbol)
{
	Decl **first = &context->locals[0];
	Decl **current = context->last_local - 1;
	while (current >= first)
	{
		if (current[0]->name.string == symbol) return current[0];
		current--;
	}
	Decl *found = module_find_symbol(context->module, symbol);
	if (found) return found;
	// TODO check imports
	return NULL;
}


static inline void context_push_scope(Context *context)
{
	if (context->current_scope == &context->scopes[MAX_SCOPE_DEPTH - 1])
	{
		FATAL_ERROR("Too deeply nested scopes.");
	}
	context->current_scope++;
	context->current_scope->local_decl_start = context->last_local;
}

static inline void context_pop_scope(Context *context)
{
	assert(context->current_scope != &context->scopes[0]);
	context->last_local = context->current_scope->local_decl_start;
	context->current_scope--;
}



static bool sema_resolve_ptr_type(Context *context, Type *type)
{
	if (!sema_resolve_type(context, type->base))
	{
		type_poison(type);
		return false;
	}
	type->canonical = type_get_canonical_ptr(type);
	type->resolve_status = RESOLVE_DONE;
	return true;
}

static bool sema_resolve_array_type(Context *context, Type *type)
{
	if (!sema_resolve_type(context, type->base))
	{
		type_poison(type);
		return false;
	}
	type->canonical = type_get_canonical_array(type);
	type->resolve_status = RESOLVE_DONE;
	return true;
}

static bool sema_resolve_type(Context *context, Type *type)
{
    LOG_FUNC

	if (type->resolve_status == RESOLVE_DONE) return type_ok(type);

	if (type->resolve_status == RESOLVE_RUNNING)
	{
		SEMA_ERROR(type->name_loc, "Circular dependency resolving type '%s'.", type->name_loc);
		type_poison(type);
		return false;
	}

	type->resolve_status = RESOLVE_RUNNING;

	switch (type->type_kind)
	{
		case TYPE_POISONED:
		case TYPE_INC_ARRAY:
			UNREACHABLE
		case TYPE_USER_DEFINED:
			break;
		case TYPE_POINTER:
			return sema_resolve_ptr_type(context, type);
		case TYPE_ARRAY:
			return sema_resolve_array_type(context, type);
		default:
			TODO
	}

	Decl *decl = stable_get(&context->local_symbols, type->name_loc.string);
	if (!decl)
	{
		decl = module_find_symbol(context->module, type->name_loc.string);
	}

	if (!decl)
	{
		SEMA_ERROR(type->name_loc, "Unknown type '%s'.", type->name_loc.string);
		type_poison(type);
		return false;
	}

	switch (decl->decl_kind)
	{
		case DECL_STRUCT:
		case DECL_UNION:
		case DECL_ERROR:
		case DECL_ENUM:
			type->decl = decl;
			type->canonical = decl->self_type;
			type->resolve_status = RESOLVE_DONE;
			DEBUG_LOG("Resolved %s.", type->name_loc.string);
			return true;
		case DECL_TYPEDEF:
			// TODO func
			if (!sema_resolve_type(context, decl->typedef_decl.type))
			{
				decl_poison(decl);
				type_poison(type);
				return false;
			}
			type->decl = decl;
			type->canonical = decl->typedef_decl.type->canonical;
			type->resolve_status = RESOLVE_DONE;
			DEBUG_LOG("Resolved %s.", type->name_loc.string);
			return true;
		case DECL_POISONED:
			type_poison(type);
			return false;
		case DECL_FUNC:
		case DECL_VAR:
		case DECL_ENUM_CONSTANT:
		case DECL_ERROR_CONSTANT:
		case DECL_ARRAY_VALUE:
		case DECL_IMPORT:
		case DECL_MACRO:
		case DECL_GENERIC:
			SEMA_ERROR(type->name_loc, "This is not a type.");
			type_poison(type);
			return false;
		case DECL_MULTI_DECL:
		case DECL_CT_ELSE:
		case DECL_CT_IF:
		case DECL_CT_ELIF:
			UNREACHABLE
	}
	UNREACHABLE
}

static inline bool sema_analyse_struct_member(Context *context, Decl *decl)
{
	LOG_FUNC
	assert(decl->decl_kind == DECL_VAR);
	assert(decl->var.kind == VARDECL_MEMBER);
	assert(!decl->var.init_expr);
	if (!sema_resolve_type(context, decl->var.type))
	{
		decl_poison(decl);
		return false;
	}
	assert(decl->var.type->canonical);
	return true;
}
static inline void sema_analyse_struct(Context *context, Decl *decl)
{
	LOG_FUNC
	DEBUG_LOG("Beginning analysis of %s.", decl->name.string);
	assert(decl->decl_kind == DECL_STRUCT);
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
	DEBUG_LOG("Analysing complete.");
}

static bool constant_fold(Context *context, Expr *expr)
{
	if (expr->expr_kind == EXPR_CONST) return true;
	switch (expr->expr_kind)
	{
		case EXPR_POISONED:
			return false;
		case EXPR_TRY:
			if ((unsigned)sema_analyse_expression(context, expr->try_expr.expr) &
				(unsigned)(!expr->try_expr.else_expr || sema_analyse_expression(context, expr->try_expr.else_expr)))
			{
				return true;
			}
			else
			{
				expr_poison(expr);
				return false;
			}
		case EXPR_CONST:
			return true;
		case EXPR_BINARY:
			TODO
			break;
		case EXPR_CONDITIONAL:
			break;
		case EXPR_UNARY:
			break;
		case EXPR_POST_UNARY:
			if (!sema_analyse_expression(context, expr->post_expr.expr))
		case EXPR_TYPE:
			break;
		case EXPR_IDENTIFIER:
			break;
		case EXPR_METHOD_REF:
			break;
		case EXPR_CALL:
			break;
		case EXPR_SIZEOF:
			break;
		case EXPR_SUBSCRIPT:
			break;
		case EXPR_ACCESS:
			break;
		case EXPR_STRUCT_VALUE:
			break;
		case EXPR_STRUCT_INIT_VALUES:
			break;
		case EXPR_INITIALIZER_LIST:
			break;
		case EXPR_EXPRESSION_LIST:
			break;
		case EXPR_DEFERRED_TOKENS:
			break;
		case EXPR_CAST:
			break;
	}
	TODO
	return true;
}
/*
static inline void add_integers(ExprConst *result, uint64_t l, bool l_is_negative, uint64_t r, bool r_is_negative)
{
	if (l_is_negative == r_is_negative)
	{
		result->type = l_is_negative ? CONST_NEGATIVE_INT : CONST_POSITIVE_INT;
		result->integer.i = l + r;
		return;
	}
	if (l > r)
	{
		result->integer.i = l - r;
		result->type = l_is_negative ? CONST_NEGATIVE_INT : CONST_POSITIVE_INT;
	}
	else
	{
		result->integer.i = r - l;
		result->type = l_is_negative ? CONST_POSITIVE_INT : CONST_NEGATIVE_INT;
	}
}
*/
static inline bool sema_analyse_binary_expr_internal_const(Context *context, Expr *expr)
{
	return constant_fold(context, expr);
}

static inline bool cast_to_common_for_operator(BinOp op, Expr *left, Expr *right)
{
	switch (op)
	{
		case BINOP_ERROR:
			return false;
		case BINOP_ASSIGN:
			return insert_assign_cast_if_needed(right, left->type->canonical);
		case BINOP_MULT:
		case BINOP_ADD:
		case BINOP_SUB:
		case BINOP_DIV:
		case BINOP_MOD:
		case BINOP_BIT_OR:
		case BINOP_BIT_AND:
		case BINOP_BIT_XOR:
		case BINOP_NE:
		case BINOP_EQ:
		case BINOP_GE:
		case BINOP_GT:
		case BINOP_LE:
		case BINOP_LT:
			return insert_arith_cast_if_needed(left, right, left->type->canonical);
		case BINOP_MULT_ASSIGN:
			return insert_arith_cast_if_needed(left, right, left->type->canonical);
		case BINOP_ADD_ASSIGN:
			break;
		case BINOP_SUB_ASSIGN:
			break;
		case BINOP_DIV_ASSIGN:
			break;
		case BINOP_MOD_ASSIGN:
		case BINOP_AND_ASSIGN:
		case BINOP_OR_ASSIGN:
			break;
		case BINOP_AND:
		case BINOP_OR:
			return insert_bool_cast_if_needed(left) & insert_bool_cast_if_needed(right);
			break;
		case BINOP_BIT_AND_ASSIGN:
			break;
		case BINOP_BIT_OR_ASSIGN:
			break;
		case BINOP_BIT_XOR_ASSIGN:
			break;
		case BINOP_SHR:
		case BINOP_SHL:
			TODO
		case BINOP_SHR_ASSIGN:
			break;
		case BINOP_SHL_ASSIGN:
			break;
		case BINOP_ELVIS:
			TODO
	}
	TODO
}

static inline bool sema_analyse_binary_expr_internal(Context *context, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_RUNNING);
	Expr *left = expr->binary_expr.left;
	Expr *right = expr->binary_expr.right;
	bool success = sema_analyse_expression(context, left);
	success = success && sema_analyse_expression(context, right);
	if (!success)
	{
		expr_poison(expr);
		return false;
	}
	if (!cast_to_common_for_operator(expr->binary_expr.operator, left, right))
	{
		expr_poison(expr);
		return false;
	}
	if (left->expr_kind == EXPR_CONST && right->expr_kind == EXPR_CONST)
	{
		if (!sema_analyse_binary_expr_internal_const(context, expr))
		{
			expr_poison(expr);
			return false;
		}
		return true;
	}
	switch (expr->binary_expr.operator)
	{
		case BINOP_ERROR:
			break;
		case BINOP_ASSIGN:
			break;
		case BINOP_MULT:
			break;
		case BINOP_MULT_ASSIGN:
			break;
		case BINOP_ADD:
			break;
		case BINOP_ADD_ASSIGN:
			break;
		case BINOP_SUB:
			break;
		case BINOP_SUB_ASSIGN:
			break;
		case BINOP_DIV:
			break;
		case BINOP_DIV_ASSIGN:
			break;
		case BINOP_MOD:
			break;
		case BINOP_MOD_ASSIGN:
			break;
		case BINOP_AND:
			break;
		case BINOP_AND_ASSIGN:
			break;
		case BINOP_OR:
			break;
		case BINOP_OR_ASSIGN:
			break;
		case BINOP_BIT_AND:
			break;
		case BINOP_BIT_AND_ASSIGN:
			break;
		case BINOP_BIT_OR:
			break;
		case BINOP_BIT_OR_ASSIGN:
			break;
		case BINOP_BIT_XOR:
			break;
		case BINOP_BIT_XOR_ASSIGN:
			break;
		case BINOP_NE:
			break;
		case BINOP_EQ:
			break;
		case BINOP_GE:
			break;
		case BINOP_GT:
			break;
		case BINOP_LE:
			break;
		case BINOP_LT:
			break;
		case BINOP_SHR:
			break;
		case BINOP_SHR_ASSIGN:
			break;
		case BINOP_SHL:
			break;
		case BINOP_SHL_ASSIGN:
			break;
		case BINOP_ELVIS:
			break;
	}
	TODO
}

static inline bool sema_analyse_unary_expr_internal(Context *context, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_RUNNING);
	Expr *inner = expr->unary_expr.expr;
	if (!sema_analyse_expression(context, inner))
	{
		expr_poison(expr);
		return false;
	}
	Type *canonical_inner_type = inner->type->canonical;
	switch (expr->unary_expr.operator)
	{
		case UNARYOP_ERROR:
			UNREACHABLE;
		case UNARYOP_DEREF:
			TODO
			break;
		case UNARYOP_ADDR:
			TODO
			break;
		case UNARYOP_NEG:
			if (!type_is_signed(canonical_inner_type) && canonical_inner_type != &type_compuint)
			{
				bool is_signed = type_is_signed(canonical_inner_type);
				bool inneer = canonical_inner_type;
				SEMA_ERROR(expr->loc, "Cannot negate %s.", inner->type->name_loc.string);
				return false;
			}
			if (canonical_inner_type == &type_compuint)
			{
				TODO
			}
			if (inner->expr_kind == EXPR_CONST)
			{
				assert(inner->const_expr.type == CONST_INT);
				expr->const_expr.type = CONST_INT;
				expr->expr_kind = EXPR_CONST;
				expr->const_expr.i = ~inner->const_expr.i;
				expr->type = inner->type;
				return true;
			}
			return true;
		case UNARYOP_BITNEG:
			TODO
			/*
			if (canonical_inner_type->type_kind != TYPE_BUILTIN || !builtin_may_negate(canonical_inner_type))
			{
				SEMA_ERROR(expr->loc, "Cannot bit negate %s.", type_to_string(inner->type));
				return false;
			}*/
			if (inner->expr_kind == EXPR_CONST)
			{
				switch (inner->const_expr.type)
				{
					case CONST_NIL:
					case CONST_BOOL:
					case CONST_STRING:
					case CONST_FLOAT:
						UNREACHABLE
					case CONST_INT:

						//inner->const_expr.type = CONST_NEGATIVE_INT;
						break;
					//case CONST_NEGATIVE_INT:
						//inner->const_expr.type = CONST_POSITIVE_INT;
						break;
						TODO
				}
			}
			return true;
		case UNARYOP_NOT:
			if (inner->expr_kind == EXPR_CONST)
			{
				switch (inner->const_expr.type)
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
				expr->type = &type_bool;
				expr->expr_kind = EXPR_CONST;
				return true;
			}
			switch (canonical_inner_type->type_kind)
			{
				case TYPE_ARRAY:
				case TYPE_POINTER:
				case TYPE_VARARRAY:
					return true;
				default:
					SEMA_ERROR(expr->loc, "Cannot use 'not' on %s", inner->type->name_loc.string);
					return false;
			}
		case UNARYOP_INC:
			TODO
			break;
		case UNARYOP_DEC:
			TODO
			break;
	}
}
static inline bool sema_analyse_cast(Context *context, Expr *expr)
{
	Expr *inner = expr->expr_cast.expr;
	bool success = sema_analyse_expression(context, inner);
	success = sema_resolve_type(context, expr->type) && success;
	if (!success) return false;
	Type *canonical = expr->type->canonical;
	Type *inner_canonical = inner->type->canonical;
	if (canonical == inner_canonical) return true;
	//BUILTIN_CONVERSION
	TODO
}

static inline bool sema_analyse_expression(Context *context, Expr *expr)
{
	switch (expr->resolve_status)
	{
		case RESOLVE_NOT_DONE:
			expr->resolve_status = RESOLVE_RUNNING;
			break;
		case RESOLVE_RUNNING:
			SEMA_ERROR(expr->loc, "Recursive resolution of expression");
			expr_poison(expr);
			return false;
		case RESOLVE_DONE:
			return expr_ok(expr);
	}
	switch (expr->expr_kind)
	{
		case EXPR_POISONED:
			UNREACHABLE
		case EXPR_TRY:
			if (!sema_analyse_expression(context, expr->try_expr.expr))
			{
				expr_poison(expr);
				return false;
			}
			if (expr->try_expr.else_expr)
			{
				if (!sema_analyse_expression(context, expr->try_expr.else_expr))
				{
					expr_poison(expr);
					return false;
				}
				if (!type_promote(expr->try_expr.else_expr, expr->try_expr.expr))
				{
					expr_poison(expr);
					return false;
				}
			}
			expr->type = expr->try_expr.expr->type;
			expr->resolve_status = RESOLVE_DONE;
			return true;
		case EXPR_CONST:
			expr->resolve_status = RESOLVE_DONE;
			return true;
		case EXPR_BINARY:
			if (!sema_analyse_binary_expr_internal(context, expr))
			{
				expr_poison(expr);
				return false;
			}
			expr->resolve_status = RESOLVE_DONE;
			return true;
		case EXPR_CONDITIONAL:
			TODO
			break;
		case EXPR_UNARY:
			if (!sema_analyse_unary_expr_internal(context, expr))
			{
				expr_poison(expr);
				return false;
			}
			expr->resolve_status = RESOLVE_DONE;
			return true;
		case EXPR_POST_UNARY:
			break;
		case EXPR_TYPE:
			break;
		case EXPR_IDENTIFIER:
			break;
		case EXPR_METHOD_REF:
			break;
		case EXPR_CALL:
			break;
		case EXPR_SIZEOF:
			break;
		case EXPR_SUBSCRIPT:
			break;
		case EXPR_ACCESS:
			break;
		case EXPR_STRUCT_VALUE:
			break;
		case EXPR_STRUCT_INIT_VALUES:
			break;
		case EXPR_INITIALIZER_LIST:
			break;
		case EXPR_EXPRESSION_LIST:
			break;
		case EXPR_DEFERRED_TOKENS:
			break;
		case EXPR_CAST:
			if (!sema_analyse_cast(context, expr))
			{
				expr_poison(expr);
				return false;
			}
			return true;
	}
	{

	}
	TODO
	return false;
}

/**
 * Convert an expression to a given type using implicit conversion.
 *
 * @param expr the expression to be implictly converted
 * @param type the type to convert to
 * @return an expression with cast if needed, or NULL if an error has been sent and the conversion failed.
 */
Expr *expr_implicit_conversion(Expr *expr, Type *type)
{
	TODO
	/*
	assert(expr->type && expr_ok(expr));
	if (expr->type->type_kind == TYPE_NIL && type->type_kind == TYPE_POINTER)
	Type *expr_type = expr->type;
	if (type->type_kind == TYPE_POINTER && expr_type->type_kind == TYPE_POINTER)
	{
		if (expr_type)
		if (type->type_kind)
	}
	Decl *canonical_decl = type->canonical;
	if (type->canonical)
	if (expr->type->canonical->decl_kind == TYPE_BUILTIN && type->canonical->decl_kind == DECL_BUILTIN)
	{

	}
	TODO*/
	return NULL;
}

static inline bool sema_analyse_function_param(Context *context, Decl *param, bool is_function)
{
	if (!decl_ok(param)) return false;
	assert(param->decl_kind == DECL_VAR);
	assert(param->var.kind == VARDECL_PARAM);
	if (!sema_resolve_type(context, param->var.type))
	{
		return false;
	}
	if (param->var.init_expr && !is_function)
	{
		SEMA_ERROR(param->var.init_expr->loc, "Function types may not have default arguments.");
		return false;
	}
	if (param->var.init_expr)
	{
		Expr *expr = param->var.init_expr;
		if (!sema_analyse_expression(context, expr)) return false;
		if (expr->expr_kind != EXPR_CONST)
		{
			SEMA_ERROR(expr->loc, "Only constant expressions may be used as default values.");
			return false;
		}
		Expr *converted_expr = expr_implicit_conversion(expr, param->var.type);
		if (!converted_expr)
		{
			return false;
		}
		param->var.init_expr = converted_expr;
	}
	return true;
}

static inline bool sema_analyse_function_signature(Context *context, FunctionSignature *signature, bool is_function)
{
	bool all_ok = true;
	all_ok = sema_resolve_type(context, signature->rtype) && all_ok;
	// TODO check parameter name appearing more than once.
	VECEACH(signature->params, i)
	{
		if (!sema_analyse_function_param(context, signature->params[i], is_function))
		{
			decl_poison(signature->params[i]);
			all_ok = false;
		}
	}
	VECEACH(signature->throws, i)
	{
		TODO
	}
	return all_ok;
}

static bool sema_analyse_statement(Context *context, Ast *parent, Ast *statement);

static inline bool sema_analyse_compound_statement(Context *context, Ast *parent, Ast *compound_statement)
{
	LOG_FUNC
	bool all_ok = ast_ok(compound_statement);
	VECEACH(compound_statement->compound_stmt.stmts, i)
	{
		if (!sema_analyse_statement(context, compound_statement, compound_statement->compound_stmt.stmts[i]))
		{
			ast_poison(compound_statement->compound_stmt.stmts[i]);
			all_ok = false;
		}
	}
	if (parent->exit < compound_statement->exit)
	{
		parent->exit = compound_statement->exit;
	}
	return all_ok;
}

static inline bool sema_analyse_return_stmt(Context *context, Ast *parent, Ast *statement)
{
	LOG_FUNC
	parent->exit = EXIT_RETURN;
	Type *expected_rtype = context->active_function_for_analysis->func.function_signature.rtype;
	if (statement->return_stmt.expr == NULL)
	{
		if (expected_rtype->canonical != &type_void)
		{
			SEMA_ERROR(statement->token, "Expected to return a result of type %s.", expected_rtype->name_loc.string);
			return false;
		}
	}
	else
	{
		if (!sema_analyse_expression(context, statement->return_stmt.expr)) return false;
		Expr *conversion = expr_implicit_conversion(statement->return_stmt.expr, expected_rtype);
		if (!conversion) return false;
		statement->return_stmt.expr = conversion;
	}
	return true;
}

static inline bool sema_analyse_var_decl(Context *context, Ast *parent, Decl *decl)
{
	assert(decl->decl_kind == DECL_VAR);
	if (!sema_resolve_type(context, decl->var.type)) return false;
	Decl *other = context_find_ident(context, decl->name.string);
	if (other)
	{
		show_shadow_error(decl, other);
		return false;
	}
	if (context->last_local == &context->locals[MAX_LOCALS - 1])
	{
		FATAL_ERROR("Reached the maximum number of locals");
	}
	if (decl->var.init_expr)
	{
		sema_analyse_expression(context, decl->var.init_expr);
		type_assign_cast(decl->var.init_expr, decl->var.type->canonical, false);
	}
	context->last_local[0] = decl;
	context->last_local++;
	return true;
}

static inline bool sema_analyse_multi_decl(Context *context, Ast *parent, Ast *statement)
{
	Decl *decl = statement->declare_stmt;
	VECEACH(statement->declare_stmt->multi_decl, i)
	{
		if (!sema_analyse_var_decl(context, parent, statement->declare_stmt->multi_decl[i]))
		{
			decl_poison(decl);
			return false;
		}
	}
	return true;
}

static inline bool sema_analyse_declare_stmt(Context *context, Ast *parent, Ast *statement)
{
	Decl *decl = statement->declare_stmt;
	if (decl->decl_kind == DECL_MULTI_DECL)
	{
		return sema_analyse_multi_decl(context, parent, statement);
	}
	return sema_analyse_var_decl(context, parent, decl);
}

static bool sema_analyse_statement(Context *context, Ast *parent, Ast *statement)
{
	LOG_FUNC
	switch (statement->ast_kind)
	{
		case AST_POISONED:
			break;
		case AST_ASM_STMT:
			break;
		case AST_ATTRIBUTE:
			break;
		case AST_BREAK_STMT:
			break;
		case AST_CASE_STMT:
			break;
		case AST_CATCH_STMT:
			break;
		case AST_COMPOUND_STMT:
			context_push_scope(context);
			sema_analyse_compound_statement(context, parent, statement);
			context_pop_scope(context);
			break;
		case AST_COND_STMT:
			break;
		case AST_CONTINUE_STMT:
			break;
		case AST_CT_IF_STMT:
			break;
		case AST_CT_ELIF_STMT:
			break;
		case AST_CT_ELSE_STMT:
			break;
		case AST_DECLARE_STMT:
			return sema_analyse_declare_stmt(context, parent, statement);
		case AST_DECL_EXPR_LIST:
			break;
		case AST_DEFAULT_STMT:
			break;
		case AST_DEFER_STMT:
			break;
		case AST_DO_STMT:
			break;
		case AST_EXPR_STMT:
			break;
		case AST_FOR_STMT:
			break;
		case AST_GOTO_STMT:
			break;
		case AST_IF_STMT:
			break;
		case AST_LABEL:
			break;
		case AST_NOP_STMT:
			break;
		case AST_RETURN_STMT:
			return sema_analyse_return_stmt(context, parent, statement);
		case AST_SWITCH_STMT:
			break;
		case AST_THROW_STMT:
			break;
		case AST_TRY_STMT:
			break;
		case AST_NEXT_STMT:
			break;
		case AST_VOLATILE_STMT:
			break;
		case AST_WHILE_STMT:
			break;
		case AST_GENERIC_CASE_STMT:
			break;
		case AST_GENERIC_DEFAULT_STMT:
			break;
	}
	TODO
}

static inline bool sema_analyse_function_body(Context *context, Decl *func)
{
	context->active_function_for_analysis = func;
	context->current_scope = &context->scopes[0];
	context->current_scope->local_decl_start = 0;
	context->last_local = &context->locals[0];
	if (!sema_analyse_compound_statement(context, func->func.body, func->func.body)) return false;
	if (func->func.body->exit != EXIT_RETURN && func->func.function_signature.rtype->type_kind != TYPE_ARRAY)
	{
		SEMA_ERROR(func->name, "Missing return statement at the end of the function.");
		return false;
	}
	return true;
}
static inline bool sema_analyse_func(Context *context, Decl *decl)
{
	bool all_ok = sema_analyse_function_signature(context, &decl->func.function_signature, true);
	if (decl->func.struct_parent)
	{
		all_ok = sema_resolve_type(context, decl->func.struct_parent) && all_ok;
	}
	all_ok = all_ok && sema_analyse_function_body(context, decl);
	if (!all_ok) decl_poison(decl);
	return all_ok;
}
static inline void sema_analyse_decl(Context *context, Decl *decl)
{
	LOG_FUNC
	DEBUG_LOG("Analyse %s", decl->name.string);
	switch (decl->decl_kind)
	{
		case DECL_IMPORT:
			// TODO
			break;
		case DECL_STRUCT:
			sema_analyse_struct(context, decl);
			break;
		case DECL_FUNC:
			sema_analyse_func(context, decl);
			break;
		default:
			TODO
	}
}


bool context_register_global(Context *context, Decl *decl)
{
	Decl *old = stable_set(&context->local_symbols, decl->name.string, decl);
	if (!old && decl->visibility != VISIBLE_LOCAL)
	{
		old = stable_set(&context->module->symbols, decl->name.string, decl);
	}
	if (!old && decl->visibility == VISIBLE_PUBLIC)
	{
		old = stable_set(&context->module->public_symbols, decl->name.string, decl);
	}
	if (old != NULL)
	{
		show_shadow_error(decl, old);
		decl_poison(decl);
		return false;
	}
	return true;
}

static inline void sema_register_declarations(Context *context)
{
	VECEACH(context->declarations, i)
	{
		context_register_global(context, context->declarations[i]);
	}
}

static inline void sema_analyse_declarations(Context *context)
{
	VECEACH(context->declarations, i)
	{
		sema_analyse_decl(context, context->declarations[i]);
	}
}

static inline void sema_process_imports(Context *context)
{
	// TODO
}
void sema_analysis(Context *context)
{
	sema_process_imports(context);
	sema_register_declarations(context);
	// Skip the ct_if for now -> assume they passed.
	sema_analyse_declarations(context);
}

