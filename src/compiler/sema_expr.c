// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"
#include <math.h>
/*
 * TODOs
 * - Disallow jumping in and out of an expression block.
 */

static bool sema_expr_apply_typeid_property(SemaContext *context, Expr *expr, Expr *typeid, const char *kw);
static bool sema_expr_apply_type_property(SemaContext *context, Expr *expr, Type *type, const char *kw);
static inline bool sema_expr_analyse_ct_eval(SemaContext *context, Expr *expr);
static bool sema_take_addr_of(Expr *inner);
static inline bool sema_expr_analyse_binary(SemaContext *context, Expr *expr);
static inline bool sema_cast_rvalue(SemaContext *context, Expr *expr);
static Expr *expr_access_inline_member(Expr *parent, Decl *parent_decl);
static inline void expr_set_as_const_list(Expr *expr, ConstInitializer *list);
static inline bool sema_expr_analyse_builtin(SemaContext *context, Expr *expr, bool throw_error);
static bool sema_check_stmt_compile_time(SemaContext *context, Ast *ast);
static bool binary_arithmetic_promotion(SemaContext *context, Expr *left, Expr *right, Type *left_type, Type *right_type, Expr *parent, const char *error_message);
static inline bool expr_both_const(Expr *left, Expr *right);
static inline void expr_set_as_const_list(Expr *expr, ConstInitializer *list)
{
	expr->expr_kind = EXPR_CONST;
	expr->const_expr.const_kind = CONST_LIST;
	expr->const_expr.list = list;
}

static bool sema_decay_array_pointers(Expr *expr)
{

	CanonicalType *pointer_type = type_pointer_type(type_no_fail(expr->type));

	if (!pointer_type || !type_is_arraylike(pointer_type)) return true;

	return cast_implicit(expr, type_get_opt_fail(type_get_ptr(pointer_type->array.base), IS_FAILABLE(expr)));
}

int BINOP_PREC_REQ[BINARYOP_LAST + 1] =
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

const char *ct_eval_error = "EVAL_ERROR";

void expr_rewrite_to_builtin_access(SemaContext *context, Expr *expr, Expr *parent, BuiltinAccessKind kind, Type *type)
{
	expr->expr_kind = EXPR_BUILTIN_ACCESS;
	expr->builtin_access_expr.kind = kind;
	expr->builtin_access_expr.inner = exprid(parent);
	expr->type = type;
	expr->resolve_status = RESOLVE_DONE;
}

const char *ct_eval_expr(SemaContext *c, const char *expr_type, Expr *inner, TokenType *type, Path **path_ref, bool report_missing)
{
	if (!sema_analyse_expr(c, inner)) return false;
	if (!expr_is_const_string(inner))
	{
		SEMA_ERROR(inner, "'%s' expects a constant string as the argument.", expr_type);
		return ct_eval_error;
	}
	const char *interned_version = NULL;
	if (!splitpathref(inner->const_expr.string.chars, inner->const_expr.string.len, path_ref, &interned_version, type))
	{
		SEMA_ERROR(inner, "A valid name was expected here.");
		return ct_eval_error;
	}
	if (*path_ref) (*path_ref)->span = inner->span;
	if (*type == TOKEN_INVALID_TOKEN)
	{
		if (report_missing)
		{
			SEMA_ERROR(inner, "'%s' could not be found, did you spell it right?", interned_version);
			return ct_eval_error;
		}
		return NULL;
	}
	return interned_version;
}

static Expr *expr_access_inline_member(Expr *parent, Decl *parent_decl)
{
	Expr *embedded_struct = expr_new(EXPR_ACCESS, parent->span);
	embedded_struct->resolve_status = RESOLVE_DONE;
	embedded_struct->access_expr.parent = parent;
	embedded_struct->access_expr.ref = parent_decl->strukt.members[0];
	embedded_struct->type = embedded_struct->access_expr.ref->type;
	return embedded_struct;
}


static inline bool expr_both_const(Expr *left, Expr *right)
{
	return left->expr_kind == EXPR_CONST && right->expr_kind == EXPR_CONST;
}


static inline bool both_any_integer_or_integer_vector(Expr *left, Expr *right)
{
	Type *flatten_left = type_flatten(left->type);
	Type *flatten_right = type_flatten(right->type);
	if (type_is_integer(flatten_left) && type_is_integer(flatten_right)) return true;

	if (flatten_left->type_kind != TYPE_VECTOR || flatten_right->type_kind != TYPE_VECTOR) return false;

	return type_is_integer(flatten_left->array.base) && type_is_integer(flatten_right->array.base);
}

Expr *expr_generate_decl(Decl *decl, Expr *assign)
{
	assert(decl->decl_kind == DECL_VAR);
	assert(decl->var.init_expr == NULL);
	Expr *expr_decl = expr_new(EXPR_DECL, decl->span);
	expr_decl->decl_expr = decl;
	if (!assign) decl->var.no_init = true;
	decl->var.init_expr = assign;
	return expr_decl;
}

void expr_insert_addr(Expr *original)
{
	assert(original->resolve_status == RESOLVE_DONE);
	if (original->expr_kind == EXPR_UNARY && original->unary_expr.operator == UNARYOP_DEREF)
	{
		*original = *original->unary_expr.expr;
		return;
	}
	Expr *inner = expr_copy(original);
	original->expr_kind = EXPR_UNARY;
	Type *inner_type = inner->type;
	bool failable = type_is_failable(inner->type);
	original->type = type_get_opt_fail(type_get_ptr(type_no_fail(inner->type)), failable);
	original->unary_expr.operator = UNARYOP_ADDR;
	original->unary_expr.expr = inner;
}

Expr *expr_variable(Decl *decl)
{
	if (decl->resolve_status == RESOLVE_DONE)
	{
		Expr *expr = expr_new(EXPR_IDENTIFIER, decl->span);
		expr->identifier_expr.decl = decl;
		expr->resolve_status = RESOLVE_DONE;
		expr->type = decl->type;
		return expr;
	}
	Expr *expr = expr_new(EXPR_IDENTIFIER, decl->span);
	expr->identifier_expr.ident = decl->name;
	expr->resolve_status = RESOLVE_NOT_DONE;
	return expr;

}

static inline bool expr_unary_addr_is_constant_eval(Expr *expr, ConstantEvalKind eval_kind)
{
	if (eval_kind != CONSTANT_EVAL_ANY) return false;
	Expr *inner = expr->unary_expr.expr;
	switch (inner->expr_kind)
	{
		case EXPR_CONST:
		case EXPR_INITIALIZER_LIST:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			return expr_is_constant_eval(inner, eval_kind);
		case EXPR_IDENTIFIER:
		{
			Decl *decl = inner->identifier_expr.decl;
			if (decl->decl_kind == DECL_FUNC) return true;
			if (decl->decl_kind != DECL_VAR) return false;
			switch (decl->var.kind)
			{
				case VARDECL_CONST:
				case VARDECL_GLOBAL:
					return true;
				case VARDECL_LOCAL:
					return decl->var.is_static;
				case VARDECL_PARAM:
				case VARDECL_MEMBER:
				case VARDECL_BITMEMBER:
				case VARDECL_PARAM_CT:
				case VARDECL_PARAM_CT_TYPE:
				case VARDECL_PARAM_REF:
				case VARDECL_PARAM_EXPR:
				case VARDECL_LOCAL_CT:
				case VARDECL_LOCAL_CT_TYPE:
				case VARDECL_UNWRAPPED:
				case VARDECL_ERASE:
				case VARDECL_REWRAPPED:
					return false;
			}
		}
		default:
			return false;
	}
}

bool expr_cast_is_constant_eval(Expr *expr, ConstantEvalKind eval_kind)
{
	switch (expr->cast_expr.kind)
	{
		case CAST_ERROR:
		case CAST_BSINT:
		case CAST_BSARRY:
			return false;
		case CAST_ANYPTR:
		case CAST_ERBOOL:
		case CAST_EUBOOL:
		case CAST_EUER:
		case CAST_EREU:
		case CAST_XIERR:
		case CAST_PTRPTR:
		case CAST_STRPTR:
		case CAST_PTRBOOL:
		case CAST_BOOLINT:
		case CAST_BOOLFP:
		case CAST_BOOLBOOL:
		case CAST_FPBOOL:
		case CAST_INTBOOL:
		case CAST_FPFP:
		case CAST_FPSI:
		case CAST_FPUI:
		case CAST_SISI:
		case CAST_SIUI:
		case CAST_SIFP:
		case CAST_XIPTR:
		case CAST_UISI:
		case CAST_UIUI:
		case CAST_UIFP:
		case CAST_APTSA:
		case CAST_SAPTR:
		case CAST_SABOOL:
		case CAST_STST:
		case CAST_PTRANY:
		case CAST_ENUMLOW:
		case CAST_VECARR:
		case CAST_ARRVEC:
			if (eval_kind == CONSTANT_EVAL_FOLDABLE) return false;
			return exprid_is_constant_eval(expr->cast_expr.expr, eval_kind);
		case CAST_EUINT:
		case CAST_ERINT:
		case CAST_PTRXI:
			if (eval_kind != CONSTANT_EVAL_ANY) return false;
			return exprid_is_constant_eval(expr->cast_expr.expr, eval_kind);
	}
	UNREACHABLE
}
static bool expr_binary_is_constant_eval(Expr *expr, ConstantEvalKind eval_kind)
{
	if (expr->binary_expr.operator >= BINARYOP_ASSIGN) return false;
	if (eval_kind == CONSTANT_EVAL_FOLDABLE) return false;
	Expr *left = exprptr(expr->binary_expr.left);
	Expr *right = exprptr(expr->binary_expr.right);
	int pointers = type_flatten(left->type)->type_kind == TYPE_POINTER ? 1 : 0;
	pointers += type_flatten(right->type)->type_kind == TYPE_POINTER ? 1 : 0;
	switch (expr->binary_expr.operator)
	{
		case BINARYOP_ERROR:
			UNREACHABLE
		case BINARYOP_SUB:
			// Pointer diffs are not compile time resolved.
			if (pointers == 2) return false;
			if (pointers == 0) eval_kind = CONSTANT_EVAL_NO_LINKTIME;
			break;
		case BINARYOP_ADD:
			assert(pointers != 2);
			if (pointers == 0) eval_kind = CONSTANT_EVAL_NO_LINKTIME;
			break;
		default:
			// For the default operations we don't accept linktime resolution
			eval_kind = CONSTANT_EVAL_NO_LINKTIME;
			break;
	}
	if (!expr_is_constant_eval(left, eval_kind)) return false;
	if (!expr_is_constant_eval(right, eval_kind)) return false;
	return true;
}

static inline bool expr_list_is_constant_eval(Expr **exprs, ConstantEvalKind eval_kind)
{
	VECEACH(exprs, i)
	{
		if (!expr_is_constant_eval(exprs[i], eval_kind)) return false;
	}
	return true;
}

// Check if the assignment fits
static bool sema_bit_assignment_check(Expr *right, Decl *member)
{
	// Don't check non-consts and non integers.
	if (!IS_CONST(right) || !type_is_integer(right->type)) return true;

	unsigned bits = member->var.end_bit - member->var.start_bit + 1;

	// If we have enough bits to fit, then we're done.
	if (bits >= type_bit_size(right->type) || int_is_zero(right->const_expr.ixx)) return true;

	if (int_bits_needed(right->const_expr.ixx) > bits)
	{
		SEMA_ERROR(right, "This constant would be truncated if stored in the bitstruct, do you need a wider bit range?");
		return false;
	}
	return true;
}

bool expr_is_constant_eval(Expr *expr, ConstantEvalKind eval_kind)
{
	RETRY:
	switch (expr->expr_kind)
	{
		case EXPR_CT_CONV:
			return true;
		case EXPR_RETVAL:
			return false;
		case EXPR_BUILTIN:
		case EXPR_CT_EVAL:
			return false;
		case EXPR_BITACCESS:
		case EXPR_ACCESS:
			expr = expr->access_expr.parent;
			goto RETRY;
		case EXPR_VARIANTSWITCH:
			return false;
		case EXPR_BITASSIGN:
			return false;
		case EXPR_BUILTIN_ACCESS:
			switch (expr->builtin_access_expr.kind)
			{
				case ACCESS_ENUMNAME:
				case ACCESS_LEN:
				case ACCESS_PTR:
					break;
				case ACCESS_TYPEOFANY:
					if (eval_kind != CONSTANT_EVAL_ANY) return false;
					break;
			}
			return exprid_is_constant_eval(expr->builtin_access_expr.inner, eval_kind);
		case EXPR_VARIANT:
			return exprid_is_constant_eval(expr->variant_expr.type_id, eval_kind) && exprid_is_constant_eval(expr->variant_expr.ptr, eval_kind);
		case EXPR_BINARY:
			return expr_binary_is_constant_eval(expr, eval_kind);
		case EXPR_CAST:
			return expr_cast_is_constant_eval(expr, eval_kind);
		case EXPR_CONST:
		case EXPR_STRINGIFY:
			return true;
		case EXPR_COND:
			return expr_list_is_constant_eval(expr->cond_expr, eval_kind);
		case EXPR_DESIGNATOR:
			expr = expr->designator_expr.value;
			goto RETRY;
		case EXPR_EXPR_BLOCK:
		case EXPR_DECL:
		case EXPR_CALL:
		case EXPR_CATCH_UNWRAP:
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_TRY_UNWRAP:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_POST_UNARY:
		case EXPR_SLICE_ASSIGN:
		case EXPR_MACRO_BLOCK:
		case EXPR_RETHROW:
			return false;
		case EXPR_IDENTIFIER:
			if (expr->identifier_expr.decl->decl_kind != DECL_VAR) return true;
			if (expr->identifier_expr.is_const)
			{
				expr = expr->identifier_expr.decl->var.init_expr;
				goto RETRY;
			}
			switch (expr->identifier_expr.decl->var.kind)
			{
				case VARDECL_CONST:
				case VARDECL_PARAM_CT_TYPE:
				case VARDECL_LOCAL_CT_TYPE:
				case VARDECL_LOCAL_CT:
				case VARDECL_PARAM_CT:
					return true;
				default:
					return false;
			}
		case EXPR_EXPRESSION_LIST:
			return expr_list_is_constant_eval(expr->expression_list, eval_kind);
		case EXPR_TYPEID_INFO:
			expr = exprptr(expr->typeid_info_expr.parent);
			goto RETRY;
		case EXPR_FAILABLE:
		case EXPR_GROUP:
			expr = expr->inner_expr;
			goto RETRY;
		case EXPR_INITIALIZER_LIST:
			return expr_list_is_constant_eval(expr->initializer_list, eval_kind);
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			return expr_list_is_constant_eval(expr->designated_init_list, eval_kind);
		case EXPR_SLICE:
			if (expr->slice_expr.start && !exprid_is_constant_eval(expr->slice_expr.start, CONSTANT_EVAL_FOLDABLE)) return false;
			if (expr->slice_expr.end && !exprid_is_constant_eval(expr->slice_expr.end, CONSTANT_EVAL_FOLDABLE)) return false;
			return exprid_is_constant_eval(expr->slice_expr.expr, eval_kind);
		case EXPR_SUBSCRIPT:
			if (!exprid_is_constant_eval(expr->subscript_expr.index, eval_kind)) return false;
			expr = exprptr(expr->subscript_expr.expr);
			goto RETRY;
		case EXPR_SUBSCRIPT_ADDR:
			if (!exprid_is_constant_eval(expr->subscript_expr.index, eval_kind)) return false;
			expr = exprptr(expr->subscript_expr.expr);
			if (expr->expr_kind == EXPR_IDENTIFIER)
			{
				Decl *decl = expr->identifier_expr.decl;
				if (decl->decl_kind == DECL_VAR)
				{
					switch (decl->var.kind)
					{
						case VARDECL_CONST:
						case VARDECL_GLOBAL:
							break;
						case VARDECL_LOCAL:
							if (decl->var.is_static) break;
						default:
							return false;
					}
					return eval_kind != CONSTANT_EVAL_FOLDABLE;
				}
			}
			goto RETRY;

		case EXPR_TERNARY:
			assert(!exprid_is_constant_eval(expr->ternary_expr.cond, eval_kind));
			return false;
		case EXPR_FORCE_UNWRAP:
		case EXPR_TRY:
		case EXPR_CATCH:
			expr = expr->inner_expr;
			goto RETRY;
		case EXPR_TYPEID:
			return eval_kind == CONSTANT_EVAL_ANY;
		case EXPR_UNARY:
			switch (expr->unary_expr.operator)
			{
				case UNARYOP_DEREF:
				case UNARYOP_ERROR:
					return false;
				case UNARYOP_ADDR:
					return expr_unary_addr_is_constant_eval(expr, eval_kind);
				case UNARYOP_NEG:
				case UNARYOP_BITNEG:
				case UNARYOP_NOT:
				case UNARYOP_TADDR:
					expr = expr->unary_expr.expr;
					goto RETRY;
				case UNARYOP_INC:
				case UNARYOP_DEC:
					return false;
			}
			UNREACHABLE
		case EXPR_CT_CALL:
		case EXPR_TYPEINFO:
		case EXPR_HASH_IDENT:
		case EXPR_CT_IDENT:
		case EXPR_FLATPATH:
		case EXPR_COMPOUND_LITERAL:
		case EXPR_COMPILER_CONST:
		case EXPR_POISONED:
		case EXPR_ARGV_TO_SUBARRAY:
			UNREACHABLE
		case EXPR_NOP:
			return true;
	}
	UNREACHABLE
}


void expr_insert_deref(Expr *original)
{
	// Assume *(&x) => x
	if (original->expr_kind == EXPR_UNARY && original->unary_expr.operator == UNARYOP_ADDR)
	{
		*original = *original->unary_expr.expr;
		return;
	}

	// Allocate our new and create our new inner, and overwrite the original.
	Expr *inner = expr_copy(original);
	original->expr_kind = EXPR_UNARY;
	original->type = NULL;
	original->unary_expr.operator = UNARYOP_DEREF;
	original->unary_expr.expr = inner;

	// In the case the original is already resolved, we want to resolve the deref as well.
	if (original->resolve_status == RESOLVE_DONE)
	{
		Type *no_fail  = type_no_fail(inner->type);
		assert(no_fail->canonical->type_kind == TYPE_POINTER);

		// Only fold to the canonical type if it wasn't a pointer.
		Type *pointee = no_fail->type_kind == TYPE_POINTER ? no_fail->pointer : no_fail->canonical->pointer;
		original->type = type_get_opt_fail(pointee, IS_FAILABLE(inner));
	}
}


static void expr_unify_binary_failability(Expr *expr, Expr *left, Expr *right)
{
	expr->type = type_get_opt_fail(left->type, IS_FAILABLE(right));
}

static inline void context_pop_returns(SemaContext *context, Ast **restore)
{
	if (!context->returns_cache && context->returns)
	{
		context->returns_cache = context->returns;
	}
	context->returns = restore;
}

static inline Ast **context_push_returns(SemaContext *context)
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

int sema_check_comp_time_bool(SemaContext *context, Expr *expr)
{
	if (!sema_analyse_cond_expr(context, expr)) return -1;
	if (expr->expr_kind != EXPR_CONST)
	{
		SEMA_ERROR(expr, "Compile time evaluation requires a compile time constant value.");
		return -1;
	}
	return expr->const_expr.b;
}


bool expr_is_ltype(Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_CT_IDENT:
			return true;
		case EXPR_IDENTIFIER:
		{
			if (expr->identifier_expr.is_const) return false;
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
				case VARDECL_MEMBER:
				case VARDECL_BITMEMBER:
				case VARDECL_PARAM_CT:
				case VARDECL_PARAM_CT_TYPE:
				case VARDECL_PARAM_EXPR:
					return false;
				case VARDECL_UNWRAPPED:
				case VARDECL_ERASE:
				case VARDECL_REWRAPPED:
					UNREACHABLE
			}
		}
		case EXPR_UNARY:
			return expr->unary_expr.operator == UNARYOP_DEREF;
		case EXPR_BITACCESS:
		case EXPR_ACCESS:
			return expr_is_ltype(expr->access_expr.parent);
		case EXPR_GROUP:
			return expr_is_ltype(expr->inner_expr);
		case EXPR_SUBSCRIPT:
		case EXPR_SLICE:
			return true;
		default:
			return false;
	}
}

bool sema_expr_check_assign(SemaContext *c, Expr *expr)
{
	if (!expr_is_ltype(expr))
	{
		SEMA_ERROR(expr, "An assignable expression, like a variable, was expected here.");
		return false;
	}
	if (expr->expr_kind == EXPR_BITACCESS || expr->expr_kind == EXPR_ACCESS) expr = expr->access_expr.parent;
	if (expr->expr_kind == EXPR_IDENTIFIER)
	{
		expr->identifier_expr.decl->var.is_written = true;
	}
	if (expr->expr_kind != EXPR_UNARY) return true;
	Expr *inner = expr->inner_expr;
	if (inner->expr_kind != EXPR_IDENTIFIER) return true;
	Decl *decl = inner->identifier_expr.decl;
	if (decl->decl_kind != DECL_VAR) return true;
	if (!decl->var.may_not_write) return true;
	SEMA_ERROR(inner, "'in' parameters may not be assigned to.");
	return false;
}

static inline bool sema_cast_ident_rvalue(SemaContext *context, Expr *expr)
{
	Decl *decl = expr->identifier_expr.decl;
	decl = decl_flatten(decl);

	switch (decl->decl_kind)
	{
		case DECL_FUNC:
			SEMA_ERROR(expr, "Expected function followed by (...) or prefixed by &.");
			return expr_poison(expr);
		case DECL_MACRO:
			SEMA_ERROR(expr, "Expected a macro followed by (...).");
			return expr_poison(expr);
		case DECL_GENERIC:
			SEMA_ERROR(expr, "Expected generic function followed by (...).");
			return expr_poison(expr);
		case DECL_FAULTVALUE:
			SEMA_ERROR(expr, "Did you forget a '!' after '%s'?", decl->name);
			return expr_poison(expr);
		case DECL_ENUM_CONSTANT:
			TODO
			//expr_replace(expr, decl->enum_constant.expr);
			return true;
		case DECL_VAR:
			break;
		case DECL_DISTINCT:
		case DECL_TYPEDEF:
		case DECL_DECLARRAY:
		case DECL_BODYPARAM:
			UNREACHABLE
		case DECL_POISONED:
			return expr_poison(expr);
		case DECL_LABEL:
			SEMA_ERROR(expr, "Did you intend to use the label '%s' here?", decl->name);
			return expr_poison(expr);
		case DECL_BITSTRUCT:
			SEMA_ERROR(expr, "Expected bitstruct followed by (...) or '.'.");
			return expr_poison(expr);
		case DECL_STRUCT:
			SEMA_ERROR(expr, "Expected struct followed by {...} or '.'.");
			return expr_poison(expr);
		case DECL_UNION:
			SEMA_ERROR(expr, "Expected union followed by {...} or '.'.");
			return expr_poison(expr);
		case DECL_ENUM:
			SEMA_ERROR(expr, "Expected enum name followed by '.' and an enum value.");
			return expr_poison(expr);
		case DECL_FAULT:
			SEMA_ERROR(expr, "Expected fault name followed by '.' and a fault value.");
			return expr_poison(expr);
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
		case VARDECL_GLOBAL:
		case VARDECL_LOCAL:
		case VARDECL_LOCAL_CT:
		case VARDECL_LOCAL_CT_TYPE:
			if (decl->var.init_expr && decl->var.init_expr->resolve_status != RESOLVE_DONE)
			{
				SEMA_ERROR(expr, "This looks like the initialization of the variable was circular.");
				return false;
			}
			break;
		default:
			break;
	}
	switch (decl->var.kind)
	{
		case VARDECL_CONST:
			if (!expr_is_constant_eval(decl->var.init_expr, CONSTANT_EVAL_ANY))
			{
				UNREACHABLE
			}
			if (type_is_abi_aggregate(decl->type)) return true;
			expr_replace(expr, expr_macro_copy(decl->var.init_expr));
			return sema_analyse_expr(context, expr);
		case VARDECL_PARAM_EXPR:
			UNREACHABLE
		case VARDECL_PARAM_CT_TYPE:
		case VARDECL_PARAM_CT:
		case VARDECL_LOCAL_CT:
		case VARDECL_LOCAL_CT_TYPE:
		case VARDECL_ERASE:
		case VARDECL_REWRAPPED:
			// Impossible to reach this, they are already unfolded
			UNREACHABLE
		case VARDECL_PARAM_REF:
			expr_replace(expr, expr_macro_copy(decl->var.init_expr));
			return sema_cast_rvalue(context, expr);
		case VARDECL_PARAM:
		case VARDECL_GLOBAL:
		case VARDECL_LOCAL:
		case VARDECL_UNWRAPPED:
			return true;
		case VARDECL_BITMEMBER:
		case VARDECL_MEMBER:
			SEMA_ERROR(expr, "Expected '%s' followed by a method call or property.", decl->name);
			return expr_poison(expr);
	}
	UNREACHABLE
}

static bool expr_is_unwrapped_ident(Expr *expr)
{
	if (expr->expr_kind != EXPR_IDENTIFIER) return false;
	Decl *decl = expr->identifier_expr.decl;
	if (decl->decl_kind != DECL_VAR) return false;
	return decl->var.kind == VARDECL_UNWRAPPED && IS_FAILABLE(decl->var.alias);
}



static inline bool sema_type_error_on_binop(Expr *expr)
{
	const char *c = token_type_to_string(binaryop_to_token(expr->binary_expr.operator));
	SEMA_ERROR(expr, "%s is not defined in the expression %s %s %s.",
	           c, type_quoted_error_string(exprptr(expr->binary_expr.left)->type),
	           c, type_quoted_error_string(exprptr(expr->binary_expr.right)->type));
	return false;
}

static bool expr_cast_to_index(Expr *index)
{
	Type *type = index->type->canonical;
	RETRY:
	switch (type->type_kind)
	{
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
			SEMA_ERROR(index, "index->type->canonical this to an int or long.");
			return false;
		case TYPE_ENUM:
			type = type->decl->enums.type_info->type->canonical;
			goto RETRY;
		default:
			SEMA_ERROR(index, "Cannot implicitly convert '%s' to an index.", type_to_error_string(index->type));
			return false;
	}
}

static inline bool sema_expr_analyse_ternary(SemaContext *context, Expr *expr)
{
	Expr *left = exprptrzero(expr->ternary_expr.then_expr);
	Expr *cond = exprptr(expr->ternary_expr.cond);
	int path = -1;
	// Normal
	if (left)
	{
		if (!sema_analyse_cond_expr(context, cond)) return expr_poison(expr);
		if (!sema_analyse_expr(context, left)) return expr_poison(expr);
		if (cond->expr_kind == EXPR_CONST)
		{
			path = cond->const_expr.b ? 1 : 0;
		}
	}
	else
	{
		// Elvis
		if (!sema_analyse_expr(context, cond)) return expr_poison(expr);
		Type *type = cond->type->canonical;
		if (type->type_kind != TYPE_BOOL && cast_to_bool_kind(type) == CAST_ERROR)
		{
			SEMA_ERROR(cond, "Cannot convert expression to boolean.");
			return false;
		}
		if (expr_is_constant_eval(cond, true))
		{
			Expr *copy = expr_macro_copy(cond);
			cast(copy, type_bool);
			assert(cond->expr_kind == EXPR_CONST);
			path = cond->const_expr.b ? 1 : 0;
		}
		left = cond;
	}

	Expr *right = exprptr(expr->ternary_expr.else_expr);
	if (!sema_analyse_expr(context, right)) return expr_poison(expr);

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
		Type *no_fail_max = type_no_fail(max);
		if (!cast_implicit(left, no_fail_max) || !cast_implicit(right, max)) return false;
	}

	if (path > -1)
	{
		expr_replace(expr, path ? left : right);
	}

	expr_unify_binary_failability(expr, left, right);
	return true;
}



static inline Decl *decl_find_enum_constant(const char *name, Decl *decl)
{
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

static inline bool sema_expr_analyse_enum_constant(Expr *expr, const char *name, Decl *decl)
{
	Decl *enum_constant = decl_find_enum_constant(name, decl);
	if (!enum_constant) return false;

	assert(enum_constant->resolve_status == RESOLVE_DONE);
	expr->type = decl->type;
	expr->expr_kind = EXPR_CONST;
	if (enum_constant->decl_kind == DECL_ENUM_CONSTANT)
	{
		expr->const_expr.const_kind = CONST_ENUM;
		expr->const_expr.enum_val = enum_constant;
	}
	else
	{
		expr->const_expr.const_kind = CONST_ERR;
		expr->const_expr.err_val = enum_constant;
	}
	return true;
}


static inline bool find_possible_inferred_identifier(Type *to, Expr *expr)
{
	if (to->canonical->type_kind != TYPE_ENUM && to->canonical->type_kind != TYPE_FAULTTYPE) return false;
	Decl *parent_decl = to->canonical->decl;
	switch (parent_decl->decl_kind)
	{
		case DECL_ENUM:
		case DECL_FAULT:
			return sema_expr_analyse_enum_constant(expr, expr->identifier_expr.ident, parent_decl);
		case DECL_UNION:
		case DECL_STRUCT:
		case DECL_BITSTRUCT:
			return false;
		default:
			UNREACHABLE
	}

}

static inline bool sema_expr_analyse_identifier(SemaContext *context, Type *to, Expr *expr)
{
	Decl *ambiguous_decl = NULL;
	Decl *private_symbol = NULL;

	assert(expr && expr->identifier_expr.ident);
	DEBUG_LOG("Now resolving %s", expr->identifier_expr.ident);

	DeclId body_param;
	if (!expr->identifier_expr.path && context->current_macro && (body_param = context->current_macro->macro_decl.body_param))
	{
		if (expr->identifier_expr.ident == declptr(body_param)->name)
		{
			expr->expr_kind = EXPR_MACRO_BODY_EXPANSION;
			expr->body_expansion_expr.ast = NULL;
			expr->body_expansion_expr.declarations = NULL;
			expr->resolve_status = RESOLVE_NOT_DONE;
			expr->type = type_void;
			return true;
		}
	}
	// Just start with inference
	if (!expr->identifier_expr.path && to)
	{
		if (find_possible_inferred_identifier(to, expr)) return true;
	}

	Decl *decl = sema_find_path_symbol(context, expr->identifier_expr.ident, expr->identifier_expr.path);
	// Is this a broken decl?
	if (!decl_ok(decl)) return false;


	// Rerun if we can't do inference.
	if (!decl)
	{
		decl = sema_resolve_symbol(context, expr->identifier_expr.ident, expr->identifier_expr.path, expr->span);
		(void)decl;
		assert(!decl_ok(decl));
		return false;
	}

	if (decl->decl_kind == DECL_VAR || decl->decl_kind == DECL_FUNC || decl->decl_kind == DECL_MACRO || decl->decl_kind == DECL_GENERIC)
	{
		if (decl->module != context->unit->module && !decl->is_autoimport && !expr->identifier_expr.path)
		{
			const char *message;
			switch (decl->decl_kind)
			{
				case DECL_VAR:
					message = "Globals from other modules must be prefixed with the module name.";
					break;
				case DECL_FUNC:
					message = "Functions from other modules must be prefixed with the module name.";
					break;
				case DECL_MACRO:
					message = "Macros from other modules must be prefixed with the module name.";
					break;
				case DECL_GENERIC:
					message = "Generic functions from other modules must be prefixed with the module name.";
					break;
				default:
					UNREACHABLE
			}
			SEMA_ERROR(expr, message);
			return false;
		}
	}
	if (decl->resolve_status != RESOLVE_DONE)
	{
		if (!sema_analyse_decl(context, decl)) return decl_poison(decl);
	}
	unit_register_external_symbol(context->compilation_unit, decl);
	if (decl->decl_kind == DECL_VAR)
	{
		decl->var.is_read = true;
		switch (decl->var.kind)
		{
			case VARDECL_CONST:
				if (!decl->type)
				{
					Expr *copy = expr_macro_copy(decl->var.init_expr);
					if (!sema_analyse_expr(context, copy)) return false;
					if (!expr_is_constant_eval(copy, false))
					{
						SEMA_ERROR(expr, "Constant value did not evaluate to a constant.");
						return false;
					}
					expr_replace(expr, copy);
					return true;
				}
				if (IS_FAILABLE(decl))
				{
					SEMA_ERROR(expr, "Constants may never be 'failable', please remove the '!'.");
					return false;
				}
				break;
			case VARDECL_GLOBAL:
				if (context->current_function_pure)
				{
					SEMA_ERROR(expr, "'@pure' functions may not access globals.");
					return false;
				}
			default:
				break;
		}
	}
	if (!decl->type) decl->type = type_void;
	expr->identifier_expr.decl = decl;
	expr->type = decl->type;
	DEBUG_LOG("Resolution successful of %s.", decl->name);
	return true;
}


static inline bool sema_expr_analyse_ct_identifier(SemaContext *context, Expr *expr)
{
	assert(expr && expr->ct_ident_expr.identifier);

	DEBUG_LOG("Now resolving %s", expr->ct_ident_expr.identifier);
	Decl *decl = sema_resolve_symbol(context, expr->ct_ident_expr.identifier, NULL, expr->span);

	// Already handled
	if (!decl_ok(decl))
	{
		return expr_poison(expr);
	}

	DEBUG_LOG("Resolution successful of %s.", decl->name);
	assert(decl->decl_kind == DECL_VAR);
	assert(decl->resolve_status == RESOLVE_DONE);

	expr->ct_ident_expr.decl = decl;
	expr->type = decl->type;
	return true;
}

static inline bool sema_expr_analyse_hash_identifier(SemaContext *context, Expr *expr)
{
	assert(expr && expr->hash_ident_expr.identifier);
	DEBUG_LOG("Now resolving %s", expr->hash_ident_expr.identifier);
	Decl *decl = sema_resolve_symbol(context, expr->hash_ident_expr.identifier, NULL, expr->span);

	// Already handled
	if (!decl_ok(decl))
	{
		return expr_poison(expr);
	}

	DEBUG_LOG("Resolution successful of %s.", decl->name);
	assert(decl->decl_kind == DECL_VAR);

	expr_replace(expr, expr_macro_copy(decl->var.init_expr));
	REMINDER("Remove analysis for hash");
	if (!sema_analyse_expr_lvalue(decl->var.hash_var.context, expr))
	{
		// Poison the decl so we don't evaluate twice.
		decl_poison(decl);
		return false;
	}
	return true;
}

static inline bool sema_widen_top_down(Expr *expr, Type *type)
{
	Type *to = type;
	Type *from = expr->type;
	RETRY:
	if (type_is_integer(from) && type_is_integer(to)) goto CONVERT_IF_BIGGER;
	if (type_is_float(from) && type_is_float(to)) goto CONVERT_IF_BIGGER;
	if (type_is_integer(from) && type_is_float(to)) goto CONVERT;
	if (type_is_vector(from) && type_is_vector(to))
	{
		to = type_vector_type(to);
		from = type_vector_type(from);
		goto RETRY;
	}
	return true;
	CONVERT_IF_BIGGER:
	if (type_size(to) <= type_size(from)) return true;
	CONVERT:
	return cast_implicit(expr, type);
}

static inline bool sema_promote_binary_top_down(SemaContext *context, Expr *binary, Expr *left, Expr *right)
{
	if (!binary->binary_expr.widen) return true;
	Type *to = binary->type;
	return sema_widen_top_down(left, to) && sema_widen_top_down(right, to);
}

static inline bool sema_expr_analyse_binary_subexpr(SemaContext *context, Expr *binary, Expr *left, Expr *right)
{
	return (int)sema_analyse_expr(context, left) & (int)sema_analyse_expr(context, right);
}

static inline bool sema_expr_analyse_binary_arithmetic_subexpr(SemaContext *context, Expr *expr, const char *error, bool bool_is_allowed)
{
	Expr *left = exprptr(expr->binary_expr.left);
	Expr *right = exprptr(expr->binary_expr.right);

	// 1. Analyse both sides.
	if (!sema_expr_analyse_binary_subexpr(context, expr, left, right)) return false;

	if (!sema_promote_binary_top_down(context, expr, left, right)) return false;

	Type *left_type = type_no_fail(left->type)->canonical;
	Type *right_type = type_no_fail(right->type)->canonical;

	if (bool_is_allowed && left_type == type_bool && right_type == type_bool) return true;
	// 2. Perform promotion to a common type.
	return binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, error);
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

static inline bool sema_expr_analyse_intrinsic_fp_invocation(SemaContext *context, Expr *expr, Decl *decl, bool *failable)
{
	unsigned arguments = vec_size(expr->call_expr.arguments);
	if (arguments != 1)
	{
		SEMA_ERROR(expr, "Expected 1 argument to intrinsic %s.", decl->name);
		return false;
	}
	Expr *arg = expr->call_expr.arguments[0];
	if (!sema_analyse_expr(context, arg)) return false;

	// Convert ints to float comptime float.
	if (type_is_integer(arg->type->canonical))
	{
		if (!cast_implicit(arg, type_double)) return false;
	}
	// If this is not a float argument => error.
	if (!type_is_float(arg->type->canonical))
	{
		SEMA_ERROR(arg, "Expected a floating point argument.", decl->name);
		return false;
	}

	// The expression type is the argument type.
	expr->type = type_with_added_failability(arg, *failable);
	return true;

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
	bool func_pointer;
	const char *block_parameter;
	Decl **params;
	Type **param_types;
	Expr *struct_var;
	unsigned param_count;
	Variadic variadic;
} CalledDecl;

static inline bool expr_promote_vararg(SemaContext *context, Expr *arg)
{
	Type *arg_type = arg->type->canonical;

	// 2. Promote any integer or bool to at least CInt
	if (type_is_promotable_integer(arg_type) || arg_type == type_bool)
	{
		return cast(arg, type_cint);
	}
	// 3. Promote any float to at least double
	if (type_is_promotable_float(arg->type))
	{
		return cast(arg, type_double);
	}
	return true;
}

static inline bool sema_check_invalid_body_arguments(SemaContext *context, Expr *call, CalledDecl *callee)
{
	Decl **body_arguments = call->call_expr.body_arguments;
	bool has_body_arguments = vec_size(body_arguments) > 0;

	// 1. Check if there are body arguments but no actual block.
	if (has_body_arguments && !callee->block_parameter)
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
		if (!callee->block_parameter)
		{
			SEMA_ERROR(call, "This macro does not support trailing statements, please remove it.");
			return false;
		}

		// 2c. This is a macro and it has a block parameter. Everything is fine!
		return true;
	}

	// 3. If we don't have a body, then if it has a block parameter this is an error.
	if (callee->block_parameter)
	{
		assert(callee->macro);
		SEMA_ERROR(call, "Expected call to have a trailing statement, did you forget to add it?");
		return false;
	}

	// 4. No body and no block parameter, this is fine.
	return true;
}


static inline bool sema_expand_call_arguments(SemaContext *context, CalledDecl *callee, Expr *call, Expr **args, unsigned func_param_count, Variadic variadic, bool *failable)
{
	unsigned num_args = vec_size(args);
	Decl **params = callee->params;
	bool is_func_ptr = callee->func_pointer;

	// 1. We need at least as many function locations as we have parameters.
	unsigned entries_needed = func_param_count > num_args ? func_param_count : num_args;
	Expr **actual_args = VECNEW(Expr*, entries_needed);
	for (unsigned i = 0; i < entries_needed; i++) vec_add(actual_args, NULL);


	// 2. Loop through the parameters.
	bool uses_named_parameters = false;
	for (unsigned i = 0; i < num_args; i++)
	{
		Expr *arg = args[i];

		// 3. Handle named parameters
		if (arg->expr_kind == EXPR_DESIGNATOR)
		{
			if (is_func_ptr)
			{
				SEMA_ERROR(arg, "Named parameters are not allowed with function pointer calls.");
				return false;
			}
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
			*failable |= IS_FAILABLE(arg->designator_expr.value);
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
			if (variadic == VARIADIC_NONE)
			{
				// 15. We have too many parameters...
				SEMA_ERROR(arg, "This argument would exceed the number of parameters, did you add too many arguments?");
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
			else if (variadic == VARIADIC_ANY)
			{
				if (!sema_analyse_expr(context, arg)) return false;
				expr_insert_addr(arg);
			}
		}
		actual_args[i] = arg;
	}

	// 17. Set default values.
	for (unsigned i = 0; i < entries_needed; i++)
	{
		// 17a. Assigned a value - skip
		if (actual_args[i]) continue;

		if (is_func_ptr) goto FAIL_MISSING;
		// 17b. Set the init expression.
		Expr *init_expr = params[i]->var.init_expr;
		if (init_expr)
		{
			if (callee->macro)
			{
				actual_args[i] = expr_macro_copy(init_expr);
			}
			else
			{
				assert(init_expr->resolve_status == RESOLVE_DONE);
				actual_args[i] = init_expr;
			}
			continue;
		}

FAIL_MISSING:

		// 17c. Vararg not set? That's fine.
		if (params && params[i]->var.vararg) continue;

		// 17d. Argument missing, that's bad.
		if (is_func_ptr)
		{
			SEMA_ERROR(call, "The call is missing parameter(s), please check the definition.");
			return false;
		}
		SEMA_ERROR(call, "The mandatory parameter '%s' was not set, please add it.", params[i]->name);
		return false;
	}
	call->call_expr.arguments = actual_args;
	return true;
}

static inline bool sema_expr_analyse_call_invocation(SemaContext *context, Expr *call, CalledDecl callee, bool *failable)
{
	// 1. Check body arguments.
	if (!sema_check_invalid_body_arguments(context, call, &callee)) return false;

	// 2. Pick out all the arguments and parameters.
	Expr **args = call->call_expr.arguments;
	Decl **decl_params = callee.params;
	Type **param_types = callee.param_types;
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
	unsigned func_param_count = callee.param_count;

	// 6. We might have a typed variadic call e.g. foo(int, double...)
	//    get that type.
	Type *variadic_type = NULL;
	if (callee.variadic == VARIADIC_TYPED || callee.variadic == VARIADIC_ANY)
	{
		// 7a. The parameter type is <type>[], so we get the <type>
		Type *last_type = callee.macro ? callee.params[func_param_count - 1]->type : callee.param_types[func_param_count - 1];
		assert(last_type->type_kind == TYPE_SUBARRAY);
		variadic_type = last_type->array.base;
		// 7b. The last function parameter is implicit so we will pretend it's not there.
		func_param_count--;
	}

	if (!sema_expand_call_arguments(context, &callee, call, args, func_param_count, callee.variadic, failable)) return false;

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
					if (!sema_analyse_expr(context, arg)) return false;

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
					if (!sema_analyse_expr_rhs(context, variadic_type, arg, true)) return false;
				}
				// Set the argument at the location.
				*failable |= IS_FAILABLE(arg);
				continue;
			}
			// 12. We might have a naked variadic argument
			if (callee.variadic == VARIADIC_RAW)
			{
				// 12a. Analyse the expression.
				if (!sema_analyse_expr(context, arg)) return false;

				// 12b. In the case of a compile time variable non macros we cast to c_int / double.
				if (!callee.macro)
				{
					if (!expr_promote_vararg(context, arg)) return false;
				}
				// Set the argument at the location.
				*failable |= IS_FAILABLE(arg);
				continue;
			}
			UNREACHABLE
		}

		Decl *param;
		VarDeclKind kind;
		Type *type;
		if (decl_params)
		{
			param = decl_params[i];
			kind = param->var.kind;
			type = param->type;
		}
		else
		{
			param = NULL;
			kind = VARDECL_PARAM;
			type = param_types[i];
		}

		// 16. Analyse a regular argument.
		switch (kind)
		{
			case VARDECL_PARAM_REF:
				// &foo
				if (!sema_analyse_expr_lvalue(context, arg)) return false;
				if (!sema_expr_check_assign(context, arg)) return false;
				if (type && type->canonical != arg->type->canonical)
				{
					SEMA_ERROR(arg, "'%s' cannot be implicitly cast to '%s'.", type_to_error_string(arg->type), type_to_error_string(type));
					return false;
				}
				if (param && !param->alignment)
				{
					if (arg->expr_kind == EXPR_IDENTIFIER)
					{
						param->alignment = arg->identifier_expr.decl->alignment;
					}
					else
					{
						param->alignment = type_alloca_alignment(arg->type);
					}
				}
				break;
			case VARDECL_PARAM:
				// foo
				if (!sema_analyse_expr_rhs(context, type, arg, true)) return false;
				if (IS_FAILABLE(arg)) *failable = true;
				if (arg->type == type_complist)
				{
					SEMA_ERROR(arg, "An untyped list can only be passed as a compile time parameter.");
					return false;
				}
				if (param && !param->alignment)
				{
					param->alignment = type_alloca_alignment(arg->type);
				}
				break;
			case VARDECL_PARAM_EXPR:
				// #foo
				param->var.hash_var.context = context;
				param->var.hash_var.span = arg->span;
				break;
			case VARDECL_PARAM_CT:
				// $foo
				assert(callee.macro);
				if (!sema_analyse_expr_rhs(context, type, arg, true)) return false;
				if (!expr_is_constant_eval(arg, CONSTANT_EVAL_ANY))
				{
					SEMA_ERROR(arg, "A compile time parameter must always be a constant, did you mistake it for a normal paramter?");
					return false;
				}
				break;
			case VARDECL_PARAM_CT_TYPE:
				// $Foo
				if (!sema_analyse_expr_lvalue(context, arg)) return false;
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
			case VARDECL_BITMEMBER:
			case VARDECL_LOCAL_CT:
			case VARDECL_LOCAL_CT_TYPE:
			case VARDECL_UNWRAPPED:
			case VARDECL_REWRAPPED:
			case VARDECL_ERASE:
				UNREACHABLE
		}
		if (param && !type) param->type = type_no_fail(arg->type);
	}
	return true;
}
static inline bool sema_expr_analyse_func_invocation(SemaContext *context, FunctionPrototype *prototype, FunctionSignature *sig, Expr *expr, Decl *decl,
                                                     Expr *struct_var, bool failable)
{
	CalledDecl callee = {
			.macro = false,
			.func_pointer = sig ? 0 : 1,
			.block_parameter = NULL,
			.struct_var = struct_var,
			.params = sig ? sig->params : NULL,
			.param_types = prototype->params,
			.param_count = vec_size(prototype->params),
			.variadic = prototype->variadic,
	};
	if (context->current_function_pure && (!sig || !sig->is_pure) && !expr->call_expr.attr_pure)
	{
		SEMA_ERROR(expr, "Only '@pure' functions may be called, you can override this with an attribute.");
		return false;
	}

	bool is_unused = expr->call_expr.result_unused;
	if (!sema_expr_analyse_call_invocation(context, expr, callee, &failable)) return false;

	Type *rtype = prototype->rtype;

	if (is_unused && rtype != type_void && decl && decl->decl_kind == DECL_FUNC)
	{
		if (decl->func_decl.attr_nodiscard)
		{
			SEMA_ERROR(expr, "The result of the function must be used.");
			return false;
		}
		if (type_is_failable(rtype) && !decl->func_decl.attr_maydiscard)
		{
			SEMA_ERROR(expr, "The optional result of the macro must be used.");
			return false;
		}
	}

	expr->type = type_get_opt_fail(rtype, failable);

	return true;
}

static inline bool sema_expr_analyse_var_call(SemaContext *context, Expr *expr, Type *func_ptr_type, bool failable)
{
	if (func_ptr_type->type_kind != TYPE_POINTER || func_ptr_type->pointer->type_kind != TYPE_FUNC)
	{
		SEMA_ERROR(expr, "Only macros, functions and function pointers maybe invoked, this is of type '%s'.", type_to_error_string(func_ptr_type));
		return false;
	}
	expr->call_expr.is_pointer_call = true;
	return sema_expr_analyse_func_invocation(context,
	                                         func_ptr_type->pointer->func.prototype,
	                                         NULL,
	                                         expr,
	                                         NULL, NULL, failable);

}

// Unify returns in a macro or expression block.
static inline Type *unify_returns(SemaContext *context)
{
	bool all_returns_need_casts = false;
	Type *common_type = NULL;

	bool failable = false;
	bool no_return = true;
	// 1. Loop through the returns.
	VECEACH(context->returns, i)
	{
		Ast *return_stmt = context->returns[i];
		if (!return_stmt)
		{
			failable = true;
			continue;
		}
		no_return = false;
		Expr *ret_expr = return_stmt->return_stmt.expr;
		Type *rtype = ret_expr ? ret_expr->type : type_void;
		if (type_is_failable_any(rtype))
		{
			failable = true;
			continue;
		}
		if (type_is_failable(rtype))
		{
			failable = true;
			rtype = type_no_fail(rtype);
		}
		// 2. If we have no common type, set to the return type.
		if (!common_type)
		{
			common_type = rtype;
			continue;
		}

		// 3. Same type -> we're done.
		if (common_type == rtype) continue;

		// 4. Find the max of the old and new.
		Type *max = type_find_max_type(common_type, rtype);

		// 5. No match -> error.
		if (!max)
		{
			SEMA_ERROR(return_stmt, "Cannot find a common parent type of %s and %s",
			           rtype, common_type);
			SEMA_PREV(context->returns[i - 1], "The previous return was here.");
			return NULL;
		}

		// 6. Set the new max, mark as needing a cast on all returns.
		common_type = max;
		all_returns_need_casts = true;
	}

	// If we have no return (or only anyfail)
	if (!common_type)
	{
		assert(!all_returns_need_casts && "We should never need casts here.");
		// A failable?
		if (failable)
		{
			// If there are only implicit returns, then we assume void!, otherwise it's an "anyfail"
			return no_return ? type_get_failable(type_void) : type_anyfail;
		}
		// No failable => void.
		return type_void;
	}

	// 7. Insert casts.
	if (all_returns_need_casts)
	{
		assert(!type_is_failable_type(common_type));
		VECEACH(context->returns, i)
		{
			Ast *return_stmt = context->returns[i];
			if (!return_stmt) continue;
			Expr *ret_expr = return_stmt->return_stmt.expr;
			// 8. All casts should work.
			if (!cast_implicit(ret_expr, common_type))
			{
				assert(false);
				return NULL;
			}
		}
	}

	return type_get_opt_fail(common_type, failable);
}

static inline bool sema_expr_analyse_func_call(SemaContext *context, Expr *expr, Decl *decl, Expr *struct_var, bool failable)
{
	expr->call_expr.is_pointer_call = false;
	return sema_expr_analyse_func_invocation(context, decl->type->func.prototype, &decl->func_decl.function_signature, expr, decl, struct_var, failable);
}


static bool sema_check_expr_compile_time(SemaContext *context, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_CONST:
			return true;
		case EXPR_MACRO_BLOCK:
		{
			AstId current = expr->macro_block.first_stmt;
			while (current)
			{
				if (!sema_check_stmt_compile_time(context, ast_next(&current))) return false;
			}
			return true;
		}
		default:
			return false;
	}
	UNREACHABLE
}

static bool sema_check_stmt_compile_time(SemaContext *context, Ast *ast)
{
	switch (ast->ast_kind)
	{
		case AST_NOP_STMT:
			return true;
		case AST_RETURN_STMT:
		case AST_BLOCK_EXIT_STMT:
			if (!ast->return_stmt.expr) return true;
			return expr_is_constant_eval(ast->return_stmt.expr, CONSTANT_EVAL_ANY);
		case AST_EXPR_STMT:
			return sema_check_expr_compile_time(context, ast->expr_stmt);
		case AST_COMPOUND_STMT:
		{
			AstId current = ast->compound_stmt.first_stmt;
			while (current)
			{
				if (!sema_check_stmt_compile_time(context, ast_next(&current))) return false;
			}
			return true;
		}
		default:
			return false;
	}
}

bool sema_expr_analyse_macro_call(SemaContext *context, Expr *call_expr, Expr *struct_var, Decl *decl, bool failable)
{
	assert(decl->decl_kind == DECL_MACRO);

	Decl **params = decl_copy_list(decl->macro_decl.parameters);
	CalledDecl callee = {
			.macro = true,
			.block_parameter = decl->macro_decl.body_param ? declptr(decl->macro_decl.body_param)->name : NULL,
			.params = params,
			.param_count = vec_size(params),
			.struct_var = struct_var
	};

	if (!sema_expr_analyse_call_invocation(context, call_expr, callee, &failable)) return false;

	Expr **args = call_expr->call_expr.arguments;
	VECEACH(params, i)
	{
		Decl *param = params[i];
		param->var.init_expr = args[i];
	}

	Decl **body_params = call_expr->call_expr.body_arguments;
	unsigned body_params_count = vec_size(body_params);
	Decl **macro_body_params = decl->macro_decl.body_param ? declptr(decl->macro_decl.body_param)->body_params : NULL;
	unsigned expected_body_params = vec_size(macro_body_params);
	if (expected_body_params > body_params_count)
	{
		SEMA_ERROR(call_expr, "Not enough parameters for the macro body, expected %d.", expected_body_params);
		return false;
	}
	if (expected_body_params < body_params_count)
	{
		SEMA_ERROR(call_expr, "Too many parameters for the macro body, expected %d.", expected_body_params);
		return false;
	}
	for (unsigned i = 0; i < expected_body_params; i++)
	{
		Decl *body_param = macro_body_params[i];
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

	Ast *body = ast_macro_copy(astptr(decl->macro_decl.body));

	DynamicScope old_scope = context->active_scope;
	context_change_scope_with_flags(context, SCOPE_NONE);

	SemaContext macro_context;

	Type *rtype = NULL;
	sema_context_init(&macro_context, decl->macro_decl.unit);
	macro_context.compilation_unit = context->unit;
	macro_context.current_function = context->current_function;
	rtype = decl->macro_decl.rtype ? type_infoptr(decl->macro_decl.rtype)->type : NULL;
	macro_context.expected_block_type = rtype;
	bool may_failable = true;
	if (rtype)
	{
		if (type_is_failable(rtype))
		{
			failable = true;
			rtype = type_no_fail(rtype);
		}
		else
		{
			may_failable = false;
		}
	}

	context_change_scope_with_flags(&macro_context, SCOPE_MACRO);

	macro_context.block_return_defer = macro_context.active_scope.defer_last;

	macro_context.current_macro = decl;
	AstId body_id = call_expr->call_expr.body;
	macro_context.yield_body = body_id ? astptr(body_id) : NULL;
	macro_context.yield_params = body_params;
	macro_context.yield_context = context;
	macro_context.original_inline_line = context->original_inline_line ? context->original_inline_line : call_expr->span.row;

	BlockExit** block_exit_ref = MALLOCS(BlockExit*);
	macro_context.block_exit_ref = block_exit_ref;

	VECEACH(params, i)
	{
		Decl *param = params[i];
		if (!sema_add_local(&macro_context, param)) goto EXIT_FAIL;
	}

	if (!sema_analyse_statement(&macro_context, body)) goto EXIT_FAIL;

	bool is_no_return = decl->macro_decl.attr_noreturn;

	if (!vec_size(macro_context.returns))
	{
		if (rtype && rtype != type_void)
		{
			SEMA_ERROR(decl,
					   "Missing return in macro that should evaluate to %s.",
					   type_quoted_error_string(rtype));
			return SCOPE_POP_ERROR();
		}
	}
	else if (is_no_return)
	{
		SEMA_ERROR(context->returns[0], "Return used despite macro being marked '@noreturn'.");
		return SCOPE_POP_ERROR();
	}

	if (rtype)
	{
		VECEACH(macro_context.returns, i)
		{
			Ast *return_stmt = macro_context.returns[i];
			if (!return_stmt)
			{
				assert(may_failable);
				continue;
			}
			Expr *ret_expr = return_stmt->return_stmt.expr;
			if (!ret_expr)
			{
				if (rtype == type_void) continue;
				SEMA_ERROR(return_stmt, "Expected returning a value of type %s.", type_quoted_error_string(rtype));
				return SCOPE_POP_ERROR();
			}
			Type *type = ret_expr->type;
			if (!cast_may_implicit(type, rtype, true, may_failable))
			{
				SEMA_ERROR(ret_expr, "Expected %s, not %s.", type_quoted_error_string(rtype),
						   type_quoted_error_string(type));
				return SCOPE_POP_ERROR();
			}
			bool success = cast_implicit(ret_expr, rtype);
			assert(success);
			if (may_failable) ret_expr->type = type_get_opt_fail(ret_expr->type, may_failable);
		}
		call_expr->type = type_get_opt_fail(rtype, failable);
	}
	else
	{
		Type *sum_returns = unify_returns(&macro_context);
		if (!sum_returns) return SCOPE_POP_ERROR();
		call_expr->type = type_get_opt_fail(sum_returns, failable);
	}

	if (call_expr->call_expr.result_unused)
	{
		Type *type = call_expr->type;
		if (type != type_void)
		{
			if (decl->macro_decl.attr_nodiscard)
			{
				SEMA_ERROR(call_expr, "The result of the macro must be used.");
				return SCOPE_POP_ERROR();
			}
			if (type_is_failable(type) && !decl->macro_decl.attr_maydiscard)
			{
				SEMA_ERROR(call_expr, "The optional result of the macro must be used.");
				return SCOPE_POP_ERROR();
			}
		}
	}
	unsigned returns_found = vec_size(macro_context.returns);
	// We may have zero normal macro returns but the active scope still has a "jump end".
	// In this case it is triggered by the @body()
	if (!returns_found && macro_context.active_scope.jump_end)
	{
		is_no_return = true;
	}
	if (returns_found == 1)
	{
		Ast *ret = macro_context.returns[0];
		Expr *result = ret ? ret->return_stmt.expr : NULL;
		if (result && expr_is_constant_eval(result, CONSTANT_EVAL_ANY))
		{
			if (sema_check_stmt_compile_time(&macro_context, body))
			{
				expr_replace(call_expr, result);
				goto EXIT;
			}
		}
	}

	call_expr->expr_kind = EXPR_MACRO_BLOCK;
	call_expr->macro_block.first_stmt = body->compound_stmt.first_stmt;
	call_expr->macro_block.params = params;
	call_expr->macro_block.args = args;
	call_expr->macro_block.block_exit = block_exit_ref;
EXIT:
	assert(context->active_scope.defer_last == context->active_scope.defer_start);
	context->active_scope = old_scope;
	if (is_no_return) context->active_scope.jump_end = true;
	return true;
EXIT_FAIL:
	return SCOPE_POP_ERROR();
}
static inline Decl *sema_generate_generic_function(SemaContext *context, Expr *call_expr, Expr *struct_var, Decl *decl, const char *mangled_name)
{
	TODO
	return NULL;
}

static inline bool sema_expr_analyse_generic_call(SemaContext *context, Expr *call_expr, Expr *struct_var, Decl *decl, bool failable)
{
	assert(decl->decl_kind == DECL_GENERIC);

	Expr **args = call_expr->call_expr.arguments;
	Decl **func_params = decl->macro_decl.parameters;

	unsigned explicit_args = vec_size(args);
	unsigned total_args = explicit_args;

	scratch_buffer_clear();
	scratch_buffer_append(decl->extname);

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

	unsigned offset = total_args - explicit_args;
	for (unsigned i = 0; i < explicit_args; i++)
	{
		Expr *arg = args[i];
		Decl *param = func_params[i + offset];
		if (param->var.kind == VARDECL_PARAM_CT_TYPE)
		{
			if (!sema_analyse_expr_lvalue(context, arg)) return false;
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
			if (!sema_analyse_expr_rhs(context, param->var.type_info->type, arg, true)) return false;
		}
		else
		{
			if (!sema_analyse_expr(context, arg)) return false;
		}
		scratch_buffer_append_char(',');
		scratch_buffer_append(arg->type->canonical->name);
	}
	const char *mangled_name = scratch_buffer_interned();
	Decl **generic_cache = decl->module->generic_cache;
	Decl *found = NULL;
	VECEACH(generic_cache, i)
	{
		TODO
		if (generic_cache[i]->extname == mangled_name)
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
	return sema_expr_analyse_func_call(context, call_expr, found, struct_var, failable);
}

static bool sema_analyse_body_expansion(SemaContext *macro_context, Expr *call)
{
	Decl *macro = macro_context->current_macro;
	assert(macro);
	DeclId body_param = macro->macro_decl.body_param;
	assert(body_param);

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
	// Theoretically we could support named arguments, but that's unnecessary.
	unsigned expressions = vec_size(call_expr->arguments);
	Decl **body_parameters = declptr(body_param)->body_params;
	if (expressions != vec_size(body_parameters))
	{
		SEMA_ERROR(call, "Expected %d parameter(s).", vec_size(body_parameters));
		return false;
	}
	Expr **args = call_expr->arguments;

	// Evaluate the expressions. TODO hash expressions
	for (unsigned i = 0; i < expressions; i++)
	{
		Expr *expr = args[i];
		if (!sema_analyse_expr(macro_context, expr)) return false;
	}

	AstId macro_defer = macro_context->active_scope.defer_last;
	Ast *first_defer = NULL;
	SemaContext *context = macro_context->yield_context;
	Decl **params = macro_context->yield_params;

	Expr *func_expr = exprptr(call_expr->function);
	assert(func_expr->expr_kind == EXPR_MACRO_BODY_EXPANSION);
	expr_replace(call, func_expr);
	call->body_expansion_expr.values = args;
	call->body_expansion_expr.declarations = macro_context->yield_params;
	AstId last_defer = context->active_scope.defer_last;
	bool success;
	bool ends_in_jump;
	SCOPE_START

		if (macro_defer)
		{
			Ast *macro_defer_ast = astptr(macro_defer);
			first_defer = macro_defer_ast;
			while (first_defer->defer_stmt.prev_defer)
			{
				first_defer = astptr(first_defer->defer_stmt.prev_defer);
			}
			first_defer->defer_stmt.prev_defer = context->active_scope.defer_last;
			context->active_scope.defer_last = macro_defer;
		}
		VECEACH(params, i)
		{
			Decl *param = params[i];
			if (!sema_add_local(context, param)) return SCOPE_POP_ERROR();
		}
		Ast *ast = call->body_expansion_expr.ast = ast_macro_copy(macro_context->yield_body);
		if (!sema_analyse_statement(context, ast)) return SCOPE_POP_ERROR();
		assert(ast->ast_kind == AST_COMPOUND_STMT);
		if (context->active_scope.jump_end)
		{
			macro_context->active_scope.jump_end = true;
		}
		if (first_defer)
		{
			first_defer->defer_stmt.prev_defer = 0;
			context->active_scope.defer_last = last_defer;
		}
	SCOPE_END;

	return true;
}

bool sema_expr_analyse_general_call(SemaContext *context, Expr *expr, Decl *decl, Expr *struct_var, bool failable)
{
	expr->call_expr.is_type_method = struct_var != NULL;
	if (decl == NULL)
	{
		return sema_expr_analyse_var_call(context, expr, type_flatten_distinct_failable(exprptr(expr->call_expr.function)->type), failable);
	}
	switch (decl->decl_kind)
	{
		case DECL_MACRO:
			expr->call_expr.func_ref = declid(decl);
			expr->call_expr.is_func_ref = true;
			return sema_expr_analyse_macro_call(context, expr, struct_var, decl, failable);
		case DECL_VAR:
			assert(struct_var == NULL);
			return sema_expr_analyse_var_call(context, expr, decl->type->canonical, failable || IS_FAILABLE(decl));
		case DECL_FUNC:
			expr->call_expr.func_ref = declid(decl);
			expr->call_expr.is_func_ref = true;
			return sema_expr_analyse_func_call(context, expr, decl, struct_var, failable);
		case DECL_GENERIC:
			expr->call_expr.func_ref = declid(decl);
			expr->call_expr.is_func_ref = true;
			return sema_expr_analyse_generic_call(context, expr, struct_var, decl, failable);
		case DECL_POISONED:
			return false;
		default:
			SEMA_ERROR(expr, "This expression cannot be called.");
			return false;
	}
}



static inline unsigned builtin_expected_args(BuiltinFunction func)
{
	switch (func)
	{
		case BUILTIN_UNREACHABLE:
		case BUILTIN_TRAP:
		case BUILTIN_STACKTRACE:
			return 0;
		case BUILTIN_CEIL:
		case BUILTIN_TRUNC:
		case BUILTIN_SQRT:
		case BUILTIN_COS:
		case BUILTIN_SIN:
		case BUILTIN_EXP:
		case BUILTIN_LOG:
		case BUILTIN_LOG2:
		case BUILTIN_LOG10:
		case BUILTIN_FABS:
		case BUILTIN_VOLATILE_LOAD:
			return 1;
		case BUILTIN_POW:
		case BUILTIN_MAX:
		case BUILTIN_MIN:
		case BUILTIN_VOLATILE_STORE:
			return 2;
		case BUILTIN_FMA:
			return 3;
		case BUILTIN_MEMSET:
			return 5;
		case BUILTIN_MEMCOPY:
			return 6;
		case BUILTIN_NONE:
			UNREACHABLE
	}
	UNREACHABLE
}

typedef enum
{
	BA_POINTER,
	BA_SIZE,
	BA_BOOL,
	BA_CHAR,
	BA_FLOATLIKE,
} BuiltinArg;

static bool sema_check_builtin_args_match(Expr **args, size_t arg_len)
{
	Type *first = args[0]->type->canonical;
	for (size_t i = 1; i < arg_len; i++)
	{
		if (first != args[i]->type->canonical)
		{
			SEMA_ERROR(args[i], "Expected an expression of type %s.", type_quoted_error_string(args[0]->type));
			return false;
		}
	}
	return true;
}

static bool sema_check_builtin_args_const(Expr **args, size_t arg_len)
{
	for (size_t i = 0; i < arg_len; i++)
	{
		if (!expr_is_const(args[i]))
		{
			SEMA_ERROR(args[i], "Expected a compile time constant value for this argument.");
			return false;
		}
	}
	return true;
}
static bool sema_check_builtin_args(Expr **args, BuiltinArg *arg_type, size_t arg_len)
{
	for (size_t i = 0; i < arg_len; i++)
	{
		Type *type = args[i]->type->canonical;
		switch (arg_type[i])
		{
			case BA_POINTER:
				if (!type_is_pointer(type))
				{
					SEMA_ERROR(args[i], "Expected a pointer.");
					return false;
				}
				break;
			case BA_CHAR:
				if (type != type_char && type != type_ichar)
				{
					SEMA_ERROR(args[i], "Expected a char or ichar.");
					return false;
				}
				break;
			case BA_SIZE:
				if (!type_is_integer(type) || type_size(type) != type_size(type_usize))
				{
					SEMA_ERROR(args[i], "Expected an usize or isize value.");
					return false;
				}
				break;
			case BA_BOOL:
				if (type != type_bool)
				{
					SEMA_ERROR(args[i], "Expected a bool.");
					return false;
				}
				break;
			case BA_FLOATLIKE:
				if (!type_is_float_or_float_vector(type))
				{
					SEMA_ERROR(args[i], "Expected a floating point or floating point array.");
					return false;
				}
				break;
		}
	}
	return true;
}

static inline bool sema_expr_analyse_builtin_call(SemaContext *context, Expr *expr)
{
	expr->call_expr.is_builtin = true;
	BuiltinFunction func = exprptr(expr->call_expr.function)->builtin_expr.builtin;
	unsigned expected_args = builtin_expected_args(func);
	Expr **args = expr->call_expr.arguments;
	unsigned arg_count = vec_size(args);

	// 1. Handle arg count, so at least we know that is ok.
	if (expected_args != arg_count)
	{
		if (arg_count == 0)
		{
			SEMA_ERROR(expr, "Expected %d arguments to builtin.\n", expected_args);
			return false;
		}
		if (arg_count < expected_args)
		{
			SEMA_ERROR(args[arg_count - 1], "Expected more arguments after this one.");
			return false;
		}
		SEMA_ERROR(args[expected_args], "Too many arguments.");
		return false;
	}

	bool failable = false;

	// 2. We can now check all the arguments, since they in general work on the
	//    exact type size, we don't do any forced promotion.
	for (unsigned i = 0; i < arg_count; i++)
	{
		if (!sema_analyse_expr(context, args[i])) return false;
		failable = failable || type_is_failable(args[i]->type);
	}

	Type *rtype = NULL;
	switch (func)
	{
		case BUILTIN_UNREACHABLE:
		case BUILTIN_TRAP:
			rtype = type_void;
			break;
		case BUILTIN_MEMCOPY:
			if (!sema_check_builtin_args(args,
										 (BuiltinArg[]) { BA_POINTER, BA_POINTER, BA_SIZE, BA_BOOL, BA_SIZE, BA_SIZE },
										 arg_count)) return false;
			if (!sema_check_builtin_args_const(&args[3], 3)) return false;
			rtype = type_void;
			break;
		case BUILTIN_MEMSET:
			if (!sema_check_builtin_args(args,
										 (BuiltinArg[]) { BA_POINTER, BA_CHAR, BA_SIZE, BA_BOOL, BA_SIZE },
										 arg_count)) return false;
			if (!sema_check_builtin_args_const(&args[3], 2)) return false;
			rtype = type_void;
			break;
		case BUILTIN_STACKTRACE:
			rtype = type_voidptr;
			break;
		case BUILTIN_CEIL:
		case BUILTIN_TRUNC:
		case BUILTIN_SQRT:
		case BUILTIN_COS:
		case BUILTIN_SIN:
		case BUILTIN_EXP:
		case BUILTIN_FABS:
		case BUILTIN_LOG:
		case BUILTIN_LOG2:
		case BUILTIN_LOG10:
			if (!sema_check_builtin_args(args,
										 (BuiltinArg[]) { BA_FLOATLIKE },
										 arg_count)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_POW:
		case BUILTIN_MAX:
		case BUILTIN_MIN:
			if (!sema_check_builtin_args(args,
										 (BuiltinArg[]) { BA_FLOATLIKE, BA_FLOATLIKE },
										 arg_count)) return false;
			if (!sema_check_builtin_args_match(args, arg_count)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_FMA:
			if (!sema_check_builtin_args(args,
										 (BuiltinArg[]) { BA_FLOATLIKE, BA_FLOATLIKE, BA_FLOATLIKE },
										 arg_count)) return false;
			if (!sema_check_builtin_args_match(args, arg_count)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_VOLATILE_LOAD:
		{
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_POINTER }, 1)) return false;
			rtype = args[0]->type->canonical->pointer;
			break;
		}
		case BUILTIN_VOLATILE_STORE:
		{
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_POINTER }, 1)) return false;
			rtype = args[0]->type->canonical->pointer;
			if (!cast_implicit(args[1], rtype)) return false;
			break;
		}
		case BUILTIN_NONE:
			UNREACHABLE
	}
	expr->type = type_get_opt_fail(rtype, failable);
	return true;
}

static inline bool sema_expr_analyse_call(SemaContext *context, Expr *expr)
{
	Expr *func_expr = exprptr(expr->call_expr.function);

	if (!sema_analyse_expr_lvalue(context, func_expr)) return false;
	if (func_expr->expr_kind == EXPR_MACRO_BODY_EXPANSION)
	{
		return sema_analyse_body_expansion(context, expr);
	}
	bool failable = func_expr->type && IS_FAILABLE(func_expr);
	Decl *decl;
	Expr *struct_var = NULL;
	switch (func_expr->expr_kind)
	{
		case EXPR_BUILTIN:
			return sema_expr_analyse_builtin_call(context, expr);
		case EXPR_IDENTIFIER:
			decl = func_expr->identifier_expr.decl;
			break;
		case EXPR_ACCESS:
			decl = func_expr->access_expr.ref;
			switch (decl->decl_kind)
			{
				case DECL_MACRO:
					if (decl->macro_decl.parameters[0]->type->type_kind == TYPE_POINTER)
					{
						expr_insert_addr(func_expr->access_expr.parent);
					}
					struct_var = func_expr->access_expr.parent;
					break;
				case DECL_FUNC:
					if (decl->func_decl.function_signature.params[0]->type->type_kind == TYPE_POINTER)
					{
						expr_insert_addr(func_expr->access_expr.parent);
					}
					struct_var = func_expr->access_expr.parent;
					break;
				default:
					break;
			}
			break;
		case EXPR_TYPEINFO:
			if (func_expr->type_expr->resolve_status == RESOLVE_DONE)
			{
				SEMA_ERROR(expr, "A type cannot be followed by (), if you intended a cast, use (type)(expression).");
			}
			else
			{
				SEMA_ERROR(expr, "A type cannot be followed by (), did you mean to use {}?");
			}
			return false;
		default:
		{
			Type *type = type_flatten_distinct(func_expr->type);
			if (type->type_kind == TYPE_POINTER && type->pointer->type_kind == TYPE_FUNC)
			{
				decl = NULL;
				break;
			}
			SEMA_ERROR(expr, "This value cannot be invoked, did you accidentally add ()?");
			return false;

		}
	}
	decl = decl ? decl_flatten(decl) : NULL;
	return sema_expr_analyse_general_call(context, expr, decl, struct_var, failable);
}

static void sema_deref_array_pointers(Expr *expr)
{
	Type *expr_type = expr->type->canonical;
	if (expr_type->type_kind == TYPE_POINTER)
	{
		switch (expr_type->pointer->type_kind)
		{
			case TYPE_ARRAY:
			case TYPE_VECTOR:
				expr_insert_deref(expr);
				break;
			default:
				break;
		}
	}
}

static bool expr_check_len_in_range(SemaContext *context, Type *type, Expr *len_expr, bool from_end, bool *remove_from_end)
{
	assert(type == type->canonical);
	if (len_expr->expr_kind != EXPR_CONST) return true;

	Int const_len = len_expr->const_expr.ixx;
	if (!int_fits(const_len, TYPE_I64))
	{
		SEMA_ERROR(len_expr, "The length cannot be stored in a 64-signed integer, which isn't supported.");
		return false;
	}
	if (int_is_neg(const_len))
	{
		SEMA_ERROR(len_expr, "The length may not be negative.");
		return false;
	}
	MemberIndex len_val = (MemberIndex)const_len.i.low;
	switch (type->type_kind)
	{
		case TYPE_POINTER:
		case TYPE_FLEXIBLE_ARRAY:
			assert(!from_end);
			FALLTHROUGH;
		case TYPE_SUBARRAY:
			return true;
		case TYPE_ARRAY:
		case TYPE_VECTOR:
		{
			MemberIndex len = (MemberIndex)type->array.len;
			bool is_vector = type->type_kind == TYPE_VECTOR;
			if (from_end)
			{
				if (len_val > len)
				{
					SEMA_ERROR(len_expr, "This would result in a negative length.");
					return false;
				}
				len_expr->const_expr.ixx.i.low = len - len_val;
				*remove_from_end = true;
				return true;
			}
			// Checking end can only be done for arrays and vectors.
			if (len_val > len)
			{
				SEMA_ERROR(len_expr,
				           is_vector ? "Length out of bounds, was %lld, exceeding vector length %lld."
				                     : "Array length out of bounds, was %lld, exceeding array length %lld.",
				           (long long)len_val, (long long)len);
				return false;
			}
			return true;
		}
		default:
			UNREACHABLE
	}
	UNREACHABLE
}

static bool expr_check_index_in_range(SemaContext *context, Type *type, Expr *index_expr, bool end_index, bool from_end, bool *remove_from_end)
{
	assert(type == type->canonical);
	if (index_expr->expr_kind != EXPR_CONST) return true;

	Int index = index_expr->const_expr.ixx;
	if (!int_fits(index, TYPE_I64))
	{
		SEMA_ERROR(index_expr, "The index cannot be stored in a 64-signed integer, which isn't supported.");
		return false;
	}
	if (from_end && int_is_neg(index))
	{
		SEMA_ERROR(index_expr, "Negative numbers are not allowed when indexing from the end.");
		return false;
	}
	MemberIndex idx = (MemberIndex)index.i.low;
	switch (type->type_kind)
	{
		case TYPE_POINTER:
		case TYPE_FLEXIBLE_ARRAY:
			assert(!from_end);
			return true;
		case TYPE_ARRAY:
		case TYPE_VECTOR:
		{
			MemberIndex len = (MemberIndex)type->array.len;
			bool is_vector = type->type_kind == TYPE_VECTOR;
			if (from_end)
			{
				idx = len - idx;
				index_expr->const_expr.ixx.i.low = idx;
				*remove_from_end = true;
			}
			// Checking end can only be done for arrays.
			if (end_index && idx >= len)
			{
				SEMA_ERROR(index_expr,
						   is_vector ? "End index out of bounds, was %lld, exceeding vector width %lld."
						   : "Array end index out of bounds, was %lld, exceeding array length %lld.",
						   (long long)idx, (long long)len);
				return false;
			}
			if (!end_index && idx >= len)
			{
				if (len == 0)
				{
					SEMA_ERROR(index_expr, "Cannot index into a zero size array.");
					return false;
				}
				SEMA_ERROR(index_expr,
						   is_vector ? "Index out of bounds, was %lld, exceeding max vector width %lld."
						   : "Array index out of bounds, was %lld, exceeding max array index %lld.", (long long)idx, (long long)len - 1);
				return false;
			}
			break;
		}
		case TYPE_SUBARRAY:
			// If not from end, just check the negative values.
			if (!from_end) break;
			// From end we can only do sanity checks ^0 is invalid for non-end index. ^-1 and less is invalid for all.
			if (idx == 0 && !end_index)
			{
				SEMA_ERROR(index_expr,
				           "Array index out of bounds, index from end (%lld) must be greater than zero or it will exceed the max array index.",
				           (long long) idx);
				return false;
			}
			return true;
		default:
			UNREACHABLE
	}
	if (idx < 0)
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

static inline ConstInitializer *initializer_for_index(ConstInitializer *initializer, uint32_t index)
{
	switch (initializer->kind)
	{
		case CONST_INIT_ZERO:
		case CONST_INIT_STRUCT:
		case CONST_INIT_UNION:
		case CONST_INIT_VALUE:
			return initializer;
		case CONST_INIT_ARRAY_FULL:
			return initializer->init_array_full[index];
		case CONST_INIT_ARRAY:
		{
			ConstInitializer **sub_values = initializer->init_array.elements;
			VECEACH(sub_values, i)
			{
				ConstInitializer *init = sub_values[i];
				assert(init->kind == CONST_INIT_ARRAY_VALUE);
				if (init->init_array_value.index == index) return init->init_array_value.element;
			}
			return NULL;
		}
		case CONST_INIT_ARRAY_VALUE:
			UNREACHABLE
	}
	UNREACHABLE
}
static inline bool sema_expr_index_const_list(Expr *const_list, Expr *index, Expr *result)
{
	assert(index->expr_kind == EXPR_CONST && index->const_expr.const_kind == CONST_INTEGER);
	if (!int_fits(index->const_expr.ixx, TYPE_U32)) return false;

	uint32_t idx = index->const_expr.ixx.i.low;
	assert(const_list->const_expr.const_kind == CONST_LIST);

	ConstInitializer *initializer = initializer_for_index(const_list->const_expr.list, idx);
	ConstInitType kind = initializer ? initializer->kind : CONST_INIT_ZERO;
	switch (kind)
	{
		case CONST_INIT_ZERO:
			expr_rewrite_to_int_const(result, type_int, 0, true);
			return true;
		case CONST_INIT_STRUCT:
		case CONST_INIT_UNION:
		case CONST_INIT_ARRAY:
		case CONST_INIT_ARRAY_FULL:
		case CONST_INIT_ARRAY_VALUE:
			return false;
		case CONST_INIT_VALUE:
			expr_replace(result, initializer->init_value);
			return true;
	}
	UNREACHABLE
}

static inline bool sema_expr_analyse_subscript(SemaContext *context, Expr *expr, bool is_addr)
{
	assert(expr->expr_kind == EXPR_SUBSCRIPT || expr->expr_kind == EXPR_SUBSCRIPT_ADDR);

	// 1. Evaluate the expression to index.
	Expr *subscripted = exprptr(expr->subscript_expr.expr);
	if (!sema_analyse_expr_lvalue(context, subscripted)) return false;
	sema_deref_array_pointers(subscripted);

	// 2. Evaluate the index.
	Expr *index = exprptr(expr->subscript_expr.index);
	if (!sema_analyse_expr(context, index)) return false;

	// 3. Check failability due to value.
	bool failable = IS_FAILABLE(subscripted);

	Type *underlying_type = type_flatten(subscripted->type);

	Type *current_type = underlying_type;
	Expr *current_expr = subscripted;

	// 4. If we are indexing into a complist
	if (current_type == type_complist)
	{
		if (is_addr)
		{
			SEMA_ERROR(subscripted, "You need to use && to take the address of a temporary.");
			return false;
		}
		// 4a. This may either be an initializer list or a CT value
		while (current_expr->expr_kind == EXPR_CT_IDENT) current_expr = current_expr->ct_ident_expr.decl->var.init_expr;

		assert(current_expr->expr_kind == EXPR_INITIALIZER_LIST);

		// 4b. Now we need to check that we actually have a valid type.
		if (index->expr_kind != EXPR_CONST || index->const_expr.const_kind != CONST_INTEGER)
		{
			SEMA_ERROR(index, "To subscript a compile time list a compile time integer index is needed.");
			return false;
		}
		// 4c. And that it's in range.
		if (int_is_neg(index->const_expr.ixx))
		{
			SEMA_ERROR(index, "The index may not be negative.");
			return false;
		}
		int64_t size = vec_size(current_expr->initializer_list);
		assert(size >= 0 && "Unexpected overflow");
		if (!int_fits(index->const_expr.ixx, TYPE_I64))
		{
			SEMA_ERROR(index, "The index is out of range.", size);
			return false;
		}
		int64_t i = int_to_i64(index->const_expr.ixx);
		if (expr->subscript_expr.from_back)
		{
			i = size - i;
		}
		if (i < 0 || i >= size)
		{
			if (expr->subscript_expr.from_back)
			{
				SEMA_ERROR(index,
				           size > 1
				           ? "An index of '%lld' from the end is out of range, a value between 1 and %lld was expected."
				           : "An index of '%lld' from the end is out of range, a value of %lld was expected.",
				           (long long)(size - i),
				           (long long)size);
			}
			else
			{
				SEMA_ERROR(index,
						   size > 1
						   ? "An index of '%lld' is out of range, a value between 0 and %lld was expected."
						   : "An index of '%lld' is out of range, a value of %lld was expected.",
						   (long long)i,
						   (long long)size - 1);
			}
			return false;
		}
		Expr *indexed_expr = current_expr->initializer_list[i];
		if (!sema_cast_rvalue(context, indexed_expr)) return false;
		expr_replace(expr, indexed_expr);
		return true;
	}

	if (!sema_cast_rvalue(context, current_expr)) return false;

	Type *inner_type = sema_expr_find_indexable_type_recursively(&current_type, &current_expr);
	if (!inner_type)
	{
		Decl *decl = NULL;
		if (is_addr) decl = sema_find_operator(context, current_expr, OVERLOAD_ELEMENT_REF);
		if (!decl)
		{
			decl = sema_find_operator(context, current_expr, OVERLOAD_ELEMENT_AT);
			if (decl && is_addr)
			{
				SEMA_ERROR(expr, "A function or macro with '@operator(%s)' is not defined for %s, so you need && to take the address of the temporary.",
				           kw_elementref, type_quoted_error_string(current_expr->type));
				return false;
			}
		}
		if (decl)
		{
			expr->expr_kind = EXPR_CALL;
			Expr **args = NULL;
			vec_add(args, index);
			expr->call_expr = (ExprCall){ .func_ref = declid(decl), .is_func_ref = true, .arguments = args };
			expr->call_expr.is_type_method = true;
			return sema_expr_analyse_macro_call(context, expr, current_expr, decl, failable);
		}
	}
	if (!inner_type)
	{
		SEMA_ERROR(subscripted, "Cannot index '%s'.", type_to_error_string(subscripted->type));
		return false;
	}

	// Cast to an appropriate type for index.
	if (!expr_cast_to_index(index)) return false;

	// Check range
	bool remove_from_back = false;
	if (!expr_check_index_in_range(context, current_type, index, false, expr->subscript_expr.from_back, &remove_from_back)) return false;
	if (remove_from_back) expr->subscript_expr.from_back = false;

	if (is_addr)
	{
		inner_type = type_get_ptr(inner_type);
	}
	else
	{
		if (current_expr->expr_kind == EXPR_CONST && current_expr->const_expr.list && index->expr_kind == EXPR_CONST)
		{
			if (sema_expr_index_const_list(current_expr, index, expr)) return true;
		}
	}
	expr->subscript_expr.expr = exprid(current_expr);
	expr->type = type_get_opt_fail(inner_type, failable);
	return true;
}

static inline bool sema_expr_analyse_slice(SemaContext *context, Expr *expr)
{
	assert(expr->expr_kind == EXPR_SLICE);
	Expr *subscripted = exprptr(expr->slice_expr.expr);
	if (!sema_analyse_expr(context, subscripted)) return false;
	bool failable = IS_FAILABLE(subscripted);
	Type *type = type_flatten(subscripted->type);
	Type *original_type = type_no_fail(subscripted->type);
	Expr *start = exprptr(expr->slice_expr.start);
	Expr *end = exprptrzero(expr->slice_expr.end);

	Expr *current_expr = subscripted;

	Type *inner_type = sema_expr_find_indexable_type_recursively(&type, &current_expr);
	if (!inner_type)
	{
		SEMA_ERROR(subscripted, "Cannot index '%s'.", type_to_error_string(subscripted->type));
		return false;
	}
	expr->slice_expr.expr = exprid(current_expr);

	if (!sema_analyse_expr(context, start)) return false;
	if (end && !sema_analyse_expr(context, end)) return false;

	// Fix index sizes
	if (!expr_cast_to_index(start)) return false;
	if (end && !expr_cast_to_index(end)) return false;
	if (end && end->type != start->type)
	{
		Type *common = type_find_max_type(start->type, end->type);
		if (!common || !cast_implicit(start, common) || !cast_implicit(end, common)) return false;
	}

	// Check range
	if (type->type_kind == TYPE_POINTER)
	{
		if (expr->slice_expr.start_from_back)
		{
			SEMA_ERROR(start, "Indexing from the end is not allowed for pointers.");
			return false;
		}
		if (!end)
		{
			SEMA_ERROR(expr, "Omitting end index is not allowed for pointers.");
			return false;
		}
		if (end && expr->slice_expr.end_from_back)
		{
			SEMA_ERROR(end, "Indexing from the end is not allowed for pointers.");
			return false;
		}
	}
	bool is_lenrange = expr->slice_expr.is_lenrange;
	bool remove_from_end = false;
	if (!expr_check_index_in_range(context, type, start, false, expr->slice_expr.start_from_back, &remove_from_end)) return false;
	if (remove_from_end) expr->slice_expr.start_from_back = false;
	remove_from_end = false;
	if (end)
	{
		if (is_lenrange)
		{
			if (!expr_check_len_in_range(context, type, end, expr->slice_expr.end_from_back, &remove_from_end)) return false;
		}
		else
		{
			if (!expr_check_index_in_range(context, type, end, true, expr->slice_expr.end_from_back, &remove_from_end)) return false;
		}
	}
	if (remove_from_end) expr->slice_expr.end_from_back = false;

	if (start && end && start->expr_kind == EXPR_CONST && end->expr_kind == EXPR_CONST)
	{
		if (!is_lenrange && expr->slice_expr.start_from_back && expr->slice_expr.end_from_back)
		{
			if (expr_const_compare(&start->const_expr, &end->const_expr, BINARYOP_LT))
			{
				SEMA_ERROR(start, "Start index greater than end index.");
				return false;
			}
		}
		else if (!is_lenrange && !expr->slice_expr.start_from_back && !expr->slice_expr.end_from_back)
		{
			if (expr_const_compare(&start->const_expr, &end->const_expr, BINARYOP_GT))
			{
				SEMA_ERROR(start, "Start index greater than end index.");
				return false;
			}
		}
		// If both are
		if (type->type_kind == TYPE_ARRAY || type->type_kind == TYPE_VECTOR)
		{
			assert(!expr->slice_expr.start_from_back);
			assert(!expr->slice_expr.end_from_back);
			if (!is_lenrange)
			{
				end->const_expr.ixx = int_sub(int_add64(end->const_expr.ixx, 1), start->const_expr.ixx);
				is_lenrange = expr->slice_expr.is_lenrange = true;
			}
		}
	}

	// Retain the original type when doing distinct slices.
	Type *result_type = type_get_subarray(inner_type);
	Type *original_type_canonical = original_type->canonical;
	if (original_type_canonical->type_kind == TYPE_DISTINCT && type_flatten_distinct(original_type_canonical) == result_type)
	{
		result_type = original_type;
	}
	expr->type = type_get_opt_fail(result_type, failable);
	return true;
}


static inline bool sema_expr_analyse_group(SemaContext *context, Expr *expr)
{
	if (!sema_analyse_expr(context, expr->inner_expr)) return false;
	*expr = *expr->inner_expr;
	return true;
}


void expr_rewrite_to_int_const(Expr *expr_to_rewrite, Type *type, uint64_t value, bool narrowable)
{
	expr_to_rewrite->expr_kind = EXPR_CONST;
	expr_const_set_int(&expr_to_rewrite->const_expr, value, type->canonical->type_kind);
	expr_to_rewrite->type = type;
	expr_to_rewrite->const_expr.narrowable = narrowable;
	expr_to_rewrite->const_expr.is_hex = false;
	expr_to_rewrite->resolve_status = RESOLVE_DONE;
}


void expr_rewrite_to_string(Expr *expr_to_rewrite, const char *string)
{
	expr_to_rewrite->expr_kind = EXPR_CONST;
	expr_to_rewrite->const_expr.const_kind = CONST_STRING;
	expr_to_rewrite->const_expr.string.chars = (char *)string;
	ArraySize len = (ArraySize)strlen(string);
	expr_to_rewrite->const_expr.string.len = len;
	expr_to_rewrite->resolve_status = RESOLVE_DONE;
	expr_to_rewrite->type = type_get_ptr(type_get_array(type_char, len));
}





static void add_members_to_context(SemaContext *context, Decl *decl)
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
	if (decl_is_enum_kind(decl))
	{
		Decl **members = decl->enums.parameters;
		VECEACH(members, i)
		{
			sema_add_member(context, members[i]);
		}
	}
	if (decl_is_struct_type(decl) || decl->decl_kind == DECL_BITSTRUCT)
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


/**
 * 1. .A -> It is an enum constant.
 * 2. .foo -> It is a function.
 * 3. .@foo -> It is a macro.
 * 4. .#bar -> It is an identifier to resolve as a member or a function
 * 5. .@#bar -> It is an identifier to resolve as a macro
 * 6. .$eval(...) -> resolve the eval and retry.
 */
static Expr *sema_expr_resolve_access_child(SemaContext *context, Expr *child, bool *missing)
{
RETRY:
	switch (child->expr_kind)
	{
		case EXPR_IDENTIFIER:
			// A path is not allowed.
			if (child->identifier_expr.path) break;
			return child;
		case EXPR_HASH_IDENT:
		{
			Decl *decl = sema_resolve_symbol(context, child->hash_ident_expr.identifier, NULL, child->span);
			if (!decl_ok(decl)) return NULL;
			return sema_expr_resolve_access_child(context, decl->var.init_expr, missing);
		}
		case EXPR_CT_EVAL:
		{
			Expr *inner = child->inner_expr;
			TokenType type;
			// Only report missing if missing var is NULL
			Path *path = NULL;
			const char *ident = ct_eval_expr(context, "$eval", inner, &type, &path, missing == NULL);
			if (!ident && missing)
			{
				*missing = true;
				return NULL;
			}
			if (ident == ct_eval_error) return NULL;
			switch (type)
			{
				case TOKEN_IDENT:
				case TOKEN_CONST_IDENT:
					child->expr_kind = EXPR_IDENTIFIER;
					child->identifier_expr.ident = ident;
					child->identifier_expr.path = path;
					child->identifier_expr.is_const = type == TOKEN_CONST_IDENT;
					goto RETRY;
				default:
					SEMA_ERROR(inner, "Only function, variable and constant names may be resolved with $eval.");
					return NULL;
			}
		}
		default:
			break;

	}
	SEMA_ERROR(child, "Expected an identifier here.");
	return NULL;
}

static inline void expr_replace_with_enum_array(Expr *enum_array_expr, Decl *enum_decl)
{
	Decl **values = enum_decl->enums.values;
	SourceSpan span = enum_array_expr->span;
	Expr *initializer = expr_new(EXPR_INITIALIZER_LIST, span);
	ArraySize elements = vec_size(values);
	Expr **element_values = elements > 0 ? VECNEW(Expr*, elements) : NULL;
	Type *kind = enum_decl->type;
	for (ArraySize i = 0; i < elements; i++)
	{
		Decl *decl = values[i];
		Expr *expr = expr_new(EXPR_CONST, span);
		expr->const_expr.const_kind = CONST_ENUM;
		expr->const_expr.enum_val = decl;
		assert(decl_ok(decl));
		expr->type = kind;
		expr->resolve_status = RESOLVE_DONE;
		vec_add(element_values, expr);
	}
	initializer->initializer_list = element_values;
	enum_array_expr->expr_kind = EXPR_COMPOUND_LITERAL;
	enum_array_expr->expr_compound_literal.initializer = initializer;
	enum_array_expr->expr_compound_literal.type_info = type_info_new_base(type_get_array(kind, elements), span);
	enum_array_expr->resolve_status = RESOLVE_NOT_DONE;
}

static inline bool sema_expr_analyse_type_access(SemaContext *context, Expr *expr, TypeInfo *parent, bool was_group, Expr *identifier)
{
	assert(identifier->expr_kind == EXPR_IDENTIFIER);

	Type *canonical = parent->type->canonical;
	const char *name = identifier->identifier_expr.ident;
	bool is_const = identifier->identifier_expr.is_const;

	if (name == kw_sizeof)
	{
		expr_rewrite_to_int_const(expr, type_usize, type_size(canonical), true);
		return true;
	}

	if (!is_const)
	{
		if (sema_expr_apply_type_property(context, expr, canonical, name)) return true;
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
			if (is_const)
			{
				if (!sema_expr_analyse_enum_constant(expr, name, decl))
				{
					SEMA_ERROR(expr, "'%s' has no enumeration value '%s'.", decl->name, name);
					return false;
				}
				return true;
			}
			if (name == kw_elements)
			{
				expr_rewrite_to_int_const(expr, type_isize, vec_size(decl->enums.values), true);
				return true;
			}
			if (name == kw_values)
			{
				expr_replace_with_enum_array(expr, decl);
				return sema_analyse_expr(context, expr);
				return true;
			}
			break;
		case DECL_FAULT:
			unit_register_external_symbol(context->compilation_unit, decl);
			if (is_const)
			{
				if (!sema_expr_analyse_enum_constant(expr, name, decl))
				{
					SEMA_ERROR(expr, "'%s' has no error value '%s'.", decl->name, name);
					return false;
				}
				return true;
			}
			if (name == kw_elements)
			{
				expr_rewrite_to_int_const(expr, type_isize, vec_size(decl->enums.values), true);
				return true;
			}
			break;
		case DECL_UNION:
		case DECL_STRUCT:
		case DECL_DISTINCT:
		case DECL_BITSTRUCT:
			break;
		default:
			UNREACHABLE
	}

	Decl *member = NULL;
	SCOPE_START
		add_members_to_context(context, decl);
		member = sema_resolve_symbol_in_current_dynamic_scope(context, name);
	SCOPE_END;
	if (!member)
	{
		SEMA_ERROR(expr, "No method or inner struct/union '%s.%s' found.", type_to_error_string(decl->type), name);
		return false;
	}

	if (member->decl_kind == DECL_VAR)
	{
		expr->expr_kind = EXPR_TYPEINFO;
		expr->type_expr->type = member->type;
		expr->type_expr->resolve_status = RESOLVE_DONE;
		expr->type = type_typeinfo;
		return true;
	}

	if (member->decl_kind == DECL_UNION || member->decl_kind == DECL_STRUCT || member->decl_kind == DECL_BITSTRUCT)
	{
		expr->expr_kind = EXPR_TYPEINFO;
		expr->type_expr->type = member->type;
		expr->type_expr->resolve_status = RESOLVE_DONE;
		expr->type = type_typeinfo;
		return true;
	}


	expr->identifier_expr.ident = name;
	expr->expr_kind = EXPR_IDENTIFIER;
	expr->identifier_expr.decl = member;
	expr->type = member->type;
	return true;
}

static inline void sema_rewrite_typeid_kind(Expr *expr, Expr *parent)
{
	Module *module = global_context_find_module(kw_std__core__types);
	Decl *type_kind = module ? module_find_symbol(module, kw_typekind) : NULL;
	Type *type_for_kind = type_kind ? type_kind->type : type_char;
	expr->expr_kind = EXPR_TYPEID_INFO;
	expr->typeid_info_expr.parent = exprid(parent);
	expr->typeid_info_expr.kind = TYPEID_INFO_KIND;
	expr->type = type_for_kind;
}


static inline bool sema_create_const_kind(SemaContext *context, Expr *expr, Type *type)
{
	Module *module = global_context_find_module(kw_std__core__types);
	Decl *type_kind = module ? module_find_symbol(module, kw_typekind) : NULL;
	Type *type_for_kind = type_kind ? type_kind->type : type_char;
	unsigned val = type_get_introspection_kind(type->type_kind);
	assert(type_for_kind->type_kind == TYPE_ENUM);
	expr_rewrite_to_int_const(expr, type_flatten(type_for_kind), val, false);
	return cast(expr, type_for_kind);
}

static inline bool sema_create_const_len(SemaContext *context, Expr *expr, Type *type)
{
	assert(type == type_flatten_distinct(type) && "Should be flattened already.");
	size_t len;
	switch (type->type_kind)
	{
		case TYPE_ARRAY:
		case TYPE_VECTOR:
			len = type->array.len;
			break;
		case TYPE_ENUM:
		case TYPE_FAULTTYPE:
			len = vec_size(type->decl->enums.values);
			break;
		case TYPE_INFERRED_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_SUBARRAY:
		default:
			return false;
	}
	expr_rewrite_to_int_const(expr, type_usize, len, true);
	return true;
}

static inline bool sema_create_const_inner(SemaContext *context, Expr *expr, Type *type)
{
	Type *inner = NULL;
	switch (type->type_kind)
	{
		case TYPE_POINTER:
			inner = type->pointer;
			break;
		case TYPE_FAILABLE:
			inner = type->failable;
			break;
		case TYPE_DISTINCT:
			inner = type->decl->distinct_decl.base_type->canonical;
			break;
		case TYPE_ENUM:
			inner = type->decl->enums.type_info->type->canonical;
			break;
		case TYPE_ARRAY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_SUBARRAY:
		case TYPE_INFERRED_ARRAY:
		case TYPE_VECTOR:
			inner = type->array.base;
			break;
		default:
			return false;
	}
	expr->expr_kind = EXPR_CONST;
	expr->const_expr.const_kind = CONST_TYPEID;
	expr->const_expr.typeid = inner->canonical;
	return true;
}

static inline bool sema_create_const_min(SemaContext *context, Expr *expr, Type *type, Type *flat)
{
	if (type_is_float(flat))
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.const_kind = CONST_FLOAT;
		expr->type = type;
		expr->resolve_status = RESOLVE_DONE;
		expr->const_expr.fxx.type = flat->type_kind;
		switch (flat->type_kind)
		{
			case TYPE_F32:
				expr->const_expr.fxx.f = FLT_MIN;
				break;
			case TYPE_F64:
				expr->const_expr.fxx.f = DBL_MIN;
				break;
			case TYPE_F128:
				REMINDER("Float 128 not complete");
				expr->const_expr.fxx.f = DBL_MIN;
				break;
			default:
				UNREACHABLE;
		}
		return true;
	}
	else if (type_is_integer(type))
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.const_kind = CONST_INTEGER;
		expr->type = type;
		expr->resolve_status = RESOLVE_DONE;
		expr->const_expr.ixx.type = flat->type_kind;
		switch (flat->type_kind)
		{
			case TYPE_I8:
				expr->const_expr.ixx.i = (Int128){ 0, 0xFF };
				break;
			case TYPE_I16:
				expr->const_expr.ixx.i = (Int128){ 0, 0xFFFF };
				break;
			case TYPE_I32:
				expr->const_expr.ixx.i = (Int128){ 0, 0xFFFFFFFFLL };
				break;
			case TYPE_I64:
				expr->const_expr.ixx.i = (Int128){ 0, ~((uint64_t)0) };
				break;
			case TYPE_I128:
				expr->const_expr.ixx.i = (Int128){ ~((uint64_t)0), ~((uint64_t)0) };
				break;
			default:
				expr->const_expr.ixx.i = (Int128){ 0, 0 };
				break;
		}
		return true;
	}
	return false;
}

static inline bool sema_create_const_max(SemaContext *context, Expr *expr, Type *type, Type *flat)
{
	if (type_is_integer(flat))
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.const_kind = CONST_INTEGER;
		expr->type = type;
		expr->resolve_status = RESOLVE_DONE;
		expr->const_expr.ixx.type = flat->type_kind;
		switch (flat->type_kind)
		{
			case TYPE_I8:
				expr->const_expr.ixx.i = (Int128){ 0, 0x7F };
				break;
			case TYPE_I16:
				expr->const_expr.ixx.i = (Int128){ 0, 0x7FFF };
				break;
			case TYPE_I32:
				expr->const_expr.ixx.i = (Int128){ 0, 0x7FFFFFFFLL };
				break;
			case TYPE_I64:
				expr->const_expr.ixx.i = (Int128){ 0, 0x7FFFFFFFFFFFFFFFLL };
				break;
			case TYPE_I128:
				expr->const_expr.ixx.i = (Int128){ 0x7FFFFFFFFFFFFFFFLL, 0xFFFFFFFFFFFFFFFFLL };
				break;
			case TYPE_U8:
				expr->const_expr.ixx.i = (Int128){ 0, 0xFF };
				break;
			case TYPE_U16:
				expr->const_expr.ixx.i = (Int128){ 0, 0xFFFF };
				break;
			case TYPE_U32:
				expr->const_expr.ixx.i = (Int128){ 0, 0xFFFFFFFFLL };
				break;
			case TYPE_U64:
				expr->const_expr.ixx.i = (Int128){ 0, 0xFFFFFFFFFFFFFFFFLL };
				break;
			case TYPE_U128:
				expr->const_expr.ixx.i = (Int128){ 0xFFFFFFFFFFFFFFFFLL, 0xFFFFFFFFFFFFFFFFLL };
				break;
			default:
				UNREACHABLE
		}
		return true;
	}
	else if (type_is_float(flat))
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.const_kind = CONST_FLOAT;
		expr->type = type;
		expr->resolve_status = RESOLVE_DONE;
		expr->const_expr.fxx.type = flat->type_kind;
		switch (flat->type_kind)
		{
			case TYPE_F32:
				expr->const_expr.fxx.f = FLT_MAX;
				break;
			case TYPE_F64:
				expr->const_expr.fxx.f = DBL_MAX;
				break;
			case TYPE_F128:
				REMINDER("Float 128 not complete");
				expr->const_expr.fxx.f = DBL_MAX;
				break;
			default:
				UNREACHABLE;
		}
		return true;
	}
	return false;
}

static bool sema_expr_rewrite_typeid_call(Expr *expr, Expr *typeid, TypeIdInfoKind kind, Type *result_type)
{
	expr->expr_kind = EXPR_TYPEID_INFO;
	expr->typeid_info_expr.parent = exprid(typeid);
	expr->typeid_info_expr.kind = TYPEID_INFO_INNER;
	expr->type = result_type;
	return true;
}
static bool sema_expr_apply_typeid_property(SemaContext *context, Expr *expr, Expr *typeid, const char *kw)
{
	if (typeid->expr_kind == EXPR_CONST)
	{
		if (kw_min == kw) return false;
		if (kw_min == kw) return false;
		if (kw_inf == kw) return false;
		if (kw_nan == kw) return false;
		return sema_expr_apply_type_property(context, expr, typeid->const_expr.typeid, kw);
	}
	if (kw == kw_kind)
	{
		sema_rewrite_typeid_kind(expr, typeid);
		return true;
	}
	if (kw == kw_inner) return sema_expr_rewrite_typeid_call(expr, typeid, TYPEID_INFO_INNER, type_typeid);
	if (kw == kw_len) return sema_expr_rewrite_typeid_call(expr, typeid, TYPEID_INFO_LEN, type_usize);
	if (kw == kw_sizeof) return sema_expr_rewrite_typeid_call(expr, typeid, TYPEID_INFO_SIZEOF, type_usize);
	return false;
}

static bool sema_expr_apply_type_property(SemaContext *context, Expr *expr, Type *type, const char *kw)
{
	assert(type == type->canonical);
	if (kw == kw_sizeof)
	{
		expr_rewrite_to_int_const(expr, type_usize, type_size(type), true);
		return true;
	}
	if (kw == kw_kind) return sema_create_const_kind(context, expr, type);
	if (kw == kw_inner) return sema_create_const_inner(context, expr, type);

	Type *flat = type_flatten_distinct(type);
	if (kw == kw_len) return sema_create_const_len(context, expr, type);
	if (kw == kw_min) return sema_create_const_min(context, expr, type, flat);
	if (kw == kw_max) return sema_create_const_max(context, expr, type, flat);
	if (kw == kw_nan && type_is_float(flat))
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.const_kind = CONST_FLOAT;
		expr->const_expr.fxx = (Float) { nan(""), flat->type_kind };
		expr->type = type;
		expr->resolve_status = RESOLVE_DONE;
		return true;
	}
	if (kw == kw_inf && type_is_float(flat))
	{
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.const_kind = CONST_FLOAT;
		expr->const_expr.fxx = (Float) { INFINITY, flat->type_kind };
		expr->type = type;
		expr->resolve_status = RESOLVE_DONE;
		return true;
	}
	return false;
}

/**
 * Analyse "x.y"
 */
static inline bool sema_expr_analyse_access(SemaContext *context, Expr *expr)
{
	Expr *parent = expr->access_expr.parent;
	bool was_group = parent->expr_kind == EXPR_GROUP;

	// 1. Resolve the left hand
	if (!sema_analyse_expr_lvalue(context, parent)) return false;

	// 2. The right hand side may be a @ident or ident
	Expr *child = expr->access_expr.child;

	// 3. Handle xxxxxx.typeid
	if (child->expr_kind == EXPR_TYPEINFO)
	{
		if (child->type_expr->resolve_status != RESOLVE_DONE || child->type_expr->type != type_typeid)
		{
			SEMA_ERROR(child, "A type can't appear here.");
			return false;
		}

		if (parent->expr_kind != EXPR_TYPEINFO)
		{
			SEMA_ERROR(expr, "'typeid' can only be used with types, not values");
			return false;
		}

		expr->type = type_typeid;
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.const_kind = CONST_TYPEID;
		expr->const_expr.typeid = parent->type_expr->type->canonical;
		expr->resolve_status = RESOLVE_DONE;
		return true;

	}

	// 3. Find the actual token.
	SourceSpan span;
	Expr *identifier = sema_expr_resolve_access_child(context, child, NULL);
	if (!identifier) return false;

	// 2. If our left-hand side is a type, e.g. MyInt.abc, handle this here.
	if (parent->expr_kind == EXPR_TYPEINFO)
	{
		return sema_expr_analyse_type_access(context, expr, parent->type_expr, was_group, identifier);
	}

	// 6. Copy failability
	bool failable = IS_FAILABLE(parent);

	assert(expr->expr_kind == EXPR_ACCESS);
	assert(parent->resolve_status == RESOLVE_DONE);

	// 7. Is this a pointer? If so we insert a deref.
	bool is_pointer = type_no_fail(parent->type)->canonical->type_kind == TYPE_POINTER;
	if (is_pointer)
	{
		if (!sema_cast_rvalue(context, parent)) return false;
		expr_insert_deref(expr->access_expr.parent);
		parent = expr->access_expr.parent;
	}

	// 8. Depending on parent type, we have some hard coded types
	Expr *current_parent = parent;

	Type *type = type_no_fail(parent->type)->canonical;
	Type *flat_type = type_flatten(type);
	const char *kw = identifier->identifier_expr.ident;

	if (kw_type == kw && flat_type->type_kind == TYPE_ANY)
	{
		expr_rewrite_to_builtin_access(context, expr, parent, ACCESS_TYPEOFANY, type_typeid);
		return true;
	}

CHECK_DEEPER:

	// 9. Fix hard coded function `len` on subarrays and arrays
	if (kw == kw_len)
	{
		if (flat_type->type_kind == TYPE_SUBARRAY)
		{
			expr_rewrite_to_builtin_access(context, expr, current_parent, ACCESS_LEN, type_usize);
			return true;
		}
		if (flat_type->type_kind == TYPE_ARRAY || flat_type->type_kind == TYPE_VECTOR)
		{
			expr_rewrite_to_int_const(expr, type_isize, flat_type->array.len, true);
			return true;
		}
	}
	if (flat_type->type_kind == TYPE_TYPEID)
	{
		if (sema_expr_apply_typeid_property(context, expr, parent, kw)) return true;
		SEMA_ERROR(identifier, "'%s' is not a valid proprty for typeid.", kw);
		return false;
	}

	// Hard coded ptr on subarrays and variant
	if (kw == kw_ptr)
	{
		if (flat_type->type_kind == TYPE_SUBARRAY)
		{
			expr_rewrite_to_builtin_access(context, expr, current_parent, ACCESS_PTR, type_get_ptr(flat_type->array.base));
			return true;
		}
		if (flat_type->type_kind == TYPE_ANY)
		{
			expr_rewrite_to_builtin_access(context, expr, current_parent, ACCESS_PTR, type_voidptr);
			return true;
		}
	}

	if (kw == kw_ordinal)
	{
		if (type->type_kind == TYPE_ENUM)
		{
			if (!cast(current_parent, type->decl->enums.type_info->type)) return false;
			expr_replace(expr, current_parent);
			return true;
		}
	}
	if (kw == kw_nameof)
	{
		if (type->type_kind == TYPE_ENUM)
		{
			if (current_parent->expr_kind == EXPR_CONST)
			{
				expr_rewrite_to_string(expr, current_parent->const_expr.enum_val->name);
				return true;
			}
			else
			{
				expr_rewrite_to_builtin_access(context, expr, current_parent, ACCESS_ENUMNAME, type_get_subarray(type_char));
				return true;
			}
		}
	}

	// 9. At this point we may only have distinct, struct, union, error, enum
	if (!type_may_have_sub_elements(type))
	{
		SEMA_ERROR(expr, "There is no member or method '%s' on '%s'", kw, type_to_error_string(type));
		return false;
	}

	// 10. Dump all members and methods into the scope.
	Decl *decl = type->decl;
	Decl *member = NULL;
	SCOPE_START
		add_members_to_context(context, decl);
		member = sema_resolve_symbol_in_current_dynamic_scope(context, kw);
	SCOPE_END;

	if (member && decl_is_enum_kind(decl) && parent->expr_kind == EXPR_CONST)
	{
		assert(parent->const_expr.const_kind == CONST_ENUM);
		Expr *copy_init = expr_macro_copy(current_parent->const_expr.enum_val->enum_constant.args[member->var.index]);
		expr_replace(expr, copy_init);
		return true;
	}
	Decl *private = NULL;
	if (!member)
	{
		Decl *ambiguous = NULL;
		member = sema_resolve_method(context->unit, decl, kw, &ambiguous, &private);
		if (ambiguous)
		{
			SEMA_ERROR(expr, "'%s' is an ambiguous name and so cannot be resolved, it may refer to method defined in '%s' or one in '%s'",
					   kw, member->module->name->module, ambiguous->module->name->module);
			return false;
		}
	}

	if (member && member->decl_kind == DECL_FUNC)
	{
		unit_register_external_symbol(context->compilation_unit, member);
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
		if (private)
		{
			SEMA_ERROR(expr, "The method '%s' has private visibility.", kw);
			return false;
		}
		SEMA_ERROR(expr, "There is no field or method '%s.%s'.", decl->name, kw);
		return false;
	}

	// Transform bitstruct access to expr_bitaccess.
	if (member->var.kind == VARDECL_BITMEMBER)
	{
		expr->expr_kind = EXPR_BITACCESS;
	}

	// 13. Copy properties.
	expr->access_expr.parent = current_parent;
	expr->type = type_get_opt_fail(member->type, failable);
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
			element->index = (MemberIndex)i;
			return decl;
		}
		if (!decl->name)
		{
			assert(type_is_structlike(decl->type) || decl->decl_kind == DECL_BITSTRUCT);
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
			anon_element->index = (MemberIndex)i;
			elements[old_index] = anon_element;
			// Advance
			(*index)++;
			return found;
		}
	}
	return NULL;
}

static MemberIndex sema_analyse_designator_index(SemaContext *context, Expr *index)
{
	if (!sema_analyse_expr(context, index))
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
	if (!int_fits(index->const_expr.ixx, TYPE_I32))
	{
		SEMA_ERROR(index, "The value of the index does not fit in an int.");
		return -1;
	}
	int64_t index_val = int_to_i64(index->const_expr.ixx);
	if (index_val < 0)
	{
		SEMA_ERROR(index, "Negative index values is not allowed.");
		return -1;
	}
	return (MemberIndex)index_val;
}

static Type *sema_find_type_of_element(SemaContext *context, Type *type, DesignatorElement **elements, unsigned *curr_index, bool *is_constant, bool *did_report_error, MemberIndex *max_index, Decl **member_ptr)
{
	Type *type_flattened = type_flatten(type);
	DesignatorElement *element = elements[*curr_index];
	if (element->kind == DESIGNATOR_ARRAY || element->kind == DESIGNATOR_RANGE)
	{
		*member_ptr = NULL;
		ByteSize len;
		Type *base;
		switch (type_flattened->type_kind)
		{
			case TYPE_INFERRED_ARRAY:
				len = MAX_ARRAYINDEX;
				base = type_flattened->array.base;
				break;
			case TYPE_ARRAY:
			case TYPE_VECTOR:
				len = type_flattened->array.len;
				base = type_flattened->array.base;
				break;
			default:
				return NULL;
		}
		MemberIndex index = sema_analyse_designator_index(context, element->index_expr);
		if (index < 0)
		{
			*did_report_error = true;
			return NULL;
		}
		if (index >= (MemberIndex)len)
		{
			SEMA_ERROR(element->index_expr, "The index may must be less than the array length (which was %llu).", (unsigned long long)len);
			*did_report_error = true;
			return NULL;
		}

		element->index = index;
		if (max_index && *max_index < index) *max_index = index;
		if (element->kind == DESIGNATOR_RANGE)
		{
			MemberIndex end_index = sema_analyse_designator_index(context, element->index_end_expr);
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
			if (end_index > (MemberIndex)len)
			{
				*did_report_error = true;
				SEMA_ERROR(element->index_expr, "The index may must be less than the array length (which was %llu).", (unsigned long long)len);
				return NULL;
			}
			element->index_end = end_index;
			if (max_index && *max_index < end_index) *max_index = end_index;
		}
		return base;
	}
	assert(element->kind == DESIGNATOR_FIELD);
	if (!type_is_structlike(type_flattened) && type_flattened->type_kind != TYPE_BITSTRUCT)
	{
		return NULL;
	}
	Decl *member = sema_resolve_element_for_name(type_flattened->decl->strukt.members, elements, curr_index);
	*member_ptr = member;
	if (!member) return NULL;
	return member->type;
}

static Type *sema_expr_analyse_designator(SemaContext *context, Type *current, Expr *expr, MemberIndex *max_index, Decl **member_ptr)
{
	DesignatorElement **path = expr->designator_expr.path;

	// Walk down into this path
	bool is_constant = true;
	bool did_report_error = false;
	*member_ptr = NULL;
	for (unsigned i = 0; i < vec_size(path); i++)
	{
		Decl *member_found;
		Type *new_current = sema_find_type_of_element(context, current, path, &i, &is_constant, &did_report_error, i == 0 ? max_index : NULL, &member_found);
		if (!new_current)
		{
			if (!did_report_error) SEMA_ERROR(expr, "This is not a valid member of '%s'.", type_to_error_string(current));
			return NULL;
		}
		current = new_current;
		*member_ptr = member_found;
	}
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
	if (value->expr_kind == EXPR_CONST && value->const_expr.const_kind == CONST_LIST)
	{
		*const_init = *value->const_expr.list;
		value->const_expr.list = const_init;
		return;
	}
	const_init->init_value = value;
	const_init->type = type_flatten(value->type);
	const_init->kind = CONST_INIT_VALUE;
}

static bool is_empty_initializer_list(Expr *value)
{
	return value->expr_kind == EXPR_CONST && value->const_expr.const_kind == CONST_LIST
			&& value->const_expr.list->kind == CONST_INIT_ZERO;
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
			ConstInitializer *element_init = MALLOCS(ConstInitializer);
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
	MemberIndex low_index = element->index;
	MemberIndex high_index = element->kind == DESIGNATOR_RANGE ? element->index_end : element->index;
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

	MemberIndex insert_index = 0;

	for (MemberIndex index = low_index; index <= high_index; index++)
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
			initializer = MALLOCS(ConstInitializer);
			initializer->type = element_type;
			initializer->kind = CONST_INIT_ARRAY_VALUE;
			initializer->init_array_value.index = index;
			inner_value = MALLOCS(ConstInitializer);
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
				initializer = MALLOCS(ConstInitializer);
				initializer->type = element_type;
				initializer->kind = CONST_INIT_ARRAY_VALUE;
				initializer->init_array_value.index = index;
				inner_value = MALLOCS(ConstInitializer);
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
		case TYPE_BITSTRUCT:
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
	Expr **init_expressions = initializer->initializer_list;
	VECEACH(init_expressions, i)
	{
		Expr *expr = init_expressions[i];
		DesignatorElement **path = expr->designator_expr.path;
		Expr *value = expr->designator_expr.value;
		sema_update_const_initializer_with_designator(const_init, path, path + vec_size(path), value);
	}
}

static bool sema_expr_analyse_designated_initializer(SemaContext *context, Type *assigned, Expr *initializer)
{
	Expr **init_expressions = initializer->designated_init_list;
	Type *original = assigned->canonical;
	bool is_bitstruct = original->type_kind == TYPE_BITSTRUCT;
	bool is_structlike = type_is_structlike(original) || is_bitstruct;
	MemberIndex max_index = -1;
	bool failable = false;
	VECEACH(init_expressions, i)
	{
		Expr *expr = init_expressions[i];
		Decl *member;
		Type *result = sema_expr_analyse_designator(context, original, expr, &max_index, &member);
		if (!result) return false;
		Expr *value = expr->designator_expr.value;
		if (!sema_analyse_expr_rhs(context, result, value, true)) return false;
		if (member && member->decl_kind == DECL_VAR && member->var.kind == VARDECL_BITMEMBER)
		{
			if (!sema_bit_assignment_check(value, member)) return false;
		}
		failable = failable || IS_FAILABLE(value);
		expr->resolve_status = RESOLVE_DONE;
	}

	if (!is_structlike && initializer->type->type_kind == TYPE_INFERRED_ARRAY)
	{
		initializer->type = sema_type_lower_by_size(initializer->type, (ArraySize)(max_index + 1));
	}

	if (expr_is_constant_eval(initializer, CONSTANT_EVAL_ANY))
	{
		ConstInitializer *const_init = MALLOCS(ConstInitializer);
		sema_create_const_initializer(const_init, initializer);
		expr_set_as_const_list(initializer, const_init);
		return true;
	}
	return true;
}

static int decl_count_elements(Decl *structlike)
{
	int elements = 0;
	Decl **members = structlike->strukt.members;
	unsigned member_size = vec_size(members);
	if (member_size == 0) return 0;
	if (structlike->decl_kind == DECL_UNION) member_size = 1;
	for (unsigned i = 0; i < member_size; i++)
	{
		Decl *member = members[i];
		if (member->decl_kind != DECL_VAR && !member->name)
		{
			elements += decl_count_elements(member);
			continue;
		}
		elements++;
	}
	return elements;
}

static inline void not_enough_elements(Expr *initializer, int element)
{
	if (element == 0)
	{
		SEMA_ERROR(initializer, "The initializer is missing elements.");
		return;
	}
	SEMA_ERROR(initializer->initializer_list[element - 1], "Too few elements in initializer, there should be elements after this one.");
}

static inline bool sema_is_anon_member(Decl *decl)
{
	return decl->decl_kind != DECL_VAR && !decl->name;
}

/**
 * Perform analysis for a plain initializer, that is one initializing all fields.
 * @return true if analysis succeeds.
 */
static inline bool sema_expr_analyse_struct_plain_initializer(SemaContext *context, Decl *assigned, Expr *initializer)
{
	Expr **elements = initializer->initializer_list;
	Decl **members = assigned->strukt.members;
	MemberIndex size = (MemberIndex)vec_size(elements);
	unsigned elements_needed = decl_count_elements(assigned);

	// 1. For struct number of members must be the same as the size of the struct.
	//    Since we already handled the case with an empty initializer before going here
	//    zero entries must be an error.
	assert(size > 0 && "We should already have handled the size == 0 case.");

	// 2. We don't support this actually, but we used to. Maybe we will in the future.
	if (elements_needed == 0)
	{
		// Generate a nice error message for zero.
		SEMA_ERROR(elements[0], "Too many elements in initializer, it must be empty.");
		return false;
	}

	bool failable = false;

	bool is_bitstruct = assigned->decl_kind == DECL_BITSTRUCT;
	if (is_bitstruct && assigned->bitstruct.overlap)
	{
		if (vec_size(assigned->strukt.members) > 1 && vec_size(elements) > 1)
		{
			SEMA_ERROR(elements[0], "Bitstructs with @overlap must use designated initialization.");
			return false;
		}
	}

	// 3. Loop through all elements.
	MemberIndex max_loop = size > elements_needed ? size : elements_needed;
	for (MemberIndex i = 0; i < max_loop; i++)
	{
		// 4. Check if we exceeded the list of elements in the struct/union.
		//    This way we can check the other elements which might help the
		//    user pinpoint where they put the double elements.
		if (i >= elements_needed)
		{
			assert(i < size);
			SEMA_ERROR(elements[i], "Too many elements in initializer, expected only %d.", elements_needed);
			return false;
		}
		// 5. We might have anonymous members
		Decl *member = members[i];
		if (member->decl_kind != DECL_VAR && !member->name)
		{
			int sub_element_count = decl_count_elements(member);
			if (!sub_element_count)
			{
				vec_add(initializer->initializer_list, NULL);
				for (int j = (int)(size - 1); j > i; j--)
				{
					initializer->initializer_list[j] = initializer->initializer_list[j - 1];
				}
				Expr *new_initializer = expr_new(EXPR_INITIALIZER_LIST, initializer->span);
				ConstInitializer *empty = CALLOCS(ConstInitializer);
				empty->kind = CONST_INIT_ZERO;
				empty->type = member->type;
				expr_set_as_const_list(new_initializer, empty);
				initializer->initializer_list[i] = new_initializer;
				size += 1;
				continue;
			}
			if (i >= size)
			{
				not_enough_elements(initializer, (int)i);
				return false;
			}
			Expr *new_initializer = expr_new(EXPR_INITIALIZER_LIST, elements[i]->span);
			int max_index_to_copy = i + sub_element_count < size ? i + sub_element_count : size;
			for (int j = i; j < max_index_to_copy; j++)
			{
				vec_add(new_initializer->initializer_list, elements[j]);
			}
			int reduce_by = max_index_to_copy - i - 1;
			size -= reduce_by;
			elements_needed -= reduce_by;
			max_loop = MAX(size, elements_needed);
			assert(size <= vec_size(initializer->initializer_list));
			vec_resize(initializer->initializer_list, (unsigned)size);
			elements = initializer->initializer_list;
			elements[i] = new_initializer;
		}
		if (i >= size)
		{
			not_enough_elements(initializer, i);
			return false;
		}
		Expr *element = elements[i];
		// 6. We know the required type, so resolve the expression.
		if (!sema_analyse_expr_rhs(context, members[i]->type, element, true)) return false;
		if (member->decl_kind == DECL_VAR && member->var.kind == VARDECL_BITMEMBER)
		{
			if (!sema_bit_assignment_check(element, members[i])) return false;
		}
		failable = failable || IS_FAILABLE(element);
	}
	assert(initializer->type);
	if (failable) initializer->type = type_get_failable(initializer->type);

	// 6. There's the case of too few values as well. Mark the last field as wrong.
	assert(elements_needed <= size);

	if (expr_is_constant_eval(initializer, CONSTANT_EVAL_ANY))
	{
		bool is_union = type_flatten_distinct(initializer->type)->type_kind == TYPE_UNION;
		assert(!is_union || vec_size(elements) == 1);
		ConstInitializer *const_init = CALLOCS(ConstInitializer);
		const_init->kind = is_union ? CONST_INIT_UNION : CONST_INIT_STRUCT;
		const_init->type = type_flatten(initializer->type);
		if (is_union)
		{
			Expr *expr = elements[0];
			const_init->init_union.index = 0;
			if (expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_LIST)
			{
				const_init->init_union.element = expr->const_expr.list;
			}
			else
			{
				ConstInitializer *element_init = MALLOCS(ConstInitializer);
				sema_create_const_initializer_value(element_init, expr);
				const_init->init_union.element = element_init;
			}
			expr_set_as_const_list(initializer, const_init);
			return true;
		}
		ConstInitializer **inits = MALLOC(sizeof(ConstInitializer *) * vec_size(elements));
		VECEACH(elements, i)
		{
			Expr *expr = elements[i];
			if (expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_LIST)
			{
				inits[i] = expr->const_expr.list;
				continue;
			}
			ConstInitializer *element_init = MALLOCS(ConstInitializer);
			sema_create_const_initializer_value(element_init, expr);
			inits[i] = element_init;
		}
		const_init->init_struct = inits;
		expr_set_as_const_list(initializer, const_init);
	}

	// 7. Done!
	return true;


}


/**
 * Perform analysis for a plain initializer, that is one initializing all fields.
 * @return true if analysis succeeds.
 */
static inline bool sema_expr_analyse_array_plain_initializer(SemaContext *context, Type *assigned, Expr *initializer)
{
	Expr **elements = initializer->initializer_list;

	Type *inner_type = type_get_indexed_type(assigned);
	assert(inner_type);

	unsigned size = vec_size(elements);
	unsigned expected_members = assigned->array.len;
	if (assigned->type_kind != TYPE_ARRAY && assigned->type_kind != TYPE_VECTOR) expected_members = size;

	assert(size > 0 && "We should already have handled the size == 0 case.");
	if (expected_members == 0)
	{
		// Generate a nice error message for zero.
		SEMA_ERROR(elements[0], "Too many elements in initializer, it must be empty.");
		return false;
	}

	bool failable = false;
	VECEACH(elements, i)
	{
		Expr *element = elements[i];
		if (i >= expected_members)
		{
			SEMA_ERROR(element, "Too many elements in initializer, expected only %d.", expected_members);
			return false;
		}

		if (!sema_analyse_expr_rhs(context, inner_type, element, true)) return false;
		failable = failable || IS_FAILABLE(element);
	}
	assert(initializer->type);
	if (failable) initializer->type = type_get_failable(initializer->type);

	if (expected_members > size)
	{
		SEMA_ERROR(elements[size - 1], "Too few elements in initializer, %d elements are needed.", expected_members);
		return false;
	}

	if (expr_is_constant_eval(initializer, CONSTANT_EVAL_ANY))
	{
		ConstInitializer *const_init = CALLOCS(ConstInitializer);
		const_init->kind = CONST_INIT_ARRAY_FULL;
		const_init->type = type_flatten(initializer->type);
		ConstInitializer **inits = VECNEW(ConstInitializer*, vec_size(elements));
		VECEACH(elements, i)
		{
			Expr *expr = elements[i];
			if (expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_LIST)
			{
				vec_add(inits, expr->const_expr.list);
				continue;
			}
			ConstInitializer *element_init = MALLOCS(ConstInitializer);
			sema_create_const_initializer_value(element_init, expr);
			vec_add(inits, element_init);
		}
		const_init->init_array_full = inits;
		expr_set_as_const_list(initializer, const_init);
	}

	// 7. Done!
	return true;
}

static inline bool sema_expr_analyse_untyped_initializer(SemaContext *context, Expr *initializer)
{
	Expr **elements = initializer->initializer_list;

	Type *element_type = NULL;
	bool no_common_elements = false;
	unsigned element_count = vec_size(elements);
	bool is_const = true;
	Expr *failable_expr = NULL;
	for (unsigned i = 0; i < element_count; i++)
	{
		Expr *element = elements[i];
		if (!sema_analyse_expr(context, element)) return false;
		if (is_const && element->expr_kind != EXPR_CONST) is_const = false;
		if (!failable_expr && IS_FAILABLE(element)) failable_expr = element;
		if (no_common_elements) continue;
		Type *current_element_type = type_no_fail(element->type);
		if (element_type == NULL)
		{
			element_type = element->type;
			continue;
		}
		element_type = type_find_max_type(element->type, current_element_type);
		if (!element_type) no_common_elements = true;
	}
	if (no_common_elements && failable_expr)
	{
		SEMA_ERROR(failable_expr, "An untyped initializer can't have failable values.");
		return false;
	}
	if (no_common_elements || is_const)
	{
		initializer->type = type_complist;
		return true;
	}
	initializer->type = type_get_opt_fail(type_get_array(element_type, element_count), failable_expr != NULL);
	return true;
}

static inline bool sema_expr_analyse_initializer(SemaContext *context, Type *external_type, Type *assigned, Expr *expr)
{
	// Note at this point this we either have
	// EXPR_DESIGNATED_INITIALIZER_LIST
	// or EXPR_INITIALIZER_LIST

	// 1. Designated initializer is separately evaluated.
	if (expr->expr_kind == EXPR_DESIGNATED_INITIALIZER_LIST)
	{
		expr->type = external_type;
		return sema_expr_analyse_designated_initializer(context, assigned, expr);
	}

	assert(expr->expr_kind == EXPR_INITIALIZER_LIST);

	// 2. Grab the expressions inside.
	Expr **init_expressions = expr->initializer_list;
	unsigned init_expression_count = vec_size(init_expressions);

	// 3. Zero size init will initialize to empty.
	if (init_expression_count == 0)
	{
		if (external_type->type_kind == TYPE_INFERRED_ARRAY)
		{
			SEMA_ERROR(expr, "Zero length arrays are not permitted.");
			return false;
		}
		external_type = sema_type_lower_by_size(external_type, 0);
		expr->type = external_type;
		ConstInitializer *initializer = CALLOCS(ConstInitializer);
		initializer->kind = CONST_INIT_ZERO;
		initializer->type = type_flatten(expr->type);
		expr_set_as_const_list(expr, initializer);
		return true;
	}

	// 4. If we have an inferred array, we need to set the size.
	external_type = sema_type_lower_by_size(external_type, init_expression_count);
	assigned = sema_type_lower_by_size(assigned, init_expression_count);

	// 5. Set the type.
	expr->type = external_type;

	// 6. We might have a complist, because were analyzing $foo = { ... } or similar.
	if (external_type == type_complist)
	{
		return sema_expr_analyse_untyped_initializer(context, expr);
	}
	// 7. If not, then we see if we have an array.
	if (assigned->type_kind == TYPE_UNTYPED_LIST ||
		assigned->type_kind == TYPE_ARRAY ||
		assigned->type_kind == TYPE_INFERRED_ARRAY ||
		assigned->type_kind == TYPE_SUBARRAY ||
		assigned->type_kind == TYPE_VECTOR)
	{
		return sema_expr_analyse_array_plain_initializer(context, assigned, expr);
	}

	return sema_expr_analyse_struct_plain_initializer(context, assigned->decl, expr);
}

static inline bool sema_expr_analyse_initializer_list(SemaContext *context, Type *to, Expr *expr)
{
	if (!to) to = type_complist;
	assert(to);
	Type *assigned = type_flatten(to);
	switch (assigned->type_kind)
	{
		case TYPE_UNTYPED_LIST:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ARRAY:
		case TYPE_BITSTRUCT:
		case TYPE_INFERRED_ARRAY:
		case TYPE_VECTOR:
			return sema_expr_analyse_initializer(context, to, assigned, expr);
		case TYPE_SUBARRAY:
		{
			if (expr->expr_kind == EXPR_INITIALIZER_LIST && !vec_size(expr->initializer_list))
			{
				expr->expr_kind = EXPR_CONST;
				expr->const_expr.const_kind = CONST_POINTER;
				expr->type = assigned;
				return true;
			}
			// Resolve this as an inferred array.
			Type *type = type_get_inferred_array(assigned->array.base);
			if (!sema_expr_analyse_initializer(context, type, type, expr)) return false;
			expr->resolve_status = RESOLVE_DONE;
			expr_insert_addr(expr);
			if (!sema_analyse_expr(context, expr)) return false;
			return cast(expr, to);
		}
		case TYPE_POINTER:
			SEMA_ERROR(expr, "Pointers cannot be initialized using an initializer list, instead you need to take the address of an array.");
			return false;
		default:
			break;
	}
	// Fix error on compound literals
	SEMA_ERROR(expr, "'%s' cannot use compound literal initialization, did you intend to use a cast?", type_to_error_string(to));
	return false;
}



static inline bool sema_expr_analyse_expr_list(SemaContext *context, Expr *expr)
{
	bool success = true;
	ByteSize last = vec_size(expr->expression_list) - 1;
	VECEACH(expr->expression_list, i)
	{
		Expr *checked_expr = expr->expression_list[i];
		success &= sema_analyse_expr(context, checked_expr);
	}
	expr->type = expr->expression_list[last]->type;
	return success;
}

static inline bool sema_expr_analyse_cast(SemaContext *context, Expr *expr)
{
	Expr *inner = exprptr(expr->cast_expr.expr);
	TypeInfo *type_info = type_infoptr(expr->cast_expr.type_info);
	bool success = sema_resolve_type_info(context, type_info);
	if (!sema_analyse_expr(context, inner) || !success) return false;

	Type *target_type = type_info->type;
	if (type_is_failable(target_type))
	{
		SEMA_ERROR(type_info, "Casting to a failable type is not allowed.");
		return false;
	}
	if (inner->type == type_complist)
	{
		// We don't support: (Foo)(x > 0 ? { 1, 2 } : { 3, 4 })
		// just write this: x > 0 ? Foo { 1, 2 } : Foo { 3, 4 }
		SEMA_ERROR(inner, "Casting from an untyped list to a concrete type is not possible.");
		return false;
	}
	if (!cast_may_explicit(inner->type, target_type, true, inner->expr_kind == EXPR_CONST))
	{
		return sema_failed_cast(expr, type_no_fail(inner->type), target_type);
	}
	if (!cast(inner, target_type))
	{
		return expr_poison(expr);
	}
	expr_replace(expr, inner);
	return true;
}

static inline bool sema_expr_analyse_slice_assign(SemaContext *context, Expr *expr, Type *left_type, Expr *right, bool is_unwrapped)
{
	// 1. Evaluate right side to required type.
	if (!sema_analyse_expr_rhs(context, left_type->array.base, right, false)) return false;

	Expr *left = exprptr(expr->binary_expr.left);
	expr->type = right->type;
	expr->expr_kind = EXPR_SLICE_ASSIGN;
	expr->slice_assign_expr.left = exprid(left);
	expr->slice_assign_expr.right = exprid(right);

	return true;
}

bool sema_expr_analyse_assign_right_side(SemaContext *context, Expr *expr, Type *left_type, Expr *right, bool is_unwrapped)
{
	if (expr && exprptr(expr->binary_expr.left)->expr_kind == EXPR_SLICE)
	{
		return sema_expr_analyse_slice_assign(context, expr, left_type, right, is_unwrapped);
	}

	// 1. Evaluate right side to required type.
	if (!sema_analyse_expr_rhs(context, left_type, right, true)) return false;
	if (IS_FAILABLE(right) && !type_is_failable(left_type))
	{
		if (is_unwrapped)
		{
			SEMA_ERROR(exprptr(expr->binary_expr.left), "The variable is unwrapped in this context, if you don't want to unwrap it, use () around the variable to suppress unwrapping, like 'catch err = (x)' and 'try (x)'.");
			return false;
		}
		if (!left_type) left_type = type_no_fail(right->type);
		return sema_failed_cast(right, right->type, left_type);
	}

	// 3. Set the result to the type on the right side.
	if (expr) expr->type = right->type;

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


static inline bool sema_expr_analyse_ct_identifier_lvalue(SemaContext *context, Expr *expr)
{
	if (!sema_expr_begin_analyse(expr)) return expr_ok(expr);

	Decl *ambiguous_decl = NULL;
	Decl *private_symbol = NULL;
	assert(expr && expr->ct_ident_expr.identifier);
	DEBUG_LOG("Now resolving %s", expr->ct_ident_expr.identifier);
	Decl *decl = sema_find_symbol(context, expr->ct_ident_expr.identifier);

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


static bool sema_expr_analyse_ct_identifier_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_expr_analyse_ct_identifier_lvalue(context, left)) return false;

	// 3. Evaluate right side to required type.
	if (!sema_expr_analyse_assign_right_side(context, expr, left->type, right, false)) return false;

	left->ct_ident_expr.decl->var.init_expr = right;
	expr_replace(expr, right);
	return true;
}

static bool sema_expr_analyse_ct_type_identifier_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	TypeInfo *info = left->type_expr;

	if (info->kind != TYPE_INFO_CT_IDENTIFIER)
	{
		SEMA_ERROR(left, "A type cannot be assigned to.");
		return false;
	}

	if (!sema_analyse_expr_lvalue(context, right)) return false;

	if (right->expr_kind != EXPR_TYPEINFO)
	{
		SEMA_ERROR(right, "Expected a type here.");
		return false;
	}

	Decl *decl = sema_find_symbol(context, info->unresolved.name);

	if (!decl)
	{
		SEMA_ERROR(info, "'%s' is not defined in this scope yet.", info->unresolved.name);
		return false;
	}
	decl->var.init_expr = right;
	expr->expr_kind = EXPR_NOP;
	expr->type = type_void;

	return true;
}

/**
 * Analyse a = b
 * @return true if analysis works
 */
static bool sema_expr_analyse_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Evaluate left side
	if (left->expr_kind == EXPR_CT_IDENT)
	{
		return sema_expr_analyse_ct_identifier_assign(context, expr, left, right);
	}

	if (left->expr_kind == EXPR_TYPEINFO)
	{
		return sema_expr_analyse_ct_type_identifier_assign(context, expr, left, right);
	}

	if (!sema_analyse_expr_lvalue(context, left)) return false;


	// 2. Check assignability
	if (!sema_expr_check_assign(context, left)) return false;

	bool is_unwrapped_var = expr_is_unwrapped_ident(left);

	// 3. Evaluate right side to required type.
	if (!sema_expr_analyse_assign_right_side(context, expr, left->type, right, is_unwrapped_var)) return false;

	if (is_unwrapped_var && IS_FAILABLE(right))
	{
		sema_rewrap_var(context, left->identifier_expr.decl);
		return true;
	}
	if (left->expr_kind == EXPR_BITACCESS)
	{
		if (!sema_bit_assignment_check(right, left->access_expr.ref)) return false;
		expr->expr_kind = EXPR_BITASSIGN;
	}
	return true;
}


/**
 * Analyse define $foo = ...
 *
 * @return true if analysis worked.
 */
static bool sema_expr_analyse_ct_common_assign(SemaContext *context, Expr *expr, Expr *left)
{

	// 1. Analyse left side.
	if (!sema_expr_analyse_ct_identifier_lvalue(context, left)) return false;

	Decl *left_var = left->ct_ident_expr.decl;

	Expr *left_value = left_var->var.init_expr;
	assert(left_value);
	assert(!IS_FAILABLE(left_value));

	expr->binary_expr.left = exprid(left_value);

	expr->binary_expr.operator = binaryop_assign_base_op(expr->binary_expr.operator);

	if (!sema_expr_analyse_binary(context, expr)) return false;

	if (expr->expr_kind != EXPR_CONST)
	{
		SEMA_ERROR(exprptr(expr->binary_expr.right), "Expected a constant expression.");
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
static bool sema_expr_analyse_common_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right, bool int_only)
{
	if (left->expr_kind == EXPR_CT_IDENT)
	{
		return sema_expr_analyse_ct_common_assign(context, expr, left);
	}

	// 1. Analyse left side.
	if (!sema_analyse_expr_lvalue(context, left)) return false;

	// 2. Verify that the left side is assignable.
	if (!sema_expr_check_assign(context, left)) return false;

	Type *no_fail = type_no_fail(left->type);

	// 3. If this is only defined for ints (*%, ^= |= &= %=) verify that this is an int.
	if (int_only && !type_is_integer(no_fail))
	{
		SEMA_ERROR(left, "Expected an integer here.");
		return false;
	}

	// 4. In any case, these ops are only defined on numbers.
	if (!type_underlying_is_numeric(no_fail))
	{
		SEMA_ERROR(left, "Expected a numeric type here.");
		return false;
	}

	// 5. Cast the right hand side to the one on the left
	if (!sema_analyse_expr(context, right)) return false;
	if (!cast_implicit(right, no_fail)) return false;
	if (IS_FAILABLE(right) && !IS_FAILABLE(left))
	{
		SEMA_ERROR(right, "This expression cannot be failable, since the assigned variable isn't.");
		return false;
	}
	// 6. Check for zero in case of div or mod.
	if (right->expr_kind == EXPR_CONST)
	{
		if (expr->binary_expr.operator == BINARYOP_DIV_ASSIGN)
		{
			switch (right->const_expr.const_kind)
			{
				case CONST_INTEGER:
					if (int_is_zero(right->const_expr.ixx))
					{
						SEMA_ERROR(right, "Division by zero not allowed.");
						return false;
					}
					break;
				case CONST_FLOAT:
					if (right->const_expr.fxx.f == 0)
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
			switch (right->const_expr.const_kind)
			{
				case CONST_INTEGER:
					if (int_is_zero(right->const_expr.ixx))
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

	if (left->expr_kind == EXPR_BITACCESS)
	{
		expr->expr_kind = EXPR_BITASSIGN;
	}
	// 7. Assign type
	expr->type = left->type;
	return true;
}


/**
 * Handle a += b, a -= b
 * @return true if analysis succeeded.
 */
static bool sema_expr_analyse_add_sub_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	if (left->expr_kind == EXPR_CT_IDENT)
	{
		return sema_expr_analyse_ct_common_assign(context, expr, left);
	}

	// 1. Analyse the left hand side
	if (!sema_analyse_expr(context, left)) return false;

	// 2. Ensure the left hand side is assignable
	if (!sema_expr_check_assign(context, left)) return false;

	Type *left_type_canonical = left->type->canonical;

	// 4. Analyse right hand side
	REMINDER("Possible deep cast here.");
	if (!sema_analyse_expr(context, right)) return false;

	// 3. Copy type & set properties.
	if (IS_FAILABLE(right) && !IS_FAILABLE(left))
	{
		SEMA_ERROR(right, "Cannot assign a failable value to a non-failable.");
		return false;
	}
	expr->type = left->type;
	bool failable = IS_FAILABLE(left) || IS_FAILABLE(right);


	// 5. In the pointer case we have to treat this differently.
	if (left_type_canonical->type_kind == TYPE_POINTER)
	{

		if (!sema_decay_array_pointers(left)) return false;
		expr->type = left->type;

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
	if (!type_underlying_is_numeric(left->type))
	{
		SEMA_ERROR(left, "Expected a numeric type here.");
		return false;
	}
	REMINDER("Check if can remove");
	if (left->expr_kind == EXPR_BITACCESS)
	{
		expr->expr_kind = EXPR_BITASSIGN;
	}
	expr->type = type_get_opt_fail(expr->type, failable);
	return true;
}

static Type *numeric_arithmetic_promotion(Type *type)
{
	if (!type) return NULL;
	switch (type->type_kind)
	{
		case ALL_SIGNED_INTS:
			if (type->builtin.bitsize < platform_target.width_c_int) return type_cint;
			return type;
		case ALL_UNSIGNED_INTS:
			if (type->builtin.bitsize < platform_target.width_c_int) return type_cuint;
			return type;
		case TYPE_F16:
			// Promote F16 to a real type.
			return type_float;
		case TYPE_FAILABLE:
			UNREACHABLE
		default:
			return type;
	}
}

static bool binary_arithmetic_promotion(SemaContext *context, Expr *left, Expr *right, Type *left_type, Type *right_type, Expr *parent, const char *error_message)
{
	Type *max = numeric_arithmetic_promotion(type_find_max_type(left_type, right_type));
	if (!max || !type_underlying_is_numeric(max))
	{
		if (!error_message)
		{
			return sema_type_error_on_binop(parent);
		}
		SEMA_ERROR(parent, error_message, type_quoted_error_string(left->type), type_quoted_error_string(right->type));
		return false;
	}
	return cast_implicit(left, max) && cast_implicit(right, max);
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

static Type *defer_iptr_cast(Expr *maybe_pointer, Expr *maybe_diff)
{
	// Do we have (iptr)(ptr) +- rhs? If so we change it to
	// (iptr)((char*)(ptr) +- 1)
	if (maybe_pointer->expr_kind == EXPR_CAST
		&& maybe_pointer->cast_expr.kind == CAST_PTRXI
		&& type_flatten(maybe_pointer->type) == type_flatten(type_iptr)
		&& cast_may_implicit(maybe_diff->type, maybe_diff->type, true, true))
	{
		Type *cast_to_iptr = maybe_pointer->type;
		maybe_pointer->cast_expr.kind = CAST_PTRPTR;
		maybe_pointer->type = type_get_ptr(type_char);
		return cast_to_iptr;
	}
	return NULL;
}

/**
 * Analyse a - b
 * @return true if analysis succeeded
 */
static bool sema_expr_analyse_sub(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyse a and b.
	if (!sema_expr_analyse_binary_subexpr(context, expr, left, right)) return false;

	// Do we have (iptr)(ptr) - rhs? If so we change it to
	// (iptr)((char*)(ptr) - 1)
	Type *cast_to_iptr = defer_iptr_cast(left, right);

	Type *left_type = type_no_fail(left->type)->canonical;
	Type *right_type = type_no_fail(right->type)->canonical;

	// 2. Handle the ptr - x and ptr - other_pointer
	if (left_type->type_kind == TYPE_POINTER)
	{
		if (!sema_decay_array_pointers(left)) return false;
		left_type = type_no_fail(left->type)->canonical;

		// 3. ptr - other pointer
		if (right_type->type_kind == TYPE_POINTER)
		{
			if (!sema_decay_array_pointers(right)) return false;
			right_type = type_no_fail(right->type)->canonical;

			// 3a. Require that both types are the same.
			unify_voidptr(left, right, &left_type, &right_type);
			if (left_type != right_type)
			{
				SEMA_ERROR(expr, "'%s' - '%s' is not allowed. Subtracting pointers of different types from each other is not possible.", type_to_error_string(left_type), type_to_error_string(right_type));
				return false;
			}

			// 3b. Set the type
			expr->type = type_iptrdiff;

			return true;
		}

		right_type = right->type->canonical;

		// 4. Check that the right hand side is an integer.
		if (!type_is_integer(right_type))
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
		expr->type = left->type;
		if (cast_to_iptr)
		{
			expr->resolve_status = RESOLVE_DONE;
			return cast(expr, cast_to_iptr);
		}
		return true;
	}

	if (!sema_promote_binary_top_down(context, expr, left, right)) return false;

	left_type = type_no_fail(left->type)->canonical;
	right_type = type_no_fail(right->type)->canonical;

	// 7. Attempt arithmetic promotion, to promote both to a common type.
	if (!binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, "The subtraction %s - %s is not possible."))
	{
		return false;
	}

	left_type = left->type->canonical;

	expr->type = type_get_opt_fail(left->type, IS_FAILABLE(right));

	// 8. Handle constant folding.
	if (expr_both_const(left, right))
	{
		expr_replace(expr, left);
		switch (left_type->type_kind)
		{
			case ALL_INTS:
				expr->const_expr.ixx = int_sub(left->const_expr.ixx, right->const_expr.ixx);
				break;
			case ALL_FLOATS:
				expr->const_expr.fxx = float_sub(left->const_expr.fxx, right->const_expr.fxx);
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
static bool sema_expr_analyse_add(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Promote everything to the recipient type – if possible
	//    this is safe in the pointer case actually.
	if (!sema_expr_analyse_binary_subexpr(context, expr, left, right)) return false;

	Type *cast_to_iptr = defer_iptr_cast(left, right);
	if (!cast_to_iptr) cast_to_iptr = defer_iptr_cast(right, left);

	Type *left_type = type_no_fail(left->type)->canonical;
	Type *right_type = type_no_fail(right->type)->canonical;

	// 2. To detect pointer additions, reorder if needed
	if (right_type->type_kind == TYPE_POINTER && left_type->type_kind != TYPE_POINTER)
	{
		Expr *temp = right;
		right = left;
		left = temp;
		right_type = left_type;
		left_type = left->type->canonical;
		expr->binary_expr.left = exprid(left);
		expr->binary_expr.right = exprid(right);
	}

	// 3. The "left" will now always be the pointer.
	//    so check if we want to do the normal pointer add special handling.
	if (left_type->type_kind == TYPE_POINTER)
	{
		if (!sema_decay_array_pointers(left)) return false;

		// 3a. Check that the other side is an integer of some sort.
		if (!type_is_integer(right_type))
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
		expr->type = left->type;

		if (cast_to_iptr)
		{
			expr->resolve_status = RESOLVE_DONE;
			return cast(expr, cast_to_iptr);
		}
		return true;
	}

	if (!sema_promote_binary_top_down(context, expr, left, right)) return false;

	left_type = type_no_fail(left->type)->canonical;
	right_type = type_no_fail(right->type)->canonical;

	assert(!cast_to_iptr);
	// 4. Do a binary arithmetic promotion
	if (!binary_arithmetic_promotion(context, left, right, left_type, right_type, expr, "Cannot do the addition %s + %s."))
	{
		return false;
	}

	// 5. Handle the "both const" case. We should only see ints and floats at this point.
	if (expr_both_const(left, right))
	{
		expr_replace(expr, left);
		switch (left->const_expr.const_kind)
		{
			case CONST_INTEGER:
				expr->const_expr.ixx = int_add(left->const_expr.ixx, right->const_expr.ixx);
				break;
			case CONST_FLOAT:
				expr->const_expr.fxx = float_add(left->const_expr.fxx, right->const_expr.fxx);
				break;
			default:
				UNREACHABLE
		}
	}

	// 6. Set the type & other properties.
	expr_unify_binary_failability(expr, left, right);

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
static bool sema_expr_analyse_mult(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{

	// 1. Analyse the sub expressions and promote to a common type
	if (!sema_expr_analyse_binary_arithmetic_subexpr(context, expr, "It is not possible to multiply %s by %s.", false)) return false;


	// 2. Handle constant folding.
	if (expr_both_const(left, right))
	{
		expr_replace(expr, left);
		switch (left->const_expr.const_kind)
		{
			case CONST_INTEGER:
				expr->const_expr.ixx = int_mul(left->const_expr.ixx, right->const_expr.ixx);
				break;
			case CONST_FLOAT:
				expr->const_expr.fxx = float_mul(left->const_expr.fxx, right->const_expr.fxx);
				break;
			default:
				UNREACHABLE
		}
	}

	// 3. Set the new type
	expr->type = type_get_opt_fail(left->type, IS_FAILABLE(right));

	return true;
}

/**
 * Analyse a / b
 * @return true if analysis completed ok.
 */
static bool sema_expr_analyse_div(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyse sub expressions and promote to a common type
	if (!sema_expr_analyse_binary_arithmetic_subexpr(context, expr, "Cannot divide %s by %s.", false)) return false;

	// 2. Check for a constant 0 on the rhs.
	if (IS_CONST(right))
	{
		switch (right->const_expr.const_kind)
		{
			case CONST_INTEGER:
				if (int_is_zero(right->const_expr.ixx))
				{
					SEMA_ERROR(right, "This expression evaluates to zero and division by zero is not allowed.");
					return false;
				}
				break;
			case CONST_FLOAT:
				// This is allowed, as it will generate a NaN
				break;
			default:
				UNREACHABLE
		}
	}

	// 3. Perform constant folding.
	if (expr_both_const(left, right))
	{
		expr_replace(expr, left);
		switch (left->const_expr.const_kind)
		{
			case CONST_INTEGER:
				expr->const_expr.ixx = int_div(left->const_expr.ixx, right->const_expr.ixx);
				break;
			case CONST_FLOAT:
				expr->const_expr.fxx = float_div(left->const_expr.fxx, right->const_expr.fxx);
				break;
			default:
				UNREACHABLE
		}
	}

	// 4. Done.
	expr->type = type_get_opt_fail(left->type, IS_FAILABLE(right));
	return true;

}

/**
 * Analyse a % b
 * @return true if analysis succeeds.
 */
static bool sema_expr_analyse_mod(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyse both sides and promote to a common type
	if (!sema_expr_analyse_binary_arithmetic_subexpr(context, expr, NULL, false)) return false;

	// 3. a % 0 is not valid, so detect it.
	if (IS_CONST(right) && int_is_zero(right->const_expr.ixx))
	{
		SEMA_ERROR(right, "Cannot perform %% with a constant zero.");
		return false;
	}

	// 4. Constant fold
	if (expr_both_const(left, right))
	{
		expr_replace(expr, left);
		// 4a. Remember this is remainder.
		expr->const_expr.ixx = int_rem(left->const_expr.ixx, right->const_expr.ixx);
	}

	expr->type = type_get_opt_fail(left->type, IS_FAILABLE(right));
	return true;
}

/**
 * Analyse a ^ b, a | b, a & b
 * @return true if the analysis succeeded.
 */
static bool sema_expr_analyse_bit(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{

	// 1. Convert to common type if possible.
	if (!sema_expr_analyse_binary_arithmetic_subexpr(context, expr, NULL, true)) return false;

	// 2. Check that both are integers or bools.
	bool is_bool = left->type->canonical == type_bool;
	if (!is_bool && !both_any_integer_or_integer_vector(left, right))
	{
		return sema_type_error_on_binop(expr);
	}

	// 3. Do constant folding if both sides are constant.
	if (expr_both_const(left, right))
	{
		BinaryOp op = expr->binary_expr.operator;
		expr_replace(expr, left);
		if (is_bool)
		{
			switch (op)
			{
				case BINARYOP_BIT_AND:
					expr->const_expr.b = left->const_expr.b & right->const_expr.b;
					break;
				case BINARYOP_BIT_XOR:
					expr->const_expr.b = left->const_expr.b ^ right->const_expr.b;
					break;
				case BINARYOP_BIT_OR:
					expr->const_expr.b = left->const_expr.b | right->const_expr.b;
					break;
				default:
					UNREACHABLE;
			}
		}
		else
		{
			switch (op)
			{
				case BINARYOP_BIT_AND:
					expr->const_expr.ixx = int_and(left->const_expr.ixx, right->const_expr.ixx);
					break;
				case BINARYOP_BIT_XOR:
					expr->const_expr.ixx = int_xor(left->const_expr.ixx, right->const_expr.ixx);
					break;
				case BINARYOP_BIT_OR:
					expr->const_expr.ixx = int_or(left->const_expr.ixx, right->const_expr.ixx);
					break;
				default:
					UNREACHABLE;
			}
		}
	}

	// 5. Assign the type
	expr_unify_binary_failability(expr, left, right);
	return true;
}

/**
 * Analyse >> and << operations.
 * @return true if the analysis succeeded.
 */
static bool sema_expr_analyse_shift(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyze both sides.
	if (!sema_expr_analyse_binary_subexpr(context, expr, left, right)) return false;

	// 2. Only integers or integer vectors may be shifted.
	if (!both_any_integer_or_integer_vector(left, right))
	{
		return sema_type_error_on_binop(expr);
	}

	if (expr->binary_expr.widen && !sema_widen_top_down(left, expr->type)) return false;

	// 3. Promote lhs using the usual numeric promotion.
	if (!cast_implicit(left, numeric_arithmetic_promotion(type_no_fail(left->type)))) return false;

	// 4. For a constant rhs side we will make a series of checks.
	if (IS_CONST(right))
	{
		// 4a. Make sure the value does not exceed the bitsize of
		//     the left hand side. We ignore this check for lhs being a constant.
		Type *left_type_no_fail = type_no_fail(left->type)->canonical;
		assert(type_kind_is_any_integer(left_type_no_fail->type_kind));
		if (int_ucomp(right->const_expr.ixx, left_type_no_fail->builtin.bitsize, BINARYOP_GT))
		{
			SEMA_ERROR(right, "The shift exceeds bitsize of %s.", type_quoted_error_string(type_no_fail(left->type)));
			return false;
		}

		// 4b. Make sure that the RHS is positive.
		if (int_is_neg(right->const_expr.ixx))
		{
			SEMA_ERROR(right, "A shift must be a positive number.");
			return false;
		}

		// 5. Fold constant expressions.
		if (IS_CONST(left))
		{
			bool shr = expr->binary_expr.operator == BINARYOP_SHR;
			expr_replace(expr, left);
			if (shr)
			{
				expr->const_expr.ixx = int_shr64(left->const_expr.ixx, right->const_expr.ixx.i.low);
			}
			else
			{
				expr->const_expr.ixx = int_shl64(left->const_expr.ixx, right->const_expr.ixx.i.low);
			}
			return true;
		}
	}

	// 6. Set the type
	expr->type = type_get_opt_fail(left->type, IS_FAILABLE(right));
	return true;
}

/**
 * Analyse a <<= b a >>= b
 * @return true is the analysis succeeds, false otherwise.
 */
static bool sema_expr_analyse_shift_assign(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{

	// 1. Analyze the two sub lhs & rhs *without coercion*
	if (!sema_expr_analyse_binary_subexpr(context, expr, left, right)) return false;

	bool failable = IS_FAILABLE(left) || IS_FAILABLE(right);

	// 2. Ensure the lhs side is assignable
	if (!sema_expr_check_assign(context, left)) return false;

	// 3. Only integers may be shifted.
	if (!both_any_integer_or_integer_vector(left, right)) return sema_type_error_on_binop(expr);

	// 4. For a constant right hand side we will make a series of checks.
	if (IS_CONST(right))
	{
		// 4a. Make sure the value does not exceed the bitsize of
		//     the left hand side.
		if (int_ucomp(right->const_expr.ixx, left->type->canonical->builtin.bitsize, BINARYOP_GT))
		{
			SEMA_ERROR(right, "The shift exceeds bitsize of '%s'.", type_to_error_string(left->type));
			return false;
		}

		// 4b. Make sure that the right hand side is positive.
		if (int_is_neg(right->const_expr.ixx))
		{
			SEMA_ERROR(right, "A shift must be a positive number.");
			return false;
		}
	}

	// 5. Set the type using the lhs side.

	if (left->expr_kind == EXPR_BITACCESS)
	{
		expr->expr_kind = EXPR_BITASSIGN;
	}
	expr->type = type_get_opt_fail(left->type, failable);
	return true;
}


static bool sema_expr_analyse_and_or(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	if (!sema_expr_analyse_binary_subexpr(context, expr, left, right)) return false;
	if (!cast_implicit(left, type_bool) || !cast_implicit(right, type_bool)) return false;

	if (expr_both_const(left, right))
	{
		if (expr->binary_expr.operator == BINARYOP_AND)
		{
			expr_replace(expr, left);
			expr->const_expr.b &= right->const_expr.b;
		}
		else
		{
			expr_replace(expr, left);
			expr->const_expr.b |= right->const_expr.b;
		}
	}
	expr->type = type_bool;
	return true;
}



static void cast_to_max_bit_size(SemaContext *context, Expr *left, Expr *right, Type *left_type, Type *right_type)
{
	unsigned bit_size_left = left_type->builtin.bitsize;
	unsigned bit_size_right = right_type->builtin.bitsize;
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
	           ? type_int_signed_by_bitsize(bit_size_left)
	           : type_int_unsigned_by_bitsize(bit_size_left);
	bool success = cast_implicit(right, to);
	assert(success);
}

static bool sema_is_unsigned_always_false_comparison(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	if (context->active_scope.flags & SCOPE_MACRO) return true;
	if (!expr_is_const(left) && !expr_is_const(right)) return true;
	if (!type_is_integer(left->type)) return true;
	if (expr_is_const(left) && type_is_unsigned(type_flatten_distinct(right->type)))
	{
		if (int_is_neg(left->const_expr.ixx))
		{
			SEMA_ERROR(left, "Comparing an unsigned value with a negative constant is only allowed inside of macros.");
			return false;
		}
		if (!int_is_zero(left->const_expr.ixx)) return true;
		switch (expr->binary_expr.operator)
		{
			case BINARYOP_GT:
				SEMA_ERROR(left, "Comparing '0 > unsigned expression' can never be true, and is only allowed inside of macro expansions.");
				return false;
			case BINARYOP_GE:
				SEMA_ERROR(left,
				           "Comparing '0 >= unsigned expression' is the same as 0 == expr and is a common bug, "
						   "so is only allowed inside of macro expansions.");
				return false;
			default:
				return true;
		}
	}
	if (!expr_is_const(right) || !type_is_unsigned(type_flatten_distinct(left->type))) return true;
	if (int_is_neg(right->const_expr.ixx))
	{
		SEMA_ERROR(right, "Comparing an unsigned value with a negative constant is only allowed inside of macros.");
		return false;
	}
	if (!int_is_zero(right->const_expr.ixx)) return true;
	switch (expr->binary_expr.operator)
	{
		case BINARYOP_LT:
			SEMA_ERROR(right,
			           "Comparing 'unsigned expression < 0' can never be true, and is only allowed inside of macro expansions.");
			return false;
		case BINARYOP_LE:
			SEMA_ERROR(right,
					   "Comparing 'unsigned expression <= 0' is the same as expr == 0 and is a common bug, "
			           "so is only allowed inside of macro expansions.");
			return false;
		default:
			return true;
	}

}

/**
 * Analyze a == b, a != b, a > b, a < b, a >= b, a <= b
 * @return
 */
static bool sema_expr_analyse_comp(SemaContext *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyse left and right side without any conversions.
	if (!sema_expr_analyse_binary_subexpr(context, expr, left, right)) return false;

	bool is_equality_type_op = expr->binary_expr.operator == BINARYOP_NE || expr->binary_expr.operator == BINARYOP_EQ;

	// Flatten enum/distinct/failable
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
		goto DONE;
	}

	if (left_type->type_kind == TYPE_VECTOR && right_type->type_kind == TYPE_VECTOR)
	{
		if (left_type->array.len == right_type->array.len)
		{
			Type *left_vec = type_vector_type(left_type);
			Type *right_vec = type_vector_type(right_type);
			if (left_vec == right_vec) goto DONE;
			if (type_size(left_vec) != type_size(right_vec)) goto DONE;
			if (type_is_integer(left_vec) && type_is_integer(right_vec)) goto DONE;
		}
		SEMA_ERROR(expr, "Vector types %s and %s cannot be compared.",
		           type_quoted_error_string(left->type), type_quoted_error_string(right->type));
		return false;
	}

	// 3. In the normal case, treat this as a binary op, finding the max type.
	Type *max = type_find_max_type(type_no_fail(left->type)->canonical, type_no_fail(right->type)->canonical);

	// 4. If no common type, then that's an error:
	if (!max)
	{
		SEMA_ERROR(expr, "%s and %s are different types and cannot be compared.",
		           type_quoted_error_string(left->type), type_quoted_error_string(right->type));
		return false;
	}

	if (!type_is_comparable(max))
	{
		SEMA_ERROR(expr, "%s does not support comparisons, you need to manually implement a comparison if you need it.",
		           type_quoted_error_string(left->type));
		return false;
	}
	if (!is_equality_type_op)
	{
		if (!type_is_ordered(max))
		{
			SEMA_ERROR(expr, "%s can only be compared using '!=' and '==' it cannot be ordered, did you make a mistake?",
			           type_quoted_error_string(left->type));
			return false;
		}
		if (type_flatten(max)->type_kind == TYPE_POINTER)
		{

			// Only comparisons between the same type is allowed. Subtypes not allowed.
			if (left_type != right_type && left_type != type_voidptr && right_type != type_voidptr)
			{
				SEMA_ERROR(expr, "You are not allowed to compare pointers of different types, "
								 "if you need to do, first convert all pointers to void*.");
				return false;
			}
		}
	}

	// 6. Do the implicit cast.
	bool success = cast_implicit(left, max) && cast_implicit(right, max);
	assert(success);
DONE:

	// 7. Do constant folding.
	if (expr_both_const(left, right))
	{
		expr->const_expr.b = expr_const_compare(&left->const_expr, &right->const_expr, expr->binary_expr.operator);
		expr->const_expr.const_kind = CONST_BOOL;
		expr->expr_kind = EXPR_CONST;
	}
	else
	{
		if (!sema_is_unsigned_always_false_comparison(context, expr, left, right)) return false;
	}

	// 8. Set the type to bool

	Type *return_type = left_type->type_kind == TYPE_VECTOR ? type_get_vector_bool(left_type) : type_bool;
	expr->type = type_get_opt_fail(return_type, IS_FAILABLE(left) || IS_FAILABLE(right));

	return true;
}

/**
 * Analyse *a
 * @return true if analysis succeeds.
 */
static bool sema_expr_analyse_deref(SemaContext *context, Expr *expr)
{
	// 1. Check the inner expression
	Expr *inner = expr->unary_expr.expr;
	if (!sema_analyse_expr(context, inner)) return false;

	Type *inner_type_nofail = type_no_fail(inner->type);
	Type *canonical = inner_type_nofail->canonical;

	// 2. Check that we have a pointer, or dereference is not allowed.
	if (canonical->type_kind != TYPE_POINTER)
	{
		SEMA_ERROR(inner, "Cannot dereference a value of type %s, it must be a pointer.", type_quoted_error_string(inner_type_nofail));
		return false;
	}
	if (canonical->pointer == type_void)
	{
		SEMA_ERROR(inner, "A 'void*' cannot be dereferenced, you need to first cast it to a concrete type.");
		return false;
	}

	// 3. This could be a constant, in which case it is a null which is an error.
	if (inner->expr_kind == EXPR_CONST)
	{
		SEMA_ERROR(inner, "Dereferencing null is not allowed, did you do it by mistake?");
		return false;
	}

	// 4. Now the type might not be a pointer because of a typedef,
	//    otherwise we need to use the canonical representation.
	Type *deref_type = inner_type_nofail->type_kind == TYPE_POINTER ? inner_type_nofail : canonical;

	// 5. And... set the type.
	expr->type = type_get_opt_fail(deref_type->pointer, IS_FAILABLE(inner));

	return true;
}

static inline bool sema_take_addr_of_var(Expr *expr, Decl *decl)
{
	if (decl->decl_kind != DECL_VAR) return false;
	decl->var.is_addr = true;
	bool is_void = type_flatten(decl->type) == type_void;
	switch (decl->var.kind)
	{
		case VARDECL_GLOBAL:
			if (is_void)
			{
				SEMA_ERROR(expr, "You cannot take the address of a global of type %s.", type_quoted_error_string(decl->type));
				return false;
			}
			return true;
		case VARDECL_LOCAL:
			if (is_void)
			{
				SEMA_ERROR(expr, "You cannot take the address of a variable with type %s.", type_quoted_error_string(decl->type));
				return false;
			}
			return true;
		case VARDECL_PARAM:
		case VARDECL_PARAM_REF:
			if (is_void)
			{
				SEMA_ERROR(expr, "You cannot take the address of a parameter with type %s.", type_quoted_error_string(decl->type));
				return false;
			}
			return true;
		case VARDECL_CONST:
			if (!decl->var.type_info)
			{
				SEMA_ERROR(expr, "The constant is not typed, either type it or use && to take the reference to a temporary.");
				SEMA_PREV(decl, "The constant was defined here.");
				return false;
			}
			assert(decl->type != type_void);
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
		case VARDECL_BITMEMBER:
		case VARDECL_UNWRAPPED:
		case VARDECL_REWRAPPED:
		case VARDECL_ERASE:
			UNREACHABLE
	}
	UNREACHABLE

}

static inline bool sema_take_addr_of_ident(Expr *inner)
{
	Decl *decl = decl_raw(inner->identifier_expr.decl);
	switch (decl->decl_kind)
	{
		case DECL_FUNC:
			return true;
		case DECL_VAR:
			return sema_take_addr_of_var(inner, decl);
		case DECL_MACRO:
			SEMA_ERROR(inner, "It is not possible to take the address of a macro.");
			return false;
		case DECL_GENERIC:
			SEMA_ERROR(inner, "It is not possible to take the address of a generic function.");
			return false;
		default:
			UNREACHABLE
	}
	UNREACHABLE
}

static bool sema_take_addr_of(Expr *inner)
{
	switch (inner->expr_kind)
	{
		case EXPR_CT_IDENT:
			SEMA_ERROR(inner, "It's not possible to take the address of a compile time value.");
			return false;
		case EXPR_IDENTIFIER:
			return sema_take_addr_of_ident(inner);
		case EXPR_UNARY:
			if (inner->unary_expr.operator == UNARYOP_DEREF) return true;
			break;
		case EXPR_ACCESS:
			return sema_take_addr_of(inner->access_expr.parent);
		case EXPR_GROUP:
			return sema_take_addr_of(inner->inner_expr);
		case EXPR_SUBSCRIPT:
			return sema_take_addr_of(exprptr(inner->subscript_expr.expr));
		case EXPR_TYPEINFO:
			SEMA_ERROR(inner, "It is not possible to take the address of a type.");
			return false;
		case EXPR_BITACCESS:
			SEMA_ERROR(inner, "You cannot take the address of a bitstruct member.");
			return false;
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
static bool sema_expr_analyse_addr(SemaContext *context, Expr *expr)
{
	// 1. Evaluate the expression
	Expr *inner = expr->unary_expr.expr;
	REDO:
	switch (inner->expr_kind)
	{
		case EXPR_POISONED:
			return false;
		case EXPR_GROUP:
			// We want to collapse any grouping here.
			expr_replace(inner, inner->inner_expr);
			goto REDO;
		case EXPR_SUBSCRIPT:
			inner->expr_kind = EXPR_SUBSCRIPT_ADDR;
			if (!sema_analyse_expr_lvalue(context, inner)) return false;
			expr_replace(expr, inner);
			return true;
		default:
		{
			if (!sema_analyse_expr_lvalue(context, inner)) return expr_poison(expr);
		}
	}

	// 2. Take the address.
	if (!sema_take_addr_of(inner)) return expr_poison(expr);

	// 3. Get the pointer of the underlying type.
	expr->type = type_get_ptr_recurse(inner->type);

	return true;
}

/**
 * Test -a
 */
static bool sema_expr_analyse_neg(SemaContext *context, Expr *expr)
{
	// 1. Check the inner expression
	Expr *inner = expr->unary_expr.expr;
	if (!sema_analyse_expr(context, inner)) return false;
	if (expr->unary_expr.widen && !sema_widen_top_down(inner, expr->type)) return false;

	// 2. Check if it's possible to negate this (i.e. is it an int, float or vector)
	Type *no_fail = type_no_fail(inner->type);
	if (!type_may_negate(no_fail))
	{
		SEMA_ERROR(expr, "Cannot negate an expression of type %s.", type_quoted_error_string(no_fail));
		return false;
	}
	// 3. Promote the type
	Type *result_type = numeric_arithmetic_promotion(no_fail);
	if (!cast_implicit(inner, result_type)) return false;

	// 4. If it's non-const, we're done.
	if (inner->expr_kind != EXPR_CONST)
	{
		expr->type = inner->type;
		return true;
	}

	// 5. Otherwise constant fold.
	expr_replace(expr, inner);
	switch (expr->const_expr.const_kind)
	{
		case CONST_INTEGER:
			expr->const_expr.ixx = int_neg(inner->const_expr.ixx);
			return true;
		case CONST_FLOAT:
			expr->const_expr.fxx = float_neg(expr->const_expr.fxx);
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
static bool sema_expr_analyse_bit_not(SemaContext *context, Expr *expr)
{
	// 1. Analyse the inner expression.
	Expr *inner = expr->unary_expr.expr;
	if (!sema_analyse_expr(context, inner)) return false;

	if (expr->unary_expr.widen && !sema_widen_top_down(inner, expr->type)) return false;

	// 2. Check that it's a vector, bool
	Type *canonical = type_no_fail(inner->type)->canonical;
	if (!type_is_integer_or_bool_kind(type_flatten_distinct(canonical)))
	{
		Type *vector_type = type_vector_type(canonical);
		if (vector_type && (type_is_integer(vector_type) || vector_type == type_bool)) goto VALID_VEC;
		SEMA_ERROR(expr, "Cannot bit negate '%s'.", type_to_error_string(inner->type));
		return false;
	}

VALID_VEC:
	// 3. The simple case, non-const.
	if (inner->expr_kind != EXPR_CONST)
	{
		expr->type = inner->type;
		return true;
	}

	// 4. Otherwise handle const bool
	expr_replace(expr, inner);
	if (expr->const_expr.const_kind == CONST_BOOL)
	{
		expr->const_expr.b = !expr->const_expr.b;
		return true;
	}

	// 5. Perform ~ constant folded
	// TODO arithmetic promotion?
	expr->const_expr.ixx = int_not(inner->const_expr.ixx);
	return true;
}

/**
 * Evaluate !a
 */
static bool sema_expr_analyse_not(SemaContext *context, Expr *expr)
{
	// 1. Evaluate inner
	Expr *inner = expr->unary_expr.expr;
	if (!sema_analyse_expr(context, inner)) return false;

	// 2. Check whether the type is a vector
	Type *type = type_no_fail(inner->type);
	if (type_is_vector(type))
	{
		// 3. This always works, so we're done.
		expr->type = type_get_opt_fail(type_get_vector_bool(type), IS_FAILABLE(inner));
		return true;
	}

	// 4. Let's see if it's possible to cast it implicitly
	if (!cast_may_implicit(type, type_bool, true, true))
	{
		SEMA_ERROR(expr, "The use of '!' on %s is not allowed as it can't be converted to a boolean value.", type_quoted_error_string(inner->type));
		return false;
	}

	expr->type = type_get_opt_fail(type_bool, IS_FAILABLE(inner));

	if (inner->expr_kind == EXPR_CONST)
	{
		bool success = cast_implicit(inner, expr->type);
		assert(success);
		assert(inner->const_expr.const_kind == CONST_BOOL);
		expr->const_expr.const_kind = CONST_BOOL;
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.b = !inner->const_expr.b;
	}
	return true;
}

static inline bool sema_expr_analyse_ct_incdec(SemaContext *context, Expr *expr, Expr *inner)
{
	assert(inner->expr_kind == EXPR_CT_IDENT);

	if (!sema_expr_analyse_ct_identifier_lvalue(context, inner)) return false;

	Decl *var = inner->ct_ident_expr.decl;
	Expr *start_value = var->var.init_expr;
	assert(start_value->expr_kind == EXPR_CONST);

	switch (start_value->const_expr.const_kind)
	{
		case CONST_INTEGER:
			break;
		default:
			SEMA_ERROR(expr, "The compile time variable '%s' does not hold an integer.", var->name);
			return false;
	}

	Expr *end_value = expr_copy(start_value);

	// Make the change.
	if (expr->unary_expr.operator == UNARYOP_DEC)
	{
		end_value->const_expr.ixx = int_sub64(start_value->const_expr.ixx, 1);
	}
	else
	{
		end_value->const_expr.ixx = int_add64(start_value->const_expr.ixx, 1);
	}
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
static inline bool sema_expr_analyse_incdec(SemaContext *context, Expr *expr)
{
	// 1. Analyse the lvalue to update
	Expr *inner = expr->unary_expr.expr;
	if (!sema_analyse_expr_lvalue(context, inner)) return false;

	// 2. Assert it's an l-value
	if (!sema_expr_check_assign(context, inner)) return false;

	// 3. This might be a $foo, if to handle it.
	if (inner->expr_kind == EXPR_CT_IDENT)
	{
		return sema_expr_analyse_ct_incdec(context, expr, inner);
	}

	// 4. Flatten typedef, enum, distinct, failable
	Type *type = type_flatten(inner->type);

	// 5. We can only inc/dec numbers or pointers.
	if (!type_underlying_is_numeric(type) && type->type_kind != TYPE_POINTER)
	{
		SEMA_ERROR(inner, "The expression must be a number or a pointer.");
		return false;
	}

	if (!sema_decay_array_pointers(inner)) return false;

	// 6. Done, the result is same as the inner type.
	expr->type = inner->type;
	return true;
}

/**
 * Take an address of a temporary &&x.
 */
static inline bool sema_expr_analyse_taddr(SemaContext *context, Expr *expr)
{
	Expr *inner = expr->unary_expr.expr;
	if (!sema_analyse_expr(context, inner)) return false;

	// 2. The type is the resulting type of the expression.
	expr->type = type_get_ptr_recurse(inner->type);
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

static inline bool sema_expr_analyse_bitassign(SemaContext *context, Expr *expr)
{
	TODO
}

static inline bool sema_expr_analyse_or_error(SemaContext *context, Expr *expr)
{
	Expr *lhs = exprptr(expr->binary_expr.left);
	Expr *rhs = exprptr(expr->binary_expr.right);
	if (lhs->expr_kind == EXPR_TERNARY || rhs->expr_kind == EXPR_TERNARY)
	{
		SEMA_ERROR(expr, "Unclear precedence using ternary with ??, please use () to remove ambiguity.");
		return false;
	}
	if (!sema_analyse_expr(context, lhs)) return false;

	if (expr->binary_expr.widen && !sema_widen_top_down(lhs, expr->type)) return false;

	Type *type = lhs->type;
	if (!type_is_failable(type))
	{
		SEMA_ERROR(lhs, "No failable to use '\?\?' with, please remove the '\?\?'.");
		return false;
	}

	// First we analyse the "else" and try to implictly cast.
	if (!sema_analyse_expr(context, rhs)) return false;
	if (expr->binary_expr.widen && !sema_widen_top_down(rhs, expr->type)) return false;


	// Here we might need to insert casts.
	Type *else_type = rhs->type;

	type = type_is_failable_any(type) ? else_type : type->failable;

	if (else_type->type_kind == TYPE_FAILABLE)
	{
		SEMA_ERROR(rhs, "The default value may not be a failable.");
		return false;
	}
	Type *common = type_find_max_type(type, else_type);
	if (!common)
	{
		SEMA_ERROR(rhs, "Cannot find a common type for %s and %s.", type_quoted_error_string(type),
				   type_quoted_error_string(else_type));
		return false;
	}
	if (!cast_implicit(lhs, common)) return false;
	if (!cast_implicit(rhs, common)) return false;
	if (IS_FAILABLE(rhs))
	{
		SEMA_ERROR(rhs, "The expression must be a non-failable.");
		return false;
	}
	expr->type = common;
	return true;
}

static inline bool sema_expr_analyse_binary(SemaContext *context, Expr *expr)
{
	if (expr->binary_expr.operator == BINARYOP_OR_ERR) return sema_expr_analyse_or_error(context, expr);
	assert(expr->resolve_status == RESOLVE_RUNNING);
	Expr *left = exprptr(expr->binary_expr.left);
	Expr *right = exprptr(expr->binary_expr.right);
	// check if both sides have a binary operation where the precedence is unclear. Example: a ^ b | c
	if (unclear_op_precedence(left, expr, right))
	{
		SEMA_ERROR(expr, "You need to add explicit parentheses to clarify precedence.");
		return expr_poison(expr);
	}
	switch (expr->binary_expr.operator)
	{
		case BINARYOP_OR_ERR:
			UNREACHABLE // Handled previously
		case BINARYOP_ASSIGN:
			return sema_expr_analyse_assign(context, expr, left, right);
		case BINARYOP_MULT:
			return sema_expr_analyse_mult(context, expr, left, right);
		case BINARYOP_ADD:
			return sema_expr_analyse_add(context, expr, left, right);
		case BINARYOP_ADD_ASSIGN:
		case BINARYOP_SUB_ASSIGN:
			return sema_expr_analyse_add_sub_assign(context, expr, left, right);
		case BINARYOP_SUB:
			return sema_expr_analyse_sub(context, expr, left, right);
		case BINARYOP_DIV:
			return sema_expr_analyse_div(context, expr, left, right);
		case BINARYOP_MULT_ASSIGN:
		case BINARYOP_DIV_ASSIGN:
			return sema_expr_analyse_common_assign(context, expr, left, right, false);
		case BINARYOP_BIT_AND_ASSIGN:
		case BINARYOP_BIT_OR_ASSIGN:
		case BINARYOP_BIT_XOR_ASSIGN:
		case BINARYOP_MOD_ASSIGN:
			return sema_expr_analyse_common_assign(context, expr, left, right, true);
		case BINARYOP_MOD:
			return sema_expr_analyse_mod(context, expr, left, right);
		case BINARYOP_AND:
		case BINARYOP_OR:
			return sema_expr_analyse_and_or(context, expr, left, right);
		case BINARYOP_BIT_OR:
		case BINARYOP_BIT_XOR:
		case BINARYOP_BIT_AND:
			return sema_expr_analyse_bit(context, expr, left, right);
		case BINARYOP_NE:
		case BINARYOP_EQ:
		case BINARYOP_GT:
		case BINARYOP_GE:
		case BINARYOP_LT:
		case BINARYOP_LE:
			return sema_expr_analyse_comp(context, expr, left, right);
		case BINARYOP_SHR:
		case BINARYOP_SHL:
			return sema_expr_analyse_shift(context, expr, left, right);
		case BINARYOP_SHR_ASSIGN:
		case BINARYOP_SHL_ASSIGN:
			return sema_expr_analyse_shift_assign(context, expr, left, right);
		case BINARYOP_ERROR:
			break;
	}
	UNREACHABLE
}

/**
 * Analyse:
 *   *x
 *   &x
 *   x++/++x/x--/--x
 *   ~x
 *   !x
 *   &&x
 *   -x
 */
static inline bool sema_expr_analyse_unary(SemaContext *context, Expr *expr)
{
	assert(expr->resolve_status == RESOLVE_RUNNING);
	switch (expr->unary_expr.operator)
	{
		case UNARYOP_DEREF:
			return sema_expr_analyse_deref(context, expr);
		case UNARYOP_ADDR:
			return sema_expr_analyse_addr(context, expr);
		case UNARYOP_NEG:
			return sema_expr_analyse_neg(context, expr);
		case UNARYOP_BITNEG:
			return sema_expr_analyse_bit_not(context, expr);
		case UNARYOP_NOT:
			return sema_expr_analyse_not(context, expr);
		case UNARYOP_DEC:
		case UNARYOP_INC:
			return sema_expr_analyse_incdec(context, expr);
		case UNARYOP_TADDR:
			return sema_expr_analyse_taddr(context, expr);
		case UNARYOP_ERROR:
			return false;
	}
	UNREACHABLE
}


static inline bool sema_expr_analyse_try(SemaContext *context, Expr *expr)
{
	Expr *inner = expr->inner_expr;
	if (!sema_analyse_expr(context, inner)) return false;
	if (!IS_FAILABLE(inner))
	{
		SEMA_ERROR(inner, "Expected a failable expression to 'try'.");
		return false;
	}
	expr->type = type_bool;
	return true;
}

static inline bool sema_expr_analyse_catch(SemaContext *context, Expr *expr)
{
	Expr *inner = expr->inner_expr;
	if (!sema_analyse_expr(context, inner)) return false;
	if (!IS_FAILABLE(inner))
	{
		SEMA_ERROR(inner, "Expected a failable expression to 'catch'.");
		return false;
	}
	expr->type = type_anyerr;
	return true;
}

static inline bool sema_expr_analyse_rethrow(SemaContext *context, Expr *expr)
{
	if (!context->current_function)
	{
		SEMA_ERROR(expr, "Rethrow cannot be used outside of a function.");
		return false;
	}
	Expr *inner = expr->rethrow_expr.inner;
	if (!sema_analyse_expr(context, inner)) return false;

	if (context->active_scope.in_defer)
	{
		SEMA_ERROR(expr, "Returns are not allowed inside of defers.");
		return false;
	}
	expr->rethrow_expr.cleanup = context_get_defers(context, context->active_scope.defer_last, 0);
	if (inner->type == type_anyfail)
	{
		SEMA_ERROR(expr, "This expression will always throw, which isn't allowed.");
		return false;
	}
	expr->type = type_no_fail(inner->type);

	if (!IS_FAILABLE(inner))
	{
		SEMA_ERROR(expr, "No failable to rethrow before '?' in the expression, please remove '?'.");
		return false;
	}


	if (context->active_scope.flags & (SCOPE_EXPR_BLOCK | SCOPE_MACRO))
	{
		vec_add(context->returns, NULL);
	}
	else
	{
		if (context->rtype && context->rtype->type_kind != TYPE_FAILABLE)
		{
			SEMA_ERROR(expr, "This expression implicitly returns with a failable result, but the function does not allow failable results. Did you mean to use 'else' instead?");
			return false;
		}
	}

	return true;
}


static inline bool sema_expr_analyse_force_unwrap(SemaContext *context, Expr *expr)
{
	Expr *inner = expr->inner_expr;
	if (!sema_analyse_expr(context, inner)) return false;
	if (inner->type == type_anyfail)
	{
		SEMA_ERROR(expr, "This expression will always throw, which isn't allowed.");
		return false;
	}
	expr->type = type_no_fail(inner->type);
	if (!IS_FAILABLE(inner))
	{
		SEMA_ERROR(expr, "No failable to rethrow before '!!' in the expression, please remove '!!'.");
		return false;
	}
	return true;
}





static inline bool sema_expr_analyse_typeid(SemaContext *context, Expr *expr)
{
	if (!sema_resolve_type_info(context, expr->typeid_expr)) return expr_poison(expr);
	Type *type = expr->type_expr->type;
	expr->expr_kind = EXPR_CONST;
	expr->const_expr.const_kind = CONST_TYPEID;
	expr->const_expr.typeid = type->canonical;
	expr->type = type_typeid;
	return true;
}


static inline bool sema_expr_analyse_expr_block(SemaContext *context, Type *infer_type, Expr *expr)
{
	bool success = true;
	expr->type = type_void;
	Ast **saved_returns = context_push_returns(context);
	Type *stored_block_type = context->expected_block_type;
	context->expected_block_type = infer_type;
	BlockExit **ref = MALLOCS(BlockExit*);
	BlockExit **stored_block_exit = context->block_exit_ref;
	context->block_exit_ref = ref;
	expr->expr_block.block_exit_ref = ref;
	SCOPE_START_WITH_FLAGS(SCOPE_EXPR_BLOCK)

		context->block_return_defer = context->active_scope.defer_last;
		PUSH_CONTINUE(NULL);
		PUSH_BREAK(NULL);
		PUSH_NEXT(NULL, NULL);

		AstId current = expr->expr_block.first_stmt;
		Ast *stmt = NULL;
		while (current)
		{
			stmt = ast_next(&current);
			if (!sema_analyse_statement(context, stmt))
			{
				success = false;
				goto EXIT;
			}
		}

		if (!vec_size(context->returns))
		{
			expr->type = type_void;
			goto EXIT;
		}

		// Let's unify the return statements.
		Type *sum_returns = unify_returns(context);
		if (!sum_returns)
		{
			success = false;
			goto EXIT;
		}
		if (type_no_fail(sum_returns) != type_void  && !context->active_scope.jump_end)
		{
			Ast *ast = ast_last(astptr(expr->expr_block.first_stmt));
			SEMA_ERROR(ast, "Expected a return statement following this statement.");
			success = false;
			goto EXIT;
		}
		expr->type = sum_returns;

	EXIT:
		POP_BREAKCONT();
		POP_NEXT();

		context_pop_defers(context, &stmt->next);
	SCOPE_END;
	context->expected_block_type = stored_block_type;
	context->block_exit_ref = stored_block_exit;
	context_pop_returns(context, saved_returns);

	return success;
}


static inline bool sema_expr_analyse_compound_literal(SemaContext *context, Expr *expr)
{
	if (!sema_resolve_type_info(context, expr->expr_compound_literal.type_info)) return false;
	Type *type = expr->expr_compound_literal.type_info->type;
	if (type_is_failable(type))
	{
		SEMA_ERROR(expr->expr_compound_literal.type_info,
		           "The type here should always be written as a plain type and not a failable, please remove the '!'.");
		return false;
	}
	if (!sema_expr_analyse_initializer_list(context, type, expr->expr_compound_literal.initializer)) return false;
	expr_replace(expr, expr->expr_compound_literal.initializer);
	return true;
}



static inline bool sema_expr_analyse_failable(SemaContext *context, Expr *expr)
{
	Expr *inner = expr->inner_expr;
	if (!sema_analyse_expr(context, inner)) return false;

	if (IS_FAILABLE(inner))
	{
		SEMA_ERROR(inner, "The inner expression is already a failable.");
		return false;
	}

	if (inner->expr_kind == EXPR_FAILABLE)
	{
		SEMA_ERROR(inner, "It looks like you added one too many '!' after the error.");
		return false;
	}

	Type *type = inner->type->canonical;
	if (type->type_kind != TYPE_FAULTTYPE && type->type_kind != TYPE_ANYERR)
	{
		SEMA_ERROR(inner, "You cannot use the '!' operator on expressions of type %s", type_quoted_error_string(type));
		return false;
	}
	expr->type = type_anyfail;
	return true;
}

static inline bool sema_expr_analyse_compiler_const(SemaContext *context, Expr *expr)
{
	const char *string = expr->builtin_expr.ident;
	if (string == kw_FILE)
	{
		expr_rewrite_to_string(expr, context->compilation_unit->file->name);
		return true;
	}
	if (string == kw_FUNC)
	{
		if (!context->current_function)
		{
			expr_rewrite_to_string(expr, "<GLOBAL>");
			return true;
		}
		expr_rewrite_to_string(expr, context->current_function->name);
		return true;
	}
	if (string == kw_LINEREAL)
	{
		expr_rewrite_to_int_const(expr, type_isize, expr->span.row, true);
		return true;
	}
	if (string == kw_LINE)
	{
		if (context->original_inline_line)
		{
			expr_rewrite_to_int_const(expr, type_isize, context->original_inline_line, true);
		}
		else
		{
			expr_rewrite_to_int_const(expr, type_isize, expr->span.row, true);
		}
		return true;
	}
	Expr *value = htable_get(&global_context.compiler_defines, string);
	if (!value)
	{
		SEMA_ERROR(expr, "The compiler constant '%s' was not defined, did you mistype or forget to add it?", string);
		return false;
	}
	expr_replace(expr, value);
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
	    || kind == VARDECL_BITMEMBER
		|| kind == VARDECL_MEMBER;
}


static bool sema_expr_analyse_type_var_path(SemaContext *context, Expr *expr, ExprFlatElement **elements, Type **type_ref,
                                            Decl **decl_ref)
{
	if (!sema_analyse_expr_lvalue(context, expr)) return false;
	Expr *current = expr;
	Decl *decl = NULL;
	Type *type = NULL;
RETRY:
	switch (current->expr_kind)
	{
		case EXPR_CT_IDENT:
			current = current->identifier_expr.decl->var.init_expr;
			goto RETRY;
		case EXPR_IDENTIFIER:
			decl = current->identifier_expr.decl;
			break;
		case EXPR_TYPEINFO:
			type = current->type_expr->type;
			break;
		default:
			SEMA_ERROR(expr, "A type or a variable was expected here.");
			return false;
	}
	if (decl)
	{
		if (!sema_analyse_decl(context, decl)) return false;
		type = decl->type;
	}
	*decl_ref = decl;
	*type_ref = type;
	return true;
}

static inline bool sema_expr_analyse_flat_element(SemaContext *context, ExprFlatElement *element, Type *type, Decl **member_ref, ArraySize *index_ref, Type **return_type, unsigned i, SourceSpan loc,
												  bool *is_missing)
{
	Expr *inner = element->inner;
	Type *actual_type = type_flatten_distinct(type);
	if (element->array)
	{
		if (!type_is_arraylike(actual_type))
		{
			if (is_missing)
			{
				*is_missing = true;
				return false;
			}
			SEMA_ERROR(inner, "It's not possible to index into something that is not an array nor vector.");
			return false;
		}
		if (!sema_analyse_expr(context, inner)) return false;
		if (!type_is_integer(inner->type))
		{
			SEMA_ERROR(inner, "Expected an integer index.");
			return false;
		}
		if (!expr_is_const(inner))
		{
			SEMA_ERROR(inner, "Expected a constant index.");
			return false;
		}
		Int value = inner->const_expr.ixx;
		if (!int_fits(value, type_isize->canonical->type_kind))
		{
			SEMA_ERROR(inner, "The index is out of range for a %s.", type_quoted_error_string(type_isize));
			return false;
		}
		if (int_is_neg(value))
		{
			SEMA_ERROR(inner, "The index must be zero or greater.");
			return false;
		}
		type = actual_type->array.base;
		ArraySize len = actual_type->array.len;
		int64_t index = int_to_i64(value);
		if (len && index >= len)
		{
			if (is_missing)
			{
				*is_missing = true;
				return false;
			}
			SEMA_ERROR(element->inner, "Index exceeds array bounds.");
			return false;
		}
		*return_type = type;
		*index_ref = index;
		*member_ref = NULL;
		return true;
	}
	inner = sema_expr_resolve_access_child(context, inner, is_missing);
	if (!inner) return false;
	if (inner->expr_kind != EXPR_IDENTIFIER)
	{
		SEMA_ERROR(inner, "Expected an identifier here.");
		return false;
	}
	if (!type_is_structlike(actual_type))
	{
		if (is_missing)
		{
			*is_missing = true;
			return false;
		}
		if (i == 0)
		{
			sema_error_at(loc, "%s has no members.", type_quoted_error_string(type));
		}
		else
		{
			sema_error_at(loc, "There is no such member in %s.", type_quoted_error_string(type));
		}
		return false;
	}
	Decl *member;
	SCOPE_START
		add_members_to_context(context, actual_type->decl);
		member = sema_resolve_symbol_in_current_dynamic_scope(context, element->inner->identifier_expr.ident);
	SCOPE_END;
	if (!member)
	{
		if (is_missing)
		{
			*is_missing = true;
			return false;
		}
		sema_error_at(loc, "There is no such member in %s.", type_quoted_error_string(type));
		return false;
	}
	*member_ref = member;
	*return_type = member->type;
	return true;
}

static inline bool sema_expr_analyse_ct_alignof(SemaContext *context, Expr *expr)
{
	Expr *main_var = expr->ct_call_expr.main_var;
	Type *type = NULL;
	Decl *decl = NULL;
	ExprFlatElement *path = expr->ct_call_expr.flat_path;

	if (!sema_expr_analyse_type_var_path(context, main_var, &path, &type, &decl)) return false;

	AlignSize align = decl && !decl_is_user_defined_type(decl) ? decl->alignment : type_abi_alignment(type);
	VECEACH(path, i)
	{
		ExprFlatElement *element = &path[i];
		Decl *member;
		ArraySize index;
		Type *result_type;
		if (!sema_expr_analyse_flat_element(context, element, type, &member, &index, &result_type, i, i == 0 ? main_var->span : expr->span, NULL)) return false;
		if (member)
		{
			align = type_min_alignment(member->offset, align);
		}
		else
		{
			TypeSize size = type_size(result_type);
			align = type_min_alignment(size * index, align);
		}
		type = result_type;
	}

	expr_rewrite_to_int_const(expr, type_isize, align, true);

	return true;
}

static inline bool sema_expr_analyse_ct_sizeof(SemaContext *context, Expr *expr)
{
	Expr *main_var = expr->ct_call_expr.main_var;
	Type *type = NULL;
	Decl *decl = NULL;
	ExprFlatElement *path = expr->ct_call_expr.flat_path;
	if (!sema_expr_analyse_type_var_path(context, main_var, &path, &type, &decl)) return false;

	if (!type) return false;

	VECEACH(path, i)
	{
		ExprFlatElement *element = &path[i];
		Decl *member;
		ArraySize index;
		Type *result_type;
		if (!sema_expr_analyse_flat_element(context, element, type, &member, &index, &result_type, i, i == 0 ? main_var->span : expr->span, NULL)) return false;
		type = result_type;
	}

	expr_rewrite_to_int_const(expr, type_isize, type_size(type), true);
	return true;
}

static inline bool sema_expr_analyse_ct_nameof(SemaContext *context, Expr *expr)
{

	Expr *main_var = expr->ct_call_expr.main_var;
	Type *type = NULL;
	Decl *decl = NULL;
	ExprFlatElement *path = expr->ct_call_expr.flat_path;
	if (!sema_expr_analyse_type_var_path(context, main_var, &path, &type, &decl)) return false;

	TokenType name_type = expr->ct_call_expr.token_type;

	if (vec_size(path))
	{
		SEMA_ERROR(main_var, "You can only take the name of types and variables, not their sub elements.");
		return false;
	}

	if (decl)
	{
		if (name_type == TOKEN_CT_EXTNAMEOF)
		{
			if (!decl->extname)
			{
				SEMA_ERROR(main_var, "'%s' does not have an external name.", decl->name);
				return false;
			}
			expr_rewrite_to_string(expr, decl->extname);
			return true;
		}
		if (!decl->module || name_type == TOKEN_CT_NAMEOF || decl_is_local(decl))
		{
			expr_rewrite_to_string(expr, decl->name);
			return true;
		}
		scratch_buffer_clear();
		scratch_buffer_append(decl->module->name->module);
		scratch_buffer_append("::");
		scratch_buffer_append(decl->name);
		expr_rewrite_to_string(expr, scratch_buffer_copy());
		return true;
	}

	assert(type);
	if (name_type == TOKEN_CT_EXTNAMEOF)
	{
		if (!type_is_user_defined(type))
		{
			SEMA_ERROR(main_var, "Only user defined types have an external name.");
			return false;
		}
		expr_rewrite_to_string(expr, type->decl->extname);
		return true;
	}

	// TODO type_is_builtin is wrong also this does not cover virtual.
	if (name_type == TOKEN_CT_NAMEOF || type_is_builtin(type->type_kind))
	{
		expr_rewrite_to_string(expr, type->name);
		return true;
	}
	scratch_buffer_clear();
	scratch_buffer_append(type->decl->module->name->module);
	scratch_buffer_append("::");
	scratch_buffer_append(type->name);
	expr_rewrite_to_string(expr, scratch_buffer_copy());
	return true;
}


static Type *sema_expr_check_type_exists(SemaContext *context, TypeInfo *type_info)
{
	if (type_info->resolve_status == RESOLVE_DONE)
	{
		return type_info->type;
	}
RETRY:
	switch (type_info->kind)
	{
		case TYPE_INFO_POISON:
			return poisoned_type;
		case TYPE_INFO_VECTOR:
		{
			ArraySize size;
			if (!sema_resolve_array_like_len(context, type_info, &size)) return poisoned_type;
			Type *type = sema_expr_check_type_exists(context, type_info->array.base);
			if (!type) return NULL;
			if (!type_ok(type)) return type;
			if (!type_is_valid_for_vector(type))
			{
				SEMA_ERROR(type_info->array.base,
				           "%s cannot be vectorized. Only integers, floats and booleans are allowed.",
				           type_quoted_error_string(type));
				return poisoned_type;
			}
			return type_get_vector(type, size);
		}
		case TYPE_INFO_ARRAY:
		{
			ArraySize size;
			if (!sema_resolve_array_like_len(context, type_info, &size)) return poisoned_type;
			Type *type = sema_expr_check_type_exists(context, type_info->array.base);
			if (!type) return NULL;
			if (!type_ok(type)) return type;
			return type_get_array(type, size);
		}
		case TYPE_INFO_CT_IDENTIFIER:
		case TYPE_INFO_IDENTIFIER:
		{
			Decl *decl = sema_find_path_symbol(context, type_info->unresolved.name, type_info->unresolved.path);
			if (!decl) return NULL;
			if (!decl_ok(decl)) return poisoned_type;
			return decl->type->canonical;
		}
		case TYPE_INFO_EXPRESSION:
			if (!sema_resolve_type_info(context, type_info)) return poisoned_type;
			return type_info->type;
		case TYPE_INFO_EVALTYPE:
		{
			Expr *expr = type_info->unresolved_type_expr;
			TokenType type;
			Path *path = NULL;
			const char *ident = ct_eval_expr(context, "$eval", expr, &type, &path, false);
			if (!ident) return NULL;
			if (ident == ct_eval_error) return poisoned_type;
			switch (type)
			{
				case TOKEN_TYPE_IDENT:
					type_info->unresolved.name = ident;
					type_info->span = expr->span;
					type_info->unresolved.path = path;
					type_info->kind = TYPE_INFO_IDENTIFIER;
					goto RETRY;
				case TYPE_TOKENS:
					return type_info->type = type_from_token(type);
				default:
					SEMA_ERROR(expr, "Only type names may be resolved with $evaltype.");
					return poisoned_type;
			}
		}
		case TYPE_INFO_SUBARRAY:
		{
			// If it's an array, make sure we can resolve the length
			Type *type = sema_expr_check_type_exists(context, type_info->array.base);
			if (!type) return NULL;
			if (!type_ok(type)) return type;
			return type_get_subarray(type);
		}
		case TYPE_INFO_INFERRED_ARRAY:
		{
			// If it's an array, make sure we can resolve the length
			Type *type = sema_expr_check_type_exists(context, type_info->array.base);
			if (!type) return NULL;
			if (!type_ok(type)) return type;
			return type_get_inferred_array(type);
		}
		case TYPE_INFO_POINTER:
		{
			// If it's an array, make sure we can resolve the length
			Type *type = sema_expr_check_type_exists(context, type_info->array.base);
			if (!type) return NULL;
			if (!type_ok(type)) return type;
			return type_get_ptr(type);
		}
	}
	UNREACHABLE
}
static inline bool sema_expr_analyse_ct_defined(SemaContext *context, Expr *expr)
{
	if (expr->resolve_status == RESOLVE_DONE) return expr_ok(expr);

	Expr *main_var = expr->ct_call_expr.main_var;
	Type *type = NULL;
	Decl *decl = NULL;
	ExprFlatElement *flat_path = expr->ct_call_expr.flat_path;
RETRY:
	switch (main_var->expr_kind)
	{
		case EXPR_IDENTIFIER:
			// 2. An identifier does a lookup
			decl = sema_find_path_symbol(context, main_var->identifier_expr.ident, main_var->identifier_expr.path);
			// 2a. If it failed, then error
			if (!decl_ok(decl)) return false;
			// 2b. If it's missing, goto not defined
			if (!decl) goto NOT_DEFINED;
			type = decl->type;
			break;
		case EXPR_TYPEINFO:
		{
			type = sema_expr_check_type_exists(context, main_var->type_expr);
			if (!type) goto NOT_DEFINED;
			if (!type_ok(type)) return false;
			break;
		}
		case EXPR_BUILTIN:
			if (!sema_expr_analyse_builtin(context, main_var, false)) goto NOT_DEFINED;
			break;
		case EXPR_CT_EVAL:
		{
			Expr *inner = main_var->inner_expr;
			TokenType token_type;
			Path *path = NULL;
			const char *ident = ct_eval_expr(context, "$eval", inner, &token_type, &path, false);
			if (ident == ct_eval_error) return false;
			if (!ident) goto NOT_DEFINED;
			switch (token_type)
			{
				case TOKEN_IDENT:
				case TOKEN_CONST_IDENT:
					main_var->expr_kind = EXPR_IDENTIFIER;
					main_var->resolve_status = RESOLVE_NOT_DONE;
					main_var->identifier_expr.ident = ident;
					main_var->identifier_expr.path = path;
					main_var->identifier_expr.is_const = token_type == TOKEN_CONST_IDENT;
					goto RETRY;
				default:
					SEMA_ERROR(inner, "Only function, variable and constant names may be resolved with $eval.");
					return false;
			}
		}
		default:
			SEMA_ERROR(main_var, "Expected an identifier here.");
			break;
	}

	VECEACH(flat_path, i)
	{
		ExprFlatElement *element = &flat_path[i];
		Decl *member = NULL;
		ArraySize index;
		Type *ret_type;
		bool missing = false;
		if (!sema_expr_analyse_flat_element(context, element, type, &member, &index, &ret_type, i, i == 0 ? main_var->span : expr->span, &missing))
		{
			if (missing) goto NOT_DEFINED;
			return false;
		}
		type = ret_type;
	}

	expr->type = type_bool;
	expr->expr_kind = EXPR_CONST;
	expr_const_set_bool(&expr->const_expr, true);
	return true;

NOT_DEFINED:
	{
		expr->type = type_bool;
	}
	expr->expr_kind = EXPR_CONST;
	expr_const_set_bool(&expr->const_expr, false);
	return true;
}

static inline bool sema_expr_analyse_variant(SemaContext *context, Expr *expr)
{
	Expr *ptr = exprptr(expr->variant_expr.ptr);
	Expr *typeid = exprptr(expr->variant_expr.type_id);
	if (!sema_analyse_expr(context, ptr)) return false;
	if (!sema_analyse_expr(context, typeid)) return false;
	if (!type_is_pointer(ptr->type))
	{
		SEMA_ERROR(ptr, "This must be a pointer, but is %s.", type_quoted_error_string(ptr->type));
		return false;
	}
	if (typeid->type != type_typeid)
	{
		SEMA_ERROR(ptr, "This must of type 'typeid', but was %s.", type_quoted_error_string(ptr->type));
		return false;
	}
	expr->type = type_any;
	return true;
}

static inline bool sema_expr_analyse_ct_stringify(SemaContext *context, Expr *expr)
{
	Expr *inner = expr->inner_expr;
	// Only hash ident style stringify reaches here.
	assert(inner->expr_kind == EXPR_HASH_IDENT);
	Decl *decl = sema_resolve_symbol(context, inner->ct_ident_expr.identifier, NULL, inner->span);
	if (!decl_ok(decl)) return false;
	const char *desc = span_to_string(decl->var.hash_var.span);
	if (!desc)
	{
		SEMA_ERROR(expr, "Failed to stringify hash variable contents - they must be a single line and not exceed 255 characters.");
		return false;
	}
	expr_rewrite_to_string(expr, desc);
	return true;
}

static inline bool sema_expr_analyse_ct_eval(SemaContext *context, Expr *expr)
{
	Expr *inner = expr->inner_expr;
	TokenType type;
	Path *path = NULL;
	const char *ident = ct_eval_expr(context, "$eval", inner, &type, &path, true);
	if (ident == ct_eval_error) return false;
	switch (type)
	{
		case TOKEN_IDENT:
		case TOKEN_CONST_IDENT:
			expr->expr_kind = EXPR_IDENTIFIER;
			expr->resolve_status = RESOLVE_NOT_DONE;
			expr->identifier_expr.ident = ident;
			expr->identifier_expr.path = path;
			expr->identifier_expr.is_const = type == TOKEN_CONST_IDENT;
			return sema_analyse_expr(context, expr);
		default:
			SEMA_ERROR(inner, "Only function, variable and constant names may be resolved with $eval.");
			return false;
	}
}


static inline bool sema_expr_analyse_ct_offsetof(SemaContext *context, Expr *expr)
{
	Expr *main_var = expr->ct_call_expr.main_var;
	Type *type = NULL;
	Decl *decl = NULL;
	ExprFlatElement *path = expr->ct_call_expr.flat_path;
	if (!sema_expr_analyse_type_var_path(context, main_var, &path, &type, &decl)) return false;
	if (!vec_size(path))
	{
		SEMA_ERROR(expr, "Expected a path to get the offset of.");
		return false;
	}

	ByteSize offset = 0;
	VECEACH(path, i)
	{
		ExprFlatElement *element = &path[i];
		Decl *member;
		ArraySize index;
		Type *result_type;
		if (!sema_expr_analyse_flat_element(context, element, type, &member, &index, &result_type, i, i == 0 ? main_var->span : expr->span, NULL)) return false;
		if (member)
		{
			offset += member->offset;
		}
		else
		{
			offset += type_size(result_type) * index;
		}
		type = result_type;
	}

	expr_rewrite_to_int_const(expr, type_iptrdiff, offset, true);

	return true;
}

static inline bool sema_expr_analyse_ct_call(SemaContext *context, Expr *expr)
{
	switch (expr->ct_call_expr.token_type)
	{
		case TOKEN_CT_DEFINED:
			return sema_expr_analyse_ct_defined(context, expr);
		case TOKEN_CT_SIZEOF:
			return sema_expr_analyse_ct_sizeof(context, expr);
		case TOKEN_CT_ALIGNOF:
			return sema_expr_analyse_ct_alignof(context, expr);
		case TOKEN_CT_OFFSETOF:
			return sema_expr_analyse_ct_offsetof(context, expr);
		case TOKEN_CT_QNAMEOF:
		case TOKEN_CT_NAMEOF:
		case TOKEN_CT_EXTNAMEOF:
			return sema_expr_analyse_ct_nameof(context, expr);
		default:
			UNREACHABLE
	}
}

static inline bool sema_expr_analyse_ct_conv(SemaContext *c, Expr *expr)
{
	TypeInfo *from = type_infoptr(expr->ct_call_expr.type_from);
	TypeInfo *to = type_infoptr(expr->ct_call_expr.type_to);
	if (!sema_resolve_type_info(c, from)) return false;
	if (!sema_resolve_type_info(c, to)) return false;
	Type *from_type = from->type;
	Type *to_type = to->type;
	if (IS_FAILABLE(from))
	{
		SEMA_ERROR(from, "Only non-optional types can be checked.");
		return false;
	}
	if (IS_FAILABLE(to))
	{
		SEMA_ERROR(to, "Only non-optional types can be checked.");
		return false;
	}
	bool result;
	switch (expr->ct_call_expr.token_type)
	{
		case TOKEN_CT_CONVERTIBLE:
			result = cast_may_implicit(from_type, to_type, true, false);
			break;
		case TOKEN_CT_CASTABLE:
			result = cast_may_explicit(from_type, to_type, true, false);
			break;
		default:
			UNREACHABLE
	}
	expr_const_set_bool(&expr->const_expr, result);
	expr->type = type_bool;
	expr->expr_kind = EXPR_CONST;
	return true;
}


static inline BuiltinFunction builtin_by_name(const char *name)
{
	for (unsigned i = 0; i < NUMBER_OF_BUILTINS; i++)
	{
		if (builtin_list[i] == name) return (BuiltinFunction)i;
	}
	return BUILTIN_NONE;
}

static inline bool sema_expr_analyse_retval(SemaContext *c, Expr *expr)
{
	if (c->active_scope.flags & SCOPE_MACRO)
	{
		TODO
	}
	if (expr->type == type_void)
	{
		SEMA_ERROR(expr, "'return' cannot be used on void functions.");
		return false;
	}
	Expr *return_value = c->return_expr;
	assert(return_value);
	expr->type = c->rtype;
	if (expr_is_const(return_value))
	{
		expr_replace(expr, return_value);
	}
	return true;
}

static inline bool sema_expr_analyse_builtin(SemaContext *context, Expr *expr, bool throw_error)
{
	const char *builtin_char = expr->builtin_expr.ident;

	BuiltinFunction func = builtin_by_name(builtin_char);

	if (func == BUILTIN_NONE)
	{
		if (throw_error) SEMA_ERROR(expr, "Unsupported builtin '%s'.", builtin_char);
		return false;
	}

	expr->builtin_expr.builtin = func;
	return true;
}

static inline bool sema_analyse_expr_dispatch(SemaContext *context, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_COND:
		case EXPR_DESIGNATOR:
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_FLATPATH:
		case EXPR_NOP:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_TRY_UNWRAP:
		case EXPR_CATCH_UNWRAP:
		case EXPR_VARIANTSWITCH:
		case EXPR_TYPEID_INFO:
			UNREACHABLE
		case EXPR_VARIANT:
			return sema_expr_analyse_variant(context, expr);
		case EXPR_STRINGIFY:
			if (!sema_expr_analyse_ct_stringify(context, expr)) return false;
			return true;
		case EXPR_ARGV_TO_SUBARRAY:
			expr->type = type_get_subarray(type_get_subarray(type_char));
			return true;
		case EXPR_DECL:
			if (!sema_analyse_var_decl(context, expr->decl_expr, true)) return false;
			expr->type = expr->decl_expr->type;
			return true;
		case EXPR_RETVAL:
			return sema_expr_analyse_retval(context, expr);
		case EXPR_BUILTIN:
			return sema_expr_analyse_builtin(context, expr, true);
		case EXPR_CT_CONV:
			return sema_expr_analyse_ct_conv(context, expr);
		case EXPR_CT_CALL:
			return sema_expr_analyse_ct_call(context, expr);
		case EXPR_HASH_IDENT:
			return sema_expr_analyse_hash_identifier(context, expr);
		case EXPR_CT_IDENT:
			return sema_expr_analyse_ct_identifier(context, expr);
		case EXPR_FAILABLE:
			return sema_expr_analyse_failable(context, expr);
		case EXPR_COMPILER_CONST:
			return sema_expr_analyse_compiler_const(context, expr);
		case EXPR_POISONED:
			return false;
		case EXPR_SLICE_ASSIGN:
		case EXPR_BUILTIN_ACCESS:
			// Created during semantic analysis
			UNREACHABLE
		case EXPR_MACRO_BLOCK:
			UNREACHABLE
		case EXPR_TYPEINFO:
			expr->type = type_typeinfo;
			return sema_resolve_type_info(context, expr->type_expr);
		case EXPR_SLICE:
			return sema_expr_analyse_slice(context, expr);
		case EXPR_FORCE_UNWRAP:
			return sema_expr_analyse_force_unwrap(context, expr);
		case EXPR_TRY:
			return sema_expr_analyse_try(context, expr);
		case EXPR_CATCH:
			return sema_expr_analyse_catch(context, expr);
		case EXPR_COMPOUND_LITERAL:
			return sema_expr_analyse_compound_literal(context, expr);
		case EXPR_EXPR_BLOCK:
			return sema_expr_analyse_expr_block(context, NULL, expr);
		case EXPR_RETHROW:
			return sema_expr_analyse_rethrow(context, expr);
		case EXPR_CONST:
			return true;
		case EXPR_CT_EVAL:
			return sema_expr_analyse_ct_eval(context, expr);
		case EXPR_BITASSIGN:
			return sema_expr_analyse_bitassign(context, expr);
		case EXPR_BINARY:
			return sema_expr_analyse_binary(context, expr);
		case EXPR_TERNARY:
			return sema_expr_analyse_ternary(context, expr);
		case EXPR_UNARY:
		case EXPR_POST_UNARY:
			return sema_expr_analyse_unary(context, expr);
		case EXPR_TYPEID:
			return sema_expr_analyse_typeid(context, expr);
		case EXPR_IDENTIFIER:
			return sema_expr_analyse_identifier(context, NULL, expr);
		case EXPR_CALL:
			return sema_expr_analyse_call(context, expr);
		case EXPR_SUBSCRIPT:
			return sema_expr_analyse_subscript(context, expr, false);
		case EXPR_SUBSCRIPT_ADDR:
			return sema_expr_analyse_subscript(context, expr, true);
		case EXPR_GROUP:
			return sema_expr_analyse_group(context, expr);
		case EXPR_BITACCESS:
			UNREACHABLE
		case EXPR_ACCESS:
			return sema_expr_analyse_access(context, expr);
		case EXPR_INITIALIZER_LIST:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			return sema_expr_analyse_initializer_list(context, type_complist, expr);
		case EXPR_CAST:
			return sema_expr_analyse_cast(context, expr);
		case EXPR_EXPRESSION_LIST:
			return sema_expr_analyse_expr_list(context, expr);
	}
	UNREACHABLE
}


bool sema_analyse_cond_expr(SemaContext *context, Expr *expr)
{
	if (expr->expr_kind == EXPR_BINARY && expr->binary_expr.operator == BINARYOP_ASSIGN)
	{
		SEMA_ERROR(expr, "Assignment expressions must be enclosed in an extra () in conditionals.");
		return false;
	}
	if (!sema_analyse_expr(context, expr)) return false;
	if (IS_FAILABLE(expr))
	{
		SEMA_ERROR(expr, "A failable %s cannot be implicitly converted to a regular boolean value, use 'try(<expr>)' "
		                 "and 'catch(<expr>)' to conditionally execute on success or failure.",
		           type_quoted_error_string(expr->type));
		return false;
	}
	return cast_implicit(expr, type_bool);
}


bool sema_analyse_expr_rhs(SemaContext *context, Type *to, Expr *expr, bool allow_failable)
{
	if (to && type_is_failable_type(to))
	{
		to = to->failable;
		assert(allow_failable);
	}
	if (!sema_analyse_inferred_expr(context, to, expr)) return false;
	if (to && !cast_implicit(expr, to)) return false;
	if (!allow_failable && IS_FAILABLE(expr))
	{
		SEMA_ERROR(expr, "You cannot have a failable here.");
		return false;
	}
	return true;
}


static inline bool sema_cast_ct_ident_rvalue(SemaContext *context, Expr *expr)
{
	Decl *decl = expr->ct_ident_expr.decl;
	Expr *copy = expr_macro_copy(decl->var.init_expr);
	if (!sema_analyse_expr(context, copy)) return false;
	expr_replace(expr, copy);
	return true;
}

static inline bool sema_cast_rvalue(SemaContext *context, Expr *expr)
{
	if (!expr_ok(expr)) return false;
	switch (expr->expr_kind)
	{
		case EXPR_MACRO_BODY_EXPANSION:
			if (!expr->body_expansion_expr.ast)
			{
				SEMA_ERROR(expr, "'@%s' must be followed by ().", declptr(context->current_macro->macro_decl.body_param)->name);
				return false;
			}
			break;
		case EXPR_BUILTIN:
			SEMA_ERROR(expr, "A builtin must be followed by ().");
			return false;
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
		case EXPR_TYPEINFO:
			SEMA_ERROR(expr, "A type must be followed by either (...) or '.'.");
			return false;
		case EXPR_CT_IDENT:
			if (!sema_cast_ct_ident_rvalue(context, expr)) return false;
			break;
		case EXPR_IDENTIFIER:
			if (!sema_cast_ident_rvalue(context, expr)) return false;
			break;
		case EXPR_UNARY:
		{
			if (expr->unary_expr.operator != UNARYOP_DEREF) break;
			Expr *inner = expr->inner_expr;
			if (inner->expr_kind != EXPR_IDENTIFIER) break;
			Decl *decl = inner->identifier_expr.decl;
			if (decl->decl_kind != DECL_VAR) break;
			if (!decl->var.may_not_read) break;
			SEMA_ERROR(expr, "'out' parameters may not be read.");
			return false;
		}
		default:
			break;
	}
	return true;
}

bool sema_analyse_ct_expr(SemaContext *context, Expr *expr)
{
	if (!sema_analyse_expr_lvalue(context, expr)) return false;
	if (expr->expr_kind == EXPR_TYPEINFO)
	{
		Type *cond_val = expr->type_expr->type;
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.const_kind = CONST_TYPEID;
		expr->const_expr.typeid = cond_val->canonical;
		expr->type = type_typeid;
	}
	return sema_cast_rvalue(context, expr);
}

bool sema_analyse_expr_lvalue(SemaContext *context, Expr *expr)
{
	switch (expr->resolve_status)
	{
		case RESOLVE_NOT_DONE:
			expr->resolve_status = RESOLVE_RUNNING;
			if (!sema_analyse_expr_dispatch(context, expr)) return expr_poison(expr);
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

MemberIndex sema_get_initializer_const_array_size(SemaContext *context, Expr *initializer, bool *may_be_array, bool *is_const_size)
{
	if (initializer->expr_kind == EXPR_CONST)
	{
		assert(initializer->const_expr.const_kind == CONST_LIST);
		ConstInitializer *init = initializer->const_expr.list;
		Type *type = type_flatten(initializer->type);
		*is_const_size = true;
		switch (init->kind)
		{
			case CONST_INIT_ZERO:
				if (type->type_kind == TYPE_ARRAY)
				{
					*may_be_array = true;
					return (MemberIndex)type->array.len;
				}
				if (type->type_kind == TYPE_SUBARRAY)
				{
					*may_be_array = true;
					return 0;
				}
				*may_be_array = false;
				return 0;
			case CONST_INIT_ARRAY:
				*may_be_array = true;
				return VECLAST(init->init_array.elements)->init_array_value.index + 1;
			case CONST_INIT_ARRAY_FULL:
				*may_be_array = true;
				return (MemberIndex)vec_size(init->init_array_full);
			case CONST_INIT_ARRAY_VALUE:
				UNREACHABLE;
			case CONST_INIT_STRUCT:
			case CONST_INIT_UNION:
			case CONST_INIT_VALUE:
				*may_be_array = false;
				return 0;
		}
		UNREACHABLE
	}
	switch (initializer->expr_kind)
	{
		case EXPR_INITIALIZER_LIST:
			*may_be_array = true;
			*is_const_size = true;
			return (MemberIndex)vec_size(initializer->initializer_list);
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			break;
		default:
			UNREACHABLE
	}
	Expr **initializers = initializer->designated_init_list;
	MemberIndex size = 0;
	// Otherwise we assume everything's a designator.
	VECEACH(initializers, i)
	{
		Expr *sub_initializer = initializers[i];
		assert(sub_initializer->expr_kind == EXPR_DESIGNATOR);

		DesignatorElement *element = sub_initializer->designator_expr.path[0];
		switch (element->kind)
		{
			case DESIGNATOR_FIELD:
				// Struct, abandon!
				*may_be_array = false;
				return -1;
			case DESIGNATOR_ARRAY:
			{
				MemberIndex index = sema_analyse_designator_index(context, element->index_expr);
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
				MemberIndex index = sema_analyse_designator_index(context, element->index_end_expr);
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

bool sema_analyse_expr(SemaContext *context, Expr *expr)
{
	return sema_analyse_expr_lvalue(context, expr) && sema_cast_rvalue(context, expr);
}

void insert_widening_type(Expr *expr, Type *infer_type)
{
	if (!infer_type) return;
	switch (expr->expr_kind)
	{
		case EXPR_BINARY:
			switch (expr->binary_expr.operator)
			{
				case BINARYOP_MULT:
				case BINARYOP_SUB:
				case BINARYOP_ADD:
				case BINARYOP_DIV:
				case BINARYOP_MOD:
				case BINARYOP_SHR:
				case BINARYOP_SHL:
				case BINARYOP_BIT_OR:
				case BINARYOP_BIT_XOR:
				case BINARYOP_BIT_AND:
				case BINARYOP_OR_ERR:
					if (!expr_is_simple(exprptr(expr->binary_expr.left)) || !expr_is_simple(exprptr(expr->binary_expr.right))) return;
					expr->type = infer_type;
					expr->binary_expr.widen = true;
					return;
				default:
					return;
			}
		case EXPR_GROUP:
			insert_widening_type(expr->inner_expr, infer_type);
			return;
		case EXPR_TERNARY:
			if (!exprid_is_simple(expr->ternary_expr.else_expr)) return;
			if (expr->ternary_expr.then_expr && !exprid_is_simple(expr->ternary_expr.else_expr)) return;
			expr->type = infer_type;
			expr->ternary_expr.widen = true;
			return;
		case EXPR_UNARY:
			switch (expr->unary_expr.operator)
			{
				case UNARYOP_NEG:
				case UNARYOP_BITNEG:
					if (!expr_is_simple(expr->unary_expr.expr)) return;
					expr->type = infer_type;
					expr->unary_expr.widen = true;
					return;
				default:
					return;
			}
		default:
			return;
	}
	UNREACHABLE
}
bool sema_analyse_inferred_expr(SemaContext *context, Type *infer_type, Expr *expr)
{
	infer_type = type_no_fail(infer_type);
	switch (expr->resolve_status)
	{
		case RESOLVE_NOT_DONE:
			break;
		case RESOLVE_RUNNING:
			SEMA_ERROR(expr, "Recursive resolution of list.");
			return expr_poison(expr);
		case RESOLVE_DONE:
			return expr_ok(expr);
		default:
			UNREACHABLE
	}

	expr->resolve_status = RESOLVE_RUNNING;

	switch (expr->expr_kind)
	{
		case EXPR_DESIGNATED_INITIALIZER_LIST:
		case EXPR_INITIALIZER_LIST:
			if (!sema_expr_analyse_initializer_list(context, infer_type, expr)) return expr_poison(expr);
			break;
		case EXPR_EXPR_BLOCK:
			if (!sema_expr_analyse_expr_block(context, infer_type, expr)) return expr_poison(expr);
			break;
		case EXPR_IDENTIFIER:
			if (!sema_expr_analyse_identifier(context, infer_type, expr)) return expr_poison(expr);
			break;
		default:
			insert_widening_type(expr, infer_type);
			if (!sema_analyse_expr_dispatch(context, expr)) return expr_poison(expr);
			break;
	}
	if (!sema_cast_rvalue(context, expr)) return false;
	expr->resolve_status = RESOLVE_DONE;
	return true;
}

bool splitpathref(const char *string, ArraySize len, Path **path_ref, const char **ident_ref, TokenType *type_ref)
{
	ArraySize path_end = 0;
	*path_ref = NULL;
	for (ArraySize i = 0; i < len; i++)
	{
		char ch = string[i];
		if (!char_is_alphanum_(ch))
		{
			if (ch == ':' && i > 0 && string[i + 1] == ':')
			{
				path_end = i;
				i++;
			}
			else
			{
				return false;
			}
		}
	}
	if (path_end > 0)
	{
		*path_ref = path_create_from_string(string, path_end, INVALID_SPAN);
		string += path_end + 2;
		len -= path_end + 2;
	}
	while (len > 0)
	{
		char c = string[0];
		if (c != ' ' && c != '\t') break;
		len--;
		string++;
	}
	if (len == 0) return false;
	uint32_t hash = FNV1_SEED;
	for (size_t i = 0; i < len; i++)
	{
		char c = string[i];
		if (!char_is_alphanum_(c)) return false;
		hash = FNV1a(c, hash);
	}
	*ident_ref = symtab_find(string, len, hash, type_ref);
	if (!*ident_ref)
	{
		scratch_buffer_clear();
		scratch_buffer_append_len(string, len);
		*ident_ref = scratch_buffer_to_string();
		*type_ref = TOKEN_INVALID_TOKEN;
	}
	return true;
}