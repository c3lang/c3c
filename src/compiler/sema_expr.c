// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"
#include <math.h>
/*
 * TODOs
 * - Disallow jumping in and out of an expression block.
 */

static bool sema_take_addr_of(Expr *inner);
static inline bool sema_expr_analyse_binary(Context *context, Expr *expr);
static inline bool sema_cast_rvalue(Context *context, Expr *expr);
static Expr *expr_access_inline_member(Expr *parent, Decl *parent_decl);
static inline void expr_set_as_const_list(Expr *expr, ConstInitializer *list);
static inline bool is_const(Expr *expr);
static inline bool sema_expr_analyse_builtin(Context *context, Expr *expr, bool throw_error);
static bool sema_check_stmt_compile_time(Context *context, Ast *ast);
static bool binary_arithmetic_promotion(Context *context, Expr *left, Expr *right, Type *left_type, Type *right_type, Expr *parent, const char *error_message);
static inline void expr_set_as_const_list(Expr *expr, ConstInitializer *list)
{
	expr->expr_kind = EXPR_CONST;
	expr->const_expr.const_kind = CONST_LIST;
	expr->const_expr.list = list;
}

static bool sema_decay_array_pointers(Expr *expr)
{
	Type *expr_type = expr->type->canonical;
	if (expr_type->type_kind != TYPE_POINTER) return true;
	switch (expr_type->pointer->type_kind)
	{
		case TYPE_ARRAY:
			return cast_implicit(expr, type_get_ptr(expr_type->pointer->array.base));
		case TYPE_VECTOR:
			return cast_implicit(expr, type_get_ptr(expr_type->pointer->vector.base));
		default:
			return true;
	}
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


static Expr *expr_access_inline_member(Expr *parent, Decl *parent_decl)
{
	Expr *embedded_struct = expr_new(EXPR_ACCESS, parent->span);
	embedded_struct->resolve_status = RESOLVE_DONE;
	embedded_struct->access_expr.parent = parent;
	embedded_struct->access_expr.ref = parent_decl->strukt.members[0];
	embedded_struct->type = embedded_struct->access_expr.ref->type;
	return embedded_struct;
}

#define IS_CONST(_x) ((_x)->expr_kind == EXPR_CONST)

static inline bool is_const(Expr *expr)
{
	return expr->expr_kind == EXPR_CONST;
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

	return type_is_integer(flatten_left->vector.base) && type_is_integer(flatten_right->vector.base);
}

Expr *expr_generate_decl(Decl *decl, Expr *assign)
{
	assert(decl->decl_kind == DECL_VAR);
	assert(decl->var.init_expr == NULL);
	Expr *expr_decl = expr_new(EXPR_DECL, decl->span);
	expr_decl->decl_expr = decl;
	if (!assign) assign = expr_new(EXPR_UNDEF, decl->span);
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
	Expr *inner = expr_alloc();
	*inner = *original;
	original->expr_kind = EXPR_UNARY;
	original->type = type_get_ptr(inner->type);
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
	expr->identifier_expr.identifier = decl->name_token;
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
		case EXPR_CONST_IDENTIFIER:
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
			return expr_is_constant_eval(expr->cast_expr.expr, eval_kind);
		case CAST_EUINT:
		case CAST_ERINT:
		case CAST_PTRXI:
			if (eval_kind != CONSTANT_EVAL_ANY) return false;
			return expr_is_constant_eval(expr->cast_expr.expr, eval_kind);
	}
	UNREACHABLE
}
static bool expr_binary_is_constant_eval(Expr *expr, ConstantEvalKind eval_kind)
{
	if (expr->binary_expr.operator >= BINARYOP_ASSIGN) return false;
	if (eval_kind == CONSTANT_EVAL_FOLDABLE) return false;
	Expr *left = expr->binary_expr.left;
	Expr *right = expr->binary_expr.right;
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

static bool sema_bit_assignment_check(Expr *right, Decl *member)
{
	if (right->expr_kind != EXPR_CONST || !type_is_integer(right->type)) return true;

	unsigned bits = member->var.end_bit - member->var.start_bit + 1;

	if (bits >= type_size(right->type) * 8 || int_is_zero(right->const_expr.ixx)) return true;

	// Check that we're not assigning consts that will be cut.

	TypeKind kind = right->const_expr.ixx.type;
	Int128 i = right->const_expr.ixx.i;
	int bits_used;
	if (type_kind_is_signed(kind))
	{
		if (i128_is_neg(i))
		{
			i = i128_neg(i);
			i = i128_sub64(i, 1);
		}
		bits_used = (int) (1 + 128 - i128_clz(&i));
	}
	else
	{
		bits_used = (int) (128 - i128_clz(&i));
	}
	if (bits_used <= bits) return true;

	SEMA_ERROR(right,
			   "This constant would be truncated if stored in the bitstruct, do you need a wider bit range?");
	return false;
}
bool expr_is_constant_eval(Expr *expr, ConstantEvalKind eval_kind)
{
	RETRY:
	switch (expr->expr_kind)
	{
		case EXPR_BUILTIN:
			return false;
		case EXPR_BITACCESS:
		case EXPR_ACCESS:
			expr = expr->access_expr.parent;
			goto RETRY;
		case EXPR_VARIANTSWITCH:
			return false;
		case EXPR_BITASSIGN:
			return false;
		case EXPR_BINARY:
			return expr_binary_is_constant_eval(expr, eval_kind);
		case EXPR_CAST:
			return expr_cast_is_constant_eval(expr, eval_kind);
		case EXPR_CONST:
			return true;
		case EXPR_CONST_IDENTIFIER:
			expr = expr->identifier_expr.decl->var.init_expr;
			goto RETRY;
		case EXPR_COND:
			return expr_list_is_constant_eval(expr->cond_expr, eval_kind);
		case EXPR_DESIGNATOR:
			expr = expr->designator_expr.value;
			goto RETRY;
		case EXPR_OR_ERROR:
			if (expr->or_error_expr.is_jump) return false;
			assert(!expr_is_constant_eval(expr->or_error_expr.expr, eval_kind));
			return false;
		case EXPR_EXPR_BLOCK:
		case EXPR_DECL:
		case EXPR_CALL:
		case EXPR_CATCH_UNWRAP:
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_TRY_UNWRAP:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_POST_UNARY:
		case EXPR_SCOPED_EXPR:
		case EXPR_SLICE_ASSIGN:
		case EXPR_MACRO_BLOCK:
		case EXPR_RETHROW:
		case EXPR_UNDEF:
			return false;
		case EXPR_IDENTIFIER:
			if (expr->identifier_expr.decl->decl_kind != DECL_VAR) return true;
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
		case EXPR_FAILABLE:
		case EXPR_GROUP:
			expr = expr->inner_expr;
			goto RETRY;
		case EXPR_INITIALIZER_LIST:
			return expr_list_is_constant_eval(expr->initializer_list, eval_kind);
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			return expr_list_is_constant_eval(expr->designated_init_list, eval_kind);
		case EXPR_LEN:
			expr = expr->len_expr.inner;
			goto RETRY;
		case EXPR_TYPEOFANY:
			if (eval_kind != CONSTANT_EVAL_ANY) return false;
			expr = expr->inner_expr;
			goto RETRY;
		case EXPR_SLICE:
			if (expr->slice_expr.start && !expr_is_constant_eval(expr->slice_expr.start, CONSTANT_EVAL_FOLDABLE)) return false;
			if (expr->slice_expr.end && !expr_is_constant_eval(expr->slice_expr.end, CONSTANT_EVAL_FOLDABLE)) return false;
			return expr_is_constant_eval(expr->slice_expr.expr, eval_kind);
		case EXPR_SUBSCRIPT:
			if (!expr_is_constant_eval(expr->subscript_expr.index, eval_kind)) return false;
			expr = expr->subscript_expr.expr;
			goto RETRY;
		case EXPR_SUBSCRIPT_ADDR:
			if (!expr_is_constant_eval(expr->subscript_expr.index, eval_kind)) return false;
			expr = expr->subscript_expr.expr;
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
			assert(!expr_is_constant_eval(expr->ternary_expr.cond, eval_kind));
			return false;
		case EXPR_FORCE_UNWRAP:
		case EXPR_TRY:
		case EXPR_CATCH:
		case EXPR_PTR:
			expr = expr->inner_expr;
			goto RETRY;
		case EXPR_TYPEID:
			return eval_kind == CONSTANT_EVAL_ANY;
		case EXPR_UNARY:
			switch (expr->unary_expr.operator)
			{
				case UNARYOP_ERROR:
				case UNARYOP_DEREF:
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
		case EXPR_MACRO_EXPANSION:
		case EXPR_PLACEHOLDER:
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


static void expr_unify_binary(Expr *expr, Expr *left, Expr *right)
{
	expr->type = type_get_opt_fail(left->type, IS_FAILABLE(right));
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

static inline bool sema_cast_ident_rvalue(Context *context, Expr *expr)
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
		case DECL_ERRVALUE:
			SEMA_ERROR(expr, "Did you forget a '!' after '%s'?", decl->name);
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
		case DECL_ERRTYPE:
			SEMA_ERROR(expr, "Expected errtype name followed by '.' and an error value.");
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
			expr_replace(expr, copy_expr(decl->var.init_expr));
			return sema_analyse_expr(context, expr);
		case VARDECL_PARAM_EXPR:
			expr_replace(expr, copy_expr(decl->var.init_expr));
			assert(decl->var.init_expr->resolve_status == RESOLVE_DONE);
			return true;
		case VARDECL_PARAM_CT_TYPE:
		case VARDECL_PARAM_CT:
		case VARDECL_LOCAL_CT:
		case VARDECL_LOCAL_CT_TYPE:
		case VARDECL_ERASE:
		case VARDECL_REWRAPPED:
			// Impossible to reach this, they are already unfolded
			UNREACHABLE
		case VARDECL_PARAM_REF:
			expr_replace(expr, copy_expr(decl->var.init_expr));
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
	               c, type_quoted_error_string(expr->binary_expr.left->type),
	               c, type_quoted_error_string(expr->binary_expr.right->type));
	return false;
}

static bool expr_cast_to_index(Expr *index)
{
	switch (index->type->canonical->type_kind)
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
			SEMA_ERROR(index, "You need to explicitly cast this to a int or long.");
			return false;
		default:
			SEMA_ERROR(index, "Cannot implicitly convert '%s' to an index.", type_to_error_string(index->type));
			return false;
	}
}

static inline bool sema_expr_analyse_ternary(Context *context, Expr *expr)
{
	Expr *left = expr->ternary_expr.then_expr;
	Expr *cond = expr->ternary_expr.cond;
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
			Expr *copy = copy_expr(cond);
			cast(copy, type_bool);
			assert(cond->expr_kind == EXPR_CONST);
			path = cond->const_expr.b ? 1 : 0;
		}
		left = cond;
	}

	Expr *right = expr->ternary_expr.else_expr;
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
	if (to->canonical->type_kind != TYPE_ENUM && to->canonical->type_kind != TYPE_ERRTYPE) return false;
	Decl *parent_decl = to->canonical->decl;
	switch (parent_decl->decl_kind)
	{
		case DECL_ENUM:
		case DECL_ERRTYPE:
			return sema_expr_analyse_enum_constant(expr, expr->identifier_expr.identifier, parent_decl);
		case DECL_UNION:
		case DECL_STRUCT:
		case DECL_BITSTRUCT:
			return false;
		default:
			UNREACHABLE
	}

}



static inline bool sema_expr_analyse_identifier(Context *context, Type *to, Expr *expr)
{
	Decl *ambiguous_decl = NULL;
	Decl *private_symbol = NULL;

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
	if (decl->decl_kind == DECL_VAR)
	{
		switch (decl->var.kind)
		{
			case VARDECL_CONST:
				if (!decl->type)
				{
					Expr *copy = copy_expr(decl->var.init_expr);
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
			expr->type = type_void;
			return true;
		}
	}
	if (!sema_analyse_expr_lvalue(context, inner)) return false;
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
	return true;
}

static inline bool sema_expr_analyse_ct_identifier(Context *context, Expr *expr)
{
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
	expr->type = decl->type;
	return true;
}

static inline bool sema_expr_analyse_hash_identifier(Context *context, Expr *expr)
{
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
	return sema_analyse_expr(context, expr);
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

static inline bool sema_promote_binary_top_down(Context *context, Expr *binary, Expr *left, Expr *right)
{
	if (!binary->binary_expr.widen) return true;
	Type *to = binary->type;
	return sema_widen_top_down(left, to) && sema_widen_top_down(right, to);
}

static inline bool sema_expr_analyse_binary_subexpr(Context *context, Expr *binary, Expr *left, Expr *right)
{
	return (int)sema_analyse_expr(context, left) & (int)sema_analyse_expr(context, right);
}

static inline bool sema_expr_analyse_binary_arithmetic_subexpr(Context *context, Expr *expr, const char *error)
{
	Expr *left = expr->binary_expr.left;
	Expr *right = expr->binary_expr.right;

	// 1. Analyse both sides.
	if (!sema_expr_analyse_binary_subexpr(context, expr, left, right)) return false;

	if (!sema_promote_binary_top_down(context, expr, left, right)) return false;

	Type *left_type = type_no_fail(left->type)->canonical;
	Type *right_type = type_no_fail(right->type)->canonical;

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

static inline bool sema_expr_analyse_intrinsic_fp_invocation(Context *context, Expr *expr, Decl *decl, bool *failable)
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
	TokenId block_parameter;
	Decl **params;
	Expr *struct_var;
	Variadic variadic;
} CalledDecl;

static inline bool expr_promote_vararg(Context *context, Expr *arg)
{
	Type *arg_type = arg->type->canonical;

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


static inline bool sema_expand_call_arguments(Context *context, CalledDecl *callee, Expr *call, Decl **params, Expr **args, unsigned func_param_count, bool variadic, bool *failable)
{
	unsigned num_args = vec_size(args);

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
			if (!variadic)
			{
				// 15. We have too many parameters...
				SEMA_ERROR(arg, "This argument would would exceed the number of parameters, did you add too many arguments?");
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
		Expr *init_expr = params[i]->var.init_expr;
		if (init_expr)
		{
			if (callee->macro)
			{
				actual_args[i] = copy_expr(init_expr);
			}
			else
			{
				assert(init_expr->resolve_status == RESOLVE_DONE);
				actual_args[i] = init_expr;
			}
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
static inline bool sema_expr_analyse_call_invocation(Context *context, Expr *call, CalledDecl callee, bool *failable)
{
	// 1. Check body arguments.
	if (!sema_check_invalid_body_arguments(context, call, &callee)) return false;

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

	if (!sema_expand_call_arguments(context, &callee, call, params, args, func_param_count, callee.variadic != VARIADIC_NONE, failable)) return false;

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

		Decl *param = params[i];

		// 16. Analyse a regular argument.
		switch (param->var.kind)
		{
			case VARDECL_PARAM_REF:
				// &foo
			{
				if (!sema_analyse_expr_lvalue(context, arg)) return false;
			}
				if (param->type && param->type->canonical != arg->type->canonical)
				{
					SEMA_ERROR(arg, "'%s' cannot be implicitly cast to '%s'.", type_to_error_string(arg->type), type_to_error_string(param->type));
					return false;
				}
				break;
			case VARDECL_PARAM:
				// foo
				if (!sema_analyse_expr_rhs(context, param->type, arg, true)) return false;
				if (IS_FAILABLE(arg)) *failable = true;
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
					if (!sema_analyse_expr_rhs(context, param->type, arg, true)) return SCOPE_POP_ERROR();
				SCOPE_END;
				if (IS_FAILABLE(arg)) *failable = true;
				break;
			case VARDECL_PARAM_CT:
				// $foo
				assert(callee.macro);
				if (!sema_analyse_expr_rhs(context, param->type, arg, true)) return false;
				if (!expr_is_constant_eval(arg, CONSTANT_EVAL_ANY))
				{
					SEMA_ERROR(arg, "A compile time parameter must always be a constant, did you mistake it for a normal paramter?");
					return false;
				}
				break;
			case VARDECL_PARAM_CT_TYPE:
				// $Foo
			{
				if (!sema_analyse_expr_lvalue(context, arg)) return false;
			}
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
		if (!param->type) param->type = type_no_fail(arg->type);
	}
	return true;
}
static inline bool sema_expr_analyse_func_invocation(Context *context, FunctionSignature *signature, Expr *expr, Decl *decl,
                                  Expr *struct_var, bool failable)
{
	CalledDecl callee = {
			.macro = false,
			.block_parameter = NO_TOKEN_ID,
			.struct_var = struct_var,
			.params = signature->params,
			.variadic = signature->variadic,
	};
	if (!sema_expr_analyse_call_invocation(context, expr, callee, &failable)) return false;

	Type *rtype = signature->rtype->type;

	expr->type = type_get_opt_fail(rtype, failable);

	return true;
}

static inline bool sema_expr_analyse_var_call(Context *context, Expr *expr, Type *func_ptr_type, bool failable)
{
	if (func_ptr_type->type_kind != TYPE_POINTER || func_ptr_type->pointer->type_kind != TYPE_FUNC)
	{
		SEMA_ERROR(expr, "Only macros, functions and function pointers maybe invoked, this is of type '%s'.", type_to_error_string(func_ptr_type));
		return false;
	}
	expr->call_expr.is_pointer_call = true;
	return sema_expr_analyse_func_invocation(context,
	                                         func_ptr_type->pointer->func.signature,
	                                         expr,
	                                         NULL, NULL, failable);

}

// Unify returns in a macro or expression block.
static inline Type *unify_returns(Context *context)
{
	bool all_returns_need_casts = false;
	Type *common_type = NULL;

	// 1. Loop through the returns.
	VECEACH(context->returns, i)
	{
		Ast *return_stmt = context->returns[i];
		Expr *ret_expr = return_stmt->return_stmt.expr;
		Type *rtype = ret_expr ? ret_expr->type : type_void;

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
			return false;
		}

		// 6. Set the new max, mark as needing a cast on all returns.
		common_type = max;
		all_returns_need_casts = true;
	}

	// 7. Insert casts.
	if (all_returns_need_casts)
	{
		VECEACH(context->returns, i)
		{
			Ast *return_stmt = context->returns[i];
			Expr *ret_expr = return_stmt->return_stmt.expr;
			// 8. All casts should work.
			if (!cast_implicit(ret_expr, type_no_fail(common_type)))
			{
				assert(false);
				return NULL;
			}
		}
	}

	// 8. On no common type -> return void
	return common_type ? common_type : type_void;
}

static inline bool sema_expr_analyse_func_call(Context *context, Expr *expr, Decl *decl, Expr *struct_var, bool failable)
{
	expr->call_expr.is_pointer_call = false;
	return sema_expr_analyse_func_invocation(context, &decl->func_decl.function_signature, expr, decl, struct_var, failable);
}


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
			return expr_is_constant_eval(ast->return_stmt.expr, CONSTANT_EVAL_ANY);
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

bool sema_expr_analyse_macro_call(Context *context, Expr *call_expr, Expr *struct_var, Decl *decl, bool failable)
{
	assert(decl->decl_kind == DECL_MACRO);

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

	if (!sema_expr_analyse_call_invocation(context, call_expr, callee, &failable)) return false;
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
				.inline_line = TOKLOC(call_expr->span.loc)->row,
				.original_inline_line = old_macro_scope.depth ? old_macro_scope.original_inline_line : TOKLOC(call_expr->span.loc)->row,
				.locals_start = context->active_scope.current_local,
				.depth = old_macro_scope.depth + 1,
				.yield_symbol_start = first_local,
				.yield_body = call_expr->call_expr.body,
				.yield_symbol_end = context->active_scope.current_local,
				.yield_args = call_expr->call_expr.body_arguments,
		};


		Ast *body = copy_ast(decl->macro_decl.body);
		Type *rtype = decl->macro_decl.rtype ? decl->macro_decl.rtype->type : NULL;

		Ast **saved_returns = context_push_returns(context);
		Type *previous_block_type = context->expected_block_type;
		context->expected_block_type = rtype;
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
				if (rtype && type_no_fail(rtype) != type_void)
				{
					SEMA_ERROR(decl, "Missing return in macro that should evaluate to %s.", type_quoted_error_string(rtype));
					ok = false;
					goto EXIT;
				}
			}

			if (rtype)
			{
				VECEACH(context->returns, i)
				{
					Ast *return_stmt = context->returns[i];
					Expr *ret_expr = return_stmt->return_stmt.expr;
					if (!ret_expr)
					{
						if (rtype == type_void) continue;
						SEMA_ERROR(return_stmt, "Expected returning a value of type %s.", type_quoted_error_string(rtype));
						ok = false;
						goto EXIT;
					}
					Type *type = ret_expr->type;
					if (!cast_may_implicit(type, rtype, true, true))
					{
						SEMA_ERROR(ret_expr, "Expected %s, not %s.", type_quoted_error_string(rtype),
						           type_quoted_error_string(type));
						ok = false;
						goto EXIT;
					}
					bool success = cast_implicit(ret_expr, rtype);
					assert(success);
				}
				call_expr->type = type_get_opt_fail(rtype, failable);
			}
			else
			{
				Type *sum_returns = unify_returns(context);
				if (!sum_returns)
				{
					ok = false;
					goto EXIT;
				}
				call_expr->type = type_get_opt_fail(sum_returns, failable);
			}
			if (vec_size(context->returns) == 1)
			{
				Expr *result = context->returns[0]->return_stmt.expr;
				if (result && expr_is_constant_eval(result, CONSTANT_EVAL_ANY))
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
		context->expected_block_type = previous_block_type;
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

static inline bool sema_expr_analyse_generic_call(Context *context, Expr *call_expr, Expr *struct_var, Decl *decl, bool failable)
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
	return sema_expr_analyse_func_call(context, call_expr, found, struct_var, failable);
}

static bool sema_analyse_body_expansion(Context *context, Expr *call)
{
	Decl *macro = context->macro_scope.macro;
	assert(macro);
	assert(macro->macro_decl.block_parameter.index);

	// TODO handle named arguments
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
		if (!sema_analyse_expr(context, expr)) return false;
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

bool sema_expr_analyse_general_call(Context *context, Expr *expr, Decl *decl, Expr *struct_var, bool is_macro, bool failable)
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
	if (decl == NULL)
	{
		return sema_expr_analyse_var_call(context, expr, type_flatten_distinct_failable(expr->call_expr.function->type), failable);
	}
	switch (decl->decl_kind)
	{
		case DECL_MACRO:
			if (!is_macro)
			{
				SEMA_ERROR(expr, "A macro neeeds to be called with a '@' prefix, please add it.");
				return false;
			}
			expr->call_expr.func_ref = decl;
			return sema_expr_analyse_macro_call(context, expr, struct_var, decl, failable);
		case DECL_VAR:
			if (is_macro)
			{
				SEMA_ERROR(expr, "A function cannot be called with a '@' prefix, please remove it.");
				return false;
			}
			assert(struct_var == NULL);
			return sema_expr_analyse_var_call(context, expr, decl->type->canonical, failable || IS_FAILABLE(decl));
		case DECL_FUNC:
			if (is_macro)
			{
				SEMA_ERROR(expr, "A function cannot be called with a '@' prefix, please remove it.");
				return false;
			}
			expr->call_expr.func_ref = decl;
			expr->call_expr.force_inline = force_inline == 1;
			expr->call_expr.force_noinline = force_inline == 0;
			return sema_expr_analyse_func_call(context, expr, decl, struct_var, failable);
		case DECL_GENERIC:
			if (is_macro)
			{
				SEMA_ERROR(expr, "A generic function cannot be called with a '@' prefix, please remove it.");
				return false;
			}
			expr->call_expr.func_ref = decl;
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
		case BUILTIN_NONE:
			UNREACHABLE
	}
	UNREACHABLE
}
static inline bool sema_expr_analyse_builtin_call(Context *context, Expr *expr)
{
	expr->call_expr.is_builtin = true;
	BuiltinFunction func = expr->call_expr.function->builtin_expr.builtin;
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
		case BUILTIN_CEIL:
		case BUILTIN_TRUNC:
		case BUILTIN_SQRT:
		case BUILTIN_COS:
		case BUILTIN_SIN:
		case BUILTIN_POW:
		case BUILTIN_EXP:
		case BUILTIN_FABS:
		case BUILTIN_LOG:
		case BUILTIN_LOG2:
		case BUILTIN_LOG10:
			if (type_is_float_or_float_vector(args[0]->type))
			{
				rtype = args[0]->type;
				break;
			}
			SEMA_ERROR(args[0], "Expected a floating point or floating point array.");
			return false;
		case BUILTIN_MAX:
		case BUILTIN_MIN:
			if (!type_is_float_or_float_vector(args[0]->type))
			{
				SEMA_ERROR(args[0], "Expected a floating point or floating point array.");
				return false;
			}
			if (!type_is_float_or_float_vector(args[1]->type))
			{
				SEMA_ERROR(args[1], "Expected a floating point or floating point array.");
				return false;
			}
			if (type_flatten(args[0]->type) != type_flatten(args[1]->type))
			{
				SEMA_ERROR(args[1], "Expected an expression of type %s.", type_quoted_error_string(args[0]->type));
				return false;
			}
			rtype = args[0]->type;
			break;
		case BUILTIN_FMA:
			if (!type_is_float_or_float_vector(args[0]->type))
			{
				SEMA_ERROR(args[0], "Expected a floating point or floating point array.");
				return false;
			}
			if (!type_is_float_or_float_vector(args[1]->type))
			{
				SEMA_ERROR(args[1], "Expected a floating point or floating point array.");
				return false;
			}
			if (!type_is_float_or_float_vector(args[2]->type))
			{
				SEMA_ERROR(args[2], "Expected a floating point or floating point array.");
				return false;
			}
			if (type_flatten(args[0]->type) != type_flatten(args[1]->type))
			{
				SEMA_ERROR(args[1], "Expected an expression of type %s.", type_quoted_error_string(args[0]->type));
				return false;
			}
			if (type_flatten(args[0]->type) != type_flatten(args[2]->type))
			{
				SEMA_ERROR(args[2], "Expected an expression of type %s.", type_quoted_error_string(args[0]->type));
				return false;
			}
			rtype = args[0]->type;
			break;
		case BUILTIN_VOLATILE_LOAD:
		{
			Type *type = type_flatten(args[0]->type);
			if (!type_is_pointer(type))
			{
				SEMA_ERROR(args[0], "Expected a pointer.");
				return false;
			}
			rtype = type->pointer;
			break;
		}
		case BUILTIN_VOLATILE_STORE:
		{
			Type *type = type_flatten(args[0]->type);
			if (!type_is_pointer(type))
			{
				SEMA_ERROR(args[0], "Expected a pointer.");
				return false;
			}
			rtype = type->pointer;
			if (!cast_implicit(args[1], rtype)) return false;
			break;
		}
		case BUILTIN_NONE:
			UNREACHABLE
	}
	expr->type = type_get_opt_fail(rtype, failable);
	return true;
}

static inline bool sema_expr_analyse_call(Context *context, Expr *expr)
{
	Expr *func_expr = expr->call_expr.function;

	if (!sema_analyse_expr_lvalue(context, func_expr)) return false;
	if (func_expr->expr_kind == EXPR_MACRO_BODY_EXPANSION)
	{
		return sema_analyse_body_expansion(context, expr);
	}
	bool failable = func_expr->type && IS_FAILABLE(func_expr);
	Decl *decl;
	Expr *struct_var = NULL;
	bool macro = false;
	switch (func_expr->expr_kind)
	{
		case EXPR_BUILTIN:
			return sema_expr_analyse_builtin_call(context, expr);
		case EXPR_IDENTIFIER:
			decl = func_expr->identifier_expr.decl;
			break;
		case EXPR_ACCESS:
			decl = func_expr->access_expr.ref;
			if (decl->decl_kind == DECL_FUNC || decl->decl_kind == DECL_MACRO)
			{
				macro = decl->decl_kind == DECL_MACRO;
				if (!macro) expr_insert_addr(func_expr->access_expr.parent);
				struct_var = func_expr->access_expr.parent;
			}
			break;
		case EXPR_MACRO_EXPANSION:
			decl = func_expr->macro_expansion_expr.decl;
			macro = true;
			break;
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
	return sema_expr_analyse_general_call(context, expr, decl, struct_var, macro, failable);
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

static bool expr_check_index_in_range(Context *context, Type *type, Expr *index_expr, bool end_index, bool from_end)
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

static inline bool sema_expr_analyse_subscript(Context *context, Expr *expr, bool is_addr)
{
	assert(expr->expr_kind == EXPR_SUBSCRIPT || expr->expr_kind == EXPR_SUBSCRIPT_ADDR);

	// 1. Evaluate the expression to index.
	Expr *subscripted = expr->subscript_expr.expr;
	if (!sema_analyse_expr_lvalue(context, subscripted)) return false;
	sema_deref_array_pointers(subscripted);

	// 2. Evaluate the index.
	Expr *index = expr->subscript_expr.index;
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
		if (is_addr) decl = sema_find_operator(context, current_expr, kw_operator_element_at_ref);
		if (!decl)
		{
			decl = sema_find_operator(context, current_expr, kw_operator_element_at);
			if (decl && is_addr)
			{
				SEMA_ERROR(expr, "'%s' is not defined for %s, so you need && to take the address of the temporary.",
						   kw_operator_element_at_ref, type_quoted_error_string(current_expr->type));
				return false;
			}
		}
		if (decl)
		{
			expr->expr_kind = EXPR_CALL;
			Expr **args = NULL;
			vec_add(args, index);
			expr->call_expr = (ExprCall){ .func_ref = decl, .arguments = args };
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
	if (!expr_check_index_in_range(context, current_type, index, false, expr->subscript_expr.from_back)) return false;

	expr->subscript_expr.expr = current_expr;
	if (is_addr) inner_type = type_get_ptr(inner_type);
	expr->type = type_get_opt_fail(inner_type, failable);
	return true;
}

static inline bool sema_expr_analyse_slice(Context *context, Expr *expr)
{
	if (!sema_analyse_expr(context, expr->slice_expr.expr)) return false;
	bool failable = IS_FAILABLE(expr->slice_expr.expr);
	assert(expr->expr_kind == EXPR_SLICE);
	Expr *subscripted = expr->slice_expr.expr;
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

	if (!sema_analyse_expr(context, start)) return false;
	if (end && !sema_analyse_expr(context, end)) return false;

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
			Int128 len = { 0, type->array.len };
			if (expr->slice_expr.start_from_back)
			{
				start->const_expr.ixx.i = i128_sub(len, start->const_expr.ixx.i);
				expr->slice_expr.start_from_back = false;
			}
			if (expr->slice_expr.end_from_back)
			{
				end->const_expr.ixx.i = i128_sub(len, end->const_expr.ixx.i);
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


	expr->type = type_get_opt_fail(type_get_subarray(inner_type), failable);
	return true;
}


static inline bool sema_expr_analyse_group(Context *context, Expr *expr)
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


static inline void expr_rewrite_to_string(Expr *expr_to_rewrite, const char *string)
{
	expr_to_rewrite->expr_kind = EXPR_CONST;
	expr_to_rewrite->const_expr.const_kind = CONST_STRING;
	expr_to_rewrite->const_expr.string.chars = (char *)string;
	ArraySize len = (ArraySize)strlen(string);
	expr_to_rewrite->const_expr.string.len = len;
	expr_to_rewrite->resolve_status = RESOLVE_DONE;
	expr_to_rewrite->type = type_get_ptr(type_get_array(type_char, len));
}





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
		case EXPR_TYPEINFO:
			// Special case: .typeid
			if (child->type_expr->resolve_status == RESOLVE_DONE && child->type_expr->type == type_typeid)
			{
				return child->type_expr->span.loc;
			}
			break;
		default:
			break;

	}
	SEMA_ERROR(child, "Expected an identifier here.");
	return INVALID_TOKEN_ID;
}

static inline bool sema_expr_analyse_type_access(Context *context, Expr *expr, TypeInfo *parent, bool was_group, bool is_macro, TokenId identifier_token)
{
	// 1. Foo*.sizeof is not allowed, it must be (Foo*).sizeof
	if (!was_group && type_kind_is_derived(parent->type->type_kind))
	{
		SEMA_ERROR(expr->access_expr.parent, "Array and pointer types must be enclosed in (), did you forget it?");
		return false;
	}

	TokenType type = TOKTYPE(identifier_token);

	// 2. Handle Foo.typeid => return a typeid expression.
	if (type == TOKEN_TYPEID)
	{
		expr->type = type_typeid;
		expr->expr_kind = EXPR_CONST;
		expr->const_expr.const_kind = CONST_TYPEID;
		expr->const_expr.typeid = parent->type->canonical;
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
			expr->const_expr.const_kind = CONST_FLOAT;
#if LONG_DOUBLE
			expr->const_expr.fxx = (Float) { nanl(""), canonical->type_kind };
#else
			expr->const_expr.fxx = (Float) { nan(""), canonical->type_kind };
#endif
			expr->type = parent->type;
			expr->resolve_status = RESOLVE_DONE;
			return true;
		}
		if (name == kw_inf)
		{
			expr->expr_kind = EXPR_CONST;
			expr->const_expr.const_kind = CONST_FLOAT;
			expr->const_expr.fxx = (Float) { INFINITY, parent->type->canonical->type_kind };
			expr->type = parent->type->canonical;
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
				expr_rewrite_to_int_const(expr, type_isize, vec_size(decl->enums.values), true);
				return true;
			}
			if (name == kw_max)
			{
				Expr *max = enum_minmax_value(decl, BINARYOP_GT);
				if (!max)
				{
					expr_rewrite_to_int_const(expr, decl->enums.type_info->type->canonical, 0, false);
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
					expr_rewrite_to_int_const(expr, decl->enums.type_info->type->canonical, 0, false);
					return true;
				}
				expr_replace(expr, min);
				return true;
			}
			break;
		case DECL_ERRTYPE:
			context_register_external_symbol(context, decl);

			if (type == TOKEN_CONST_IDENT)
			{
				if (!sema_expr_analyse_enum_constant(expr, identifier_token, decl))
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
			if (name == kw_max)
			{
				Expr *max = enum_minmax_value(decl, BINARYOP_GT);
				if (!max)
				{
					expr_rewrite_to_int_const(expr, decl->enums.type_info->type->canonical, 0, false);
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
					expr_rewrite_to_int_const(expr, decl->enums.type_info->type->canonical, 0, false);
					return true;
				}
				expr_replace(expr, min);
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
		SEMA_ERROR(expr, "You cannot use '.' on normal members, only nested structs and unions.");
		return false;
	}

	// 12b. If the member was *not* a macro but was prefixed with `@` then that's an error.
	if (member->decl_kind != DECL_MACRO && is_macro)
	{
		SEMA_ERROR(expr, "'@' should only be placed in front of macro names.");
		return false;
	}

	if (member->decl_kind == DECL_UNION || member->decl_kind == DECL_STRUCT || member->decl_kind == DECL_BITSTRUCT)
	{
		expr->expr_kind = EXPR_TYPEINFO;
		expr->type_expr->type = member->type;
		expr->type_expr->resolve_status = RESOLVE_DONE;
		expr->type = type_typeinfo;
		return true;
	}

	// 12a. If the member was a macro and it isn't prefixed with `@` then that's an error.
	if (member->decl_kind == DECL_MACRO && !is_macro)
	{
		SEMA_ERROR(expr, "Expected '@' before the macro name.");
		return false;
	}

	expr->identifier_expr.identifier = identifier_token;
	expr->expr_kind = EXPR_IDENTIFIER;
	expr->identifier_expr.decl = member;
	expr->type = member->type;
	return true;
}

/**
 * Analyse "x.y"
 */
static inline bool sema_expr_analyse_access(Context *context, Expr *expr)
{
	Expr *parent = expr->access_expr.parent;
	bool was_group = parent->expr_kind == EXPR_GROUP;

	// 1. Resolve the left hand
	if (!sema_analyse_expr_lvalue(context, parent)) return false;

	// 2. The right hand side may be a @ident or ident
	Expr *child = expr->access_expr.child;
	bool is_macro = child->expr_kind == EXPR_MACRO_EXPANSION;

	// 3. Find the actual token.
	TokenId identifier_token = sema_expr_resolve_access_child(child);
	if (TOKEN_IS_INVALID(identifier_token)) return false;

	// 2. If our left hand side is a type, e.g. MyInt.abc, handle this here.
	if (parent->expr_kind == EXPR_TYPEINFO)
	{
		return sema_expr_analyse_type_access(context, expr, parent->type_expr, was_group, is_macro, identifier_token);
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
	TokenType token_type = TOKTYPE(identifier_token);
	Expr *current_parent = parent;

	Type *type = type_no_fail(parent->type)->canonical;
	Type *flat_type = type_flatten(type);
	if (!is_macro && token_type == TOKEN_TYPEID && flat_type->type_kind == TYPE_ANY)
	{
		expr->expr_kind = EXPR_TYPEOFANY;
		expr->inner_expr = parent;
		expr->type = type_typeid;
		return true;
	}
	const char *kw = TOKSTR(identifier_token);

CHECK_DEEPER:

	// 9. Fix hard coded function `len` on subarrays and arrays
	if (!is_macro && kw == kw_len)
	{
		if (flat_type->type_kind == TYPE_SUBARRAY)
		{
			expr->expr_kind = EXPR_LEN;
			expr->len_expr.inner = parent;
			expr->type = type_usize;
			expr->resolve_status = RESOLVE_DONE;
			return true;
		}
		if (flat_type->type_kind == TYPE_ARRAY)
		{
			expr_rewrite_to_int_const(expr, type_isize, flat_type->array.len, true);
			return true;
		}
	}

	// Hard coded ptr on subarrays and variant
	if (!is_macro && kw == kw_ptr)
	{
		if (flat_type->type_kind == TYPE_SUBARRAY)
		{
			expr->expr_kind = EXPR_PTR;
			expr->inner_expr = parent;
			expr->type = type_get_ptr(flat_type->array.base);
			expr->resolve_status = RESOLVE_DONE;
			return true;
		}
		if (flat_type->type_kind == TYPE_ANY)
		{
			expr->expr_kind = EXPR_PTR;
			expr->inner_expr = parent;
			expr->type = type_voidptr;
			expr->resolve_status = RESOLVE_DONE;
			return true;
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

	if (!member)
	{
		Decl *ambiguous = NULL;
		Decl *private = NULL;
		member = sema_resolve_method(context, decl, kw, &ambiguous, &private);
	}

	if (member && member->decl_kind == DECL_FUNC)
	{
		context_register_external_symbol(context, member);
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

static MemberIndex sema_analyse_designator_index(Context *context, Expr *index)
{
	if (!sema_analyse_expr_lvalue(context, index))
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

static Type *sema_find_type_of_element(Context *context, Type *type, DesignatorElement **elements, unsigned *curr_index, bool *is_constant, bool *did_report_error, MemberIndex *max_index, Decl **member_ptr)
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
				len = type_flattened->array.len;
				base = type_flattened->array.base;
				break;
			case TYPE_VECTOR:
				len = type_flattened->vector.len;
				base = type_flattened->vector.base;
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

static Type *sema_expr_analyse_designator(Context *context, Type *current, Expr *expr, MemberIndex *max_index, Decl **member_ptr)
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

static bool sema_expr_analyse_designated_initializer(Context *context, Type *assigned, Expr *initializer)
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
		DesignatorElement *element = VECLAST(expr->designator_expr.path);
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
		ConstInitializer *const_init = MALLOC(sizeof(ConstInitializer));
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
	VECEACH(members, i)
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
/**
 * Perform analysis for a plain initializer, that is one initializing all fields.
 * @return true if analysis succeeds.
 */
static inline bool sema_expr_analyse_struct_plain_initializer(Context *context, Decl *assigned, Expr *initializer)
{
	Expr **elements = initializer->initializer_list;
	Decl **members = assigned->strukt.members;
	MemberIndex size = (MemberIndex)vec_size(elements);
	MemberIndex expected_members = (MemberIndex)vec_size(members);

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
	MemberIndex max_loop = MAX(size, expected_members);
	for (MemberIndex i = 0; i < max_loop; i++)
	{
		// 4. Check if we exceeded the list of elements in the struct/union.
		//    This way we can check the other elements which might help the
		//    user pinpoint where they put the double elements.
		if (i >= expected_members)
		{
			assert(i < size);
			SEMA_ERROR(elements[i], "Too many elements in initializer, expected only %d.", expected_members);
			return false;
		}
		// 5. We might have anonymous members
		Decl *member = members[i];
		if (member->decl_kind != DECL_VAR && !member->name)
		{
			int sub_element_count = decl_count_elements(members[i]);
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
			int max_index_to_copy = MIN(i + sub_element_count, size);
			for (int j = i; j < max_index_to_copy; j++)
			{
				vec_add(new_initializer->initializer_list, elements[j]);
			}
			int reduce_by = max_index_to_copy - i - 1;
			size -= reduce_by;
			max_loop = MAX(size, expected_members);
			assert(size <= vec_size(initializer->initializer_list));
			vec_resize(initializer->initializer_list, (unsigned)size);
			elements = initializer->initializer_list;
			elements[i] = new_initializer;
		}
		if (i >= size)
		{
			not_enough_elements(initializer, i);
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
	assert(expected_members <= size);

	if (expr_is_constant_eval(initializer, CONSTANT_EVAL_ANY))
	{
		ConstInitializer *const_init = CALLOCS(ConstInitializer);
		const_init->kind = CONST_INIT_STRUCT;
		const_init->type = type_flatten(initializer->type);
		ConstInitializer **inits = MALLOC(sizeof(ConstInitializer *) * vec_size(elements));
		VECEACH(elements, i)
		{
			Expr *expr = elements[i];
			if (expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_LIST)
			{
				inits[i] = expr->const_expr.list;
				continue;
			}
			ConstInitializer *element_init = MALLOC(sizeof(ConstInitializer));
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
static inline bool sema_expr_analyse_array_plain_initializer(Context *context, Type *assigned, Expr *initializer)
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
			ConstInitializer *element_init = MALLOC(sizeof(ConstInitializer));
			sema_create_const_initializer_value(element_init, expr);
			vec_add(inits, element_init);
		}
		const_init->init_array_full = inits;
		expr_set_as_const_list(initializer, const_init);
	}

	// 7. Done!
	return true;
}

static inline bool sema_expr_analyse_untyped_initializer(Context *context, Expr *initializer)
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

static inline bool sema_expr_analyse_initializer(Context *context, Type *external_type, Type *assigned, Expr *expr)
{
	// Note at this point this we either have
	// EXPR_DESIGNATED_INITIALIZER_LIST
	// or EXPR_INITIALIZER_LIST

	if (expr->expr_kind == EXPR_DESIGNATED_INITIALIZER_LIST)
	{
		expr->type = external_type;
		return sema_expr_analyse_designated_initializer(context, assigned, expr);
	}

	assert(expr->expr_kind == EXPR_INITIALIZER_LIST);

	Expr **init_expressions = expr->initializer_list;

	unsigned init_expression_count = vec_size(init_expressions);

	// 1. Zero size init will initialize to empty.
	if (init_expression_count == 0)
	{
		external_type = sema_type_lower_by_size(external_type, 0);
		expr->type = external_type;
		ConstInitializer *initializer = CALLOCS(ConstInitializer);
		initializer->kind = CONST_INIT_ZERO;
		initializer->type = type_flatten(expr->type);
		expr_set_as_const_list(expr, initializer);
		return true;
	}

	external_type = sema_type_lower_by_size(external_type, init_expression_count);
	assigned = sema_type_lower_by_size(assigned, init_expression_count);
	expr->type = external_type;

	if (external_type == type_complist)
	{
		return sema_expr_analyse_untyped_initializer(context, expr);
	}
	// 3. Otherwise use the plain initializer.
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

static inline bool sema_expr_analyse_initializer_list(Context *context, Type *to, Expr *expr)
{
	if (!to)
	{
		return sema_expr_analyse_initializer(context, type_complist, type_complist, expr);
	}
	assert(to);
	Type *assigned = type_flatten(to);
	switch (assigned->type_kind)
	{
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ARRAY:
		case TYPE_BITSTRUCT:
		case TYPE_INFERRED_ARRAY:
		case TYPE_VECTOR:
			return sema_expr_analyse_initializer(context, to, assigned, expr);
		case TYPE_SUBARRAY:
		{
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



static inline bool sema_expr_analyse_expr_list(Context *context, Expr *expr)
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

static inline bool sema_expr_analyse_cast(Context *context, Expr *expr)
{
	Expr *inner = expr->cast_expr.expr;
	bool success = sema_resolve_type_info(context, expr->cast_expr.type_info);
	if (!sema_analyse_expr(context, inner) || !success) return false;

	Type *target_type = expr->cast_expr.type_info->type;
	if (type_is_failable(target_type))
	{
		SEMA_ERROR(expr->cast_expr.type_info, "Casting to a failable type is not allowed.");
		return false;
	}
	if (!cast_may_explicit(inner->type, target_type, true, inner->expr_kind == EXPR_CONST))
	{
		return sema_failed_cast(expr, type_no_fail(inner->type), target_type);
	}
	cast(inner, target_type);
	expr_replace(expr, inner);
	return true;
}

static inline bool sema_expr_analyse_slice_assign(Context *context, Expr *expr, Type *left_type, Expr *right, bool is_unwrapped)
{
	// 1. Evaluate right side to required type.
	if (!sema_analyse_expr_rhs(context, left_type->array.base, right, false)) return false;

	Expr *left = expr->binary_expr.left;
	expr->type = right->type;
	expr->expr_kind = EXPR_SLICE_ASSIGN;
	expr->slice_assign_expr.left = left;
	expr->slice_assign_expr.right = right;

	return true;
}

bool sema_expr_analyse_assign_right_side(Context *context, Expr *expr, Type *left_type, Expr *right, bool is_unwrapped)
{
	if (expr && expr->binary_expr.left->expr_kind == EXPR_SLICE)
	{
		return sema_expr_analyse_slice_assign(context, expr, left_type, right, is_unwrapped);
	}

	// 1. Evaluate right side to required type.
	if (!sema_analyse_expr_rhs(context, left_type, right, true)) return false;
	if (IS_FAILABLE(right) && !type_is_failable(left_type))
	{
		if (is_unwrapped)
		{
			SEMA_ERROR(expr->binary_expr.left, "The variable is unwrapped in this context, if you don't want to unwrap it, use () around the variable to suppress unwrapping, like 'catch err = (x)' and 'try (x)'.");
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
		SEMA_ERROR(expr, "The compile time variable '%s' was not defined in this scope.", TOKSTR(expr->ct_ident_expr.identifier));
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
	if (!sema_expr_analyse_assign_right_side(context, expr, left->type, right, false)) return false;

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

	if (!sema_analyse_expr_lvalue(context, right)) return false;

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
	if (!expr_is_ltype(left))
	{
		SEMA_ERROR(left, "This expression is not assignable, did you make a mistake?");
		return false;
	}

	bool is_unwrapped_var = expr_is_unwrapped_ident(left);

	// 3. Evaluate right side to required type.
	if (!sema_expr_analyse_assign_right_side(context, expr, left->type, right, is_unwrapped_var)) return false;

	if (is_unwrapped_var && IS_FAILABLE(right))
	{
		return sema_rewrap_var(context, left->identifier_expr.decl);
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
static bool sema_expr_analyse_ct_common_assign(Context *context, Expr *expr, Expr *left)
{

	// 1. Analyse left side.
	if (!sema_expr_analyse_ct_identifier_lvalue(context, left)) return false;

	Decl *left_var = left->ct_ident_expr.decl;

	Expr *left_value = left_var->var.init_expr;
	assert(left_value);
	assert(!IS_FAILABLE(left_value));

	expr->binary_expr.left = left_value;

	expr->binary_expr.operator = binaryop_assign_base_op(expr->binary_expr.operator);

	if (!sema_expr_analyse_binary(context, expr)) return false;

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
	if (left->expr_kind == EXPR_CT_IDENT)
	{
		return sema_expr_analyse_ct_common_assign(context, expr, left);
	}

	// 1. Analyse left side.
	if (!sema_analyse_expr_lvalue(context, left)) return false;

	// 2. Verify that the left side is assignable.
	if (!expr_is_ltype(left))
	{
		SEMA_ERROR(left, "Expression is not assignable.");
		return false;
	}

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
static bool sema_expr_analyse_add_sub_assign(Context *context, Expr *expr, Expr *left, Expr *right)
{
	if (left->expr_kind == EXPR_CT_IDENT)
	{
		return sema_expr_analyse_ct_common_assign(context, expr, left);
	}

	// 1. Analyse the left hand side
	if (!sema_analyse_expr(context, left)) return false;

	// 2. Ensure the left hand side is assignable
	if (!expr_is_ltype(left))
	{
		SEMA_ERROR(left, "Expression is not assignable.");
		return false;
	}

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
			if (type->builtin.bitsize < platform_target.width_c_int) return type_cint();
			return type;
		case ALL_UNSIGNED_INTS:
			if (type->builtin.bitsize < platform_target.width_c_int) return type_cuint();
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

static bool binary_arithmetic_promotion(Context *context, Expr *left, Expr *right, Type *left_type, Type *right_type, Expr *parent, const char *error_message)
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
static bool sema_expr_analyse_sub(Context *context, Expr *expr, Expr *left, Expr *right)
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
static bool sema_expr_analyse_add(Context *context, Expr *expr, Expr *left, Expr *right)
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
		expr->binary_expr.left = left;
		expr->binary_expr.right = right;
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
	// 4. Do an binary arithmetic promotion
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
static bool sema_expr_analyse_mult(Context *context, Expr *expr, Expr *left, Expr *right)
{

	// 1. Analyse the sub expressions and promote to a common type
	if (!sema_expr_analyse_binary_arithmetic_subexpr(context, expr, "It is not possible to multiply %s by %s.")) return false;


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
static bool sema_expr_analyse_div(Context *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyse sub expressions and promote to a common type
	if (!sema_expr_analyse_binary_arithmetic_subexpr(context, expr, "Cannot divide %s by %s.")) return false;

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
static bool sema_expr_analyse_mod(Context *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Analyse both sides and promote to a common type
	if (!sema_expr_analyse_binary_arithmetic_subexpr(context, expr, NULL)) return false;

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
static bool sema_expr_analyse_bit(Context *context, Expr *expr, Expr *left, Expr *right)
{
	// 1. Convert to common type if possible.
	if (!sema_expr_analyse_binary_arithmetic_subexpr(context, expr, NULL)) return false;

	// 2. Check that both are integers.
	if (!both_any_integer_or_integer_vector(left, right))
	{
		return sema_type_error_on_binop(expr);
	}

	// 3. Do constant folding if both sides are constant.
	if (expr_both_const(left, right))
	{
		BinaryOp op = expr->binary_expr.operator;
		expr_replace(expr, left);
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

	// 5. Assign the type
	expr_unify_binary(expr, left, right);
	return true;
}

/**
 * Analyse >> and << operations.
 * @return true if the analysis succeeded.
 */
static bool sema_expr_analyse_shift(Context *context, Expr *expr, Expr *left, Expr *right)
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
static bool sema_expr_analyse_shift_assign(Context *context, Expr *expr, Expr *left, Expr *right)
{

	// 1. Analyze the two sub lhs & rhs *without coercion*
	if (!sema_expr_analyse_binary_subexpr(context, expr, left, right)) return false;

	bool failable = IS_FAILABLE(left) || IS_FAILABLE(right);

	// 2. Ensure the lhs side is assignable
	if (!expr_is_ltype(left))
	{
		SEMA_ERROR(left, "Expression is not assignable.");
		return false;
	}

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


static bool sema_expr_analyse_and_or(Context *context, Expr *expr, Expr *left, Expr *right)
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



static void cast_to_max_bit_size(Context *context, Expr *left, Expr *right, Type *left_type, Type *right_type)
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

static bool sema_is_unsigned_always_false_comparison(Context *context, Expr *expr, Expr *left, Expr *right)
{
	if (context->macro_scope.macro) return true;
	if (!is_const(left) && !is_const(right)) return true;
	if (!type_is_integer(left->type)) return true;
	if (is_const(left) && type_is_unsigned(type_flatten_distinct(right->type)))
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
	if (!is_const(right) || !type_is_unsigned(type_flatten_distinct(left->type))) return true;
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
static bool sema_expr_analyse_comp(Context *context, Expr *expr, Expr *left, Expr *right)
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
		if (left_type->vector.len == right_type->vector.len)
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
static bool sema_expr_analyse_deref(Context *context, Expr *expr)
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
		case EXPR_MACRO_EXPANSION:
			SEMA_ERROR(inner, "It's not possible to take the address of a macro.");
			return false;
		case EXPR_IDENTIFIER:
		case EXPR_CONST_IDENTIFIER:
			return sema_take_addr_of_ident(inner);
		case EXPR_UNARY:
			if (inner->unary_expr.operator == UNARYOP_DEREF) return true;
			break;
		case EXPR_ACCESS:
			return sema_take_addr_of(inner->access_expr.parent);
		case EXPR_GROUP:
			return sema_take_addr_of(inner->inner_expr);
		case EXPR_SUBSCRIPT:
			return sema_take_addr_of(inner->subscript_expr.expr);
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
static bool sema_expr_analyse_addr(Context *context, Expr *expr)
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
static bool sema_expr_analyse_neg(Context *context, Expr *expr)
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
static bool sema_expr_analyse_bit_not(Context *context, Expr *expr)
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
static bool sema_expr_analyse_not(Context *context, Expr *expr)
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

static inline bool sema_expr_analyse_ct_incdec(Context *context, Expr *expr, Expr *inner)
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
static inline bool sema_expr_analyse_incdec(Context *context, Expr *expr)
{
	// 1. Analyse the lvalue to update
	Expr *inner = expr->unary_expr.expr;
	if (!sema_analyse_expr_lvalue(context, inner)) return false;

	// 2. Assert it's an l-value
	if (!expr_is_ltype(inner))
	{
		SEMA_ERROR(inner, "An assignable expression, like a variable, was expected here.");
		return false;
	}

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
static inline bool sema_expr_analyse_taddr(Context *context, Expr *expr)
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

static inline bool sema_expr_analyse_bitassign(Context *context, Expr *expr)
{
	TODO
}
static inline bool sema_expr_analyse_binary(Context *context, Expr *expr)
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
	switch (expr->binary_expr.operator)
	{
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
static inline bool sema_expr_analyse_unary(Context *context, Expr *expr)
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


static inline bool sema_expr_analyse_try(Context *context, Expr *expr)
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

static inline bool sema_expr_analyse_catch(Context *context, Expr *expr)
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

static inline bool sema_expr_analyse_or_error(Context *context, Expr *expr)
{
	Expr *inner = expr->or_error_expr.expr;
	if (!sema_analyse_expr(context, inner)) return false;

	if (expr->or_error_expr.widen && !sema_widen_top_down(inner, expr->type)) return false;

	Type *type = inner->type;
	if (type->type_kind != TYPE_FAILABLE)
	{
		SEMA_ERROR(inner, "No failable to use '\?\?' with, please remove the '\?\?'.");
		return false;
	}
	type = type->failable;
	if (expr->or_error_expr.is_jump)
	{
		if (!sema_analyse_statement(context, expr->or_error_expr.or_error_stmt)) return false;
		expr->type = inner->type;
		return true;
	}

	// First we analyse the "else" and try to implictly cast.
	Expr *else_expr = expr->or_error_expr.or_error_expr;
	if (!sema_analyse_expr(context, else_expr)) return false;
	if (expr->or_error_expr.widen && !sema_widen_top_down(else_expr, expr->type)) return false;

	// Here we might need to insert casts.
	Type *else_type = else_expr->type;
	if (else_type->type_kind == TYPE_FAILABLE)
	{
		SEMA_ERROR(else_expr, "The default value may not be a failable.");
		return false;
	}
	Type *common = type_find_max_type(type, else_type);
	if (!common)
	{
		SEMA_ERROR(else_expr, "Cannot find a common type for %s and %s.", type_quoted_error_string(type),
		           type_quoted_error_string(else_type));
		return false;
	}
	if (!cast_implicit(inner, common)) return false;
	if (!cast_implicit(else_expr, common)) return false;
	if (IS_FAILABLE(else_expr))
	{
		SEMA_ERROR(else_expr, "The expression must be a non-failable.");
		return false;
	}
	expr->type = common;

	return true;
}

static inline bool sema_expr_analyse_rethrow(Context *context, Expr *expr)
{
	Expr *inner = expr->rethrow_expr.inner;
	if (!sema_analyse_expr(context, inner)) return false;
	expr->rethrow_expr.defer = context->active_scope.defer_last;
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

	if (context->rtype->type_kind != TYPE_FAILABLE)
	{
		SEMA_ERROR(expr, "This expression implicitly returns with a failable result, but the function does not allow failable results. Did you mean to use 'else' instead?");
		return false;
	}

	return true;
}


static inline bool sema_expr_analyse_force_unwrap(Context *context, Expr *expr)
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





static inline bool sema_expr_analyse_typeid(Context *context, Expr *expr)
{
	if (!sema_resolve_type_info(context, expr->typeid_expr)) return expr_poison(expr);
	Type *type = expr->type_expr->type;
	expr->expr_kind = EXPR_CONST;
	expr->const_expr.const_kind = CONST_TYPEID;
	expr->const_expr.typeid = type->canonical;
	expr->type = type_typeid;
	return true;
}


static inline bool sema_expr_analyse_expr_block(Context *context, Expr *expr)
{
	bool success = true;
	expr->type = type_void;
	bool saved_expr_failable_return = context->expr_failable_return;
	Type *prev_expected_block_type = context->expected_block_type;
	Ast **saved_returns = context_push_returns(context);
	Type *stored_block_type = context->expected_block_type;
	context->expected_block_type = NULL;
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
		expr->type = sum_returns;

	EXIT:
		POP_BREAKCONT();
		POP_NEXT();

	SCOPE_END;
	context->expected_block_type = stored_block_type;
	context_pop_returns(context, saved_returns);
	context->expr_failable_return = saved_expr_failable_return;

	return success;
}


static inline bool sema_expr_analyse_compound_literal(Context *context, Expr *expr)
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



static inline bool sema_expr_analyse_failable(Context *context, Expr *expr)
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
	if (type->type_kind != TYPE_ERRTYPE && type->type_kind != TYPE_ANYERR)
	{
		SEMA_ERROR(inner, "You cannot use the '!' operator on expressions of type %s", type_quoted_error_string(type));
		return false;
	}
	expr->type = type_anyfail;
	return true;
}

static inline bool sema_expr_analyse_placeholder(Context *context, Expr *expr)
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
		expr_rewrite_to_int_const(expr, type_isize, TOKLOC(expr->placeholder_expr.identifier)->row, true);
		return true;
	}
	if (string == kw_LINE)
	{
		if (context->macro_scope.depth)
		{
			expr_rewrite_to_int_const(expr, type_isize, context->macro_scope.original_inline_line, true);
		}
		else
		{
			expr_rewrite_to_int_const(expr, type_isize, TOKLOC(expr->placeholder_expr.identifier)->row, true);
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
	return lexer->chars[lexer->index + (uint32_t)offset];
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
	uint64_t value = 0;
	while (is_number(minilex_peek(lexer, 0)))
	{
		uint64_t old_value = value;
		value = value * 10 + (uint64_t)minilex_next(lexer) - '0';
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
		if (minilex_match(lexer, '\0')) break;
		if (minilex_match(lexer, '['))
		{
			minilex_skip_whitespace(lexer);
			if (!is_number(minilex_peek(lexer, 0))) return false;
			uint64_t value = minilex_parse_number(lexer, MAX_ARRAYINDEX);
			if (value == UINT64_MAX) return false;
			if (!minilex_match(lexer, ']')) return false;
			element.array = true;
			element.index = (MemberIndex)value;
			vec_add(elements, element);
			continue;
		}
		if (minilex_match(lexer, '.'))
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
			vec_add(elements, element);
			continue;
		}
		return false;
	}
	*elements_ref = elements;
	return true;
}

static inline bool sema_analyse_identifier_path_string(Context *context, SourceSpan span, Expr *expr, Decl **decl_ref, Type **type_ref, ExprFlatElement **idents_ref, bool report_missing)
{
	const char *chars = expr->const_expr.string.chars;
	uint32_t len = expr->const_expr.string.len;
	if (!len)
	{
		sema_error_range(span, "Expected a name here.");
		return false;
	}

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
		if (!path) return false;
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
	if (!global_context.scratch_buffer_len)
	{
		sema_error_range(span, "A valid identifier was expected here, did you want to take the length of a string literal? If so use '.len'.", chars);
		return false;
	}
	TokenType token_type;
	const char *symbol = symtab_find(global_context.scratch_buffer,
	                                 global_context.scratch_buffer_len,
	                                 fnv1a(global_context.scratch_buffer, global_context.scratch_buffer_len),
	                                 &token_type);
	if (!symbol)
	{
		if (report_missing)
		{
			sema_error_range(span, "'%s' could not be found, did you misspell it?", chars);
			return false;
		}
		return true;
	}
	Type *type = NULL;
	Decl *decl = NULL;
	if (token_is_type(token_type))
	{
		type = type_from_token(token_type);
	}
	else
	{
		ASSIGN_DECL_ELSE(decl, sema_resolve_string_symbol(context, symbol, expr->span, path, report_missing), false);
		if (!decl) return true;
		if (!sema_analyse_decl(context, decl)) return false;
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
			ArraySize array_size = 0;
			while (is_number(minilex_peek(&lexer, 0)))
			{
				ByteSize old_array_size = array_size;
				array_size = array_size * 10 + (ArraySize)minilex_next(&lexer) - '0';
				if (old_array_size > array_size || array_size > MAX_ARRAYINDEX)
				{
					sema_error_range(span, "Array index out of bounds.");
					return false;
				}
			}
			minilex_skip_whitespace(&lexer);
			if (!minilex_match(&lexer, ']')) goto FAIL_PARSE;
			type = type_get_array(type, array_size);
		}
	}

	if (!sema_analyse_idents_string(context, &lexer, idents_ref))
	{
		if (report_missing)
		{
			sema_error_range(span, "The path to an existing member was expected after '%s', did you make a mistake?", symbol);
			return false;
		}
		else
		{
			return true;
		}
	}

	*decl_ref = decl;
	*type_ref = type;
	return true;


FAIL_PARSE:
	SEMA_ERROR(expr, "'%s' could not be parsed as an identifier, did you make a mistake?", chars);
	return false;

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


static bool sema_expr_analyse_type_var_path(Context *context, Expr *expr, ExprFlatElement **elements, Type **type_ref,
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
		case EXPR_CONST_IDENTIFIER:
		case EXPR_IDENTIFIER:
			decl = current->identifier_expr.decl;
			break;
		case EXPR_TYPEINFO:
			type = current->type_expr->type;
			break;
		case EXPR_CONST:
			if (expr->const_expr.const_kind == CONST_STRING)
			{
				if (!sema_analyse_identifier_path_string(context, expr->span, current, &decl, &type, elements, true)) return false;
				break;
			}
			FALLTHROUGH;
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

static inline bool sema_expr_analyse_ct_alignof(Context *context, Expr *expr)
{
	Expr *main_var = expr->ct_call_expr.main_var;
	Type *type = NULL;
	Decl *decl = NULL;
	ExprFlatElement *path = expr->ct_call_expr.flat_path;

	if (!sema_expr_analyse_type_var_path(context, main_var, &path, &type, &decl)) return false;

	AlignSize align = decl && !decl_is_user_defined_type(decl) ? decl->alignment : type_abi_alignment(type);
	VECEACH(path, i)
	{
		ExprFlatElement element = path[i];
		Type *actual_type = type_flatten_distinct(type);
		if (element.array)
		{
			if (actual_type->type_kind != TYPE_ARRAY)
			{
				SEMA_ERROR(expr, "It's possible to index into a non fixed size array.");
				return false;
			}
			type = actual_type->array.base;
			TypeSize size = type_size(type);
			align = type_min_alignment(size * (AlignSize)element.index, align);
			continue;
		}
		if (!type_is_structlike(actual_type))
		{
			if (i == 0)
			{
				SEMA_ERROR(main_var, "%s has no members.", type_quoted_error_string(type));
			}
			else
			{
				SEMA_ERROR(expr, "There is no such member in %s.", type_quoted_error_string(type));
			}
			return false;
		}
		Decl *member;
		SCOPE_START
			add_members_to_context(context, actual_type->decl);
			member = sema_resolve_symbol_in_current_dynamic_scope(context, element.ident);
		SCOPE_END;
		if (!member)
		{
			SEMA_ERROR(expr, "There is no such member in %s.", type_quoted_error_string(type));
			return false;
		}
		type = member->type;
		align = type_min_alignment(member->offset, align);
	}

	expr_rewrite_to_int_const(expr, type_isize, align, true);

	return true;
}

static inline bool sema_expr_analyse_ct_sizeof(Context *context, Expr *expr)
{
	Expr *main_var = expr->ct_call_expr.main_var;
	Type *type = NULL;
	Decl *decl = NULL;
	ExprFlatElement *path = expr->ct_call_expr.flat_path;
	if (!sema_expr_analyse_type_var_path(context, main_var, &path, &type, &decl)) return false;

	if (!type) return false;

	VECEACH(path, i)
	{
		ExprFlatElement element = path[i];
		Type *actual_type = type_flatten_distinct(type);
		if (element.array)
		{
			if (actual_type->type_kind != TYPE_ARRAY)
			{
				SEMA_ERROR(expr, "It's possible to index into a non fixed size array.");
				return false;
			}
			type = actual_type->array.base;
			continue;
		}
		if (!type_is_structlike(actual_type))
		{
			if (i == 0)
			{
				SEMA_ERROR(main_var, "%s has no members.", type_quoted_error_string(type));
			}
			else
			{
				SEMA_ERROR(expr, "There is no such member in %s.", type_quoted_error_string(type));
			}
			return false;
		}
		Decl *member;
		SCOPE_START
			add_members_to_context(context, actual_type->decl);
			member = sema_resolve_symbol_in_current_dynamic_scope(context, element.ident);
		SCOPE_END;
		if (!member)
		{
			SEMA_ERROR(expr, "There is no such member in %s.", type_quoted_error_string(type));
			return false;
		}
		type = member->type;
	}

	expr_rewrite_to_int_const(expr, type_isize, type_size(type), true);
	return true;
}

static inline bool sema_expr_analyse_ct_nameof(Context *context, Expr *expr)
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
			if (!decl->external_name)
			{
				SEMA_ERROR(main_var, "'%s' does not have an external name.", decl->name);
				return false;
			}
			expr_rewrite_to_string(expr, decl->extname ? decl->extname : decl->external_name);
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
		expr_rewrite_to_string(expr, scratch_buffer_interned());
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
		expr_rewrite_to_string(expr, type->decl->extname ? type->decl->extname : type->decl->external_name);
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
	expr_rewrite_to_string(expr, scratch_buffer_interned());
	return true;
}


static Type *sema_expr_check_type_exists(Context *context, TypeInfo *type_info)
{
	if (type_info->resolve_status == RESOLVE_DONE)
	{
		return type_info->type;
	}
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
		case TYPE_INFO_IDENTIFIER:
		{
			Decl *decl = sema_resolve_normal_symbol(context, type_info->unresolved.name_loc, type_info->unresolved.path, false);
			if (!decl) return NULL;
			if (!decl_ok(decl)) return poisoned_type;
			return decl->type->canonical;
		}
			break;
		case TYPE_INFO_EXPRESSION:
			if (!sema_resolve_type_info(context, type_info)) return poisoned_type;
			return type_info->type;
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
static inline bool sema_expr_analyse_ct_defined(Context *context, Expr *expr)
{
	if (expr->resolve_status == RESOLVE_DONE) return expr_ok(expr);

	Expr *main_var = expr->ct_call_expr.main_var;
	Type *type = NULL;
	Decl *decl = NULL;
	ExprFlatElement *path = expr->ct_call_expr.flat_path;
	switch (main_var->expr_kind)
	{
		case EXPR_CONST_IDENTIFIER:
		case EXPR_IDENTIFIER:
			// 2. An identifier does a lookup
			decl = sema_resolve_normal_symbol(context, main_var->identifier_expr.identifier, main_var->identifier_expr.path, false);
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
		default:
			if (!sema_analyse_expr_lvalue(context, main_var)) return false;
			if (main_var->expr_kind == EXPR_TYPEINFO)
			{
				type = expr->type_expr->type;
				break;
			}
			if (main_var->expr_kind != EXPR_CONST || main_var->const_expr.const_kind != CONST_STRING)
			{
				SEMA_ERROR(main_var, "A constant string containing an identifier or type was expected here.");
				return false;
			}
			if (!sema_analyse_identifier_path_string(context,
			                                         expr->span,
			                                         main_var,
			                                         &decl,
			                                         &type,
			                                         &path,
			                                         true))
			{
				return false;
			}
			break;
	}

	VECEACH(path, i)
	{
		ExprFlatElement element = path[i];
		Type *actual_type = type_flatten_distinct(type);
		if (element.array)
		{
			if (actual_type->type_kind != TYPE_ARRAY)
			{
				SEMA_ERROR(expr, "It's possible to index into a non fixed size array.");
				return false;
			}
			type = actual_type->array.base;
			continue;
		}
		if (!type_is_structlike(actual_type)) goto NOT_DEFINED;
		Decl *member;
		SCOPE_START
			add_members_to_context(context, actual_type->decl);
			member = sema_resolve_symbol_in_current_dynamic_scope(context, element.ident);
		SCOPE_END;
		if (!member) goto NOT_DEFINED;
		type = member->type;
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


static inline bool sema_expr_analyse_ct_offsetof(Context *context, Expr *expr)
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
		ExprFlatElement element = path[i];
		Type *actual_type = type_flatten_distinct(type);
		if (element.array)
		{
			if (actual_type->type_kind != TYPE_ARRAY)
			{
				SEMA_ERROR(expr, "It's possible to index into a non fixed size array.");
				return false;
			}
			type = actual_type->array.base;
			offset += type_size(type) * (ArraySize)element.index;
			continue;
		}
		if (!type_is_structlike(actual_type))
		{
			if (i == 0)
			{
				SEMA_ERROR(main_var, "%s has no members.", type_quoted_error_string(type));
			}
			else
			{
				SEMA_ERROR(expr, "There is no such member in %s.", type_quoted_error_string(type));
			}
			return false;
		}
		Decl *member;
		SCOPE_START
			add_members_to_context(context, actual_type->decl);
			member = sema_resolve_symbol_in_current_dynamic_scope(context, element.ident);
		SCOPE_END;
		if (!member)
		{
			SEMA_ERROR(expr, "There is no such member in %s.", type_quoted_error_string(type));
			return false;
		}
		type = member->type;
		offset += member->offset;
	}

	expr_rewrite_to_int_const(expr, type_iptrdiff, offset, true);

	return true;
}

static inline bool sema_expr_analyse_ct_call(Context *context, Expr *expr)
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

static inline BuiltinFunction builtin_by_name(const char *name)
{
	for (unsigned i = 0; i < NUMBER_OF_BUILTINS; i++)
	{
		if (builtin_list[i] == name) return (BuiltinFunction)i;
	}
	return BUILTIN_NONE;
}

static inline bool sema_expr_analyse_builtin(Context *context, Expr *expr, bool throw_error)
{
	const char *builtin_char = TOKSTR(expr->builtin_expr.identifier);

	BuiltinFunction func = builtin_by_name(builtin_char);

	if (func == BUILTIN_NONE)
	{
		if (throw_error) SEMA_TOKEN_ERROR(expr->builtin_expr.identifier, "Unsupported builtin '%s'.", builtin_char);
		return false;
	}

	expr->builtin_expr.builtin = func;
	return true;
}

static inline bool sema_analyse_expr_dispatch(Context *context, Expr *expr)
{
	switch (expr->expr_kind)
	{
		case EXPR_COND:
		case EXPR_UNDEF:
		case EXPR_DESIGNATOR:
		case EXPR_MACRO_BODY_EXPANSION:
		case EXPR_FLATPATH:
		case EXPR_NOP:
		case EXPR_TRY_UNWRAP_CHAIN:
		case EXPR_TRY_UNWRAP:
		case EXPR_CATCH_UNWRAP:
		case EXPR_PTR:
		case EXPR_VARIANTSWITCH:
			UNREACHABLE
		case EXPR_ARGV_TO_SUBARRAY:
			expr->type = type_get_subarray(type_get_subarray(type_char));
			return true;
		case EXPR_DECL:
			if (!sema_analyse_var_decl(context, expr->decl_expr, true)) return false;
			expr->type = expr->decl_expr->type;
			return true;
		case EXPR_BUILTIN:
			return sema_expr_analyse_builtin(context, expr, true);
		case EXPR_CT_CALL:
			return sema_expr_analyse_ct_call(context, expr);
		case EXPR_HASH_IDENT:
			return sema_expr_analyse_hash_identifier(context, expr);
		case EXPR_CT_IDENT:
			return sema_expr_analyse_ct_identifier(context, expr);
		case EXPR_FAILABLE:
			return sema_expr_analyse_failable(context, expr);
		case EXPR_PLACEHOLDER:
			return sema_expr_analyse_placeholder(context, expr);
		case EXPR_POISONED:
			return false;
		case EXPR_LEN:
		case EXPR_SLICE_ASSIGN:
		case EXPR_TYPEOFANY:
			// Created during semantic analysis
			UNREACHABLE
		case EXPR_MACRO_BLOCK:
		case EXPR_SCOPED_EXPR:
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
		case EXPR_OR_ERROR:
			return sema_expr_analyse_or_error(context, expr);
		case EXPR_COMPOUND_LITERAL:
			return sema_expr_analyse_compound_literal(context, expr);
		case EXPR_EXPR_BLOCK:
			return sema_expr_analyse_expr_block(context, expr);
		case EXPR_RETHROW:
			return sema_expr_analyse_rethrow(context, expr);
		case EXPR_CONST:
			return true;
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
		case EXPR_MACRO_EXPANSION:
			return sema_expr_analyse_macro_expansion(context, expr);
		case EXPR_CONST_IDENTIFIER:
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
			return sema_expr_analyse_initializer_list(context, NULL, expr);
		case EXPR_CAST:
			return sema_expr_analyse_cast(context, expr);
		case EXPR_EXPRESSION_LIST:
			return sema_expr_analyse_expr_list(context, expr);
	}
	UNREACHABLE
}


bool sema_analyse_cond_expr(Context *context, Expr *expr)
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


bool sema_analyse_expr_rhs(Context *context, Type *to, Expr *expr, bool allow_failable)
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


static inline bool sema_cast_ct_ident_rvalue(Context *context, Expr *expr)
{
	Decl *decl = expr->ct_ident_expr.decl;
	Expr *copy = copy_expr(decl->var.init_expr);
	if (!sema_analyse_expr(context, copy)) return false;
	expr_replace(expr, copy);
	return true;
}

static inline bool sema_cast_rvalue(Context *context, Expr *expr)
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
		case EXPR_MACRO_EXPANSION:
			SEMA_ERROR(expr, "Expected macro followed by (...).", expr->ct_macro_ident_expr.identifier);
			return expr_poison(expr);
		case EXPR_CT_IDENT:
			if (!sema_cast_ct_ident_rvalue(context, expr)) return false;
			break;
		case EXPR_IDENTIFIER:
		case EXPR_CONST_IDENTIFIER:
			if (!sema_cast_ident_rvalue(context, expr)) return false;
			break;
		default:
			break;
	}
	return true;
}

bool sema_analyse_expr_lvalue(Context *context, Expr *expr)
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

MemberIndex sema_get_initializer_const_array_size(Context *context, Expr *initializer, bool *may_be_array, bool *is_const_size)
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

bool sema_analyse_expr(Context *context, Expr *expr)
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
					if (!expr_is_simple(expr->binary_expr.left) || !expr_is_simple(expr->binary_expr.right)) return;
					expr->type = infer_type;
					expr->binary_expr.widen = true;
					return;
				default:
					return;
			}
		case EXPR_OR_ERROR:
			if (!expr_is_simple(expr->or_error_expr.expr)) return;
			if (!expr->or_error_expr.is_jump && !expr_is_simple(expr->or_error_expr.or_error_expr)) return;
			expr->type = infer_type;
			expr->or_error_expr.widen = true;
			return;
		case EXPR_GROUP:
			insert_widening_type(expr->inner_expr, infer_type);
			return;
		case EXPR_TERNARY:
			if (!expr_is_simple(expr->ternary_expr.else_expr)) return;
			if (expr->ternary_expr.then_expr && !expr_is_simple(expr->ternary_expr.else_expr)) return;
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
bool sema_analyse_inferred_expr(Context *context, Type *infer_type, Expr *expr)
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
		case EXPR_INITIALIZER_LIST:
		case EXPR_DESIGNATED_INITIALIZER_LIST:
			if (!sema_expr_analyse_initializer_list(context, infer_type, expr)) return expr_poison(expr);
			break;
		case EXPR_CONST_IDENTIFIER:
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

