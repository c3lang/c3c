// Copyright (c) 2019-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.
//
// This source file contains functions related to both explicit and
// implicit conversions. C3 has a fairly complex set of rules,
// which makes this code somewhat lengthy.

#include "sema_internal.h"

typedef struct
{
	bool is_binary_conversion;
	SemaContext *context;
	Expr *expr;
	Type *from;
	Type *to_type;
	Type *to;
	ConvGroup from_group;
	ConvGroup to_group;
} CastContext;

#define RETURN_CAST_ERROR(_node, ...) do { print_error_at((_node)->span, __VA_ARGS__); sema_print_inline(cc->context); return false; } while (0)

static bool sema_error_const_int_out_of_range(CastContext *cc, Expr *expr, Expr *problem, Type *to_type);
static Expr *recursive_may_narrow(Expr *expr, Type *type);
static void expr_recursively_rewrite_untyped_list(Expr *expr, Expr **list);
static void vector_const_initializer_convert_to_type(SemaContext *context, ConstInitializer *initializer, Type *to_type);
static bool cast_is_allowed(CastContext *cc, bool is_explicit, bool is_silent);

static bool cast_if_valid(SemaContext *context, Expr *expr, Type *to_type, bool is_explicit, bool is_silent,
                          bool is_binary_conversion);
INLINE ConvGroup type_to_group(Type *type);
INLINE void cast_context_set_from(CastContext *cc, Type *new_from);
INLINE void cast_context_set_to(CastContext *cc, Type *new_to);

typedef bool(*CastRule)(CastContext *cc, bool is_explicit, bool is_silent);
typedef void(*CastFunction)(SemaContext *context, Expr *expr, Type *to_type);
extern CastFunction cast_function[CONV_LAST + 1][CONV_LAST + 1];
extern CastRule cast_rules[CONV_LAST + 1][CONV_LAST + 1];


/**
 * Try to make an implicit cast. Optional types are allowed.
 */
bool cast_implicit(SemaContext *context, Expr *expr, Type *to_type, bool is_binary)
{
	return cast_if_valid(context, expr, to_type, false, false, is_binary);
}

bool cast_implicit_binary(SemaContext *context, Expr *expr, Type *to_type, bool is_silent)
{
	return cast_if_valid(context, expr, to_type, false, is_silent, true);
}
/**
 * Try to make an explicit cast, Optional types are allowed.
 */
bool cast_explicit(SemaContext *context, Expr *expr, Type *to_type)
{
	return cast_if_valid(context, expr, to_type, true, false, false);
}

/**
 * Try to make an explicit cast, Optional types are allowed.
 */
bool cast_explicit_silent(SemaContext *context, Expr *expr, Type *to_type)
{
	return cast_if_valid(context, expr, to_type, true, true, false);
}
/**
 * Silent implicit casting will attempt a cast, but will silently back out if it fails.
 */
bool cast_implicit_silent(SemaContext *context, Expr *expr, Type *to_type, bool is_binary_conversion)
{
	return cast_if_valid(context, expr, to_type, false, true, is_binary_conversion);
}


bool may_cast(SemaContext *context, Expr *expr, Type *to_type, bool is_explicit, bool is_silent)
{
	Type *from_type = expr->type->canonical;
	Type *to = to_type->canonical;
	CastContext cc = {
			.from_group = type_to_group(from_type),
			.from = from_type,
			.to_group = type_to_group(to),
			.to = to,
			.to_type = to_type,
			.expr = expr,
			.context = context
	};
	return cast_is_allowed(&cc, is_explicit, is_silent);
}

static bool cast_is_allowed(CastContext *cc, bool is_explicit, bool is_silent)
{
	Type *from_type = cc->from;
	ASSERT0(from_type == from_type->canonical);
	// Check simple equality.
	from_type = from_type->canonical;
	if (from_type == cc->to) return true;

	// Make sure they have the same group.
	ConvGroup from_group = cc->from_group;
	ConvGroup to_group = cc->to_group;
	CastRule rule = (from_group == CONV_NO || to_group == CONV_NO) ? NULL : cast_rules[from_group][to_group];

	// No rule => no
	if (!rule)
	{
		if (is_silent) return false;
		if (type_is_inner_type(cc->to_type))
		{
			RETURN_CAST_ERROR(cc->expr, "You cannot cast %s to the inner type %s.", type_quoted_error_string(cc->expr->type), type_quoted_error_string(cc->to_type));
		}
		RETURN_CAST_ERROR(cc->expr, "You cannot cast %s to %s.", type_quoted_error_string(cc->expr->type), type_quoted_error_string(cc->to_type));
	}

	return rule(cc, is_explicit, is_silent);
}

/**
 * Perform the cast with no additional checks. Casting from untyped not allowed.
 */
void cast_no_check(SemaContext *context, Expr *expr, Type *to_type, bool add_optional)
{
	Type *to = type_flatten(to_type);
	Type *from = type_flatten(expr->type);
	if (from == to)
	{
		expr->type = type_add_optional(to_type, add_optional);
		return;
	}
	ConvGroup to_group = type_to_group(to);
	ConvGroup from_group = type_to_group(from);
	CastFunction func = cast_function[from_group][to_group];
	if (func)
	{
		func(context, expr, to_type);
		expr->type = type_add_optional(expr->type, add_optional);
		return;
	}
	error_exit("Trying cast function from %s to %s\n", type_quoted_error_string(expr->type), type_quoted_error_string(to_type));
}

/**
 * Given lhs and rhs, promote to the maximum bit size, this will retain
 * signed/unsigned type of each side.
 */
void cast_to_int_to_max_bit_size(SemaContext *context, Expr *lhs, Expr *rhs, Type *left_type, Type *right_type)
{
	unsigned bit_size_left = left_type->builtin.bitsize;
	unsigned bit_size_right = right_type->builtin.bitsize;

	ASSERT0(bit_size_left && bit_size_right);

	// Simple case they are the same size, just return.
	if (bit_size_left == bit_size_right) return;

	// Lhs is smaller than rhs, so widen it using the right type
	if (bit_size_left < bit_size_right)
	{

		Type *to = lhs->type->type_kind < TYPE_U8
		           ? type_int_signed_by_bitsize(bit_size_right)
		           : type_int_unsigned_by_bitsize(bit_size_right);
		cast_no_check(context, lhs, to, IS_OPTIONAL(lhs));
		return;
	}

	// Rhs is smaller, do the same thing as above but with the rhs.
	Type *to = rhs->type->type_kind < TYPE_U8
	           ? type_int_signed_by_bitsize(bit_size_left)
	           : type_int_unsigned_by_bitsize(bit_size_left);
	cast_no_check(context, rhs, to, IS_OPTIONAL(rhs));
}

/**
 * Perform vararg promotions typical for C style varargs:
 * 1. Widen int and bool to C int size
 * 2. Widen float and smaller to double
 * 3. Turn slices into pointers
 */
void cast_promote_vararg(SemaContext *context, Expr *arg)
{
	// Remove things like distinct, optional, enum etc.
	Type *arg_type = type_flatten(arg->type);

	// 1. Promote any integer or bool to at least CInt
	if (type_is_promotable_int_bool(arg_type))
	{
		cast_no_check(context, arg, type_cint, IS_OPTIONAL(arg));
		return;
	}

	// 2. Promote any float to at least double
	if (type_is_promotable_float(arg_type))
	{
		cast_no_check(context, arg, type_double, IS_OPTIONAL(arg));
		return;
	}

	// 3. Turn slices into pointers
	if (arg_type->type_kind == TYPE_SLICE)
	{
		cast_no_check(context, arg, type_get_ptr(arg_type->array.base), IS_OPTIONAL(arg));
		return;
	}

}

/**
 * General error due to casts.
 */
bool sema_error_failed_cast(SemaContext *context, Expr *expr, Type *from, Type *to)
{
	RETURN_SEMA_ERROR(expr, "The cast %s to %s is not allowed.", type_quoted_error_string(from), type_quoted_error_string(to));
}

/**
 * Create a type by inferring the length.
 */
Type *type_infer_len_from_actual_type(Type *to_infer, Type *actual_type)
{
	// This may be called on types not inferrable,
	// if so we assume the original type
	if (!type_len_is_inferred(to_infer)) return to_infer;

	// Handle int[*]! a = { ... } by stripping the optional.
	bool is_optional = type_is_optional(to_infer);

	assert((is_optional || !type_is_optional(actual_type)) && "int[*] x = { may_fail } should have been caught.");

	// Strip the optional
	if (is_optional) to_infer = to_infer->optional;

	// And from the actual type.
	actual_type = type_no_optional(actual_type);

	Type *actual = type_get_indexed_type(actual_type);
	if (!actual) return actual_type;

	// Grab the underlying indexed type,
	// because we can only have [*] [] [<*>] [<>] * here
	Type *indexed = type_get_indexed_type(to_infer);

	// We should always have indexed types.
	ASSERT0(indexed);

	// The underlying type may also be inferred.
	// In this case, infer it.
	if (type_len_is_inferred(indexed))
	{
		// if we have int[*][*] => the inner is int[*], we cast it here.
		indexed = type_infer_len_from_actual_type(indexed, actual);
	}

	// Construct the real type
	switch (to_infer->type_kind)
	{
		case TYPE_POINTER:
			// The case of int[*]* x = ...
			return type_add_optional(type_get_ptr(indexed), is_optional);
		case TYPE_ARRAY:
			// The case of int[*][2] x = ...
			return type_add_optional(type_get_array(indexed, to_infer->array.len), is_optional);
		case TYPE_INFERRED_ARRAY:
			ASSERT0(type_is_arraylike(type_flatten(actual_type)));
			return type_add_optional(type_get_array(indexed, type_flatten(actual_type)->array.len), is_optional);
		case TYPE_INFERRED_VECTOR:
			ASSERT0(type_is_arraylike(type_flatten(actual_type)));
			return type_add_optional(type_get_vector(indexed, type_flatten(actual_type)->array.len), is_optional);
		case TYPE_SLICE:
			return type_add_optional(type_get_slice(indexed), is_optional);
		case TYPE_VECTOR:
			// The case of int[*]*[<2>] x = ...
			return type_add_optional(type_get_vector(indexed, to_infer->array.len), is_optional);
		default:
			UNREACHABLE
	}
}

static bool cast_if_valid(SemaContext *context, Expr *expr, Type *to_type, bool is_explicit, bool is_silent,
                          bool is_binary_conversion)
{
	Type *from_type = expr->type;

	if (from_type == to_type) return true;

	if (to_type->canonical->type_kind == TYPE_POINTER && from_type->canonical->type_kind != TYPE_POINTER
	    && to_type->canonical->pointer == from_type->canonical && expr->expr_kind == EXPR_IDENTIFIER
	    && expr->identifier_expr.was_ref)
	{
		if (is_silent) return false;
		RETURN_SEMA_ERROR(expr, "A macro ref parameter is a dereferenced pointer ('*&foo'). You can prefix it"
		                        " with '&' to pass it as a pointer.");
	}


	bool is_void_silence = type_is_void(to_type) && is_explicit;
	bool add_optional = type_is_optional(to_type) || type_is_optional(from_type);
	from_type = type_no_optional(from_type);
	to_type = type_no_optional(to_type);

	if (is_void_silence && from_type != type_untypedlist)
	{
		expr_rewrite_discard(expr);
		return true;
	}

	from_type = from_type->canonical;
	Type *to = to_type->canonical;
	CastContext cc = {
			.is_binary_conversion = is_binary_conversion,
			.from_group = type_to_group(from_type),
			.from = from_type,
			.to_group = type_to_group(to),
			.to = to,
			.to_type = to_type,
			.expr = expr,
			.context = context
	};
	if (!sema_resolve_type_decl(context, to)) return false;
	if (!cast_is_allowed(&cc, is_explicit, is_silent))
	{
		return false;
	}

	cast_no_check(context, expr, to_type, add_optional);
	return true;
}


/**
 * Check whether an expression may narrow.
 * 1. If it has an intrinsic type, then compare it against the type. If the bitwidth is smaller or same => ok
 * 2. If it is a constant, then if it fits in the type it is ok.
 * 3. If it has sub expressions, recursively check those if they affect the type.
 * 4. Widening casts are ignored, all other casts are opaque.
 */
Expr *recursive_may_narrow(Expr *expr, Type *type)
{
RETRY:
	switch (expr->expr_kind)
	{
		case EXPR_RECAST:
			expr = expr->inner_expr;
			goto RETRY;
		case EXPR_BITASSIGN:
		case EXPR_BINARY:
			switch (expr->binary_expr.operator)
			{
				case BINARYOP_ERROR:
					UNREACHABLE
				case BINARYOP_MULT:
				case BINARYOP_SUB:
				case BINARYOP_ADD:
				case BINARYOP_DIV:
				case BINARYOP_MOD:
				case BINARYOP_BIT_OR:
				case BINARYOP_BIT_XOR:
				case BINARYOP_BIT_AND:
				case BINARYOP_ELSE:
				{
					// *, -, +, /, %, |, ^, &, ?? -> check both sides.
					Expr *res = recursive_may_narrow(exprptr(expr->binary_expr.left), type);
					if (res) return res;
					expr = exprptr(expr->binary_expr.right);
					goto RETRY;
				}
				case BINARYOP_SHR:
				case BINARYOP_SHL:
				case BINARYOP_ASSIGN:
				case BINARYOP_ADD_ASSIGN:
				case BINARYOP_BIT_AND_ASSIGN:
				case BINARYOP_BIT_OR_ASSIGN:
				case BINARYOP_BIT_XOR_ASSIGN:
				case BINARYOP_DIV_ASSIGN:
				case BINARYOP_MOD_ASSIGN:
				case BINARYOP_MULT_ASSIGN:
				case BINARYOP_SHR_ASSIGN:
				case BINARYOP_SHL_ASSIGN:
				case BINARYOP_SUB_ASSIGN:
					// For shifts and assignment, ignore the right hand side.
					expr = exprptr(expr->binary_expr.left);
					goto RETRY;
				case BINARYOP_AND:
				case BINARYOP_OR:
				case BINARYOP_GT:
				case BINARYOP_GE:
				case BINARYOP_LT:
				case BINARYOP_LE:
				case BINARYOP_NE:
				case BINARYOP_EQ:
					// This type is bool, so check should never happen.
					UNREACHABLE
				case BINARYOP_CT_OR:
				case BINARYOP_CT_AND:
				case BINARYOP_CT_CONCAT:
					// This should be folded already.
					UNREACHABLE
				case BINARYOP_VEC_GT:
				case BINARYOP_VEC_GE:
				case BINARYOP_VEC_LT:
				case BINARYOP_VEC_LE:
				case BINARYOP_VEC_NE:
				case BINARYOP_VEC_EQ:
					// Functions
					return false;

			}
			UNREACHABLE
		case EXPR_SLICE_LEN:
			if (type_size(type) < type_size(type_cint)) return expr;
			return NULL;
		case EXPR_BUILTIN_ACCESS:
			switch (expr->builtin_access_expr.kind)
			{
				case ACCESS_TYPEOFANYFAULT:
				case ACCESS_TYPEOFANY:
				case ACCESS_ENUMNAME:
				case ACCESS_FAULTNAME:
				case ACCESS_FAULTORDINAL:
					// For the rest, just check size.
					goto CHECK_SIZE;
			}
			UNREACHABLE;
		case EXPR_EXPRESSION_LIST:
			// Only the last expression counts for narrowing.
			// It's unclear if this can happen.
			expr = VECLAST(expr->expression_list);
			goto RETRY;
		case EXPR_TERNARY:
		{
			// In the case a ?: b -> check a and b
			// In the case a ? b : c -> check b and c
			Expr *res = recursive_may_narrow(exprptr(expr->ternary_expr.then_expr
					? expr->ternary_expr.then_expr
					: expr->ternary_expr.cond), type);
			if (res) return res;
			expr = exprptr(expr->ternary_expr.else_expr);
			goto RETRY;
		}
		case EXPR_EXT_TRUNC:
			if (type_size(type) >= type_size(expr->type))
			{
				return NULL;
			}
			// Otherwise just look through it.
			expr = expr->ext_trunc_expr.inner;
			goto RETRY;
		case EXPR_CAST:
			UNREACHABLE
		case EXPR_CONST:
			// For constants, just check that they will fit.
			if (type_is_integer(type))
			{
				ASSERT0(expr->const_expr.const_kind == CONST_INTEGER || expr->const_expr.const_kind == CONST_ENUM);
				if (expr_const_will_overflow(&expr->const_expr, type_flatten(type)->type_kind))
				{
					return expr;
				}
				return NULL;
			}
			ASSERT0(type_is_float(type));
			ASSERT0(expr->const_expr.const_kind == CONST_FLOAT);
			if (!expr_const_float_fits_type(&expr->const_expr, type_flatten(type)->type_kind))
			{
				return expr;
			}
			return NULL;
		case EXPR_POST_UNARY:
			expr = expr->unary_expr.expr;
			goto RETRY;
		case EXPR_FORCE_UNWRAP:
			expr = expr->inner_expr;
			goto RETRY;
		case EXPR_RETHROW:
			expr = expr->rethrow_expr.inner;
			goto RETRY;
		case EXPR_UNARY:
		{
			switch (expr->unary_expr.operator)
			{
				case UNARYOP_ERROR:
				case UNARYOP_ADDR:
				case UNARYOP_NOT:
				case UNARYOP_TADDR:
					UNREACHABLE
				case UNARYOP_DEREF:
					// Check sizes.
					goto CHECK_SIZE;
				case UNARYOP_PLUS:
				case UNARYOP_NEG:
				case UNARYOP_BITNEG:
				case UNARYOP_INC:
				case UNARYOP_DEC:
					expr = expr->unary_expr.expr;
					goto RETRY;
			}
		}
		default:
			// Check type sizes
			goto CHECK_SIZE;
	}
CHECK_SIZE:
	if (type_size(expr->type) > type_size(type)) return expr;
	return NULL;
}



static bool sema_error_const_int_out_of_range(CastContext *cc, Expr *expr, Expr *problem, Type *to_type)
{
	ASSERT0(expr_is_const(expr));
	if (expr->const_expr.is_character && expr->type->type_kind != TYPE_U128)
	{
		RETURN_CAST_ERROR(problem, "The unicode character U+%04x cannot fit in a %s.", (uint32_t)expr->const_expr.ixx.i.low, type_quoted_error_string(to_type));
	}
	if (expr->const_expr.const_kind == CONST_ENUM)
	{
		RETURN_CAST_ERROR(problem, "The ordinal '%d' is out of range for %s, so you need an explicit cast to truncate the value.",
				   expr->const_expr.enum_err_val->var.index,
				   type_quoted_error_string(to_type));
	}
	const char *error_value = expr->const_expr.is_hex ? int_to_str(expr->const_expr.ixx, 16, true)
	                                                  : expr_const_to_error_string(&expr->const_expr);
	RETURN_CAST_ERROR(problem, "The value '%s' is out of range for %s, so you need an explicit cast to truncate the value.", error_value,
			   type_quoted_error_string(to_type));
}



/**
 * Recursively change a const list to an initializer list.
 */
static void expr_recursively_rewrite_untyped_list(Expr *expr, Expr **list)
{
	if (!expr_is_const_untyped_list(expr)) return;
	expr->expr_kind = EXPR_INITIALIZER_LIST;
	expr->initializer_list = list;
	expr->resolve_status = RESOLVE_NOT_DONE;
	FOREACH(Expr *, inner, list)
	{
		expr_recursively_rewrite_untyped_list(inner, inner->const_expr.untyped_list);
	}
}


bool cast_to_index(SemaContext *context, Expr *index, Type *subscripted_type)
{
	Type *type = index->type;
	RETRY:
	type = type_flat_distinct_inline(type);
	type = type_no_optional(type);
	switch (type->type_kind)
	{
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
			return cast_explicit(context, index, type_isz);
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
			return cast_explicit(context, index, type_usz);
		case TYPE_U128:
			SEMA_ERROR(index, "You need to explicitly cast this to a uint or ulong.");
			return false;
		case TYPE_I128:
			SEMA_ERROR(index, "You need to explicitly cast this to an int or long.");
			return false;
		case TYPE_ENUM:
			type = type->decl->enums.type_info->type;
			goto RETRY;
		default:
			RETURN_SEMA_ERROR(index, "An integer value was expected here, but it is a value of type %s, which can't be implicitly converted into an integer index.",
			                  type_quoted_error_string(index->type));
	}
}

Type *cast_numeric_arithmetic_promotion(Type *type)
{
	if (!type) return NULL;
	Type *canonical = type->canonical;
	switch (canonical->type_kind)
	{
		case ALL_SIGNED_INTS:
			if (canonical->builtin.bitsize < compiler.platform.width_c_int) return type_cint;
			return type;
		case ALL_UNSIGNED_INTS:
			if (canonical->builtin.bitsize < compiler.platform.width_c_int) return type_cuint;
			return type;
		case TYPE_BF16:
		case TYPE_F16:
			// Promote F16 to a real type.
			return type_float;
		case TYPE_OPTIONAL:
			UNREACHABLE
		default:
			return type;
	}
}

static bool report_cast_error(CastContext *cc, bool may_cast_explicit)
{
	Expr *expr = cc->expr;
	Type *to = cc->to_type;
	if (type_is_inner_type(to))
	{
		if (may_cast_explicit)
		{
			RETURN_CAST_ERROR(expr,
			                  "Implicitly casting %s to the inner type %s is not permitted, but you may do an explicit cast by placing '($typefrom(%s.typeid))' before the expression.",
			                  type_quoted_error_string(type_no_optional(expr->type)),
			                  type_quoted_error_string(to),
			                  type_to_error_string(type_no_optional(to)));
		}
		else
		{
			RETURN_CAST_ERROR(expr,
			                  "It is not possible to cast %s to the inner type %s.",
			                  type_quoted_error_string(type_no_optional(expr->type)), type_quoted_error_string(to));
		}

	}
	if (may_cast_explicit)
	{
		RETURN_CAST_ERROR(expr,
		           "Implicitly casting %s to %s is not permitted, but you may do an explicit cast by placing '(%s)' before the expression.",
		           type_quoted_error_string(type_no_optional(expr->type)),
		           type_quoted_error_string(to),
		           type_to_error_string(type_no_optional(to)));
	}
	else
	{
		if (to->type_kind == TYPE_INTERFACE)
		{
			if (expr->type->canonical->type_kind != TYPE_POINTER)
			{
				RETURN_CAST_ERROR(expr,
				                  "You can only convert pointers to an interface like %s. "
								  "Try passing the address of the expression instead.",
				                  type_quoted_error_string(to));
			}
		}
		else if (to->type_kind == TYPE_ANY && expr->type->canonical->type_kind != TYPE_POINTER)
		{
			RETURN_CAST_ERROR(expr,  "You can only convert pointers to 'any'. "
									 "Try passing the address of the expression instead.");
		}
		RETURN_CAST_ERROR(expr,
		           "It is not possible to cast %s to %s.",
		           type_quoted_error_string(type_no_optional(expr->type)), type_quoted_error_string(to));
	}
}

INLINE bool sema_cast_error(CastContext *cc, bool may_cast_explicit, bool is_silent)
{
	if (is_silent) return false;
	report_cast_error(cc, may_cast_explicit);
	return false;
}

// RULES ----

static TypeCmpResult match_pointers(CastContext *cc, Type *to_ptr, Type *from_ptr, bool flatten, bool is_silent)
{
	return type_is_pointer_equivalent(cc->context, to_ptr, from_ptr, flatten);
}

static bool rule_voidptr_to_any(CastContext *cc, bool is_explicit, bool is_silent)
{
	if (expr_is_const_pointer(cc->expr) && !cc->expr->const_expr.ptr) return true;
	RETURN_CAST_ERROR(cc->expr,
	                  "Casting a 'void*' to %s is not permitted (except when the 'void*' is a constant null).",
	                  type_quoted_error_string(cc->to));
}

static bool rule_ptr_to_ptr(CastContext *cc, bool is_explicit, bool is_silent)
{
	if (is_explicit) return true;
	switch (match_pointers(cc, cc->to, cc->from, is_silent, false))
	{
		case TYPE_SAME:
			return true;
		case TYPE_ERROR:
			return false;
		case TYPE_ALIGNMENT_INCREASE:
			if (is_explicit) return false;
			ASSERT0(!is_explicit);
			RETURN_CAST_ERROR(cc->expr,
			                  "Implicitly casting %s (alignment %d) to %s (alignment %d) is not permitted, "
							  "it would require an explicit cast. Before using an explicit cast, please make "
							  "sure you understand the ramifications as the explicit cast might crash your program if used incorrectly.",
			                  type_quoted_error_string(type_no_optional(cc->expr->type)),
			                  type_abi_alignment(type_get_indexed_type(cc->expr->type)),
							  type_quoted_error_string(cc->to),
							  type_abi_alignment(type_get_indexed_type(cc->to)));
		case TYPE_MISMATCH:
		case TYPE_SAME_INT_SIZE:
			return sema_cast_error(cc, true, is_silent);
	}
	UNREACHABLE
}


static bool rule_all_ok(CastContext *cc, bool is_explicit, bool silent)
{
	return true;
}

static bool rule_int_to_ptr(CastContext *cc, bool is_explicit, bool is_silent)
{
	// Handle const:
	Expr *expr = cc->expr;
	if (sema_cast_const(expr))
	{
		if (!is_explicit) return sema_cast_error(cc, true, is_silent);

		// For if the type doesn't fit, insert an error.
		Int i = expr->const_expr.ixx;
		if (!int_fits(i, type_uptr->canonical->type_kind))
		{
			if (is_silent) return false;
			RETURN_CAST_ERROR(expr, "'%s' does not fit in a pointer.", int_to_str(i, 16, true));
		}
		return true;
	}

	if (type_size(cc->from) < type_size(type_iptr))
	{
		if (is_silent) return false;
		RETURN_CAST_ERROR(expr, "You cannot convert an integer smaller than a pointer size to a pointer.");
	}

	if (!is_explicit) return sema_cast_error(cc, true, is_silent);

	return true;
}


static bool rule_ptr_to_int(CastContext *cc, bool is_explicit, bool is_silent)
{
	bool too_small = type_size(cc->to) < type_size(type_uptr);
	if (!is_explicit) return sema_cast_error(cc, !too_small, is_silent);

	// The type must be uptr or bigger.
	if (too_small)
	{
		if (is_silent) return false;
		RETURN_CAST_ERROR(cc->expr, "Casting %s to %s is not allowed because '%s' is smaller than a pointer. "
		                            "Use (%s)(iptr) if you want this lossy cast.",
		                  type_quoted_error_string(cc->expr->type), type_quoted_error_string(cc->to_type),
		                  type_to_error_string(cc->to_type), type_to_error_string(cc->to_type));
	}
	return true;
}

static bool rule_arrptr_to_slice(CastContext *cc, bool is_explicit, bool is_silent)
{
	Type *slice_base = cc->to->array.base;
	Type *from_base = cc->from->pointer->array.base;

	// int[<2>]*, int[2]*
	if (is_explicit)
	{
		slice_base = type_flatten(slice_base);
		from_base = type_flatten(from_base);
	}
	// Same base type? E.g. int[2]* -> int[], then we're done.
	if (from_base == slice_base)
	{
		return true;
	}

	if (slice_base->type_kind == TYPE_POINTER && from_base->type_kind == TYPE_POINTER)
	{
		switch (match_pointers(cc, slice_base, from_base, is_explicit, is_silent))
		{
			case TYPE_SAME:
				return true;
			case TYPE_SAME_INT_SIZE:
				if (is_explicit) return true;
				break;
			case TYPE_ERROR:
				return false;
			case TYPE_MISMATCH:
				break;
			default:
				UNREACHABLE
		}
	}
	bool may_explicit = !is_silent && rule_arrptr_to_slice(cc, true, true);
	return sema_cast_error(cc, may_explicit, is_silent);
}

static bool rule_ulist_to_struct(CastContext *cc, bool is_explicit, bool is_silent)
{
	ASSERT0(expr_is_const_untyped_list(cc->expr));
	Expr **expressions = cc->expr->const_expr.untyped_list;
	unsigned size = vec_size(expressions);
	if (!size) return true;
	Decl *strukt = cc->to->decl;
	Decl **members = strukt->strukt.members;
	if (size != vec_size(members))
	{
		if (is_silent) return false;
		RETURN_CAST_ERROR(cc->expr, "%s may only be initialized with 0 elements or %d, not %d.",
		                  type_quoted_error_string(cc->to_type),
		                  vec_size(members), size);
	}
	if (!sema_analyse_decl(cc->context, strukt)) return false;
	FOREACH_IDX(i, Expr *, expr, expressions)
	{
		if (!may_cast(cc->context, expr, members[i]->type, false, is_silent)) return false;
	}
	return true;
}

static bool rule_ulist_to_vecarr(CastContext *cc, bool is_explicit, bool is_silent)
{
	Expr **expressions = cc->expr->const_expr.untyped_list;
	unsigned size = vec_size(expressions);
	if (!size) return true;
	if (size != cc->to->array.len)
	{
		if (is_silent) return false;
		RETURN_CAST_ERROR(cc->expr, "%s may only be initialized with 0 elements or %d, not %d.",
		                  type_quoted_error_string(cc->to_type),
						  cc->to->array.len, size);
	}
	Type *base = cc->to->array.base;
	FOREACH(Expr *, expr, expressions)
	{
		if (!may_cast(cc->context, expr, base, false, is_silent)) return false;
	}
	return true;
}

static bool rule_ulist_to_slice(CastContext *cc, bool is_explicit, bool is_silent)
{
	ASSERT0(expr_is_const_untyped_list(cc->expr));
	Type *base = cc->to->array.base;
	FOREACH(Expr *, expr, cc->expr->const_expr.untyped_list)
	{
		if (!may_cast(cc->context, expr, base, false, is_silent)) return false;
	}
	return true;
}

static bool rule_ulist_to_inferred(CastContext *cc, bool is_explicit, bool is_silent)
{
	Expr **expressions = cc->expr->const_expr.untyped_list;
	unsigned size = vec_size(expressions);
	if (!size)
	{
		if (is_silent) return false;
		RETURN_CAST_ERROR(cc->expr, "This untyped list would infer to a zero elements, which is not allowed.");
	}
	Type *base = cc->to->array.base;
	FOREACH(Expr *, expr, expressions)
	{
		if (!may_cast(cc->context, expr, base, false, is_silent)) return false;
	}
	return true;
}

static bool rule_slice_to_ptr(CastContext *cc, bool is_explicit, bool is_silent)
{
	Type *slice_base = cc->from->array.base->canonical;
	Type *natural_ptr = type_get_ptr(slice_base);
	switch (match_pointers(cc, natural_ptr, cc->to, is_explicit, is_silent))
	{
		case TYPE_SAME:
			return true;
		case TYPE_SAME_INT_SIZE:
			if (is_explicit || expr_is_const_string(cc->expr)) return true;
			break;
		case TYPE_ERROR:
			return false;
		case TYPE_MISMATCH:
			break;
		default:
			UNREACHABLE
	}
	bool may_explicit = !is_silent && rule_slice_to_ptr(cc, true, true);
	return sema_cast_error(cc, may_explicit, is_silent);
}

static bool rule_slice_to_slice(CastContext *cc, bool is_explicit, bool is_silent)
{
	Type *from_type = cc->from;
	Type *from_base = from_type->array.base;
	Type *array_base = cc->to->array.base;

	// Same base type? Ok
	if (from_base->canonical == array_base->canonical) return true;

	switch (type_array_element_is_equivalent(cc->context, array_base, from_base, is_explicit))
	{
		case TYPE_ERROR:
			return false;
		case TYPE_MISMATCH:
		case TYPE_ALIGNMENT_INCREASE:
			if (is_silent) return false;
			return sema_cast_error(cc, is_explicit ? false : rule_slice_to_slice(cc, true, true), is_silent);
		case TYPE_SAME:
		case TYPE_SAME_INT_SIZE:
			return true;
	}
	UNREACHABLE
}


static bool rule_arr_to_arr(CastContext *cc, bool is_explicit, bool is_silent)
{
	if (!rule_slice_to_slice(cc, is_explicit, is_silent)) return false;
	if (type_flatten(cc->to)->array.len != type_flatten(cc->from)->array.len)
	{
		if (is_silent) return false;
		RETURN_CAST_ERROR(cc->expr, "Arrays of different lengths may not be converted.");
	}
	return true;
}

static bool rule_arr_to_vec(CastContext *cc, bool is_explicit, bool is_silent)
{
	ArraySize len = cc->from->array.len;
	if (len != cc->to->array.len) return sema_cast_error(cc, false, is_silent);
	Type *base = cc->from->array.base;
	switch (type_to_group(type_flatten(base)))
	{
		case CONV_BOOL:
		case CONV_INT:
		case CONV_FLOAT:
		case CONV_POINTER:
		case CONV_FAULT:
		case CONV_ENUM:
		case CONV_TYPEID:
		case CONV_ANYFAULT:
		case CONV_VOIDPTR:
		case CONV_VAPTR:
			break;
		default:
			return sema_cast_error(cc, false, is_silent);
	}
	cast_context_set_from(cc, type_get_vector(base, len));
	return cast_is_allowed(cc, is_explicit, is_silent);
}

static bool rule_vec_to_arr(CastContext *cc, bool is_explicit, bool is_silent)
{
	ArraySize len = cc->from->array.len;
	if (len != cc->to->array.len) return sema_cast_error(cc, false, is_silent);
	Type *base = cc->from->array.base;
	cast_context_set_from(cc, type_get_array(base, len));
	return cast_is_allowed(cc, is_explicit, is_silent);
}

static bool rule_vecarr_to_slice(CastContext *cc, bool is_explicit, bool is_silent)
{
	Expr *expr = cc->expr;
	if (expr_is_const(expr))
	{
		cast_context_set_to(cc, type_get_inferred_array(type_get_indexed_type(cc->to_type)));
		return cast_is_allowed(cc, is_explicit, is_silent);
	}
	if (is_silent) return false;
	RETURN_CAST_ERROR(expr, "Conversions from arrays or vectors to slices are only permitted on constant arrays, use `&arr` or `arr[..]` instead.");
}

static bool rule_slice_to_vecarr(CastContext *cc, bool is_explicit, bool is_silent)
{
	Expr *expr = cc->expr;
	ArrayIndex size = sema_len_from_const(expr);
	if (size < 0)
	{
		if (is_silent) return false;
		RETURN_CAST_ERROR(expr, "Conversions from slices to arrays or vectors are only permitted on constant slices.");
	}
	if (size == 0)
	{
		if (is_silent) return false;
		RETURN_CAST_ERROR(expr, "Zero sized slices can't be converted to arrays or vectors.");
	}
	if (cc->to_group == CONV_ARRAY)
	{
		if (expr_is_const_string(expr) || expr_is_const_bytes(expr))
		{
			if (cc->to->array.len > size) size = cc->to->array.len;
		}
		cast_context_set_from(cc, type_get_array(cc->from->array.base, size));
	}
	else
	{
		cast_context_set_from(cc, type_get_vector(cc->from->array.base, size));
	}
	return cast_is_allowed(cc, is_explicit, is_silent);
}

static bool rule_slice_to_infer(CastContext *cc, bool is_explicit, bool is_silent)
{
	Expr *expr = cc->expr;
	// 1. We might infer something above.
	if (cc->to->type_kind == TYPE_SLICE)
	{
		cast_context_set_from(cc, cc->from->array.base);
		cast_context_set_to(cc, cc->to->array.base);
		return cast_is_allowed(cc, is_explicit, is_silent);
	}
	// 2. Otherwise there is a vector matching.
	ArrayIndex size = sema_len_from_const(expr);
	if (size < 0)
	{
		if (is_silent) return false;
		RETURN_CAST_ERROR(expr, "Conversions from slices to arrays or vectors are only permitted on constant slices.");
	}
	if (size == 0)
	{
		if (is_silent) return false;
		RETURN_CAST_ERROR(expr, "Zero sized slices can't be converted to arrays or vectors.");
	}
	cast_context_set_from(cc, type_get_array(cc->from->array.base, size));
	return cast_is_allowed(cc, is_explicit, is_silent);
}

static bool rule_vecarr_to_infer(CastContext *cc, bool is_explicit, bool is_silent)
{
	Type *new_type = type_infer_len_from_actual_type(cc->to, cc->from);
	cast_context_set_to(cc, new_type);
	return cast_is_allowed(cc, is_explicit, is_silent);
}

static inline bool type_implements_interface(CastContext *cc, Decl *decl, Type *interface)
{
RETRY:;
	FOREACH(TypeInfo *, interface_type, decl->interfaces)
	{
		if (!sema_resolve_type_info(cc->context, interface_type, RESOLVE_TYPE_DEFAULT)) return false;
		if (interface_type->type == interface) return true;
	}
	if (!decl->is_substruct) return false;
	Type *inner;
	if (decl->decl_kind == DECL_DISTINCT)
	{
		inner = decl->distinct->type->canonical;
	}
	else
	{
		ASSERT0(decl->decl_kind == DECL_STRUCT);
		inner = decl->strukt.members[0]->type->canonical;
	}
	if (!type_may_implement_interface(inner)) return false;
	decl = inner->decl;
	goto RETRY;
}
static bool rule_ptr_to_interface(CastContext *cc, bool is_explicit, bool is_silent)
{
	if (is_explicit) return true;

	Type *pointee = cc->from->pointer;
	if (type_may_implement_interface(pointee))
	{
		Type *interface = cc->to;
		if (type_implements_interface(cc, pointee->decl, interface)) return true;
	}
	if (is_silent) return false;
	RETURN_CAST_ERROR(cc->expr, "%s cannot be implicitly cast to %s, but you can use an explicit "
					  "cast to (unsafely) assume the interface is implemented.",
	                  type_quoted_error_string(cc->expr->type), type_quoted_error_string(cc->to_type));
}

static bool rule_interface_to_interface(CastContext *cc, bool is_explicit, bool is_silent)
{
	if (is_explicit) return true;

	Type *from_interface = cc->from;
	Type *interface = cc->to->canonical;
	if (!sema_resolve_type_decl(cc->context, from_interface)) return false;
	FOREACH(TypeInfo *, parent, from_interface->decl->interfaces)
	{
		if (parent->type->canonical == interface) return true;
	}
	if (is_silent) return false;
	RETURN_CAST_ERROR(cc->expr, "%s is not a parent interface of %s, but you can insert an explicit cast '(%s)value' to enforce the (unsafe) conversion.",
	                  type_quoted_error_string(cc->to), type_quoted_error_string(from_interface),
	                  type_to_error_string(cc->to_type));
}

static bool rule_ptr_to_infer(CastContext *cc, bool is_explicit, bool is_silent)
{
	if (cc->to->type_kind != TYPE_POINTER) return sema_cast_error(cc, false, is_silent);

	Type *new_type = type_infer_len_from_actual_type(cc->to, cc->from);
	cast_context_set_to(cc, new_type->pointer->canonical);
	cast_context_set_from(cc, cc->from->pointer);
	return cast_is_allowed(cc, is_explicit, is_silent);
}

static bool rule_explicit_ok(CastContext *cc, bool is_explicit, bool silent)
{
	if (is_explicit) return true;
	if (!silent)
	{
		RETURN_CAST_ERROR(cc->expr, "%s cannot implicitly be converted to %s, but you may use a cast.", type_quoted_error_string(cc->expr->type), type_quoted_error_string(cc->to_type));
	}
	return false;
}


static bool rule_anyfault_to_fault(CastContext *cc, bool is_explicit, bool is_silent)
{
	if (!is_explicit)
	{
		if (is_silent) return false;
		return sema_cast_error(cc, rule_anyfault_to_fault(cc, true, true), false);
	}
	Expr *expr = cc->expr;
	if (!expr_is_const_fault(expr)) return true;
	if (type_flatten(cc->to) == expr->const_expr.enum_err_val->type) return true;
	if (is_silent) return false;
	RETURN_CAST_ERROR(expr, "This expression is known at compile time to be a fault of type %s, so it cannot be cast to %s.",
	                  type_quoted_error_string(expr->const_expr.enum_err_val->type),
	                  type_quoted_error_string(cc->to_type));
}

static bool rule_int_to_float(CastContext *cc, bool is_explicit, bool is_silent)
{
	if (is_explicit) return true;

	Expr *expr = cc->expr;

	if (!expr_is_simple(expr, true))
	{
		if (is_silent) return false;
		RETURN_CAST_ERROR(expr, "This conversion requires an explicit cast to %s, because the widening of the expression may be done in more than one way.",
		           type_quoted_error_string(cc->to_type));
	}
	return true;
}

static bool rule_widen_narrow(CastContext *cc, bool is_explicit, bool is_silent)
{
	if (is_explicit) return true;

	ByteSize to_size = type_size(cc->to);
	ByteSize from_size = type_size(cc->from);

	Expr *expr = cc->expr;
	// If widening, require simple.
	if (to_size > from_size)
	{
		if (expr_is_simple(cc->expr, type_is_float(cc->to))) return true;
		if (is_silent) return false;
		{
			RETURN_CAST_ERROR(expr, "This conversion requires an explicit cast to %s, because the widening of the expression may be done in more than one way.",
							  type_quoted_error_string(cc->to_type));
		}
		return false;
	}

	// If const, check in range.
	if (sema_cast_const(expr) && expr_const_will_overflow(&expr->const_expr, cc->to->type_kind))
	{
		if (!is_silent)
		{
			if (cc->to_group != CONV_INT)
			{
				RETURN_CAST_ERROR(expr, "The value '%s' is out of range for %s, so you need an explicit cast to truncate the value.", expr_const_to_error_string(&expr->const_expr),
				                  type_quoted_error_string(cc->to_type));
			}
			sema_error_const_int_out_of_range(cc, expr, expr, cc->to_type);
		}
		return false;
	}

	// Allow int <-> uint
	if (to_size == from_size) return true;

	// Check if narrowing works
	Expr *problem = recursive_may_narrow(expr, cc->to);
	if (problem)
	{
		if (is_silent) return false;
		// If it's an integer that's the problem, zoom in on that one.
		if (type_is_integer(type_flatten(problem->type))) expr = problem;
		// Otherwise require a cast.
		RETURN_CAST_ERROR(expr, "%s cannot implicitly be converted to %s, but you may use a cast.",
		           type_quoted_error_string(expr->type), type_quoted_error_string(cc->to_type));
	}
	return true;
}

static bool rule_not_applicable(CastContext *cc, bool is_explicit, bool is_silent)
{
	UNREACHABLE
}


static bool rule_from_distinct(CastContext *cc, bool is_explicit, bool is_silent)
{
	Type *from_type = cc->from;
	ASSERT0(from_type == from_type->canonical);

	// Explicit just flattens and tries again.
	if (is_explicit)
	{
		cast_context_set_from(cc, type_flatten(from_type));
		return cast_is_allowed(cc, is_explicit, is_silent);
	}
	// No inline? Then it's an error.
	if (!from_type->decl->is_substruct)
	{
		return sema_cast_error(cc, rule_from_distinct(cc, true, true), is_silent);
	}
	cast_context_set_from(cc, type_flat_distinct_inline(from_type));
	return cast_is_allowed(cc, is_explicit, is_silent);
}

static bool rule_to_distinct(CastContext *cc, bool is_explicit, bool is_silent)
{
	Type *from_type = cc->from;
	ASSERT0(from_type == from_type->canonical);
	Type *flat = type_flatten(cc->to);
	ConvGroup flat_group = type_to_group(flat);
	Expr *expr = cc->expr;
	bool is_const = sema_cast_const(expr);
	// Allow DistinctFooFn a = &myfunc;
	if (!is_const && from_type->type_kind == TYPE_FUNC_PTR
		&& expr->expr_kind == EXPR_UNARY && expr->unary_expr.operator == UNARYOP_ADDR
		&& expr->unary_expr.expr->expr_kind == EXPR_IDENTIFIER
		&& expr->unary_expr.expr->identifier_expr.decl->decl_kind == DECL_FUNC)
	{
		is_const = true;
	}
	if (is_const)
	{
		cc->to = flat;
		cc->to_group = flat_group;

		// If it's silent or explicit, just run it:
		if (is_silent || is_explicit) return cast_is_allowed(cc, is_explicit, is_silent);
		// Loud and implicit:
		if (cast_is_allowed(cc, false, true)) return true;
		return sema_cast_error(cc, cast_is_allowed(cc, true, true), is_silent);
	}

	cc->to = flat;
	cc->to_group = flat_group;
	bool may_cast = cast_is_allowed(cc, true, true);
	if (may_cast && is_explicit) return true;
	return sema_cast_error(cc, may_cast, is_silent);
}

static bool rule_to_from_distinct(CastContext *cc, bool is_explicit, bool is_silent)
{
	// Handle the explicit case, in this case it's allowed if they cast to the same underlying type.
	if (is_explicit)
	{
		cast_context_set_from(cc, type_flatten(cc->from));
		return rule_to_distinct(cc, is_explicit, is_silent);
	}
	// The implicit case, if to is a parent of from, then it is ok.
	if (type_is_subtype(cc->to, cc->from)) return true;
	// It's not possible to inline from, so it's an error.
	if (!cc->from->decl->is_substruct)
	{
		return sema_cast_error(cc, rule_to_from_distinct(cc, true, is_silent), is_silent);
	}
	// Let's to a flattering of the from, and see if it's allowed then.
	cast_context_set_from(cc, type_flat_distinct_inline(cc->from));
	return cast_is_allowed(cc, is_explicit, is_silent);
}

static bool rule_to_struct_to_distinct(CastContext *cc, bool is_explicit, bool is_silent)
{
	Type *from = cc->from;
	// 1. The distinct type is a subtype.
	if (type_is_subtype(cc->to, from)) return true;
	// 2. We don't check for subtype after this, just use regular "to distinct" rules.
	return rule_to_distinct(cc, is_explicit, is_silent);
}

static bool rule_struct_to_struct(CastContext *cc, bool is_explicit, bool is_silent)
{
	Type *from = cc->from;
	// 1. The distinct type is a subtype.
	if (type_is_subtype(cc->to, from)) return true;

	return sema_cast_error(cc, false, is_silent);
}

static bool rule_vec_to_vec(CastContext *cc, bool is_explicit, bool is_silent)
{
	if (cc->from->array.len != cc->to->array.len) return sema_cast_error(cc, false, is_silent);
	Type *from_base = cc->from->array.base;
	cast_context_set_to(cc, cc->to->array.base);
	// Allow bool vectors to expand to any int.
	if (from_base == type_bool && cc->to_group == CONV_INT) return true;
	cast_context_set_from(cc, from_base);
	return cast_is_allowed(cc, is_explicit, is_silent);
}

static bool rule_expand_to_vec(CastContext *cc, bool is_explicit, bool is_silent)
{
	if (!is_explicit && compiler.build.vector_conv == VECTOR_CONV_DEFAULT && !cc->is_binary_conversion)
	{
		if (is_silent) return false;
		bool explicit_works = rule_expand_to_vec(cc, true, true);
		return sema_cast_error(cc, explicit_works, false);
	}
	cast_context_set_to(cc, cc->to->array.base);
	return cast_is_allowed(cc, is_explicit, is_silent);
}

static bool rule_int_to_bits(CastContext *cc, bool is_explicit, bool is_silent)
{
	Type *base_type = cc->to->decl->strukt.container_type->type;
	Type *from_type = cc->from;
	bool success = type_is_integer(base_type) && type_size(from_type) == type_size(base_type);
	if (!is_explicit || !success) return sema_cast_error(cc, success, is_silent);
	return true;
}

static bool rule_arr_to_bits(CastContext *cc, bool is_explicit, bool is_silent)
{
	Type *base_type = cc->to->decl->strukt.container_type->type;
	Type *from_type = cc->from;
	bool success = from_type == base_type;
	if (!is_explicit || !success) return sema_cast_error(cc, success, is_silent);
	return true;
}

static bool rule_int_to_enum(CastContext *cc, bool is_explicit, bool is_silent)
{
	if (!is_explicit) return sema_cast_error(cc, true, is_silent);

	if (!sema_cast_const(cc->expr)) return true;

	Decl *enum_decl = cc->to->decl;
	// Check that the type is within limits.
	unsigned max_enums = vec_size(enum_decl->enums.values);
	Int to_convert = cc->expr->const_expr.ixx;

	// Negative numbers are always wrong.
	if (int_is_neg(to_convert))
	{
		if (!is_silent) RETURN_CAST_ERROR(cc->expr, "A negative number cannot be converted to an enum.");
		return false;
	}

	// Check the max, we don't support more than 4 billion,
	// so we can safely use TYPE_U32.
	Int max = {.i.low = max_enums, .type = TYPE_U32};
	if (int_comp(to_convert, max, BINARYOP_GE))
	{
		if (!is_silent) RETURN_CAST_ERROR(cc->expr, "This value exceeds the number of enums in %s.", enum_decl->name);
		return false;
	}
	return true;
}

static bool rule_bits_to_arr(CastContext *cc, bool is_explicit, bool is_silent)
{
	if (is_silent && !is_explicit) return false;
	Type *base_type = cc->from->decl->strukt.container_type->type->canonical;
	Type *to = cc->to;
	if (base_type != to) return sema_cast_error(cc, false, is_silent);
	if (!is_explicit) return sema_cast_error(cc, true, is_silent);
	return true;
}

static bool rule_bits_to_int(CastContext *cc, bool is_explicit, bool is_silent)
{
	if (is_silent && !is_explicit) return false;
	Type *base_type = cc->from->decl->strukt.container_type->type->canonical;
	Type *to = cc->to;
	if (base_type != to)
	{
		if (!type_is_integer(base_type) || type_size(to) != type_size(base_type))
		{
			return sema_cast_error(cc, false, is_silent);
		}
	}
	if (!is_explicit) return sema_cast_error(cc, true, is_silent);
	return true;
}

// CASTS ----


static void cast_vaptr_to_slice(SemaContext *context, Expr *expr, Type *type)
{
	Type *flat = type_flatten(expr->type);
	ASSERT0(flat->type_kind == TYPE_POINTER);
	flat = flat->pointer;
	ASSERT0(flat->array.len > 0);
	Expr *inner = expr_copy(expr);
	expr->make_slice_expr = (ExprMakeSlice) {.ptr = inner, .len = flat->array.len};
	expr->expr_kind = EXPR_MAKE_SLICE;
	expr->resolve_status = RESOLVE_DONE;
	expr->type = type;
}

static void cast_ptr_to_any(SemaContext *context, Expr *expr, Type *type)
{
	Expr *inner = expr_copy(expr);
	Expr *typeid = expr_copy(expr);
	expr_rewrite_const_typeid(typeid, type_no_optional(expr->type)->canonical->pointer);
	expr->expr_kind = EXPR_MAKE_ANY;
	expr->make_any_expr = (ExprMakeAny) { .inner = inner, .typeid = typeid };
	expr->type = type;
}
static void cast_struct_to_inline(SemaContext *context, Expr *expr, Type *type) { expr_rewrite_addr_conversion(expr, type); }
static void cast_fault_to_anyfault(SemaContext *context, Expr *expr, Type *type) { expr->type = type; };
static void cast_fault_to_ptr(SemaContext *context, Expr *expr, Type *type) { expr_rewrite_to_int_to_ptr(expr, type); }
static void cast_typeid_to_int(SemaContext *context, Expr *expr, Type *type) { expr_rewrite_ext_trunc(expr, type, type_is_signed(type_flatten_to_int(type))); }
static void cast_fault_to_int(SemaContext *context, Expr *expr, Type *type) { cast_typeid_to_int(context, expr, type); }
static void cast_typeid_to_ptr(SemaContext *context, Expr *expr, Type *type) { expr_rewrite_to_int_to_ptr(expr, type); }
static void cast_any_to_bool(SemaContext *context, Expr *expr, Type *type) {
	expr_rewrite_ptr_access(expr, expr_copy(expr), type_voidptr);
	expr_rewrite_int_to_bool(expr, false);
}
static void cast_any_to_ptr(SemaContext *context, Expr *expr, Type *type) { expr_rewrite_ptr_access(expr, expr_copy(expr), type); }
static void cast_all_to_void(SemaContext *context, Expr *expr, Type *to_type) { expr_rewrite_discard(expr); }
static void cast_retype(SemaContext *context, Expr *expr, Type *to_type) { expr->type = to_type; }

static void vector_const_initializer_convert_to_type(SemaContext *context, ConstInitializer *initializer, Type *to_type)
{
	switch (initializer->kind)
	{
		case CONST_INIT_ARRAY:
		{
			Type *element_type = type_flatten(to_type)->array.base;
			FOREACH(ConstInitializer *, element, initializer->init_array.elements)
			{
				vector_const_initializer_convert_to_type(context, element, element_type);
			}
			break;
		}
		case CONST_INIT_ARRAY_FULL:
		{
			Type *element_type = type_flatten(to_type)->array.base;
			FOREACH(ConstInitializer *, element, initializer->init_array_full)
			{
				vector_const_initializer_convert_to_type(context, element, element_type);
			}
			break;
		}
		case CONST_INIT_VALUE:
		{
			Type *to_flat = type_flatten(to_type);
			bool is_neg_conversion = to_flat && initializer->type == type_bool;
			if (is_neg_conversion)
			{
				bool is_true = initializer->init_value->const_expr.b;
				initializer->init_value->const_expr = (ExprConst)
						{
							.const_kind = CONST_INTEGER,
							.ixx = (Int) { .i = is_true ? (Int128) { UINT64_MAX, UINT64_MAX } : (Int128) { 0, 0 }, .type = to_flat->type_kind },
						};
				initializer->init_value->type = to_type;
			}
			else
			{
				cast_no_check(context, initializer->init_value, to_type, IS_OPTIONAL(initializer->init_value));
			}
			break;
		}
		case CONST_INIT_ZERO:
			break;
		case CONST_INIT_UNION:
		case CONST_INIT_STRUCT:
			UNREACHABLE
		case CONST_INIT_ARRAY_VALUE:
			vector_const_initializer_convert_to_type(context, initializer->init_array_value.element, to_type);
			break;
	}
	initializer->type = type_flatten(to_type);
}

/**
 * Insert a PTRPTR cast or update the pointer type
 */
static void cast_ptr_to_ptr(SemaContext *context, Expr *expr, Type *type)
{
	if (!sema_cast_const(expr) || expr->const_expr.const_kind == CONST_STRING)
	{
		expr_rewrite_rvalue(expr, type);
		return;
	}

	// Insert the cast, this removes the ability to narrow it.
	expr->type = type;
	expr->const_expr.is_hex = false;
}


/**
 * Convert any fp to another fp type using CAST_FPFP
 */
static void cast_float_to_float(SemaContext *context, Expr *expr, Type *type)
{
	// Change to same type should never enter here.
	ASSERT0(type_flatten(type) != type_flatten(expr->type));

	// Insert runtime cast if needed.
	if (!sema_cast_const(expr))
	{
		expr_rewrite_ext_trunc(expr, type, true);
		return;
	}

	// Otherwise rewrite the const, which may cause rounding.
	expr_rewrite_const_float(expr, type, expr->const_expr.fxx.f);
}

/**
 * Convert from any floating point to int using CAST_FPINT
 * Const conversion will disable narrowable and hex.
 */
static void cast_float_to_int(SemaContext *context, Expr *expr, Type *type)
{
	if (!sema_cast_const(expr))
	{
		expr_rewrite_to_float_to_int(expr, type);
		return;
	}

	// Run the int->real to and rewrite.
	Real d = expr->const_expr.fxx.f;
	expr->const_expr.ixx = int_from_real(d, type_flatten(type)->type_kind);
	expr->const_expr.const_kind = CONST_INTEGER;
	expr->const_expr.is_character = false;
	expr->type = type;
	expr->const_expr.is_hex = false;
}


/**
 * Convert from integer to enum using CAST_INTENUM / or do a const conversion.
 * This will ensure that the conversion is valid (i.e. in the range 0 .. enumcount - 1)
 */
static void cast_int_to_enum(SemaContext *context, Expr *expr, Type *type)
{
	SEMA_DEPRECATED(expr, "Using casts to convert integers to enums is deprecated in favour of using 'MyEnum.from_ordinal(i)`.");
	Type *canonical = type_flatten(type);
	ASSERT0(canonical->type_kind == TYPE_ENUM);
	if (!sema_cast_const(expr))
	{
		expr_rewrite_enum_from_ord(expr, type);
		return;
	}

	Decl *enum_decl = canonical->decl;
	// Fold the const into the actual enum.
	// Check is already done.
	Decl *decl = enum_decl->enums.values[expr->const_expr.ixx.i.low];
	ASSERT0(decl->resolve_status == RESOLVE_DONE);
	expr->const_expr = (ExprConst) {
			.enum_err_val = decl,
			.const_kind = CONST_ENUM
	};
	expr->type = type;
}


/**
 * Convert between integers: CAST_INTINT
 */
static void cast_int_to_int(SemaContext *context, Expr *expr, Type *type)
{
	// Fold pointer casts if narrowing
	// So (int)(uptr)&x => (int)&x in the backend.
	if (expr->expr_kind == EXPR_PTR_TO_INT
	    && type_size(type) <= type_size(expr->type))
	{
		expr->type = type;
		return;
	}

	// Insert runtime casts on non-const.
	if (!expr_is_const(expr))
	{
		expr_rewrite_ext_trunc(expr, type, type_is_signed(type_flatten_to_int(expr->type)));
		return;
	}

	Type *flat = type_flatten_to_int(type);
	// Hand this off to the int conversion.
	expr->const_expr.ixx = int_conv(expr->const_expr.ixx, flat->type_kind);
	expr->const_expr.const_kind = CONST_INTEGER;
	expr->type = type;
	expr->const_expr.is_hex = false;
}

/**
 * Convert 1 => { 1, 1, 1, 1 } using CAST_EXPVEC
 */
static void cast_expand_to_vec(SemaContext *context, Expr *expr, Type *type)
{
	// Fold pointer casts if narrowing
	Type *base = type_get_indexed_type(type);
	cast_no_check(context, expr, base, IS_OPTIONAL(expr));
	Expr *inner = expr_copy(expr);
	expr->expr_kind = EXPR_SCALAR_TO_VECTOR;
	expr->inner_expr = inner;
	expr->type = type;
	expr->resolve_status = RESOLVE_DONE;
}

static void cast_bitstruct_to_int_arr(SemaContext *context, Expr *expr, Type *type) { expr_rewrite_recast(expr, type); }
static void cast_int_arr_to_bitstruct(SemaContext *context, Expr *expr, Type *type) { expr_rewrite_recast(expr, type); }

static void cast_bitstruct_to_bool(SemaContext *context, Expr *expr, Type *type)
{
	expr_rewrite_int_to_bool(expr, false);
	expr->type = type;
}


/**
 * Cast a signed or unsigned integer -> floating point, using CAST_INTFP
 * for runtime, otherwise do const transformation.
 */
static void cast_int_to_float(SemaContext *context, Expr *expr, Type *type)
{
	if (!sema_cast_const(expr))
	{
		expr_rewrite_to_int_to_float(expr, type);
		return;
	}

	Real f = int_to_real(expr->const_expr.ixx);
	expr_rewrite_const_float(expr, type, f);
}

static void cast_enum_to_int(SemaContext *context, Expr* expr, Type *to_type)
{
	SEMA_DEPRECATED(expr, "Using casts to convert enums to integers is deprecated in favour of using 'the_enum.ordinal`.");
	sema_expr_convert_enum_to_int(context, expr);
	cast_int_to_int(context, expr, to_type);
}

/**
 * Cast using CAST_VECARR, casting an array to a vector. For the constant, this
 * is a simple type change, see array_to_vec.
 */
static void cast_vec_to_arr(SemaContext *context, Expr *expr, Type *to_type)
{
	if (!sema_cast_const(expr))
	{
		expr->inner_expr = expr_copy(expr);
		expr->expr_kind = EXPR_VECTOR_TO_ARRAY;
		expr->type = to_type;
		expr->resolve_status = RESOLVE_DONE;
		return;
	}

	ASSERT0(expr->const_expr.const_kind == CONST_INITIALIZER);
	ConstInitializer *list = expr->const_expr.initializer;
	list->type = type_flatten(to_type);
	expr->type = to_type;
}

/**
 * Convert vector -> vector. This is somewhat complex as there are various functions
 * we need to invoke depending on the underlying type.
 */
static void cast_vec_to_vec(SemaContext *context, Expr *expr, Type *to_type)
{
	if (!sema_cast_const(expr))
	{
		// Extract indexed types.
		Type *from_type = type_flatten(expr->type);
		Type *from_element = from_type->array.base;
		to_type = type_flatten(to_type);
		Type *to_element = to_type->array.base;

		// float vec -> float/int/bool vec
		if (type_is_float(from_element))
		{
			switch (to_element->type_kind)
			{
				case ALL_FLOATS:
					expr_rewrite_ext_trunc(expr, to_type, true);
					return;
				case TYPE_BOOL:
				{
					Expr *left = expr_copy(expr);
					Expr *right = expr_new_expr(EXPR_CONST, expr);
					expr_rewrite_to_const_zero(right, left->type);
					expr_rewrite_to_binary(expr, left, right, BINARYOP_VEC_NE);
					expr->type = to_type;
					return;
				}
				case ALL_INTS:
					expr_rewrite_to_float_to_int(expr, to_type);
					return;
				default:
					UNREACHABLE;
			}
		}

		// bool vec -> float vec
		if (from_element == type_bool)
		{
			// Special conversion to retain the sign.
			if (type_is_integer(to_element))
			{
				expr_rewrite_ext_trunc(expr, to_type, true);
				return;
			}
			if (type_is_float(to_element))
			{
				expr_rewrite_to_int_to_float(expr, to_type);
				return;
			}
			UNREACHABLE;
		}

		if (type_is_integer(from_element))
		{
			switch (to_element->type_kind)
			{
				case ALL_FLOATS:
					expr_rewrite_to_int_to_float(expr, to_type);
					return;
				case TYPE_BOOL:
				{
					Expr *left = expr_copy(expr);
					Expr *right = expr_new_expr(EXPR_CONST, expr);
					expr_rewrite_to_const_zero(right, left->type);
					expr_rewrite_to_binary(expr, left, right, BINARYOP_VEC_NE);
					expr->type = to_type;
					return;
				}
				case ALL_INTS:
					expr_rewrite_ext_trunc(expr, to_type, type_is_signed(type_flatten_to_int(expr->type)));
					return;
				case TYPE_POINTER:
				case TYPE_FUNC_PTR:
				case TYPE_TYPEID:
				case TYPE_ANYFAULT:
				case TYPE_FAULTTYPE:
					expr_rewrite_to_int_to_ptr(expr, to_type);
				default:
					UNREACHABLE;
			}
		}
		// The rest will be different pointer types
		switch (to_element->type_kind)
		{
			case ALL_FLOATS:
				UNREACHABLE
				return;
			case TYPE_BOOL:
			{
				Expr *inner = expr_copy(expr);
				expr->expr_kind = EXPR_INT_TO_BOOL;
				expr->int_to_bool_expr = (ExprIntToBool) { .inner = inner, .negate = false };
				expr->type = to_type;
				return;
			}
			case ALL_INTS:
				expr_rewrite_to_int_to_ptr(expr, to_type);
				return;
			case TYPE_POINTER:
			case TYPE_FUNC_PTR:
			case TYPE_TYPEID:
			case TYPE_ANYFAULT:
			case TYPE_FAULTTYPE:
				expr_rewrite_rvalue(expr, to_type);
				return;
			default:
				UNREACHABLE;
		}
	}

	ASSERT0(expr->const_expr.const_kind == CONST_INITIALIZER);

	// For the const initializer we need to change the internal type
	ConstInitializer *list = expr->const_expr.initializer;
	vector_const_initializer_convert_to_type(context, list, to_type);
	expr->type = to_type;
}


static void cast_untyped_list_to_other(SemaContext *context, Expr *expr, Type *to_type)
{
	ASSERT0(expr_is_const_untyped_list(expr));
	// Recursively set the type of all ConstInitializer inside.
	expr_recursively_rewrite_untyped_list(expr, expr->const_expr.untyped_list);
	// We can now analyse the list (this is where the actual check happens)
	bool success = sema_expr_analyse_initializer_list(context, type_flatten(to_type), expr);
	ASSERT0(success);
	// And set the type.
	expr->type = type_infer_len_from_actual_type(to_type, expr->type);
}

static void cast_anyfault_to_fault(SemaContext *context, Expr *expr, Type *type)
{
	if (!sema_cast_const(expr))
	{
		expr->inner_expr = expr_copy(expr);
		expr->expr_kind = EXPR_ANYFAULT_TO_FAULT;
		expr->type = type;
		expr->resolve_status = RESOLVE_DONE;
		return;
	}
	ASSERT0(expr_is_const_fault(expr));
	Decl *value = expr->const_expr.enum_err_val;
	ASSERT0(value->type != type);
	expr->type = type;
}

static void cast_slice_to_ptr(SemaContext *context, Expr *expr, Type *type)
{
	if (expr_is_const_string(expr) || expr_is_const_bytes(expr))
	{
		expr->type = type;
		return;
	}

	expr_rewrite_ptr_access(expr, expr_copy(expr), type);
}

/**
 * Cast any int to a pointer, will use CAST_INTPTR after a conversion to uptr for runtime.
 * Compile time it will check that the value fits the pointer size.
 */
static void cast_int_to_ptr(SemaContext *context, Expr *expr, Type *type)
{
	ASSERT0(type_bit_size(type_uptr) <= 64 && "For > 64 bit pointers, this code needs updating.");

	// Handle const:
	if (sema_cast_const(expr))
	{
		expr->type = type;
		expr->const_expr.ptr = expr->const_expr.ixx.i.low;
		expr->const_expr.const_kind = CONST_POINTER;
		return;
	}
	// This may be a narrowing
	cast_no_check(context, expr, type_uptr, IS_OPTIONAL(expr));
	expr_rewrite_to_int_to_ptr(expr, type);
}

/**
 * Bool into a signed or unsigned int using CAST_BOOLINT
 * or rewrite to 0 / 1 for false / true.
 */
static void cast_bool_to_int(SemaContext *context, Expr *expr, Type *type)
{
	if (!sema_cast_const(expr))
	{
		expr_rewrite_ext_trunc(expr, type, false);
		return;
	}

	expr_rewrite_const_int(expr, type, expr->const_expr.b ? 1 : 0);
}


/**
 * Cast bool to float using CAST_BOOLFP
 * or rewrite to 0.0 / 1.0 for false / true
 */
static void cast_bool_to_float(SemaContext *context, Expr *expr, Type *type)
{
	if (!sema_cast_const(expr))
	{
		expr_rewrite_to_int_to_float(expr, type);
		return;
	}

	ASSERT0(expr->const_expr.const_kind == CONST_BOOL);
	expr_rewrite_const_float(expr, type, expr->const_expr.b ? 1.0 : 0.0);
}

/**
 * Cast int to bool using CAST_INTBOOL
 * or rewrite 0 => false, any other value => true
 */
static void cast_int_to_bool(SemaContext *context, Expr *expr, Type *type)
{
	if (!expr_is_const(expr))
	{
		expr_rewrite_int_to_bool(expr, false);
		expr->type = type;
		return;
	}

	expr_rewrite_const_bool(expr, type, !int_is_zero(expr->const_expr.ixx));
}

/**
 * Cast any float to bool using CAST_FPBOOL
 * or rewrite 0.0 => false, any other value => true
 */
static void cast_float_to_bool(SemaContext *context, Expr *expr, Type *type)
{
	if (!expr_is_const(expr))
	{
		Expr *left = expr_copy(expr);
		Expr *right = expr_new_expr(EXPR_CONST, expr);
		expr_rewrite_const_float(right, left->type, 0);
		expr->expr_kind = EXPR_BINARY;
		expr->binary_expr.left = exprid(left);
		expr->binary_expr.right = exprid(right);
		expr->binary_expr.operator = BINARYOP_NE;
		expr->type = type;
		return;
	}

	expr_rewrite_const_bool(expr, type, expr->const_expr.fxx.f != 0.0);
}

/**
 * Insert the PTRXI cast, or on const do a rewrite.
 */
static void cast_ptr_to_int(SemaContext *context, Expr *expr, Type *type)
{
	if (sema_cast_const(expr) && expr_is_const_pointer(expr))
	{
		// Revisit this to support pointers > 64 bits.
		expr_rewrite_const_int(expr, type, expr->const_expr.ptr);
		return;
	}
	expr_rewrite_to_ptr_to_int(expr, type);
}

/**
 * Insert the PTRBOOL cast or on const do a rewrite.
 */
static void cast_ptr_to_bool(SemaContext *context, Expr *expr, Type *type)
{
	if (!expr_is_const(expr))
	{
		expr_rewrite_int_to_bool(expr, false);
		expr->type = type;
		return;
	}

	// It may be a pointer
	switch (expr->const_expr.const_kind)
	{
		case CONST_POINTER:
			expr_rewrite_const_bool(expr, type, expr->const_expr.ptr != 0);
			return;
		case CONST_REF:
			expr_rewrite_const_bool(expr, type, true);
			return;
		default:
			break;
	}


	// Or it's a string, in which case it is always true.
	ASSERT0(expr->const_expr.const_kind == CONST_STRING);
	expr_rewrite_const_bool(expr, type, true);
}


static void cast_slice_to_bool(SemaContext *context, Expr *expr, Type *type)
{
	if (expr_is_const_slice(expr))
	{
		expr_rewrite_const_bool(expr, type, expr->const_expr.slice_init != NULL);
		return;
	}
	Expr *inner = expr_copy(expr);
	Expr *len = expr_copy(expr);
	expr_rewrite_slice_len(len, inner, type_usz);
	expr_rewrite_to_binary(expr, len, expr_new_const_int(expr->span, type_usz, 0), BINARYOP_NE);
	expr->type = type;
}

/**
 * We have two cases:
 * 1. int[] -> Foo[] where Foo is a distinct or typedef or pointer. Then we can just redefine
 * 2. The second case is something like int[] -> float[] for this case we need to make a bitcast using CAST_SASA.
 */
static void cast_slice_to_slice(SemaContext *context, Expr *expr, Type *to_type)
{
	if (sema_cast_const(expr))
	{
		expr->type = to_type;
		return;
	}
	expr_rewrite_recast(expr, to_type);
}

static void cast_vecarr_to_slice(SemaContext *context, Expr *expr, Type *to_type)
{
	if (!sema_cast_const(expr))
	{
		UNREACHABLE
	}
	ASSERT0(expr_is_const(expr));
	switch (expr->const_expr.const_kind)
	{
		case CONST_FLOAT:
		case CONST_INTEGER:
		case CONST_BOOL:
		case CONST_ENUM:
		case CONST_ERR:
		case CONST_POINTER:
		case CONST_TYPEID:
		case CONST_SLICE:
		case CONST_UNTYPED_LIST:
		case CONST_REF:
		case CONST_MEMBER:
			UNREACHABLE
		case CONST_BYTES:
		case CONST_STRING:
			expr->type = to_type;
			return;
		case CONST_INITIALIZER:
		{
			ConstInitializer *init = expr->const_expr.slice_init;
			expr_rewrite_const_slice(expr, to_type, init);
			return;
		}
	}
	UNREACHABLE
}
static void cast_slice_to_vecarr(SemaContext *context, Expr *expr, Type *to_type)
{
	if (!sema_cast_const(expr))
	{
		switch (expr->expr_kind)
		{
			case EXPR_SLICE:
			{
				expr->inner_expr = expr_copy(expr);
				expr->expr_kind = EXPR_SLICE_TO_VEC_ARRAY;
				expr->type = to_type;
				expr->resolve_status = RESOLVE_DONE;
				return;
			}
			default:
				UNREACHABLE;
		}
	}
	if (expr_is_const_slice(expr))
	{
		expr->const_expr.const_kind = CONST_INITIALIZER;
	}
	ASSERT0(expr_is_const(expr));
	expr->type = to_type;
	return;
}

static void cast_slice_to_infer(SemaContext *context, Expr *expr, Type *to_type)
{
	ArraySize len = sema_len_from_const(expr);
	ASSERT0(len > 0);
	Type *indexed = type_get_indexed_type(expr->type);
	to_type = type_infer_len_from_actual_type(to_type, type_get_array(indexed, len));
	cast_no_check(context, expr, to_type, false);
}

static void cast_vecarr_to_infer(SemaContext *context, Expr *expr, Type *to_type)
{
	to_type = type_infer_len_from_actual_type(to_type, type_flatten(expr->type));
	cast_no_check(context, expr, to_type, false);
}

static void cast_ptr_to_infer(SemaContext *context, Expr *expr, Type *to_type)
{
	to_type = type_infer_len_from_actual_type(to_type, type_flatten(expr->type));
	cast_no_check(context, expr, to_type, false);
}


/**
 * Cast using CAST_ARRVEC, casting an array to a vector. For the constant, this
 * is a simple type change.
 */
static void cast_arr_to_vec(SemaContext *context, Expr *expr, Type *to_type)
{
	Type *index_vec = type_flatten(type_get_indexed_type(to_type));
	Type *index_arr = type_flatten(type_get_indexed_type(expr->type));
	Type *to_temp = index_vec == index_arr ? to_type : type_get_vector(index_arr, type_flatten(expr->type)->array.len);
	if (sema_cast_const(expr))
	{
		// For the array -> vector this is always a simple rewrite of type.
		ASSERT0(expr->const_expr.const_kind == CONST_INITIALIZER);
		ConstInitializer *list = expr->const_expr.initializer;
		list->type = type_flatten(to_temp);
		expr->type = to_temp;
	}
	else
	{
		Expr *inner = expr_copy(expr);
		expr->expr_kind = EXPR_VECTOR_FROM_ARRAY;
		expr->inner_expr = inner;
		expr->type = to_temp;
	}
	if (to_temp != to_type)
	{
		cast_vec_to_vec(context, expr, to_type);
	}
}

static void cast_arr_to_arr(SemaContext *context, Expr *expr, Type *to_type)
{
	ASSERT0(type_size(to_type) == type_size(expr->type));
	expr->type = to_type;
}

static void cast_anyfault_to_bool(SemaContext *context, Expr *expr, Type *to_type)
{
	expr_rewrite_int_to_bool(expr, false);
	expr->type = to_type;
}
static void cast_typeid_to_bool(SemaContext *context, Expr *expr, Type *to_type) { expr_rewrite_int_to_bool(expr, false); expr->type = to_type; }

#define XX2XX &cast_retype
#define BS2IA &cast_bitstruct_to_int_arr
#define BS2BO &cast_bitstruct_to_bool
#define IA2BS &cast_int_arr_to_bitstruct
#define EX2VC &cast_expand_to_vec
#define BO2IN &cast_bool_to_int          
#define BO2FP &cast_bool_to_float        
#define IN2BO &cast_int_to_bool          
#define IN2IN &cast_int_to_int           
#define IN2FP &cast_int_to_float         
#define IN2PT &cast_int_to_ptr           
#define IN2EN &cast_int_to_enum          
#define EN2IN &cast_enum_to_int          
#define FP2BO &cast_float_to_bool        
#define FP2IN &cast_float_to_int         
#define FP2FP &cast_float_to_float       
#define PT2BO &cast_ptr_to_bool          
#define PT2IN &cast_ptr_to_int           
#define PT2PT &cast_ptr_to_ptr           
#define PT2AY &cast_ptr_to_any
#define AP2SL &cast_vaptr_to_slice
#define SL2BO &cast_slice_to_bool
#define SL2PT &cast_slice_to_ptr
#define SL2SL &cast_slice_to_slice
#define VC2AR &cast_vec_to_arr           
#define VC2VC &cast_vec_to_vec           
#define AR2VC &cast_arr_to_vec           
#define AR2AR &cast_arr_to_arr           
#define ST2LN &cast_struct_to_inline     
#define AY2BO &cast_any_to_bool          
#define AY2PT &cast_any_to_ptr           
#define FA2IN &cast_fault_to_int         
#define FA2PT &cast_fault_to_ptr         
#define FA2AF &cast_fault_to_anyfault    
#define TI2BO &cast_typeid_to_bool       
#define TI2IN &cast_typeid_to_int        
#define TI2PT &cast_typeid_to_ptr        
#define AF2BO &cast_anyfault_to_bool     
#define AF2FA &cast_anyfault_to_fault    
#define SL2VA &cast_slice_to_vecarr
#define VA2SL &cast_vecarr_to_slice
#define XX2VO &cast_all_to_void          
#define SL2FE &cast_slice_to_infer
#define VA2FE &cast_vecarr_to_infer
#define PT2FE &cast_ptr_to_infer
#define UL2XX &cast_untyped_list_to_other

#define _NO__ NULL                        /* No                                                                                                */
#define RXXDI &rule_to_distinct           /* Type -> distinct (match + is explicit)                                                            */
#define REXPL &rule_explicit_ok           /* Is explicit                                                                                       */
#define _NA__ &rule_not_applicable        /* "Not applicable" - should not be seen.                                                            */
#define RWIDE &rule_widen_narrow          /* Widen / narrow conversion of int/float                                                            */
#define RINFL &rule_int_to_float          /* Simple expressions, check sizes                                                                   */
#define ROKOK &rule_all_ok                /* Always works                                                                                      */
#define RAFFA &rule_anyfault_to_fault     /* Runtime check that it's valid, otherwise ok if explicit                                           */
#define RINPT &rule_int_to_ptr            /* Int -> ptr (explicit + size match)                                                                */
#define RPTIN &rule_ptr_to_int            /* Ptr -> int (explicit + size match)                                                                */
#define RINBS &rule_int_to_bits           /* Int -> bits (explicit + int + size match)                                                         */
#define RARBS &rule_arr_to_bits           /* Char[*] -> bits (explicit + base match)                                                           */
#define RINEN &rule_int_to_enum           /* Int -> enum (explicit, range check const)                                                         */
#define RPTPT &rule_ptr_to_ptr            /* Ptr -> ptr (explicit or ptr match)                                                                */
#define RAPSL &rule_arrptr_to_slice       /* Arrptr -> Slice (explicit flattens distinct, pointer match)                                       */
#define RSLPT &rule_slice_to_ptr          /* Slice -> ptr (explicit flatens distinct, pointer match)                                           */
#define RSLSL &rule_slice_to_slice        /* Slice -> slice (explicit same size, align safe, match base otherwise) void* <-> int* match.       */
#define RSLVA &rule_slice_to_vecarr       /* Slice -> vec/arr (if const, convert to vec/arr, check)                                            */
#define RVCVC &rule_vec_to_vec            /* Vec -> vec (as underlying type)                                                                   */
#define REXVC &rule_expand_to_vec         /* Int/Ptr/Float/Bool -> vec (expand if base match)                                                  */
#define RBSAR &rule_bits_to_arr           /* Bits -> arr (explicit + base match)                                                               */
#define RBSIN &rule_bits_to_int           /* Bits -> int (explicit + size match)                                                               */
#define RDIXX &rule_from_distinct         /* Distinct -> internal (explicit or inline)                                                         */
#define RDIDI &rule_to_from_distinct      /* Distinct -> other distinct                                                                        */
#define RARVC &rule_arr_to_vec            /* Arr -> Vec (len matches, valid elements, flatten if explicit)                                     */
#define RVCAR &rule_vec_to_arr            /* Vec -> Arr (len matches, if base can be converted, flatten if explicit)                           */
#define RARAR &rule_arr_to_arr            /* Array to array conversion (like slice, but len must match)                                        */
#define RSTST &rule_struct_to_struct      /* Struct -> struct (if inline)                                                                      */
#define RSTDI &rule_to_struct_to_distinct /* Struct -> inline (struct inline = distinct, distinct inline = struct if explicit flatten)         */
#define RSLFE &rule_slice_to_infer        /* Slice -> infer (only if slice is constant or can infer)                                           */
#define RVAFE &rule_vecarr_to_infer       /* Vec/arr -> infer (if base matches)                                                                */
#define RVASL &rule_vecarr_to_slice       /* Vec/arr -> slice (if base matches)                                                                */
#define RPTFE &rule_ptr_to_infer          /* Ptr -> infer (if pointee may infer)                                                               */
#define RPTIF &rule_ptr_to_interface      /* Ptr -> Interface if the pointee implements it                                                     */
#define RIFIF &rule_interface_to_interface/* Interface -> Interface if the latter implements all of the former                                 */
#define RULST &rule_ulist_to_struct       /* Untyped list -> bitstruct or union                                                                */
#define RULAR &rule_ulist_to_vecarr       /* Untyped list -> vector or array                                                                   */
#define RULFE &rule_ulist_to_inferred     /* Untyped list -> inferred vector or array                                                          */
#define RULSL &rule_ulist_to_slice        /* Untyped list -> slice                                                                             */
#define RVPAN &rule_voidptr_to_any        /* void* -> interface/any                                                                            */

CastRule cast_rules[CONV_LAST + 1][CONV_LAST + 1] = {
// void, wildc,  bool,   int, float,   ptr, slice,   vec, bitst, distc, array, strct, union,   any,  infc, fault,  enum, func,  typid, afaul, voidp, arrpt, infer, ulist (to)
 {_NA__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__}, // VOID    (from)
 {ROKOK, _NA__, ROKOK, ROKOK, ROKOK, ROKOK, ROKOK, ROKOK, ROKOK, ROKOK, ROKOK, ROKOK, ROKOK, ROKOK, ROKOK, ROKOK, ROKOK, ROKOK, ROKOK, ROKOK, ROKOK, ROKOK, _NO__, _NO__}, // WILDCARD
 {REXPL, _NO__, _NA__, REXPL, REXPL, _NO__, _NO__, REXVC, _NO__, RXXDI, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__}, // BOOL
 {REXPL, _NO__, REXPL, RWIDE, RINFL, RINPT, _NO__, REXVC, RINBS, RXXDI, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, RINEN, RINPT, _NO__, _NO__, RINPT, RINPT, _NO__, _NO__}, // INT
 {REXPL, _NO__, REXPL, REXPL, RWIDE, _NO__, _NO__, REXVC, _NO__, RXXDI, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__}, // FLOAT
 {REXPL, _NO__, REXPL, RPTIN, _NO__, RPTPT, _NO__, REXVC, _NO__, RXXDI, _NO__, _NO__, _NO__, ROKOK, RPTIF, _NO__, _NO__, _NO__, _NO__, _NO__, ROKOK, RPTPT, RPTFE, _NO__}, // PTR
 {REXPL, _NO__, REXPL, _NO__, _NO__, RSLPT, RSLSL, RSLVA, _NO__, RXXDI, RSLVA, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, ROKOK, RSLPT, RSLFE, _NO__}, // SLICE
 {REXPL, _NO__, _NO__, _NO__, _NO__, _NO__, RVASL, RVCVC, _NO__, RXXDI, RVCAR, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, RVAFE, _NO__}, // VECTOR
 {REXPL, _NO__, REXPL, RBSIN, _NO__, _NO__, _NO__, _NO__, _NO__, RXXDI, RBSAR, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__}, // BITSTRUCT
 {REXPL, _NO__, RDIXX, RDIXX, RDIXX, RDIXX, RDIXX, RDIXX, RDIXX, RDIDI, RDIXX, RDIXX, RDIXX, RDIXX, RDIXX, RDIXX, RDIXX, RDIXX, RDIXX, RDIXX, RDIXX, RDIXX, RDIXX, _NO__}, // DISTINCT
 {REXPL, _NO__, _NO__, _NO__, _NO__, _NO__, RVASL, RARVC, RARBS, RXXDI, RARAR, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, RVAFE, _NO__}, // ARRAY
 {REXPL, _NO__, RSTST, RSTST, RSTST, RSTST, RSTST, RSTST, RSTST, RSTDI, RSTST, RSTST, RSTST, RSTST, RSTST, RSTST, RSTST, RSTST, RSTST, RSTST, RSTST, RSTST, _NO__, _NO__}, // STRUCT
 {REXPL, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, RXXDI, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__}, // UNION
 {REXPL, _NO__, REXPL, _NO__, _NO__, REXPL, _NO__, _NO__, _NO__, RXXDI, _NO__, _NO__, _NO__, _NA__, REXPL, _NO__, _NO__, _NO__, _NO__, _NO__, ROKOK, REXPL, _NO__, _NO__}, // ANY
 {REXPL, _NO__, REXPL, _NO__, _NO__, REXPL, _NO__, _NO__, _NO__, RXXDI, _NO__, _NO__, _NO__, ROKOK, RIFIF, _NO__, _NO__, _NO__, _NO__, _NO__, ROKOK, REXPL, _NO__, _NO__}, // INTERFACE
 {REXPL, _NO__, REXPL, RPTIN, _NO__, REXPL, _NO__, REXVC, _NO__, RXXDI, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, ROKOK, REXPL, REXPL, _NO__, _NO__}, // FAULT
 {REXPL, _NO__, _NO__, REXPL, _NO__, _NO__, _NO__, REXVC, _NO__, RXXDI, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__}, // ENUM
 {REXPL, _NO__, REXPL, RPTIN, _NO__, _NO__, _NO__, REXVC, _NO__, RXXDI, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, RPTPT, _NO__, _NO__, ROKOK, _NO__, _NO__, _NO__}, // FUNC
 {REXPL, _NO__, REXPL, RPTIN, _NO__, REXPL, _NO__, REXVC, _NO__, RXXDI, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NA__, _NO__, REXPL, REXPL, _NO__, _NO__}, // TYPEID
 {REXPL, _NO__, REXPL, RPTIN, _NO__, REXPL, _NO__, REXVC, _NO__, RXXDI, _NO__, _NO__, _NO__, _NO__, _NO__, RAFFA, _NO__, _NO__, _NO__, _NA__, REXPL, REXPL, _NO__, _NO__}, // ANYFAULT
 {REXPL, _NO__, REXPL, RPTIN, _NO__, ROKOK, _NO__, REXVC, _NO__, RXXDI, _NO__, _NO__, _NO__, RVPAN, RVPAN, _NO__, _NO__, ROKOK, _NO__, _NO__, _NA__, ROKOK, _NO__, _NO__}, // VOIDPTR
 {REXPL, _NO__, REXPL, RPTIN, _NO__, RPTPT, RAPSL, REXVC, _NO__, RXXDI, _NO__, _NO__, _NO__, ROKOK, ROKOK, _NO__, _NO__, _NO__, _NO__, _NO__, ROKOK, RPTPT, RPTFE, _NO__}, // ARRPTR
 {_NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__}, // INFERRED
 {_NO__, _NO__, _NO__, _NO__, _NO__, _NO__, RULSL, RULAR, RULST, RXXDI, RULAR, RULST, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, _NO__, RULFE, _NO__}, // UNTYPED_LIST
};

CastFunction cast_function[CONV_LAST + 1][CONV_LAST + 1] = {
//void,  wildcd, bool,    int, float,   ptr, slice,  vec,  bitst, dist,  array, struct,union,   any,  infc, fault,  enum, func,typeid, anyfa,  vptr,  aptr, infer, ulist (to)
 {0,          0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,    0,     0,     0,     0,     0,     0     }, // VOID (from)
 {XX2XX,      0, XX2XX, XX2XX, XX2XX, XX2XX, XX2XX, XX2XX, XX2XX,     0, XX2XX, XX2XX, XX2XX, XX2XX, XX2XX, XX2XX, XX2XX, XX2XX,XX2XX, XX2XX, XX2XX, XX2XX,     0,     0     }, // WILDCARD
 {XX2VO,      0,     0, BO2IN, BO2FP,     0,     0, EX2VC,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,    0,     0,     0,     0,     0,     0     }, // BOOL
 {XX2VO,      0, IN2BO, IN2IN, IN2FP, IN2PT,     0, EX2VC, IA2BS,     0,     0,     0,     0,     0,     0,     0, IN2EN, IN2PT,    0,     0, IN2PT, IN2PT,     0,     0     }, // INT
 {XX2VO,      0, FP2BO, FP2IN, FP2FP,     0,     0, EX2VC,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,    0,     0,     0,     0,     0,     0     }, // FLOAT
 {XX2VO,      0, PT2BO, PT2IN,     0, PT2PT,     0, EX2VC,     0,     0,     0,     0,     0, PT2AY, PT2AY,     0,     0,     0,    0,     0, PT2PT, PT2PT, PT2FE,     0     }, // PTR
 {XX2VO,      0, SL2BO,     0,     0, SL2PT, SL2SL, SL2VA,     0,     0, SL2VA,     0,     0,     0,     0,     0,     0,     0,    0,     0, SL2PT, SL2PT, SL2FE,     0     }, // SLICE
 {XX2VO,      0,     0,     0,     0,     0, VA2SL, VC2VC,     0,     0, VC2AR,     0,     0,     0,     0,     0,     0,     0,    0,     0,     0,     0, VA2FE,     0     }, // VECTOR
 {XX2VO,      0, BS2BO, BS2IA,     0,     0,     0,     0,     0,     0, BS2IA,     0,     0,     0,     0,     0,     0,     0,    0,     0,     0,     0,     0,     0     }, // BITSTRUCT
 {    0,      0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,    0,     0,     0,     0,     0,     0     }, // DISTINCT
 {XX2VO,      0,     0,     0,     0,     0, VA2SL, AR2VC, IA2BS,     0, AR2AR,     0,     0,     0,     0,     0,     0,     0,    0,     0,     0,     0, VA2FE,     0     }, // ARRAY
 {XX2VO,      0, ST2LN, ST2LN, ST2LN, ST2LN, ST2LN, ST2LN, ST2LN,     0, ST2LN, ST2LN, ST2LN, ST2LN, ST2LN, ST2LN, ST2LN, ST2LN,ST2LN, ST2LN, ST2LN, ST2LN,     0,     0     }, // STRUCT
 {XX2VO,      0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,    0,     0,     0,     0,     0,     0     }, // UNION
 {XX2VO,      0, AY2BO,     0,     0, AY2PT,     0,     0,     0,     0,     0,     0,     0,     PT2PT, PT2PT, 0,     0,     0,    0,     0, AY2PT, AY2PT,     0,     0     }, // ANY
 {XX2VO,      0, AY2BO,     0,     0, AY2PT,     0,     0,     0,     0,     0,     0,     0,     PT2PT, PT2PT, 0,     0,     0,    0,     0, AY2PT, AY2PT,     0,     0     }, // INTERFACE
 {XX2VO,      0, AF2BO, FA2IN,     0, FA2PT,     0, EX2VC,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,    0, FA2AF, FA2PT, FA2PT,     0,     0     }, // FAULT
 {XX2VO,      0,     0, EN2IN,     0,     0,     0, EX2VC,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,    0,     0,     0,     0,     0,     0     }, // ENUM
 {XX2VO,      0, PT2BO, PT2IN,     0,     0,     0, EX2VC,     0,     0,     0,     0,     0,     0,     0,     0,     0, PT2PT,    0,     0, PT2PT,     0,     0,     0     }, // FUNC
 {XX2VO,      0, TI2BO, TI2IN,     0, TI2PT,     0, EX2VC,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,    0,     0, TI2PT, TI2PT,     0,     0     }, // TYPEID
 {XX2VO,      0, AF2BO, FA2IN,     0, FA2IN,     0, EX2VC,     0,     0,     0,     0,     0,     0,     0,     AF2FA, 0,     0,    0,     0, FA2IN, FA2IN,     0,     0     }, // ANYFAULT
 {XX2VO,      0, PT2BO, PT2IN,     0, PT2PT,     0, EX2VC,     0,     0,     0,     0,     0,     PT2AY, PT2AY, 0,     0, PT2PT,    0,     0,     0, PT2PT,     0,     0     }, // VOIDPTR
 {XX2VO,      0, PT2BO, PT2IN,     0, PT2PT, AP2SL, EX2VC,     0,     0,     0,     0,     0,     PT2AY, PT2AY, 0,     0,     0,    0,     0, PT2PT, PT2PT, PT2FE,     0     }, // ARRAYPTR
 {    0,      0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,    0,     0,     0,     0,     0,     0     }, // INFERRED
 {    0,      0,     0,     0,     0,     0, UL2XX, UL2XX, UL2XX,     0, UL2XX, UL2XX,     0,     0,     0,     0,     0,     0,    0,     0,     0,     0, UL2XX,     0     }, // UNTYPED
};

static ConvGroup group_from_type[TYPE_LAST + 1] = {
	[TYPE_POISONED]         = CONV_NO,
	[TYPE_VOID]             = CONV_VOID,
	[TYPE_BOOL]             = CONV_BOOL,
	[TYPE_I8]               = CONV_INT,
	[TYPE_I16]              = CONV_INT,
	[TYPE_I32]              = CONV_INT,
	[TYPE_I64]              = CONV_INT,
	[TYPE_I128]             = CONV_INT,
	[TYPE_U8]               = CONV_INT,
	[TYPE_U16]              = CONV_INT,
	[TYPE_U32]              = CONV_INT,
	[TYPE_U64]              = CONV_INT,
	[TYPE_U128]             = CONV_INT,
	[TYPE_F16]              = CONV_FLOAT,
	[TYPE_BF16]             = CONV_FLOAT,
	[TYPE_F32]              = CONV_FLOAT,
	[TYPE_F64]              = CONV_FLOAT,
	[TYPE_F128]             = CONV_FLOAT,
	[TYPE_ANY]              = CONV_ANY,
	[TYPE_INTERFACE]        = CONV_INTERFACE,
	[TYPE_ANYFAULT]         = CONV_ANYFAULT,
	[TYPE_TYPEID]           = CONV_TYPEID,
	[TYPE_POINTER]          = CONV_POINTER,
	[TYPE_ENUM]             = CONV_ENUM,
	[TYPE_FUNC_PTR]         = CONV_FUNC,
	[TYPE_STRUCT]           = CONV_STRUCT,
	[TYPE_UNION]            = CONV_UNION,
	[TYPE_BITSTRUCT]        = CONV_BITSTRUCT,
	[TYPE_FAULTTYPE]        = CONV_FAULT,
	[TYPE_TYPEDEF]          = CONV_NO,
	[TYPE_DISTINCT]         = CONV_DISTINCT,
	[TYPE_ARRAY]            = CONV_ARRAY,
	[TYPE_SLICE]            = CONV_SLICE,
	[TYPE_FLEXIBLE_ARRAY]   = CONV_NO,
	[TYPE_INFERRED_ARRAY]   = CONV_NO,
	[TYPE_VECTOR]           = CONV_VECTOR,
	[TYPE_INFERRED_VECTOR]  = CONV_NO,
	[TYPE_UNTYPED_LIST]     = CONV_UNTYPED_LIST,
	[TYPE_OPTIONAL]         = CONV_NO,
	[TYPE_WILDCARD]         = CONV_WILDCARD,
	[TYPE_TYPEINFO]         = CONV_NO,
	[TYPE_MEMBER]           = CONV_NO,
};

INLINE ConvGroup type_to_group(Type *type)
{
	type = type->canonical;
	if (type == type_voidptr) return CONV_VOIDPTR;
	if (type->type_kind == TYPE_POINTER && (type->pointer->type_kind == TYPE_ARRAY || type->pointer->type_kind == TYPE_VECTOR)) return CONV_VAPTR;
	if (type_len_is_inferred(type)) return CONV_INFERRED;
	return group_from_type[type->type_kind];
}

INLINE void cast_context_set_from(CastContext *cc, Type *new_from)
{
	cc->from_group = type_to_group(cc->from = new_from);
}

INLINE void cast_context_set_to(CastContext *cc, Type *new_to)
{
	cc->to_group = type_to_group(cc->to = new_to);
}
