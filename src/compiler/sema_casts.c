// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "sema_internal.h"

#pragma clang diagnostic push
#pragma ide diagnostic ignored "ConstantFunctionResult"


static inline bool sema_error_cannot_convert(Expr *expr, Type *to, bool may_cast_explicit, bool silent);
static bool cast_expr_inner(SemaContext *context, Expr *expr, Type *to_type, bool is_explicit, bool silent,
                            bool may_not_be_optional);
static bool bitstruct_cast(Expr *expr, Type *from_type, Type *to, Type *to_type);
static void sema_error_const_int_out_of_range(Expr *expr, Expr *problem, Type *to_type);
static Expr *recursive_may_narrow(Expr *expr, Type *type);
static void recursively_rewrite_untyped_list(Expr *expr, Expr **list);
static inline bool cast_may_implicit_ptr(Type *from_pointee, Type *to_pointee);
static inline bool cast_may_array(Type *from, Type *to, bool is_explicit);
static inline bool insert_cast(Expr *expr, CastKind kind, Type *type);
static bool pointer_to_integer(Expr *expr, Type *type);
static bool pointer_to_bool(Expr *expr, Type *type);
static bool pointer_to_pointer(Expr* expr, Type *type);
static bool bool_to_int(Expr *expr, Type *canonical, Type *type);
static bool bool_to_float(Expr *expr, Type *canonical, Type *type);
static bool integer_to_bool(Expr *expr, Type *type);
static bool integer_to_enum(Expr *expr, Type *canonical, Type *type);
static bool integer_to_integer(Expr *expr, Type *canonical, Type *type);
static bool integer_to_pointer(Expr *expr, Type *type);
static bool integer_expand_to_vector_conversion(Expr *expr, Type *canonical, Type *type);
static bool float_to_bool(Expr *expr, Type *type);
static bool float_to_float(Expr* expr, Type *canonical, Type *type);
static bool float_to_integer(Expr *expr, Type *canonical, Type *type);
static bool float_expand_to_vector_conversion(Expr *expr, Type *canonical, Type *type);
static bool array_to_vector(Expr *expr, Type *to_type);
static bool vector_to_array(Expr *expr, Type *to_type);
static bool vector_to_vector(Expr *expr, Type *to_type);
INLINE bool subarray_to_subarray(Expr *expr, Type *to_type);

static void vector_const_initializer_convert_to_type(ConstInitializer *initializer, Type *to_type);

static void enum_to_int_lowering(Expr* expr);

static bool voidfail_to_error(Expr *expr, Type *type);

INLINE bool insert_runtime_cast_unless_const(Expr *expr, CastKind kind, Type *type);

/**
 * Insert a cast. This will assume that the cast is valid. No typeinfo will be registered.
 */
static inline bool insert_cast(Expr *expr, CastKind kind, Type *type)
{
	assert(expr->resolve_status == RESOLVE_DONE);
	assert(expr->type);
	Expr *inner = expr_copy(expr);
	expr->expr_kind = EXPR_CAST;
	expr->cast_expr.kind = kind;
	expr->cast_expr.expr = exprid(inner);
	expr->cast_expr.type_info = 0;
	expr->type = type;
	return true;
}

/**
 * General error due to casts.
 */
bool sema_error_failed_cast(Expr *expr, Type *from, Type *to)
{
	SEMA_ERROR(expr, "The cast %s to %s is not allowed.", type_quoted_error_string(from), type_quoted_error_string(to));
	return false;
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

	// Grab the underlying indexed type,
	// because we can only have [*] [] [<*>] [<>] * here
	Type *indexed = type_get_indexed_type(to_infer);
	Type *actual = type_get_indexed_type(actual_type);

	// We should always have indexed types.
	assert(indexed && actual);

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
			assert(type_is_arraylike(type_flatten(actual_type)));
			return type_add_optional(type_get_array(indexed, type_flatten(actual_type)->array.len), is_optional);
		case TYPE_INFERRED_VECTOR:
			assert(type_is_arraylike(type_flatten(actual_type)));
			return type_add_optional(type_get_vector(indexed, type_flatten(actual_type)->array.len), is_optional);
		case TYPE_VECTOR:
			// This is unreachable, because unlike arrays, there is no inner type that may be
			// the inferred part.
			UNREACHABLE
		case TYPE_SUBARRAY:
			// The case of int[*][] y = ... is disallowed
			UNREACHABLE
		default:
			UNREACHABLE
	}
}

/**
 * Insert a cast on non-const only
 */
INLINE bool insert_runtime_cast_unless_const(Expr *expr, CastKind kind, Type *type)
{
	if (expr->expr_kind == EXPR_CONST) return false;
	return insert_cast(expr, kind, type);
}

/**
 * Insert the PTRXI cast, or on const do a rewrite.
 */
static bool pointer_to_integer(Expr *expr, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_PTRINT, type)) return true;

	// Revisit this to support pointers > 64 bits.
	expr_rewrite_const_int(expr, type, expr->const_expr.ptr);
	return true;
}

/**
 * Insert the PTRBOOL cast or on const do a rewrite.
 */
static bool pointer_to_bool(Expr *expr, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_PTRBOOL, type)) return true;

	// It may be a pointer
	if (expr->const_expr.const_kind == CONST_POINTER)
	{
		expr_rewrite_const_bool(expr, type, expr->const_expr.ptr != 0);
		return true;
	}

	// Or it's a string, in which case it is always true.
	assert(expr->const_expr.const_kind == CONST_STRING);
	expr_rewrite_const_bool(expr, type, true);
	return true;
}

/**
 * Insert a PTRPTR cast or update the pointer type
 */
static bool pointer_to_pointer(Expr* expr, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_PTRPTR, type)) return true;

	// Strings cannot be compile-time folded, so insert a runtime cast.
	if (expr->const_expr.const_kind == CONST_STRING)
	{
		return insert_cast(expr, CAST_PTRPTR, type);
	}

	// Insert the cast, this removes the ability to narrow it.
	expr->type = type;
	expr->const_expr.is_hex = false;
	return true;
}


/**
 * Bool into a signed or unsigned int using CAST_BOOLINT
 * or rewrite to 0 / 1 for false / true.
 */
static bool bool_to_int(Expr *expr, Type *canonical, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_BOOLINT, type)) return true;

	expr_rewrite_const_int(expr, type, expr->const_expr.b ? 1 : 0);
	return true;
}


/**
 * Cast bool to float using CAST_BOOLFP
 * or rewrite to 0.0 / 1.0 for false / true
 */
static bool bool_to_float(Expr *expr, Type *canonical, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_BOOLFP, type)) return true;

	assert(expr->const_expr.const_kind == CONST_BOOL);
	expr_rewrite_const_float(expr, type, expr->const_expr.b ? 1.0 : 0.0);
	return true;
}

/**
 * Insert a cast from `void!` to some fault type by inserting a `catch`,
 * so "anyerr a = returns_voidfail()" => "anyerr a = catch(returns_voidfail())"
 */
static bool voidfail_to_error(Expr *expr, Type *type)
{
	assert(type->canonical->type_kind == TYPE_FAULTTYPE || type == type_anyerr);
	Expr *inner = expr_copy(expr);
	expr->expr_kind = EXPR_CATCH;
	expr->inner_expr = inner;
	expr->type = type;
	return true;
}


/**
 * Cast int to bool using CAST_INTBOOL
 * or rewrite 0 => false, any other value => true
 */
static bool integer_to_bool(Expr *expr, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_INTBOOL, type)) return true;

	expr_rewrite_const_bool(expr, type, !int_is_zero(expr->const_expr.ixx));
	return true;
}

/**
 * Cast any float to bool using CAST_FPBOOL
 * or rewrite 0.0 => false, any other value => true
 */
static bool float_to_bool(Expr *expr, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_FPBOOL, type)) return true;

	expr_rewrite_const_bool(expr, type, expr->const_expr.fxx.f != 0.0);
	return true;
}


/**
 * Convert any fp to another fp type using CAST_FPFP
 */
static bool float_to_float(Expr* expr, Type *canonical, Type *type)
{
	// Change to same type should never enter here.
	assert(type_flatten(canonical) != type_flatten(expr->type));

	// Insert runtime cast if needed.
	if (insert_runtime_cast_unless_const(expr, CAST_FPFP, type)) return true;

	// Otherwise rewrite the const, which may cause rounding.
	expr_rewrite_const_float(expr, type, expr->const_expr.fxx.f);
	return true;
}

/**
 * Convert from any floating point to int using CAST_FPINT
 * Const conversion will disable narrowable and hex.
 */
static bool float_to_integer(Expr *expr, Type *canonical, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_FPINT, type)) return true;

	// Run the int->real to and rewrite.
	assert(type_is_integer(canonical));
	Real d = expr->const_expr.fxx.f;
	expr->const_expr.ixx = int_from_real(d, canonical->type_kind);
	expr->const_expr.const_kind = CONST_INTEGER;
	expr->type = type;
	expr->const_expr.is_hex = false;
	return true;
}


/**
 * Convert from integer to enum using CAST_INTENUM / or do a const conversion.
 * This will ensure that the conversion is valid (i.e. in the range 0 .. enumcount - 1)
 */
static bool integer_to_enum(Expr *expr, Type *canonical, Type *type)
{
	assert(canonical->type_kind == TYPE_ENUM);
	Decl *enum_decl = canonical->decl;

	if (insert_runtime_cast_unless_const(expr, CAST_INTENUM, type)) return true;

	// Check that the type is within limits.
	unsigned max_enums = vec_size(enum_decl->enums.values);
	Int to_convert = expr->const_expr.ixx;

	// Negative numbers are always wrong.
	if (int_is_neg(to_convert))
	{
		SEMA_ERROR(expr, "A negative number cannot be converted to an enum.");
		return false;
	}

	// Check the max, we don't support more than 4 billion,
	// so we can safely use TYPE_U32.
	Int max = { .i.low = max_enums, .type = TYPE_U32 };
	if (int_comp(to_convert, max, BINARYOP_GE))
	{
		SEMA_ERROR(expr, "This value exceeds the number of enums in %s.", canonical->decl->name);
		return false;
	}

	// Fold the const into the actual enum.
	Decl *decl = enum_decl->enums.values[to_convert.i.low];
	expr->const_expr = (ExprConst) {
		.enum_err_val = decl,
		.const_kind = CONST_ENUM
	};
	expr->type = type;
	return true;
}

/**
 * Convert between integers: CAST_INTINT
 */
static bool integer_to_integer(Expr *expr, Type *canonical, Type *type)
{
	// Fold pointer casts if narrowing
	// So (int)(uptr)&x => (int)&x in the backend.
	if (expr->expr_kind == EXPR_CAST && expr->cast_expr.kind == CAST_PTRINT
	    && type_size(type) <= type_size(expr->type))
	{
		expr->type = type;
		return true;
	}

	// Insert runtime casts on non-const.
	if (insert_runtime_cast_unless_const(expr, CAST_INTINT, type)) return true;

	// Hand this off to the int conversion.
	expr->const_expr.ixx = int_conv(expr->const_expr.ixx, canonical->type_kind);
	expr->const_expr.const_kind = CONST_INTEGER;
	expr->type = type;
	expr->const_expr.is_hex = false;
	return true;
}

/**
 * Convert 1 => { 1, 1, 1, 1 } using CAST_NUMVEC
 */
static bool integer_expand_to_vector_conversion(Expr *expr, Type *canonical, Type *type)
{
	// Fold pointer casts if narrowing
	Type *base = type_get_indexed_type(type);
	cast(expr, base);
	return insert_cast(expr, CAST_NUMVEC, type);
}

/**
 * Convert 1.0 => { 1, 1, 1, 1 } using CAST_NUMVEC
 */
static bool float_expand_to_vector_conversion(Expr *expr, Type *canonical, Type *type)
{
	// Fold pointer casts if narrowing
	Type *base = type_get_indexed_type(type);
	cast(expr, base);
	return insert_cast(expr, CAST_NUMVEC, type);
}


/**
 * Cast a signed or unsigned integer -> floating point, using CAST_INTFP
 * for runtime, otherwise do const transformation.
 */
static bool integer_to_float(Expr *expr, Type *canonical, Type *type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_INTFP, type)) return true;

	Real f = int_to_real(expr->const_expr.ixx);
	expr_rewrite_const_float(expr, type, f);
	return true;
}


/**
 * Cast any int to a pointer, will use CAST_INTPTR after a conversion to uptr for runtime.
 * Compile time it will check that the value fits the pointer size.
 */
static bool integer_to_pointer(Expr *expr, Type *type)
{
	assert(type_bit_size(type_uptr) <= 64 && "For > 64 bit pointers, this code needs updating.");

	// Handle const:
	if (expr->expr_kind == EXPR_CONST)
	{
		// For if the type doesn't fit, insert an error.
		if (!int_fits(expr->const_expr.ixx, type_uptr->canonical->type_kind))
		{
			SEMA_ERROR(expr, "'0x%s' does not fit in a pointer.", int_to_str(expr->const_expr.ixx, 16));
			return false;
		}
		// Otherwise just update.
		expr->type = type;
		expr->const_expr.ptr = expr->const_expr.ixx.i.low;
		expr->const_expr.const_kind = CONST_POINTER;
		return true;
	}
	// Insert widening or narrowing cast as needed.
	cast(expr, type_uptr);
	return insert_cast(expr, CAST_INTPTR, type);
}

/**
 * Convert an enum to its underlying integer. Is a no-op on for enum expressions.
 */
static void enum_to_int_lowering(Expr* expr)
{
	assert(type_flatten_distinct_optional(expr->type)->type_kind == TYPE_ENUM);
	Type *underlying_type = type_flatten_distinct_optional(expr->type)->decl->enums.type_info->type;
	if (expr->expr_kind == EXPR_CONST)
	{
		assert(expr->const_expr.const_kind == CONST_ENUM);
		expr_rewrite_const_int(expr, underlying_type, expr->const_expr.enum_err_val->enum_constant.ordinal);

	}
	expr->type = type_add_optional(underlying_type, IS_OPTIONAL(expr));
}

/**
 * Cast using CAST_ARRVEC, casting an array to a vector. For the constant, this
 * is a simple type change.
 */
static bool array_to_vector(Expr *expr, Type *to_type)
{
	// Runtime cast
	if (insert_runtime_cast_unless_const(expr, CAST_ARRVEC, to_type)) return true;

	// For the array -> vector this is always a simple rewrite of type.
	assert(expr->const_expr.const_kind == CONST_INITIALIZER);
	ConstInitializer *list = expr->const_expr.initializer;
	list->type = to_type;
	expr->type = to_type;
	return true;
}

/**
 * We have two cases:
 * 1. int[] -> Foo[] where Foo is a distinct or typedef OR it is a constant. Then we can just redefine
 * 2. The second case is something like int*[] -> void*[] for this case we need to make a bitcast using CAST_SASA.
 */
INLINE bool subarray_to_subarray(Expr *expr, Type *to_type)
{
	if (expr_is_const(expr) || type_flatten(type_flatten(to_type)->array.base) == type_flatten(type_flatten(expr->type)->array.base))
	{
		// Here we assume int[] -> float[] can't happen.
		expr->type = to_type;
		return true;
	}
	return insert_cast(expr, CAST_SASA, to_type);
}

/**
 * Bitstruct casts go from its base type to any integer or char array of the same size.
 */
static bool bitstruct_cast(Expr *expr, Type *bitstruct_type, Type *to, Type *to_type)
{
	assert(type_size(to) == type_size(bitstruct_type) && "Only casts to the same width expected.");

	// The case where the bitstruct is backed by an integer
	if (type_is_integer(bitstruct_type))
	{
		// The same size integer, this is the simple case.
		if (type_is_integer(to))
		{
			expr->type = to_type;
			return true;
		}
		// Now we expect a char array of the same size. This
		// is runtime only.
		assert(to->type_kind == TYPE_ARRAY);
		return insert_cast(expr, CAST_BSARRY, to_type);
	}

	// Converting from a char array.
	assert(bitstruct_type->type_kind == TYPE_ARRAY);

	// Converting to a char array is the simple case, just change the type.
	if (to->type_kind == TYPE_ARRAY)
	{
		expr->type = to_type;
		return true;
	}

	// Converting to an integer, this is a runtime cast.
	assert(type_is_integer(to));
	return insert_cast(expr, CAST_BSINT, to_type);
}

/**
 * Cast using CAST_VECARR, casting an array to a vector. For the constant, this
 * is a simple type change, see array_to_vector.
 */
static bool vector_to_array(Expr *expr, Type *to_type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_VECARR, to_type)) return true;

	assert(expr->const_expr.const_kind == CONST_INITIALIZER);
	ConstInitializer *list = expr->const_expr.initializer;
	list->type = to_type;
	expr->type = to_type;
	return true;
}

/**
 * Convert vector -> vector. This is somewhat complex as there are various functions
 * we need to invoke depending on the underlying type.
 */
static bool vector_to_vector(Expr *expr, Type *to_type)
{
	//
	if (expr->expr_kind != EXPR_CONST)
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
				case ALL_FLOATS: return insert_cast(expr, CAST_FPFP, to_type);
				case TYPE_BOOL: return insert_cast(expr, CAST_FPBOOL, to_type);
				case ALL_INTS: return insert_cast(expr, CAST_FPINT, to_type);
				default: UNREACHABLE;
			}
		}

		// bool vec -> float vec
		if (from_element == type_bool)
		{
			// Special conversion to retain the sign.
			if (type_is_integer(to_element)) return insert_cast(expr, CAST_BOOLVECINT, to_type);
			if (type_is_float(to_element)) return insert_cast(expr, CAST_BOOLFP, to_type);
			UNREACHABLE;
		}

		// The last possibility is int -> bool/int/fp
		assert(type_is_integer(from_element));
		switch (to_element->type_kind)
		{
			case ALL_FLOATS: return insert_cast(expr, CAST_INTFP, to_type);
			case TYPE_BOOL: return insert_cast(expr, CAST_INTBOOL, to_type);
			case ALL_INTS: return insert_cast(expr, CAST_INTINT, to_type);
			default: UNREACHABLE;
		}
	}

	assert(expr->const_expr.const_kind == CONST_INITIALIZER);

	// For the const initializer we need to change the internal type
	ConstInitializer *list = expr->const_expr.initializer;
	vector_const_initializer_convert_to_type(list, to_type);
	expr->type = to_type;
	return true;
}

/**
 * Perform vararg promotions typical for C style varargs:
 * 1. Widen int and bool to C int size
 * 2. Widen float and smaller to double
 */
bool cast_promote_vararg(Expr *arg)
{
	// Remove things like distinct, optional, enum etc.
	Type *arg_type = type_flatten(arg->type);

	// 1. Promote any integer or bool to at least CInt
	if (type_is_promotable_int_bool(arg_type)) return cast(arg, type_cint);

	// 2. Promote any float to at least double
	if (type_is_promotable_float(arg_type)) return cast(arg, type_double);

	return true;
}

/**
 * Given lhs and rhs, promote to the maximum bit size, this will retain
 * signed/unsigned type of each side.
 */
void cast_to_int_to_max_bit_size(SemaContext *context, Expr *lhs, Expr *rhs, Type *left_type, Type *right_type)
{
	unsigned bit_size_left = left_type->builtin.bitsize;
	unsigned bit_size_right = right_type->builtin.bitsize;

	assert(bit_size_left && bit_size_right);

	// Simple case they are the same size, just return.
	if (bit_size_left == bit_size_right) return;

	// Lhs is smaller than rhs, so widen it using the right type
	if (bit_size_left < bit_size_right)
	{
		Type *to = lhs->type->type_kind < TYPE_U8
		           ? type_int_signed_by_bitsize(bit_size_right)
		           : type_int_unsigned_by_bitsize(bit_size_right);
		bool success = cast(lhs, to);
		assert(success);
		return;
	}

	// Rhs is smaller, do the same thing as above but with the rhs.
	Type *to = rhs->type->type_kind < TYPE_U8
	           ? type_int_signed_by_bitsize(bit_size_left)
	           : type_int_unsigned_by_bitsize(bit_size_left);
	bool success = cast(rhs, to);
	assert(success);
}

/**
 * For implicit casts to bool, in a conditional, return the type of cast to
 * insert.
 */
CastKind cast_to_bool_kind(Type *type)
{
	switch (type_flatten(type)->type_kind)
	{
		case TYPE_BOOL:
			return CAST_BOOLBOOL;
		case TYPE_ANYERR:
			return CAST_EUBOOL;
		case TYPE_SUBARRAY:
			return CAST_SABOOL;
		case ALL_INTS:
			return CAST_INTBOOL;
		case ALL_FLOATS:
			return CAST_FPBOOL;
		case TYPE_POINTER:
			return CAST_PTRBOOL;
		case TYPE_FAULTTYPE:
			return CAST_ERBOOL;
		case TYPE_POISONED:
		case TYPE_VOID:
		case TYPE_STRUCT:
		case TYPE_UNION:
		case TYPE_ENUM:
		case TYPE_FUNC:
		case TYPE_ARRAY:
		case TYPE_TYPEID:
		case TYPE_TYPEINFO:
		case TYPE_VECTOR:
		case TYPE_BITSTRUCT:
		case TYPE_UNTYPED_LIST:
		case TYPE_OPTIONAL:
		case TYPE_ANY:
		case TYPE_OPTIONAL_ANY:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_SCALED_VECTOR:
		case TYPE_INFERRED_VECTOR:
		case TYPE_MEMBER:
			return CAST_ERROR;
		case TYPE_TYPEDEF:
		case TYPE_DISTINCT:
		case TYPE_INFERRED_ARRAY:
			UNREACHABLE
	}
	UNREACHABLE
}


static inline bool cast_may_array(Type *from, Type *to, bool is_explicit)
{
	RETRY:;
	assert(!type_is_optional(from) && !type_is_optional(to) && "Optional should already been handled");

	bool compare_len = true;
	if (from->type_kind != to->type_kind)
	{
		switch (to->type_kind)
		{
			case TYPE_INFERRED_ARRAY:
				switch (from->type_kind)
				{
					case TYPE_INFERRED_VECTOR:
					case TYPE_VECTOR:
						if (!is_explicit) return false;
						FALLTHROUGH;
					case TYPE_ARRAY:
						compare_len = false;
						break;
					default:
						return false;
				}
				break;
			case TYPE_ARRAY:
				switch (from->type_kind)
				{
					case TYPE_INFERRED_VECTOR:
						compare_len = false;
						FALLTHROUGH;
					case TYPE_VECTOR:
						if (!is_explicit) return false;
						break;
					case TYPE_INFERRED_ARRAY:
						compare_len = false;
						break;
					default:
						return false;
				}
				break;
			case TYPE_INFERRED_VECTOR:
				switch (from->type_kind)
				{
					case TYPE_INFERRED_ARRAY:
					case TYPE_ARRAY:
						if (!is_explicit) return false;
						FALLTHROUGH;
					case TYPE_VECTOR:
					case TYPE_SCALED_VECTOR:
						compare_len = false;
						break;
					default:
						return false;
				}
				break;
			case TYPE_VECTOR:
				switch (from->type_kind)
				{
					case TYPE_INFERRED_ARRAY:
						compare_len = false;
						FALLTHROUGH;
					case TYPE_ARRAY:
						if (!is_explicit) return false;
						break;
					case TYPE_INFERRED_VECTOR:
						compare_len = false;
						break;
					default:
						return false;
				}
				break;
			case TYPE_SCALED_VECTOR:
				if (from->type_kind != TYPE_INFERRED_VECTOR) return false;
				compare_len = false;
				break;
			default:
				return false;
		}
	}
	if (compare_len && to->array.len != from->array.len) return false;

	Type *from_base = from->array.base;
	Type *to_base = to->array.base;
	if (is_explicit)
	{
		from_base = type_flatten(from_base);
		to_base = type_flatten(to_base);
	}

	if (from_base == to_base) return true;

	switch (to_base->type_kind)
	{
		case TYPE_POINTER:
			if (from_base->type_kind == TYPE_POINTER)
			{
				if (is_explicit) return true;
				return cast_may_implicit_ptr(to_base->pointer, from_base->pointer);
			}
			return false;
		case TYPE_ARRAY:
		case TYPE_INFERRED_ARRAY:
		case TYPE_VECTOR:
		case TYPE_INFERRED_VECTOR:
			to = to_base;
			from = from_base;
			goto RETRY;
		default:
			return is_explicit && type_is_structurally_equivalent(to_base, from_base);
	}
}

static inline bool cast_may_implicit_ptr(Type *from_pointee, Type *to_pointee)
{
	assert(!type_is_optional(from_pointee) && !type_is_optional(to_pointee) && "Optional should already been handled");
	if (from_pointee == to_pointee) return true;

	// For void* on either side, no checks.
	if (to_pointee == type_voidptr || from_pointee == type_voidptr) return true;

	// Step through all *:
	while (from_pointee->type_kind == TYPE_POINTER && to_pointee->type_kind == TYPE_POINTER)
	{
		if (from_pointee == type_voidptr || to_pointee == type_voidptr) return true;
		from_pointee = from_pointee->pointer;
		to_pointee = to_pointee->pointer;
	}

	assert(to_pointee != from_pointee);

	// Functions compare raw types.
	if (from_pointee->type_kind == TYPE_FUNC && to_pointee->type_kind == TYPE_FUNC)
	{
		return to_pointee->function.prototype->raw_type == from_pointee->function.prototype->raw_type;
	}

	// Special handling of int* = int[4]* (so we have int[4] -> int)
	if (type_is_arraylike(from_pointee))
	{
		if (cast_may_implicit_ptr(to_pointee, from_pointee->array.base)) return true;
	}

	if (type_is_any_arraylike(to_pointee) || type_is_any_arraylike(from_pointee))
	{
		return cast_may_array(from_pointee, to_pointee, false);
	}
	// Use subtype matching
	return type_is_subtype(to_pointee, from_pointee);
}

bool cast_may_bool_convert(Type *type)
{
	switch (type_flatten_distinct(type)->type_kind)
	{
		case TYPE_BOOL:
		case ALL_INTS:
		case ALL_FLOATS:
		case TYPE_SUBARRAY:
		case TYPE_POINTER:
		case TYPE_ANY:
		case TYPE_FAULTTYPE:
		case TYPE_ANYERR:
			return true;
		default:
			return false;
	}
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
			}
			UNREACHABLE
		case EXPR_BUILTIN_ACCESS:
			switch (expr->builtin_access_expr.kind)
			{
				case ACCESS_LEN:
					// Special: we may resize this, but not smaller than cint.
					if (type_size(type) < type_size(type_cint)) return expr;
					return NULL;
				case ACCESS_PTR:
				case ACCESS_TYPEOFANY:
				case ACCESS_ENUMNAME:
				case ACCESS_FAULTNAME:
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
		case EXPR_CAST:
			switch (expr->cast_expr.kind)
			{
				case CAST_INTINT:
				case CAST_FPFP:
					// If this is a narrowing cast that makes it smaller that then target type
					// we're done.
					if (type_size(type) >= type_size(expr->type))
					{
						return NULL;
					}
					// Otherwise just look through it.
					expr = exprptr(expr->cast_expr.expr);
					goto RETRY;
				default:
					// For all other casts we regard them as opaque.
					goto CHECK_SIZE;
			}
		case EXPR_CONST:
			// For constants, just check that they will fit.
			if (type_is_integer(type))
			{
				assert(expr->const_expr.const_kind == CONST_INTEGER || expr->const_expr.const_kind == CONST_ENUM);
				if (expr_const_will_overflow(&expr->const_expr, type_flatten(type)->type_kind))
				{
					return expr;
				}
				return NULL;
			}
			assert(type_is_float(type));
			assert(expr->const_expr.const_kind == CONST_FLOAT);
			if (!expr_const_float_fits_type(&expr->const_expr, type_flatten(type)->type_kind))
			{
				return expr;
			}
			return NULL;
		case EXPR_POST_UNARY:
			expr = expr->unary_expr.expr;
			goto RETRY;
		case EXPR_GROUP:
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

static void sema_error_const_int_out_of_range(Expr *expr, Expr *problem, Type *to_type)
{
	assert(expr->expr_kind == EXPR_CONST);
	if (expr->const_expr.is_character)
	{
		SEMA_ERROR(problem, "The unicode character U+%04x cannot fit in a %s.", (uint32_t)expr->const_expr.ixx.i.low, type_quoted_error_string(to_type));
		return;
	}
	if (expr->const_expr.const_kind == CONST_ENUM)
	{
		SEMA_ERROR(problem, "The ordinal '%d' is out of range for %s, so you need an explicit cast to truncate the value.",
				   expr->const_expr.enum_err_val->var.index,
		           type_quoted_error_string(to_type));
		return;
	}
	const char *error_value = expr->const_expr.is_hex ? int_to_str(expr->const_expr.ixx, 16) : expr_const_to_error_string(&expr->const_expr);
	SEMA_ERROR(problem, "The value '%s' is out of range for %s, so you need an explicit cast to truncate the value.", error_value,
	           type_quoted_error_string(to_type));
}


static inline bool cast_maybe_string_lit_to_char_array(Expr *expr, Type *expr_canonical, Type *to_canonical, Type *to_original)
{
	if (expr->expr_kind != EXPR_CONST || expr->const_expr.const_kind != CONST_STRING) return false;
	if (expr_canonical->type_kind != TYPE_POINTER) return false;
	if (to_canonical->type_kind != TYPE_ARRAY && to_canonical->type_kind != TYPE_INFERRED_ARRAY) return false;
	if (to_canonical->array.base != type_char) return false;
	Type *pointer = expr_canonical->pointer;
	if (pointer->type_kind != TYPE_ARRAY) return false;
	if (pointer->array.base != type_char) return false;
	assert(!type_is_optional(expr->type));
	if (to_canonical->type_kind == TYPE_INFERRED_ARRAY)
	{
		assert(to_original->type_kind == TYPE_INFERRED_ARRAY);
		to_original = type_get_array(to_original->array.base, pointer->array.len);
	}
	expr->type = to_original;
	return true;
}

bool cast_untyped_to_type(SemaContext *context, Expr *expr, Type *to_type)
{
	recursively_rewrite_untyped_list(expr, expr->const_expr.untyped_list);
	if (!sema_expr_analyse_initializer_list(context, type_flatten(to_type), expr)) return false;
	expr->type = to_type;
	return true;
}

static void recursively_rewrite_untyped_list(Expr *expr, Expr **list)
{
	expr->expr_kind = EXPR_INITIALIZER_LIST;
	expr->initializer_list = list;
	expr->resolve_status = RESOLVE_NOT_DONE;
	FOREACH_BEGIN(Expr *inner, list)
		if (expr_is_const_untyped_list(inner))
		{
			recursively_rewrite_untyped_list(inner, inner->const_expr.untyped_list);
		}
	FOREACH_END();
}

bool cast_implicit(SemaContext *context, Expr *expr, Type *to_type)
{
	return cast_expr_inner(context, expr, to_type, false, false, false);
}

bool cast_implicit_maybe_optional(SemaContext *context, Expr *expr, Type *to_type, bool may_be_optional)
{
	return cast_expr_inner(context, expr, to_type, false, false, !may_be_optional);
}

bool cast_implicit_silent(SemaContext *context, Expr *expr, Type *to_type)
{
	return cast_expr_inner(context, expr, to_type, false, true, false);
}

bool cast_explicit(SemaContext *context, Expr *expr, Type *to_type)
{
	return cast_expr_inner(context, expr, to_type, true, false, false);
}

static inline bool cast_with_optional(Expr *expr, Type *to_type, bool add_optional)
{
	if (!cast(expr, to_type)) return false;
	if (add_optional) expr->type = type_add_optional(expr->type, true);
	return true;
}

static inline bool sema_error_cannot_convert(Expr *expr, Type *to, bool may_cast_explicit, bool silent)
{
	if (silent) return false;
	if (may_cast_explicit)
	{
		SEMA_ERROR(expr,
		           "Implicitly casting %s to %s is not permitted, but you can do an explicit cast by placing '(%s)' before the expression.",
		           type_quoted_error_string(type_no_optional(expr->type)),
		           type_quoted_error_string(to),
		           type_to_error_string(type_no_optional(to)));
	}
	else
	{
		SEMA_ERROR(expr,
		           "It is not possible to convert %s to %s.",
				   type_quoted_error_string(type_no_optional(expr->type)), type_quoted_error_string(to));
	}
	return false;

}

static inline bool cast_subarray(SemaContext *context, Expr *expr, Type *from, Type *to, Type *to_type, bool add_optional, bool is_explicit, bool silent)
{
	switch (to->type_kind)
	{
		case TYPE_SUBARRAY:
			if (type_array_element_is_equivalent(from->array.base, to->array.base, is_explicit)) goto CAST;
			return sema_error_cannot_convert(expr, to, !is_explicit && type_array_element_is_equivalent(from->array.base, to->array.base, true), silent);
		case TYPE_POINTER:
			if (to == type_voidptr) goto CAST;
			if (type_array_element_is_equivalent(from->array.base, to->pointer, is_explicit)) goto CAST;
			return sema_error_cannot_convert(expr, to, !is_explicit && type_array_element_is_equivalent(from->array.base, to->pointer, true), silent);
		case TYPE_BOOL:
			if (!is_explicit) goto CAST_MAY_EXPLICIT;
			goto CAST;
		default:
			goto CAST_ILLEGAL;
	}
CAST_ILLEGAL:
	return sema_error_cannot_convert(expr, to_type, false, silent);
CAST_MAY_EXPLICIT:
	return sema_error_cannot_convert(expr, to_type, true, silent);
CAST:
	return cast_with_optional(expr, to_type, add_optional);
}

static inline bool cast_array(SemaContext *context, Expr *expr, Type *from, Type *to, Type *to_type, bool add_optional, bool is_explicit, bool silent)
{
	bool infer_type = false;
	switch (to->type_kind)
	{
		case TYPE_INFERRED_ARRAY:
		case TYPE_INFERRED_VECTOR:
			infer_type = true;
			goto CAST_ELEMENT;
		case TYPE_VECTOR:
		case TYPE_ARRAY:
			if (to->array.len != from->array.len) goto CAST_ILLEGAL;
			goto CAST_ELEMENT;
		case TYPE_STRUCT:
			if (type_is_structurally_equivalent(from, to))
			{
				if (is_explicit) goto CAST;
				if (silent) return false;
				return sema_error_cannot_convert(expr, to_type, true, silent);
			}
		default:
			goto CAST_ILLEGAL;
	}
CAST_ELEMENT:
	if (!type_array_element_is_equivalent(from->array.base, to->array.base, is_explicit))
	{
		if (!silent && !is_explicit && type_array_element_is_equivalent(from->array.base, to->array.base, true))
		{
			return sema_error_cannot_convert(expr, to_type, true, silent);
		}
		goto CAST_ILLEGAL;
	}
CAST:
	if (infer_type)
	{
		to_type = type_infer_len_from_actual_type(to_type, from);
	}
	return cast_with_optional(expr, to_type, add_optional);
CAST_ILLEGAL:
	return sema_error_cannot_convert(expr, to_type, false, silent);
}

static bool cast_may_number_convert(Expr *expr, Type *from, Type *to, bool is_explicit, bool cast_from_bool)
{
	// Same canonical type - we're fine.
	if (from == to) return true;

	if (from->type_kind == TYPE_DISTINCT || to->type_kind == TYPE_DISTINCT) return false;

	// Handle floats
	if (type_is_float(to))
	{
		// Any integer may convert to a float.
		if (type_is_integer(from)) return true;

		// Cast from bool if explicit or cast from bool.
		if (from == type_bool) return cast_from_bool || is_explicit;

		// Any narrower float
		if (type_is_float(from))
		{
			if (is_explicit) return true;
			ByteSize to_size = type_size(to);
			ByteSize from_size = type_size(from);
			if (to_size == from_size) return true;
			return to_size > from_size && expr_is_simple(expr);
		}

		UNREACHABLE;
	}

	// Handle ints
	if (type_is_integer(to))
	{
		if (type_is_float(from)) return is_explicit;

		// Cast from bool if explicit or cast from bool.
		if (from == type_bool) return cast_from_bool || is_explicit;

		if (type_is_integer(from))
		{
			if (is_explicit) return true;
			ByteSize to_size = type_size(to);
			ByteSize from_size = type_size(from);
			if (to_size == from_size) return true;
			return to_size > from_size && expr_is_simple(expr);
		}

		UNREACHABLE
	}
	assert(to == type_bool);
	if (!is_explicit) return false;
	return true;
}
static inline bool cast_vector(SemaContext *context, Expr *expr, Type *from, Type *to, Type *to_type, bool add_optional, bool is_explicit, bool silent)
{
	bool is_scaled = from->type_kind == TYPE_SCALED_VECTOR;
	bool to_vector = type_kind_is_any_vector(to->type_kind);
	bool infer_len = false;
	switch (to->type_kind)
	{
		case TYPE_INFERRED_ARRAY:
			infer_len = true;
			if (is_scaled) goto CAST_ILLEGAL;
			goto TRY_CAST;
		case TYPE_INFERRED_VECTOR:
			infer_len = true;
			if (is_scaled) goto CAST_ILLEGAL;
			goto TRY_CONVERT;
		case TYPE_ARRAY:
			if (is_scaled || to->array.len != from->array.len) goto CAST_ILLEGAL;
			goto TRY_CAST;
		case TYPE_VECTOR:
			if (is_scaled || to->array.len != from->array.len) goto CAST_ILLEGAL;
			goto TRY_CONVERT;
		case TYPE_SCALED_VECTOR:
			if (!is_scaled) goto CAST_ILLEGAL;
			goto TRY_CONVERT;
		default:
			goto CAST_ILLEGAL;
	}
TRY_CONVERT:
	if (cast_may_number_convert(expr, from->array.base, to->array.base, is_explicit, to_vector)) goto CAST;
	return sema_error_cannot_convert(expr, to, !is_explicit && cast_may_number_convert(expr, from->array.base, to->array.base, true, to_vector), silent);
TRY_CAST:
	if (type_array_element_is_equivalent(from->array.base, to->array.base, is_explicit)) goto CAST;
	if (silent) return false;
	return sema_error_cannot_convert(expr, to, !is_explicit && type_array_element_is_equivalent(from->array.base, to->array.base, true), true);
CAST_ILLEGAL:
	return sema_error_cannot_convert(expr, to_type, false, silent);
CAST:
	if (infer_len) to_type = type_infer_len_from_actual_type(to_type, from);
	return cast_with_optional(expr, to_type, add_optional);
}

static inline bool cast_integer(SemaContext *context, Expr *expr, Type *from, Type *to, Type *to_type, bool add_optional, bool is_explicit, bool silent)
{
	bool no_report = silent;
RETRY:
	switch (to->type_kind)
	{
		case ALL_FLOATS:
			goto CAST;
		case TYPE_BOOL:
			if (is_explicit) goto CAST;
			goto REQUIRE_CAST;
		case TYPE_VECTOR:
			to = to->array.base->canonical;
			goto RETRY;
		case TYPE_ENUM:
			if (from->type_kind == TYPE_ENUM) break;
			to = to->decl->enums.type_info->type->canonical;
			if (is_explicit) to = type_flatten_distinct(to);
			FALLTHROUGH;
		case ALL_INTS:
		{
			if (is_explicit) goto CAST;
			ByteSize to_size = type_size(to);
			ByteSize from_size = type_size(from);
			if (to_size > from_size) goto ONLY_SIMPLE;
			if (expr_is_const(expr) && expr_const_will_overflow(&expr->const_expr, type_flatten(to)->type_kind))
			{
				sema_error_const_int_out_of_range(expr, expr, to_type);
				return false;
			}
			if (to_size == from_size) goto CAST;
			assert(to == type_flatten(to));
			Expr *problem = recursive_may_narrow(expr, to);
			if (problem)
			{
				if (no_report) return false;
				expr = problem;
				goto REQUIRE_CAST;
			}
			goto CAST;
		}
		case TYPE_DISTINCT:
			if (expr_is_const(expr))
			{
				to = type_flatten_distinct(to);
				goto RETRY;
			}
			else
			{
				if (no_report) return false;
				bool may_explicit = cast_expr_inner(context, expr_copy(expr), to_type, true, true, false);
				if (may_explicit) goto REQUIRE_CAST;
				break;
			}
		case TYPE_POINTER:
		{
			if (from->type_kind == TYPE_ENUM) break;
			bool may_cast = expr_is_const(expr) || type_size(from) >= type_size(type_voidptr);
			if (!is_explicit)
			{
				if (may_cast) goto REQUIRE_CAST;
				break;
			}
			if (!may_cast)
			{
				if (no_report) return false;
				SEMA_ERROR(expr, "You cannot cast from a type smaller than %s.",
				           type_quoted_error_string(type_iptr));
				return false;
			}
			goto CAST;
		}
		default:
			break;
	}
	return sema_error_cannot_convert(expr, to_type, false, no_report);
ONLY_SIMPLE:
	if (expr_is_simple(expr)) goto CAST;
	if (no_report) return false;
	SEMA_ERROR(expr, "This conversion requires an explicit cast to %s, because the widening of the expression may be done in more than one way.", type_quoted_error_string(to_type));
	return false;
REQUIRE_CAST:
	if (no_report) return false;
	SEMA_ERROR(expr, "%s cannot implicitly be converted to %s, but you may use a cast.", type_quoted_error_string(expr->type), type_quoted_error_string(to_type));
	return false;
CAST:
	return cast_with_optional(expr, to_type, add_optional);
}

static inline bool cast_float(SemaContext *context, Expr *expr, Type *from, Type *to, Type *to_type, bool add_optional, bool is_explicit, bool silent)
{
RETRY:
	switch (to->type_kind)
	{
		case ALL_INTS:
		case TYPE_BOOL:
			if (is_explicit) goto CAST;
			goto REQUIRE_CAST;
		case TYPE_VECTOR:
			to = to->array.base->canonical;
			goto RETRY;
		case ALL_FLOATS:
		{
			if (is_explicit) goto CAST;
			ByteSize to_size = type_size(to);
			ByteSize from_size = type_size(from);
			if (to_size > from_size) goto ONLY_SIMPLE;
			if (expr_is_const(expr) && expr_const_will_overflow(&expr->const_expr, type_flatten(to)->type_kind))
			{
				if (silent) return false;
				SEMA_ERROR(expr, "The value '%s' is out of range for %s, so you need an explicit cast to truncate the value.", expr_const_to_error_string(&expr->const_expr),
					           type_quoted_error_string(to_type));
				return false;
			}
			if (to_size == from_size) goto CAST;
			Expr *problem = recursive_may_narrow(expr, to);
			if (problem)
			{
				if (silent) return false;
				expr = problem;
				goto REQUIRE_CAST;
			}
			goto CAST;
		}
		case TYPE_DISTINCT:
			if (expr_is_const(expr))
			{
				to = type_flatten_distinct(to);
				goto RETRY;
			}
			else
			{
				if (silent) return false;
				bool may_explicit = cast_expr_inner(context, expr_copy(expr), to_type, true, true, false);
				if (may_explicit) goto REQUIRE_CAST;
				break;
			}
		default:
			break;
	}
	return sema_error_cannot_convert(expr, to_type, false, silent);
ONLY_SIMPLE:
	if (expr_is_simple(expr)) goto CAST;
	if (silent) return false;
	SEMA_ERROR(expr, "This conversion requires an explicit cast to %s, because the widening of the expression may be done in more than one way.", type_quoted_error_string(to_type));
	return false;
REQUIRE_CAST:
	if (silent) return false;
	SEMA_ERROR(expr, "%s cannot implicitly be converted to %s, but you may use a cast.", type_quoted_error_string(expr->type), type_quoted_error_string(to_type));
	return false;
CAST:
	return cast_with_optional(expr, to_type, add_optional);
}

static inline bool cast_pointer(SemaContext *context, Expr *expr, Type *from, Type *to, Type *to_type, bool add_optional, bool is_explicit, bool silent)
{
	// pointer -> any, void* -> pointer pointer -> void*
	if (to == type_any || to == type_voidptr || (from == type_voidptr && type_is_pointer(to))) return cast_with_optional(expr, to_type, add_optional);

	Type *pointee = from->pointer;
	pointee = is_explicit ? type_flatten(pointee) : pointee->canonical;
	TypeKind pointee_kind = pointee->type_kind;
	switch (to->type_kind)
	{
		case ALL_INTS:
			if (!is_explicit) return sema_error_cannot_convert(expr, to_type, true, silent);
			if (type_size(to_type) < type_size(type_iptr))
			{
				SEMA_ERROR(expr, "Casting %s to %s is not allowed because '%s' is smaller than a pointer. "
								 "Use (%s)(iptr) if you want this lossy cast.",
								 type_quoted_error_string(expr->type), type_quoted_error_string(to_type),
								 type_to_error_string(to_type), type_to_error_string(to_type));
				return false;
			}
			return cast_with_optional(expr, to_type, add_optional);
		case TYPE_SUBARRAY:
			// int[<2>], int[2], int[<*>]
			if (pointee_kind == TYPE_ARRAY || pointee_kind == TYPE_VECTOR || pointee_kind == TYPE_SCALED_VECTOR)
			{
				Type *subarray_base = to->array.base->canonical;
				Type *from_base = pointee->array.base;
				if (is_explicit)
				{
					subarray_base = type_flatten_distinct(subarray_base);
					from_base = type_flatten_distinct(from_base);
				}
				if (subarray_base == from_base) return cast_with_optional(expr, to_type, add_optional);
				if (subarray_base->type_kind == TYPE_POINTER && from_base->type_kind == TYPE_POINTER)
				{
					if (type_is_pointer_equivalent(subarray_base, from_base, is_explicit)) return cast_with_optional(expr, to_type, add_optional);
				}
				if (silent) return false;
				bool would_work_explicit = false;
				if (!is_explicit)
				{
					would_work_explicit = cast_pointer(context, expr_copy(expr), from, to, to_type, add_optional, true, true);
				}
				return sema_error_cannot_convert(expr, to_type, would_work_explicit, false);
			}
			return sema_error_cannot_convert(expr, to_type, false, silent);
		case TYPE_BOOL:
			if (is_explicit) return cast_with_optional(expr, to_type, add_optional);
			return sema_error_cannot_convert(expr, to_type, true, silent);
		case TYPE_POINTER:
			if (is_explicit) return cast_with_optional(expr, to_type, add_optional);
			if (cast_may_implicit_ptr(from->pointer, to->pointer))
			{
				return cast_with_optional(expr, to_type, add_optional);
			}
			return sema_error_cannot_convert(expr, to_type, true, silent);
		case TYPE_OPTIONAL_ANY:
		case TYPE_OPTIONAL:
			UNREACHABLE
		default:
			return sema_error_cannot_convert(expr, to_type, false, silent);
	}
}
/**
 * Do the following:
 * 1. Special optional conversions.
 * 2. String literal conversions
 * 3. Constant pointer conversions
 */
static bool cast_expr_inner(SemaContext *context, Expr *expr, Type *to_type, bool is_explicit, bool silent,
                            bool may_not_be_optional)
{
	Type *from_type = expr->type;

	assert(!type_is_optional(to_type) || may_not_be_optional);

	// Allow (void)foo
	if (is_explicit && to_type == type_void)
	{
		return cast(expr, to_type);
	}

	Type *to = is_explicit ? type_flatten_distinct_optional(to_type) : type_no_optional(to_type)->canonical;

	// Step one, cast from optional.
	// This handles:
	// 1. *! -> any type
	// 2. void! -> anyerr
	// 3. void! -> SomeFault (explicit)
	if (type_is_optional(from_type))
	{
		// *! -> int => ok, gives int!
		if (from_type == type_anyfail)
		{
			if (may_not_be_optional)
			{
				if (silent) return false;
				SEMA_ERROR(expr, "An optional value cannot be converted to a non-optional %s.", type_quoted_error_string(to_type));
				return false;
			}
			// Just add the optional.
			expr->type = type_add_optional(to_type, true);
			return true;
		}

		// Here we have something like int!
		assert(from_type->type_kind == TYPE_OPTIONAL);

		// If it is void!, then there are special rules:
		if (from_type->optional == type_void)
		{
			// void! x; anyerr y = x;
			if (!type_is_optional(to_type) && to == type_anyerr)
			{
				cast(expr, to_type);
				return true;
			}

			// void! x; FooFault y = (FooFault)x;
			// Only allowed if explicit.
			if (to->type_kind == TYPE_FAULTTYPE)
			{
				if (!is_explicit)
				{
					if (silent) return false;
					SEMA_ERROR(expr, "A 'void!' can only be cast into %s using an explicit cast. You can try using (%s)",
					           type_quoted_error_string(to_type), type_to_error_string(to_type));
					return false;
				}
				cast(expr, to_type);
				return true;
			}
		}
		if (may_not_be_optional)
		{
			if (silent) return false;
			char *format = is_explicit ? "Cannot cast an optional %s to %s." : "Cannot convert an optional %s to %s.";
			SEMA_ERROR(expr, format, type_quoted_error_string(from_type), type_quoted_error_string(to_type));
			return false;
		}
	}

	// We're now done and can remove the optional
	bool add_optional = type_is_optional(to_type) || type_is_optional(from_type);
	to_type = type_no_optional(to_type);
	from_type = type_no_optional(from_type);

	// Grab the underlying expression type.
	Type *from = is_explicit ? type_flatten_distinct(from_type) : from_type->canonical;

	// We may already be done.
	if (from == to)
	{
		expr->type = type_add_optional(to_type, add_optional);
		return true;
	}


	// Handle strings, these don't actually mess with the underlying data,
	// just the type.
	if (cast_maybe_string_lit_to_char_array(expr, from, to, to_type)) return true;

	// For constant pointers cast into anything pointer-like:
	if (expr_is_const_pointer(expr) && from == type_voidptr && type_flatten_distinct(to)->type_kind == TYPE_POINTER)
	{
		assert(!add_optional);
		expr->type = to_type;
		return true;
	}
	switch (from->type_kind)
	{
		case TYPE_UNTYPED_LIST:
			if (!cast_untyped_to_type(context, expr, to_type)) return false;
			if (add_optional) expr->type = type_add_optional(expr->type, true);
			return true;
		case TYPE_FAULTTYPE:
			// Allow MyError.A -> error, to an integer or to bool
			if (to == type_anyerr) return cast(expr, to_type);
			if (type_is_integer(to) || to == type_bool) goto CAST_IF_EXPLICIT;
			goto CAST_FAILED;
		case TYPE_ANYERR:
			if (to_type == type_bool || to->type_kind == TYPE_FAULTTYPE || type_is_integer(to))
			{
				goto CAST_IF_EXPLICIT;
			}
			goto CAST_FAILED;
		case TYPE_POINTER:
			return cast_pointer(context, expr, from, to, to_type, add_optional, is_explicit, silent);
		case TYPE_SUBARRAY:
			return cast_subarray(context, expr, from, to, to_type, add_optional, is_explicit, silent);
		case TYPE_BOOL:
			// Bool may convert into integers and floats but only explicitly.
			if (type_is_integer(to) || type_is_float(to)) goto CAST_IF_EXPLICIT;
			goto CAST_FAILED;
		case TYPE_VECTOR:
		case TYPE_SCALED_VECTOR:
			return cast_vector(context, expr, from, to, to_type, add_optional, is_explicit, silent);
		case TYPE_ARRAY:
			return cast_array(context, expr, from, to, to_type, add_optional, is_explicit, silent);
		case TYPE_ENUM:
		case ALL_INTS:
			return cast_integer(context, expr, from, to, to_type, add_optional, is_explicit, silent);
		case ALL_FLOATS:
			return cast_float(context, expr, from, to, to_type, add_optional, is_explicit, silent);
		case TYPE_POISONED:
			return false;
		case TYPE_VOID:
		case TYPE_INFERRED_ARRAY:
		case TYPE_TYPEINFO:
		case TYPE_FUNC:
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_INFERRED_VECTOR:
		case TYPE_MEMBER:
			goto CAST_FAILED;
		case TYPE_TYPEID:
			if (!type_is_pointer_sized_or_more(to_type)) goto CAST_FAILED;
			goto CAST_IF_EXPLICIT;
		case TYPE_OPTIONAL:
		case TYPE_OPTIONAL_ANY:
		case TYPE_TYPEDEF:
			UNREACHABLE;
		case TYPE_ANY:
			if (to->type_kind != TYPE_POINTER) goto CAST_FAILED;
			goto CAST_IF_EXPLICIT;
		case TYPE_STRUCT:
			if (type_is_substruct(from))
			{
				Type *type;
				Expr *access = expr_access_inline_member(expr_copy(expr), from->decl);
				while (1)
				{
					type = access->type->canonical;
					if (type == to) break;
					if (!type_is_substruct(type)) goto CAST_FAILED;
				}
				expr_replace(expr, access);
				return true;
			}
			goto CAST_FAILED;
		case TYPE_UNION:
			goto CAST_FAILED;
		case TYPE_BITSTRUCT:
			// A bitstruct can convert to:
			// 1. An int of the same length
			// 2. An integer array of the same length
			if (type_size(to_type) != type_size(from_type)) goto CAST_FAILED;
			if (type_is_integer(to_type)) goto CAST_IF_EXPLICIT;
			if (to_type->type_kind == TYPE_ARRAY && type_is_integer(to_type->array.base)) goto CAST_IF_EXPLICIT;
			goto CAST_FAILED;
			UNREACHABLE
		case TYPE_DISTINCT:
			assert(!is_explicit);
			if (cast_expr_inner(context, expr, to_type, true, true, false))
			{
				goto CAST_IF_EXPLICIT;
			}
			goto CAST_FAILED;
	}
	UNREACHABLE

CAST_IF_EXPLICIT:
	if (!is_explicit)
	{
		if (silent) return false;
		SEMA_ERROR(expr,
		           "Implicitly casting %s to %s is not permitted, but you can do an explicit cast by placing '(%s)' before the expression.",
		           type_quoted_error_string(type_no_optional(from_type)),
		           type_quoted_error_string(type_no_optional(to_type)),
		           type_to_error_string(to_type));
		return false;
	}
	return cast(expr, to_type);
CAST_FAILED:
	if (!silent)
	{
		if (!is_explicit)
		{
			SEMA_ERROR(expr, "You cannot cast %s into %s even with an explicit cast, so this looks like an error.", type_quoted_error_string(expr->type), type_quoted_error_string(to_type));
			return false;
		}
		return sema_error_failed_cast(expr, expr->type, to_type);
	}
	return false;
}

static void vector_const_initializer_convert_to_type(ConstInitializer *initializer, Type *to_type)
{
	switch (initializer->kind)
	{
		case CONST_INIT_ARRAY:
		{
			Type *element_type = type_flatten(to_type)->array.base;
			FOREACH_BEGIN(ConstInitializer *element, initializer->init_array.elements)
				vector_const_initializer_convert_to_type(element, element_type);
			FOREACH_END();
			break;
		}
		case CONST_INIT_ARRAY_FULL:
		{
			Type *element_type = type_flatten(to_type)->array.base;
			FOREACH_BEGIN(ConstInitializer *element, initializer->init_array_full)
				vector_const_initializer_convert_to_type(element, element_type);
			FOREACH_END();
			break;
		}
		case CONST_INIT_VALUE:
		{
			Type *to_flat = type_flatten(to_type);
			bool is_neg_conversion = to_flat && type_flatten(initializer->type) == type_bool;
			if (is_neg_conversion)
			{
				bool is_true = initializer->init_value->const_expr.b;
				initializer->init_value->const_expr.ixx = (Int)
						{ .i = is_true ? (Int128) { UINT64_MAX, UINT64_MAX } : (Int128) { 0, 0 },
						  .type = to_flat->type_kind };
				initializer->init_value->type = to_type;
			}
			else
			{
				cast(initializer->init_value, to_type);
			}
			break;
		}
		case CONST_INIT_ZERO:
			break;
		case CONST_INIT_UNION:
		case CONST_INIT_STRUCT:
			UNREACHABLE
		case CONST_INIT_ARRAY_VALUE:
			vector_const_initializer_convert_to_type(initializer->init_array_value.element, to_type);
			break;
	}
	initializer->type = to_type;
}


static bool err_to_anyerr(Expr *expr, Type *to_type)
{
	expr->type = to_type;
	return true;
}

static bool err_to_bool(Expr *expr, Type *to_type)
{
	if (insert_runtime_cast_unless_const(expr, CAST_ERBOOL, to_type)) return true;

	assert(expr->const_expr.const_kind == CONST_ERR);

	expr_rewrite_const_bool(expr, type_bool, expr->const_expr.enum_err_val != NULL);
	return true;
}

static inline bool subarray_to_bool(Expr *expr)
{
	if (expr->expr_kind == EXPR_CONST && expr->const_expr.const_kind == CONST_INITIALIZER)
	{
		ConstInitializer *list = expr->const_expr.initializer;
		switch (list->kind)
		{
			case CONST_INIT_ZERO:
				expr_rewrite_const_bool(expr, type_bool, false);
				return true;
			case CONST_INIT_ARRAY:
				expr_rewrite_const_bool(expr, type_bool, vec_size(list->init_array.elements) > 0);
				return true;
			case CONST_INIT_ARRAY_FULL:
				expr_rewrite_const_bool(expr, type_bool, vec_size(list->init_array_full) > 0);
				return true;
			case CONST_INIT_STRUCT:
			case CONST_INIT_UNION:
			case CONST_INIT_VALUE:
			case CONST_INIT_ARRAY_VALUE:
				break;
		}
	}
	return insert_cast(expr, CAST_SABOOL, type_bool);
}

static bool cast_inner(Expr *expr, Type *from_type, Type *to, Type *to_type)
{
	switch (from_type->type_kind)
	{
		case TYPE_OPTIONAL_ANY:
		case TYPE_OPTIONAL:
		case TYPE_VOID:
			UNREACHABLE
		case TYPE_DISTINCT:
		case TYPE_FUNC:
		case TYPE_TYPEDEF:
		case CT_TYPES:
			UNREACHABLE
		case TYPE_BITSTRUCT:
			return bitstruct_cast(expr, type_flatten(from_type->decl->bitstruct.base_type->type), to, to_type);
		case TYPE_BOOL:
			// Bool may convert into integers and floats but only explicitly.
			if (type_is_integer(to)) return bool_to_int(expr, to, to_type);
			if (type_is_float(to)) return bool_to_float(expr, to, to_type);
			break;
		case TYPE_ANYERR:
			if (to->type_kind == TYPE_BOOL) return insert_cast(expr, CAST_EUBOOL, to_type);
			if (to->type_kind == TYPE_FAULTTYPE)
			{
				REMINDER("Improve anyerr -> fault conversion.");
				return insert_cast(expr, CAST_EUER, to_type);
			}
			if (type_is_integer(to)) return insert_cast(expr, CAST_EUINT, to_type);
			break;
		case ALL_SIGNED_INTS:
			if (type_is_integer(to)) return integer_to_integer(expr, to, to_type);
			if (type_is_float(to)) return integer_to_float(expr, to, to_type);
			if (to == type_bool) return integer_to_bool(expr, to_type);
			if (to->type_kind == TYPE_POINTER) return integer_to_pointer(expr, to_type);
			if (to->type_kind == TYPE_ENUM) return integer_to_enum(expr, to, to_type);
			if (type_kind_is_any_vector(to->type_kind)) return integer_expand_to_vector_conversion(expr, to, to_type);
			break;
		case ALL_UNSIGNED_INTS:
			if (type_is_integer(to)) return integer_to_integer(expr, to, to_type);
			if (type_is_float(to)) return integer_to_float(expr, to, to_type);
			if (to == type_bool) return integer_to_bool(expr, to_type);
			if (to->type_kind == TYPE_POINTER) return integer_to_pointer(expr, to_type);
			if (to->type_kind == TYPE_ENUM) return integer_to_enum(expr, to, to_type);
			if (type_kind_is_any_vector(to->type_kind)) return integer_expand_to_vector_conversion(expr, to, to_type);
			break;
		case ALL_FLOATS:
			if (type_is_integer(to)) return float_to_integer(expr, to, to_type);
			if (to == type_bool) return float_to_bool(expr, to_type);
			if (type_is_float(to)) return float_to_float(expr, to, to_type);
			if (type_kind_is_any_vector(to->type_kind)) return float_expand_to_vector_conversion(expr, to, to_type);
			break;
		case TYPE_TYPEID:
		case TYPE_POINTER:
			if (type_is_integer(to)) return pointer_to_integer(expr, to_type);
			if (to->type_kind == TYPE_BOOL) return pointer_to_bool(expr, to_type);
			if (to->type_kind == TYPE_POINTER) return pointer_to_pointer(expr, to_type);
			if (to->type_kind == TYPE_SUBARRAY) return insert_cast(expr, CAST_APTSA, to_type);
			if (to->type_kind == TYPE_ANY) return insert_cast(expr, CAST_PTRANY, to_type);
			break;
		case TYPE_ANY:
			if (to->type_kind == TYPE_POINTER) return insert_cast(expr, CAST_ANYPTR, to_type);
			break;
		case TYPE_ENUM:
			enum_to_int_lowering(expr);
			if (type_is_integer(to)) return integer_to_integer(expr, to, to_type);
			if (type_is_float(to)) return integer_to_float(expr, to, to_type);
			if (to == type_bool) return integer_to_bool(expr, to_type);
			if (to->type_kind == TYPE_POINTER) return integer_to_pointer(expr, to_type);
			if (to->type_kind == TYPE_ENUM) return integer_to_enum(expr, to, to_type);
			break;
		case TYPE_FAULTTYPE:
			if (to->type_kind == TYPE_ANYERR) return err_to_anyerr(expr, to_type);
			if (to == type_bool) return err_to_bool(expr, to_type);
			if (type_is_integer(to)) return insert_cast(expr, CAST_ERINT, to_type);
			break;
		case TYPE_FLEXIBLE_ARRAY:
		case TYPE_SCALED_VECTOR:
			return false;
		case TYPE_ARRAY:
			if (to->type_kind == TYPE_VECTOR) return array_to_vector(expr, to_type);
			FALLTHROUGH;
		case TYPE_STRUCT:
		case TYPE_UNION:

			if (to->type_kind == TYPE_ARRAY || to->type_kind == TYPE_STRUCT || to->type_kind == TYPE_UNION)
			{
				return insert_cast(expr, CAST_STST, to_type);
			} // Starting in a little while...
			break;
		case TYPE_SUBARRAY:
			if (to->type_kind == TYPE_POINTER) return insert_cast(expr, CAST_SAPTR, to);
			if (to->type_kind == TYPE_BOOL) return subarray_to_bool(expr);
			if (to->type_kind == TYPE_SUBARRAY) return subarray_to_subarray(expr, to);
			break;
		case TYPE_VECTOR:
			if (to->type_kind == TYPE_ARRAY) return vector_to_array(expr, to_type);
			if (to->type_kind == TYPE_VECTOR) return vector_to_vector(expr, to_type);
			break;
	}
	UNREACHABLE
}


bool cast(Expr *expr, Type *to_type)
{
	if (to_type == type_void)
	{
		insert_cast(expr, CAST_VOID, type_void);
		return true;
	}

	assert(!type_is_optional(to_type));
	Type *from_type = expr->type;
	bool from_is_optional = false;
	Type *to = type_flatten_distinct(to_type);

	// Special case *! => error
	if (to == type_anyerr || to->type_kind == TYPE_FAULTTYPE)
	{
		if (type_is_optional(from_type)) return voidfail_to_error(expr, to_type);
	}

	if (type_is_optional_any(from_type))
	{
		expr->type = type_get_optional(to_type);
		return true;
	}

	if (type_is_optional_type(from_type))
	{
		from_type = from_type->optional;
		from_is_optional = true;
	}
	from_type = type_flatten_distinct(from_type);
	if (type_len_is_inferred(to_type))
	{
		to_type = from_type;
		to = type_flatten(from_type);
	}
	if (from_type == to)
	{
		expr->type = type_add_optional(to_type, from_is_optional);
		if (expr->expr_kind == EXPR_CONST)
		{
			expr->const_expr.is_hex = false;
		}
		return true;
	}

	if (!cast_inner(expr, from_type, to, to_type)) return false;

	Type *result_type = expr->type;
	if (from_is_optional && !type_is_optional(result_type))
	{
		expr->type = type_get_optional(result_type);
	}
	return true;
}

bool cast_to_index(Expr *index)
{
	Type *type = index->type->canonical;
	RETRY:
	switch (type->type_kind)
	{
		case TYPE_I8:
		case TYPE_I16:
		case TYPE_I32:
		case TYPE_I64:
			return cast(index, type_isz);
		case TYPE_U8:
		case TYPE_U16:
		case TYPE_U32:
		case TYPE_U64:
			return cast(index, type_usz);
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

bool cast_widen_top_down(SemaContext *context, Expr *expr, Type *type)
{
	Type *to = type_no_optional(type);
	Type *from = type_no_optional(expr->type);
	RETRY:
	if (type_is_integer(from) && type_is_integer(to)) goto CONVERT_IF_BIGGER;
	if (type_is_float(from) && type_is_float(to)) goto CONVERT_IF_BIGGER;
	if (type_is_integer(from) && type_is_float(to)) goto CONVERT;
	if (type_flat_is_vector(from) && type_flat_is_vector(to))
	{
		to = type_vector_type(to);
		from = type_vector_type(from);
		goto RETRY;
	}
	return true;
	CONVERT_IF_BIGGER:
	if (type_size(to) <= type_size(from)) return true;
	CONVERT:
	return cast_implicit(context, expr, type);
}


Type *cast_numeric_arithmetic_promotion(Type *type)
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
		case TYPE_OPTIONAL:
			UNREACHABLE
		default:
			return type;
	}
}




#pragma clang diagnostic pop