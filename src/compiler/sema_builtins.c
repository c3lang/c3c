// Copyright (c) 2022-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.
#include "sema_internal.h"


typedef enum
{
	BA_POINTER,
	BA_SIZE,
	BA_BOOL,
	BA_CHAR,
	BA_FLOATLIKE,
	BA_INTEGER,
	BA_FLOAT,
	BA_INTLIKE,
	BA_NUMLIKE,
	BA_BOOLVEC,
	BA_BOOLINTVEC,
	BA_BOOLINT,
	BA_INTVEC,
	BA_FLOATVEC,
	BA_VEC,
	BA_NUMVEC,
	BA_PTRVEC,
	BA_NUM,
	BA_TYPEID,
} BuiltinArg;

static bool sema_check_builtin_args_match(Expr **args, size_t arg_len);
static bool sema_check_builtin_args_const(Expr **args, size_t arg_len);
static bool sema_check_builtin_args(Expr **args, BuiltinArg *arg_type, size_t arg_len);
static bool sema_expr_analyse_compare_exchange(SemaContext *context, Expr *expr);
static bool sema_expr_analyse_syscall(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_swizzle(SemaContext *context, Expr *expr, bool swizzle_two);
static inline int builtin_expected_args(BuiltinFunction func);
static inline bool is_valid_atomicity(Expr* expr);
static bool sema_check_alignment_expression(SemaContext *context, Expr *align);

static bool sema_expr_is_valid_mask_for_value(Expr *expr, Expr *value)
{
	if (type_flatten(value->type)->array.len != type_flatten(expr->type)->array.len)
	{
		RETURN_SEMA_ERROR(expr, "The mask must have the same length as the value.");
	}
	return true;
}

/**
 * Used for when we have a builtin that has a constraint between argument types that
 * they should be the same.
 */
static bool sema_check_builtin_args_match(Expr **args, size_t arg_len)
{
	assert(arg_len > 1);
	Type *first = type_no_optional(args[0]->type->canonical);
	for (size_t i = 1; i < arg_len; i++)
	{
		if (first != type_no_optional(args[i]->type->canonical))
		{
			SEMA_ERROR(args[i], "Expected an expression of type %s.", type_quoted_error_string(args[0]->type));
			return false;
		}
	}
	return true;
}

/**
 * Check the constraint that the arguments are constant.
 */
static bool sema_check_builtin_args_const(Expr **args, size_t arg_len)
{
	for (size_t i = 0; i < arg_len; i++)
	{
		if (!expr_is_const(args[i])) RETURN_SEMA_ERROR(args[i], "Expected a compile time constant value for this argument.");
	}
	return true;
}

static bool sema_check_alignment_expression(SemaContext *context, Expr *align)
{
	if (!sema_analyse_expr_rhs(context, type_usz, align, false, NULL)) return false;
	if (!expr_is_const_int(align)
	    || !int_fits(align->const_expr.ixx, TYPE_U64)
	    || (!is_power_of_two(align->const_expr.ixx.i.low) && align->const_expr.ixx.i.low))
	{
		RETURN_SEMA_ERROR(align, "Expected a constant power-of-two alignment or zero.");
	}
	return true;
}

static bool sema_check_builtin_args(Expr **args, BuiltinArg *arg_type, size_t arg_len)
{
	for (size_t i = 0; i < arg_len; i++)
	{
		Expr *arg = args[i];
		Type *type = type_flatten(arg->type->canonical);
		switch (arg_type[i])
		{
			case BA_POINTER:
				if (type_is_pointer(type)) continue;
				RETURN_SEMA_ERROR(arg, "Expected a pointer.");
			case BA_CHAR:
				if (type == type_char || type == type_ichar) continue;
				RETURN_SEMA_ERROR(arg, "Expected a char or ichar.");
			case BA_SIZE:
				if (type_is_integer(type) && type_size(type) == type_size(type_usz)) continue;
				RETURN_SEMA_ERROR(arg, "Expected an usz or isz value.");
			case BA_BOOL:
				if (type == type_bool) continue;
				RETURN_SEMA_ERROR(arg, "Expected a bool.");
			case BA_NUM:
				if (type_is_number(type)) continue;
				RETURN_SEMA_ERROR(arg, "Expected an integer or a float.");
			case BA_TYPEID:
				if (type == type_typeid) continue;
				RETURN_SEMA_ERROR(arg, "Expected a typeid.");
			case BA_NUMLIKE:
				if (type_flat_is_numlike(type)) continue;
				RETURN_SEMA_ERROR(arg, "Expected a number or vector.");
			case BA_FLOATLIKE:
				if (type_flat_is_floatlike(type)) continue;
				RETURN_SEMA_ERROR(arg, "Expected a floating point or floating point vector, but was %s.",
							   type_quoted_error_string(type));
			case BA_VEC:
				if (type->type_kind == TYPE_VECTOR) continue;
				RETURN_SEMA_ERROR(arg, "Expected a vector.");
			case BA_PTRVEC:
				if (type_is_pointer_vector(type)) continue;
				RETURN_SEMA_ERROR(arg, "Expected a pointer vector.");
			case BA_NUMVEC:
				if (type->type_kind == TYPE_VECTOR && type_is_number_or_bool(type->array.base)) continue;
				RETURN_SEMA_ERROR(arg, "Expected a numeric vector.");
			case BA_INTVEC:
				if (type->type_kind == TYPE_VECTOR && type_flat_is_intlike(type->array.base)) continue;
				RETURN_SEMA_ERROR(arg, "Expected an integer vector.");
			case BA_BOOLINT:
				if (type_is_integer_or_bool_kind(type)) continue;
				RETURN_SEMA_ERROR(arg, "Expected a boolean or integer value.");
			case BA_BOOLVEC:
				if (type_flat_is_bool_vector(type)) continue;
				RETURN_SEMA_ERROR(arg, "Expected a boolean vector.");
			case BA_BOOLINTVEC:
				if (type->type_kind == TYPE_VECTOR && type_flat_is_boolintlike(type->array.base)) continue;
				RETURN_SEMA_ERROR(arg, "Expected a boolean or integer vector.");
			case BA_FLOATVEC:
				if (type->type_kind == TYPE_VECTOR && type_flat_is_floatlike(type->array.base)) continue;
				RETURN_SEMA_ERROR(arg, "Expected an float vector.");
			case BA_INTLIKE:
				if (type_flat_is_intlike(type)) continue;
				RETURN_SEMA_ERROR(arg, "Expected an integer or integer vector.");
			case BA_INTEGER:
				if (type_is_integer(type)) continue;
				RETURN_SEMA_ERROR(arg, "Expected an integer.");
			case BA_FLOAT:
				if (type_is_float(type)) continue;
				RETURN_SEMA_ERROR(arg, "Expected a float or double.");
		}
		UNREACHABLE
	}
	return true;
}

/**
 * Analyse both $$swizzle and $$swizzle2
 */
static inline bool sema_expr_analyse_swizzle(SemaContext *context, Expr *expr, bool swizzle_two)
{
	Expr **args = expr->call_expr.arguments;
	unsigned arg_count = vec_size(args);
	bool optional = false;
	int first_mask_value = swizzle_two ? 2 : 1;
	for (unsigned i = 0; i < first_mask_value; i++)
	{
		Expr *arg = args[i];
		// Analyse the expressions
		if (!sema_analyse_expr(context, arg)) return false;
		// Expect vector
		if (!type_flat_is_vector(arg->type)) RETURN_SEMA_ERROR(arg, "A vector was expected here.");
		// Optional-ness updated
		optional = optional || type_is_optional(args[i]->type);
	}
	// Ensure matching types
	if (swizzle_two && !sema_check_builtin_args_match(args, 2)) return false;

	unsigned components = type_flatten(args[0]->type)->array.len;
	if (swizzle_two) components *= 2;
	for (unsigned i = first_mask_value; i < arg_count; i++)
	{
		Expr *mask_val = args[i];
		if (!sema_analyse_expr_rhs(context, type_int, mask_val, false, NULL)) return false;
		if (!expr_is_const_int(mask_val))
		{
			RETURN_SEMA_ERROR(mask_val, "The swizzle positions must be compile time constants.");
		}
		if (mask_val->const_expr.ixx.i.low >= components)
		{
			if (components == 1) RETURN_SEMA_ERROR(mask_val, "The only possible swizzle position is 0.");
			RETURN_SEMA_ERROR(mask_val, "The swizzle position must be in the range 0-%d.", components - 1);
		}
	}
	expr->type = type_add_optional(type_get_vector(type_get_indexed_type(args[0]->type), arg_count - first_mask_value), optional);
	return true;
}

static inline bool is_valid_atomicity(Expr* expr)
{
	if (!expr_is_const_int(expr) || !int_fits(expr->const_expr.ixx, TYPE_U8) || expr->const_expr.ixx.i.low > 6)
	{
		RETURN_SEMA_ERROR(expr, "Expected a constant integer value < 7.");
	}
	return true;
}


static bool sema_expr_analyse_compare_exchange(SemaContext *context, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	Expr *pointer = args[0];

	if (!sema_analyse_expr(context, pointer)) return false;

	bool optional = IS_OPTIONAL(pointer);
	Type *comp_type = type_flatten(pointer->type);
	if (!type_is_pointer(comp_type)) RETURN_SEMA_ERROR(pointer, "Expected a pointer here.");
	Type *pointee = comp_type->pointer;
	for (int i = 1; i < 3; i++)
	{
		Expr *arg = args[i];
		if (!sema_analyse_expr_rhs(context, type_is_void(pointee) ? NULL : pointee, arg, true, NULL)) return false;
		if (type_is_void(pointee)) pointee = arg->type->canonical;
		if (!type_is_atomic(type_flatten(arg->type)))
		{
			RETURN_SEMA_ERROR(arg, "%s may not be used with atomics.", type_quoted_error_string(arg->type));
		}

		optional = optional || IS_OPTIONAL(args[i]);
	}
	for (int i = 3; i < 5; i++)
	{
		if (!sema_analyse_expr_rhs(context, type_bool, args[i], false, NULL)) return false;
		if (!expr_is_const(args[i]))
		{
			SEMA_ERROR(args[i], "Expected a constant boolean value.");
			return false;
		}
	}
	for (int i = 5; i < 7; i++)
	{
		if (!sema_analyse_expr_rhs(context, type_char, args[i], false, NULL)) return false;
		if (!is_valid_atomicity(args[i])) return false;
	}
	unsigned success = args[5]->const_expr.ixx.i.low;
	unsigned failure = args[6]->const_expr.ixx.i.low;
	if (success < ATOMIC_RELAXED) RETURN_SEMA_ERROR(args[5], "Success ordering must be at least RELAXED.");
	if (failure < ATOMIC_RELAXED) RETURN_SEMA_ERROR(args[6], "Failure ordering must be at least RELAXED.");
	if (failure == ATOMIC_ACQUIRE_RELEASE || failure == ATOMIC_RELEASE)
	{
		RETURN_SEMA_ERROR(args[6], "Failure ordering may not be RELEASE / ACQUIRE_RELEASE.");
	}
	Expr *align = args[7];
	if (!sema_check_alignment_expression(context, align)) return false;
	expr->type = type_add_optional(args[1]->type, optional);
	return true;
}

static bool sema_expr_analyse_syscall(SemaContext *context, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	unsigned arg_count = vec_size(args);
	if (arg_count > 7)
	{
		RETURN_SEMA_ERROR(args[7], "Only 7 arguments supported for $$syscall.");
	}
	bool optional = false;
	for (unsigned i = 0; i < arg_count; i++)
	{
		Expr *arg = args[i];
		if (!sema_analyse_expr_rhs(context, type_uptr, arg, true, NULL)) return false;
		optional = optional || type_is_optional(arg->type);
	}
	switch (platform_target.arch)
	{
		case ARCH_TYPE_AARCH64:
		case ARCH_TYPE_AARCH64_BE:
		case ARCH_TYPE_X86:
		case ARCH_TYPE_X86_64:
			break;
		default:
			RETURN_SEMA_ERROR(expr, "Target does not support $$syscall.");
	}
	expr->type = type_add_optional(type_uptr, optional);
	return true;
}

bool sema_expr_analyse_builtin_call(SemaContext *context, Expr *expr)
{
	expr->call_expr.is_builtin = true;
	expr->call_expr.arguments = sema_expand_vasplat_exprs(context, expr->call_expr.arguments);

	BuiltinFunction func = exprptr(expr->call_expr.function)->builtin_expr.builtin;
	Expr **args = expr->call_expr.arguments;
	unsigned arg_count = vec_size(args);
	int expected_args = builtin_expected_args(func);
	bool expect_vararg = false;
	if (expected_args < 0)
	{
		expected_args = -expected_args;
		expect_vararg = true;
	}

	// 1. Handle arg count, so at least we know that is ok.
	if (arg_count < expected_args || (arg_count > expected_args  && !expect_vararg))
	{
		if (arg_count == 0)
		{
			RETURN_SEMA_ERROR(expr, "Expected %s%d arguments to builtin.", expect_vararg ? "at least " : "", expected_args);
		}
		if (arg_count < expected_args) RETURN_SEMA_ERROR(args[arg_count - 1], "Expected more arguments after this one.");
		RETURN_SEMA_ERROR(args[expected_args], "Too many arguments.");
	}

	switch (func)
	{
		case BUILTIN_SWIZZLE2:
		case BUILTIN_SWIZZLE:
			return sema_expr_analyse_swizzle(context, expr, func == BUILTIN_SWIZZLE2);
		case BUILTIN_SYSCALL:
			return sema_expr_analyse_syscall(context, expr);
		case BUILTIN_TRAP:
		case BUILTIN_UNREACHABLE:
			expr->type = type_void;
			expr->call_expr.no_return = true;
			return true;
		case BUILTIN_SYSCLOCK:
			expr->type = type_ulong;
			return true;
		case BUILTIN_GET_ROUNDING_MODE:
			expr->type = type_int;
			return true;
		case BUILTIN_COMPARE_EXCHANGE:
			return sema_expr_analyse_compare_exchange(context, expr);
		default:
			break;
	}

	bool optional = false;

	// 2. We can now check all the arguments, since they in general work on the
	//    exact type size, we don't do any forced promotion.
	for (unsigned i = 0; i < arg_count; i++)
	{
		if (!sema_analyse_expr(context, args[i])) return false;
		optional = optional || type_is_optional(args[i]->type);
	}

	Type *rtype = NULL;
	switch (func)
	{
		case BUILTIN_SET_ROUNDING_MODE:
			assert(arg_count == 1);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_INTEGER }, 1)) return false;
			rtype = type_void;
			break;
		case BUILTIN_SYSCALL:
			UNREACHABLE
		case BUILTIN_VECCOMPGE:
		case BUILTIN_VECCOMPEQ:
		case BUILTIN_VECCOMPLE:
		case BUILTIN_VECCOMPGT:
		case BUILTIN_VECCOMPLT:
		case BUILTIN_VECCOMPNE:
		{
			assert(arg_count == 2);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_NUMVEC, BA_NUMVEC }, 2)) return false;
			if (!sema_check_builtin_args_match(args, 2)) return false;
			Type *vec_type = type_flatten(args[0]->type);
			rtype = type_get_vector(type_bool, vec_type->array.len);
			break;
		}
		case BUILTIN_SELECT:
			assert(arg_count == 3);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_BOOLVEC, BA_VEC, BA_VEC }, 3)) return false;
			if (!sema_check_builtin_args_match(&args[1], 2)) return false;
			rtype = args[1]->type;
			if (!sema_expr_is_valid_mask_for_value(args[0], args[1])) return false;
			break;
		case BUILTIN_OVERFLOW_ADD:
		case BUILTIN_OVERFLOW_MUL:
		case BUILTIN_OVERFLOW_SUB:
			assert(arg_count == 3);
			if (!sema_check_builtin_args(args,
										 (BuiltinArg[]) { BA_INTEGER, BA_INTEGER, BA_POINTER },
										 3)) return false;
			if (!sema_check_builtin_args_match(args, 2)) return false;
			if (type_no_optional(args[0]->type->canonical) != type_no_optional(args[2]->type->canonical->pointer))
			{
				RETURN_SEMA_ERROR(args[2], "Expected %s, not %s.",
								  type_to_error_string(type_get_ptr(args[0]->type)),
								  type_to_error_string(args[2]->type));
			}
			rtype = type_bool;
			break;
		case BUILTIN_EXACT_ADD:
		case BUILTIN_EXACT_DIV:
		case BUILTIN_EXACT_MUL:
		case BUILTIN_EXACT_SUB:
		case BUILTIN_EXACT_MOD:
			assert(arg_count == 2);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_INTEGER, BA_INTEGER }, 2)) return false;
			if (!sema_check_builtin_args_match(args, 2)) return false;
			rtype = args[0]->type->canonical;
			break;
		case BUILTIN_ANY_MAKE:
			assert(arg_count == 2);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_POINTER, BA_TYPEID }, 2)) return false;
			rtype = type_any;
			break;
		case BUILTIN_EXACT_NEG:
			assert(arg_count == 1);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_INTLIKE }, 1)) return false;
			rtype = args[0]->type->canonical;
			break;
		case BUILTIN_MEMCOPY_INLINE:
			assert(arg_count == 6);
			if (!sema_check_builtin_args(args,
			                             (BuiltinArg[]) { BA_POINTER, BA_POINTER, BA_SIZE, BA_BOOL, BA_SIZE, BA_SIZE },
			                             6)) return false;
			if (!sema_check_builtin_args_const(&args[2], 4)) return false;
			rtype = type_void;
			break;
		case BUILTIN_MEMCOPY:
		case BUILTIN_MEMMOVE:
			assert(arg_count == 6);
			if (!sema_check_builtin_args(args,
										 (BuiltinArg[]) { BA_POINTER, BA_POINTER, BA_SIZE, BA_BOOL, BA_SIZE, BA_SIZE },
										 6)) return false;
			if (!sema_check_builtin_args_const(&args[3], 3)) return false;
			rtype = type_void;
			break;
		case BUILTIN_MEMSET:
			assert(arg_count == 5);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_POINTER, BA_CHAR, BA_SIZE, BA_BOOL, BA_SIZE },
			                             5)) return false;
			if (!sema_check_builtin_args_const(&args[3], 2)) return false;
			rtype = type_void;
			break;
		case BUILTIN_MEMSET_INLINE:
			assert(arg_count == 5);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_POINTER, BA_CHAR, BA_SIZE, BA_BOOL, BA_SIZE },
										 5)) return false;
			if (!sema_check_builtin_args_const(&args[2], 3)) return false;
			rtype = type_void;
			break;
		case BUILTIN_BITREVERSE:
		case BUILTIN_BSWAP:
		case BUILTIN_CTLZ:
		case BUILTIN_POPCOUNT:
		case BUILTIN_CTTZ:
			assert(arg_count == 1);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_INTLIKE }, 1)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_SAT_SHL:
		case BUILTIN_SAT_SUB:
		case BUILTIN_SAT_ADD:
			assert(arg_count == 2);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_INTLIKE, BA_INTLIKE }, 2)) return false;
			if (!sema_check_builtin_args_match(args, 2)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_REVERSE:
			assert(arg_count == 1);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_VEC }, 1)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_EXPECT:
			assert(arg_count == 2);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_BOOLINT, BA_BOOLINT }, 2)) return false;
			if (!sema_check_builtin_args_match(args, 2)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_EXPECT_WITH_PROBABILITY:
			assert(arg_count == 3);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_BOOLINT, BA_BOOLINT }, 2)) return false;
			if (!cast_implicit(context, args[2], type_double))
			{
				RETURN_SEMA_ERROR(args[2], "Expected a 'double', but was %s.", type_quoted_error_string(args[2]->type));
			}
			if (!expr_is_const(args[2]))
			{
				RETURN_SEMA_ERROR(args[2], "This value must be a constant.");
			}
			else
			{
				Real r = args[2]->const_expr.fxx.f;
				if (r < 0 || r > 1) RETURN_SEMA_ERROR(args[2], "The probability must be between 0 and 1.");
			}
			if (!sema_check_builtin_args_match(args, 2)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_CEIL:
		case BUILTIN_COPYSIGN:
		case BUILTIN_COS:
		case BUILTIN_EXP:
		case BUILTIN_EXP2:
		case BUILTIN_FLOOR:
		case BUILTIN_LLRINT:
		case BUILTIN_LLROUND:
		case BUILTIN_LOG:
		case BUILTIN_LOG2:
		case BUILTIN_LOG10:
		case BUILTIN_LRINT:
		case BUILTIN_LROUND:
		case BUILTIN_NEARBYINT:
		case BUILTIN_RINT:
		case BUILTIN_ROUND:
		case BUILTIN_ROUNDEVEN:
		case BUILTIN_SIN:
		case BUILTIN_SQRT:
		case BUILTIN_TRUNC:
			assert(arg_count);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_FLOATLIKE, BA_FLOATLIKE, BA_FLOATLIKE },
										 arg_count)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_FRAMEADDRESS:
		case BUILTIN_RETURNADDRESS:
			assert(arg_count);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_INTEGER }, arg_count)) return false;
			if (!cast_implicit(context, args[0], type_int)) return false;
			if (!expr_is_const_int(args[0])) RETURN_SEMA_ERROR(args[0], "Expected a compile time constant integer.");
			rtype = type_voidptr;
			break;
		case BUILTIN_WASM_MEMORY_SIZE:
			assert(arg_count == 1);
			if (!cast_implicit(context, args[0], type_uint)) return false;
			rtype = type_uptr;
			break;
		case BUILTIN_WASM_MEMORY_GROW:
			assert(arg_count == 2);
			if (!cast_implicit(context, args[0], type_uint)) return false;
			if (!cast_implicit(context, args[1], type_uptr)) return false;
			rtype = type_iptr;
			break;
		case BUILTIN_PREFETCH:
			assert(arg_count == 3);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_POINTER, BA_INTEGER, BA_INTEGER }, 3)) return false;
			for (unsigned i = 1; i < 3; i++)
			{
				if (!expr_is_const(args[i])) RETURN_SEMA_ERROR(args[i], "A constant value is required.");
				if (!cast_implicit(context, args[i], type_int)) return false;
			}
			if (!expr_in_int_range(args[1], 0, 1))
			{
				SEMA_ERROR(args[1], "Expected a value between 0 and 1.");
				return false;
			}
			if (!expr_in_int_range(args[2], 0, 3))
			{
				SEMA_ERROR(args[2], "Expected a value between 0 and 3.");
				return false;
			}
			if (!cast_implicit(context, args[0], type_voidptr)) return false;
			rtype = type_void;
			break;
		case BUILTIN_POW:
			assert(arg_count == 2);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_FLOATLIKE, BA_FLOATLIKE }, 2)) return false;
			if (!sema_check_builtin_args_match(args, 2)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_POW_INT:
			assert(arg_count == 2);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_FLOATLIKE, BA_INTLIKE }, 2)) return false;
			if (!cast_implicit(context, args[1], type_cint)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_REDUCE_FMUL:
		case BUILTIN_REDUCE_FADD:
			assert(arg_count == 2);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_FLOATVEC, BA_FLOAT }, 2)) return false;
			if (!cast_implicit(context, args[1], args[0]->type->canonical->array.base)) return false;
			{
				Expr *arg = args[0];
				args[0] = args[1];
				args[1] = arg;
			}
			rtype = args[0]->type;
			break;
		case BUILTIN_REDUCE_MAX:
		case BUILTIN_REDUCE_MIN:
		{
			assert(arg_count == 1);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_NUMVEC }, 1)) return false;
			rtype = type_get_indexed_type(args[0]->type);
			break;
		}
		case BUILTIN_REDUCE_ADD:
		case BUILTIN_REDUCE_AND:
		case BUILTIN_REDUCE_OR:
		case BUILTIN_REDUCE_XOR:
		case BUILTIN_REDUCE_MUL:
			assert(arg_count == 1);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_BOOLINTVEC }, 1)) return false;
			rtype = type_get_indexed_type(args[0]->type);
			break;
		case BUILTIN_ABS:
			assert(arg_count == 1);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_NUMLIKE }, 1)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_GATHER:
		{
			assert(arg_count == 4);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_PTRVEC, BA_BOOLVEC, BA_VEC, BA_INTEGER }, 4)) return false;
			Type *flat_pointer_vec = type_flatten(args[0]->type);
			Type *flat_passthru_vec = type_flatten(args[2]->type);
			Type *pointer_type = flat_pointer_vec->array.base;
			ArraySize len = flat_pointer_vec->array.len;
			if (pointer_type->pointer->canonical != flat_passthru_vec->array.base->canonical)
			{
				RETURN_SEMA_ERROR(args[2], "Expected the vector to have elements of type %s.", type_quoted_error_string(pointer_type->pointer));
			}
			if (len != flat_passthru_vec->array.len)
			{
				RETURN_SEMA_ERROR(args[2], "Expected the vector to be %s, not %s.",
				                  type_quoted_error_string(
						                  type_get_vector(pointer_type->pointer, len)),
				                  type_quoted_error_string(args[2]->type));
			}
			if (!sema_check_alignment_expression(context, args[3])) return false;
			if (!sema_expr_is_valid_mask_for_value(args[1], args[2])) return false;
			rtype = type_get_vector(pointer_type->pointer, len);
			break;
		}
		case BUILTIN_SCATTER:
		{
			assert(arg_count == 4);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_PTRVEC, BA_VEC, BA_BOOLVEC, BA_INTEGER }, 4)) return false;
			Type *flat_pointer_vec = type_flatten(args[0]->type);
			Type *flat_value_vec = type_flatten(args[1]->type);
			Type *pointer_type = flat_pointer_vec->array.base;
			if (pointer_type->pointer->canonical != flat_value_vec->array.base->canonical)
			{
				RETURN_SEMA_ERROR(args[1], "Expected the vector to have elements of type %s.", type_quoted_error_string(pointer_type->pointer));
			}
			if (flat_pointer_vec->array.len != flat_value_vec->array.len)
			{
				RETURN_SEMA_ERROR(args[1], "Expected the vector to be %s, not %s.",
				                  type_quoted_error_string(
						                  type_get_vector(pointer_type->pointer, flat_pointer_vec->array.len)),
				                  type_quoted_error_string(args[2]->type));
			}
			if (!sema_check_alignment_expression(context, args[3])) return false;
			if (!sema_expr_is_valid_mask_for_value(args[2], args[1])) return false;
			rtype = type_void;
			break;
		}
		case BUILTIN_MASKED_LOAD:
		{
			assert(arg_count == 4);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_POINTER, BA_BOOLVEC, BA_VEC, BA_INTEGER }, 4)) return false;
			Type *pointer_type = args[0]->type;
			if (!type_is_pointer(pointer_type)) RETURN_SEMA_ERROR(args[0], "Expected a direct pointer.");
			if (pointer_type->pointer->canonical != args[2]->type->canonical)
			{
				RETURN_SEMA_ERROR(args[2], "Expected the value to be of type '%s'.", type_quoted_error_string(pointer_type->pointer));
			}
			if (!sema_check_alignment_expression(context, args[3])) return false;
			if (!sema_expr_is_valid_mask_for_value(args[1], args[2])) return false;
 			rtype = pointer_type->pointer;
			break;
		}
		case BUILTIN_MASKED_STORE:
		{
			assert(arg_count == 4);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_POINTER, BA_VEC, BA_BOOLVEC, BA_INTEGER }, 4)) return false;
			Type *pointer_type = args[0]->type;
			if (!type_is_pointer(pointer_type)) RETURN_SEMA_ERROR(args[0], "Expected a direct pointer.");
			if (pointer_type->pointer->canonical != args[1]->type->canonical)
			{
				RETURN_SEMA_ERROR(args[2], "Expected the value to be of type %s.", type_quoted_error_string(pointer_type->pointer));
			}
			if (!sema_check_alignment_expression(context, args[3])) return false;
			if (!sema_expr_is_valid_mask_for_value(args[2], args[1])) return false;
			rtype = type_void;
			break;
		}
		case BUILTIN_MAX:
		case BUILTIN_MIN:
			assert(arg_count == 2);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_NUMLIKE, BA_NUMLIKE }, 2)) return false;
			if (!sema_check_builtin_args_match(args, 2)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_FMA:
			assert(arg_count == 3);
			if (!sema_check_builtin_args(args,
										 (BuiltinArg[]) { BA_FLOATLIKE, BA_FLOATLIKE, BA_FLOATLIKE },
										 3)) return false;
			if (!sema_check_builtin_args_match(args, 3)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_FSHL:
		case BUILTIN_FSHR:
			assert(arg_count == 3);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_INTLIKE, BA_INTLIKE, BA_INTLIKE },
										 3)) return false;
			if (!sema_check_builtin_args_match(args, 3)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_FMULADD:
			assert(arg_count == 3);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_FLOAT, BA_FLOAT, BA_FLOAT },
										 3)) return false;
			if (!sema_check_builtin_args_match(args, 3)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_ATOMIC_LOAD:
		{
			assert(arg_count == 3);
			if (!sema_check_builtin_args(args, (BuiltinArg[]){ BA_POINTER, BA_BOOL, BA_INTEGER }, 3)) return false;
			Type *original = type_flatten(args[0]->type);
			if (original == type_voidptr) RETURN_SEMA_ERROR(args[0], "Expected a typed pointer.");
			if (!expr_is_const(args[1])) RETURN_SEMA_ERROR(args[1], "'is_volatile' must be a compile time constant.");
			if (!expr_is_const(args[2])) RETURN_SEMA_ERROR(args[2], "Ordering must be a compile time constant.");
			if (!is_valid_atomicity(args[2])) return false;
			switch (args[2]->const_expr.ixx.i.low)
			{
				case ATOMIC_ACQUIRE_RELEASE:
				case ATOMIC_RELEASE:
					RETURN_SEMA_ERROR(args[2], "'release' and 'acquire release' are not valid for atomic loads.");
			}
			rtype = original->pointer;
			break;
		}
		case BUILTIN_VOLATILE_LOAD:
		{
			assert(arg_count == 1);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_POINTER }, 1)) return false;
			Type *original = type_flatten(args[0]->type);
			if (original == type_voidptr) RETURN_SEMA_ERROR(args[0], "Expected a typed pointer.");
			rtype = original->pointer;
			break;
		}
		case BUILTIN_VOLATILE_STORE:
		{
			assert(arg_count == 2);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_POINTER }, 1)) return false;
			Type *original = type_flatten(args[0]->type);
			if (original != type_voidptr)
			{
				if (!cast_implicit(context, args[1], original->pointer)) return false;
			}
			rtype = args[1]->type;
			break;
		}
		case BUILTIN_ATOMIC_FETCH_INC_WRAP:
		case BUILTIN_ATOMIC_FETCH_DEC_WRAP:
		{
			assert(arg_count == 5);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_POINTER, BA_INTEGER }, 2)) return false;
			Type *original = type_flatten(args[0]->type);
			Type *val = type_flatten(args[1]->type);
			if (!type_is_unsigned(val) )
			{
				RETURN_SEMA_ERROR(args[1], "Expected an unsigned integer.");
			}
			if (original != type_voidptr)
			{
				Type *pointer = type_flatten(original->pointer);
				if (!type_is_unsigned(pointer))
				{
					RETURN_SEMA_ERROR(args[0], "Expected a pointer to an unsigned integer.");
				}
				if (!cast_implicit(context, args[1], original->pointer)) return false;
			}
			if (!expr_is_const(args[2])) RETURN_SEMA_ERROR(args[2], "'is_volatile' must be a compile time constant.");
			if (!expr_is_const(args[3])) RETURN_SEMA_ERROR(args[3], "Ordering must be a compile time constant.");
			if (!is_valid_atomicity(args[3])) return false;
			switch (args[3]->const_expr.ixx.i.low)
			{
				case ATOMIC_UNORDERED:
					RETURN_SEMA_ERROR(args[3], "'unordered' is not valid ordering.");
			}
			if (!sema_check_alignment_expression(context, args[4])) return false;
			rtype = args[1]->type;
			break;
		}
		case BUILTIN_ATOMIC_FETCH_AND:
		case BUILTIN_ATOMIC_FETCH_NAND:
		case BUILTIN_ATOMIC_FETCH_OR:
		case BUILTIN_ATOMIC_FETCH_XOR:
		{
			assert(arg_count == 5);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_POINTER, BA_INTEGER }, 2)) return false;
			Type *original = type_flatten(args[0]->type);
			if (original != type_voidptr)
			{
				if (!cast_implicit(context, args[1], original->pointer)) return false;
			}
			if (!expr_is_const(args[2])) RETURN_SEMA_ERROR(args[2], "'is_volatile' must be a compile time constant.");
			if (!expr_is_const(args[3])) RETURN_SEMA_ERROR(args[3], "Ordering must be a compile time constant.");
			if (!is_valid_atomicity(args[3])) return false;
			switch (args[3]->const_expr.ixx.i.low)
			{
				case ATOMIC_UNORDERED:
					RETURN_SEMA_ERROR(args[3], "'unordered' is not valid ordering.");
			}
			if (!sema_check_alignment_expression(context, args[4])) return false;
			rtype = args[1]->type;
			break;
		}
		case BUILTIN_ATOMIC_FETCH_EXCHANGE:
		{
			assert(arg_count == 5);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_POINTER }, 1)) return false;
			Type *original = type_flatten(args[0]->type);
			if (original != type_voidptr)
			{
				if (!cast_implicit(context, args[1], original->pointer)) return false;
			}
			Type *val = type_flatten(args[1]->type);
			if (!type_is_atomic(val)) RETURN_SEMA_ERROR(args[1], "%s exceeds pointer size.", val);
			if (!expr_is_const(args[2])) RETURN_SEMA_ERROR(args[2], "'is_volatile' must be a compile time constant.");
			if (!expr_is_const(args[3])) RETURN_SEMA_ERROR(args[3], "Ordering must be a compile time constant.");
			if (!is_valid_atomicity(args[3])) return false;
			switch (args[3]->const_expr.ixx.i.low)
			{
				case ATOMIC_UNORDERED:
					RETURN_SEMA_ERROR(args[3], "'unordered' is not valid ordering.");
			}
			if (!sema_check_alignment_expression(context, args[4])) return false;
			rtype = args[1]->type;
			break;
		}
		case BUILTIN_ATOMIC_FETCH_ADD:
		case BUILTIN_ATOMIC_FETCH_SUB:
		case BUILTIN_ATOMIC_FETCH_MAX:
		case BUILTIN_ATOMIC_FETCH_MIN:
		{
			assert(arg_count == 5);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_POINTER, BA_NUM }, 2)) return false;
			Type *original = type_flatten(args[0]->type);
			if (original != type_voidptr)
			{
				if (!cast_implicit(context, args[1], original->pointer)) return false;
			}
			if (!expr_is_const(args[2])) RETURN_SEMA_ERROR(args[2], "'is_volatile' must be a compile time constant.");
			if (!expr_is_const(args[3])) RETURN_SEMA_ERROR(args[3], "Ordering must be a compile time constant.");
			if (!is_valid_atomicity(args[3])) return false;
			switch (args[3]->const_expr.ixx.i.low)
			{
				case ATOMIC_UNORDERED:
					RETURN_SEMA_ERROR(args[3], "'unordered' is not valid ordering.");
			}
			if (!sema_check_alignment_expression(context, args[4])) return false;
			rtype = args[1]->type;
			break;
		}
		case BUILTIN_ATOMIC_STORE:
		{
			assert(arg_count == 4);
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_POINTER }, 1)) return false;
			if (!sema_check_builtin_args(&args[2], (BuiltinArg[]) { BA_BOOL, BA_INTEGER }, 2)) return false;
			Type *original = type_flatten(args[0]->type);
			if (original != type_voidptr)
			{
				if (!cast_implicit(context, args[1], original->pointer)) return false;
			}
			if (!expr_is_const(args[2])) RETURN_SEMA_ERROR(args[2], "'is_volatile' must be a compile time constant.");
			if (!expr_is_const(args[3])) RETURN_SEMA_ERROR(args[3], "Ordering must be a compile time constant.");
			if (!is_valid_atomicity(args[3])) return false;
			switch (args[3]->const_expr.ixx.i.low)
			{
				case ATOMIC_ACQUIRE_RELEASE:
				case ATOMIC_ACQUIRE:
					RETURN_SEMA_ERROR(args[3], "'acquire' and 'acquire release' are not valid for atomic stores.");
			}
			rtype = args[1]->type;
			break;
		}
		case BUILTIN_NONE:
		case BUILTIN_COMPARE_EXCHANGE:
		case BUILTIN_GET_ROUNDING_MODE:
		case BUILTIN_SWIZZLE:
		case BUILTIN_SWIZZLE2:
		case BUILTIN_SYSCLOCK:
		case BUILTIN_TRAP:
		case BUILTIN_UNREACHABLE:
			UNREACHABLE
	}
	expr->type = type_add_optional(rtype, optional);
	return true;
}

static inline int builtin_expected_args(BuiltinFunction func)
{
	switch (func)
	{
		case BUILTIN_SYSCALL:
			return -1;
		case BUILTIN_SWIZZLE:
			return -2;
		case BUILTIN_SWIZZLE2:
			return -3;
		case BUILTIN_GET_ROUNDING_MODE:
		case BUILTIN_SYSCLOCK:
		case BUILTIN_TRAP:
		case BUILTIN_UNREACHABLE:
			return 0;
		case BUILTIN_ABS:
		case BUILTIN_BITREVERSE:
		case BUILTIN_BSWAP:
		case BUILTIN_CEIL:
		case BUILTIN_COS:
		case BUILTIN_CTLZ:
		case BUILTIN_POPCOUNT:
		case BUILTIN_CTTZ:
		case BUILTIN_EXACT_NEG:
		case BUILTIN_EXP:
		case BUILTIN_EXP2:
		case BUILTIN_FLOOR:
		case BUILTIN_LLRINT:
		case BUILTIN_LLROUND:
		case BUILTIN_LOG:
		case BUILTIN_LOG2:
		case BUILTIN_LOG10:
		case BUILTIN_LRINT:
		case BUILTIN_LROUND:
		case BUILTIN_NEARBYINT:
		case BUILTIN_REVERSE:
		case BUILTIN_RINT:
		case BUILTIN_ROUND:
		case BUILTIN_ROUNDEVEN:
		case BUILTIN_SIN:
		case BUILTIN_SQRT:
		case BUILTIN_TRUNC:
		case BUILTIN_VOLATILE_LOAD:
		case BUILTIN_REDUCE_MUL:
		case BUILTIN_REDUCE_AND:
		case BUILTIN_REDUCE_ADD:
		case BUILTIN_REDUCE_OR:
		case BUILTIN_REDUCE_XOR:
		case BUILTIN_REDUCE_MAX:
		case BUILTIN_REDUCE_MIN:
		case BUILTIN_SET_ROUNDING_MODE:
		case BUILTIN_WASM_MEMORY_SIZE:
		case BUILTIN_FRAMEADDRESS:
		case BUILTIN_RETURNADDRESS:
			return 1;
		case BUILTIN_COPYSIGN:
		case BUILTIN_EXACT_ADD:
		case BUILTIN_EXACT_DIV:
		case BUILTIN_EXACT_MOD:
		case BUILTIN_EXACT_MUL:
		case BUILTIN_EXACT_SUB:
		case BUILTIN_EXPECT:
		case BUILTIN_MAX:
		case BUILTIN_MIN:
		case BUILTIN_POW:
		case BUILTIN_POW_INT:
		case BUILTIN_REDUCE_FADD:
		case BUILTIN_REDUCE_FMUL:
		case BUILTIN_SAT_ADD:
		case BUILTIN_SAT_SHL:
		case BUILTIN_SAT_SUB:
		case BUILTIN_VOLATILE_STORE:
		case BUILTIN_VECCOMPNE:
		case BUILTIN_VECCOMPLT:
		case BUILTIN_VECCOMPLE:
		case BUILTIN_VECCOMPGE:
		case BUILTIN_VECCOMPGT:
		case BUILTIN_VECCOMPEQ:
		case BUILTIN_WASM_MEMORY_GROW:
		case BUILTIN_ANY_MAKE:
			return 2;
		case BUILTIN_EXPECT_WITH_PROBABILITY:
		case BUILTIN_FMA:
		case BUILTIN_FSHL:
		case BUILTIN_FSHR:
		case BUILTIN_FMULADD:
		case BUILTIN_OVERFLOW_ADD:
		case BUILTIN_OVERFLOW_MUL:
		case BUILTIN_OVERFLOW_SUB:
		case BUILTIN_PREFETCH:
		case BUILTIN_ATOMIC_LOAD:
		case BUILTIN_SELECT:
			return 3;
		case BUILTIN_ATOMIC_STORE:
		case BUILTIN_MASKED_STORE:
		case BUILTIN_MASKED_LOAD:
		case BUILTIN_GATHER:
		case BUILTIN_SCATTER:
			return 4;
		case BUILTIN_ATOMIC_FETCH_EXCHANGE:
		case BUILTIN_ATOMIC_FETCH_ADD:
		case BUILTIN_ATOMIC_FETCH_INC_WRAP:
		case BUILTIN_ATOMIC_FETCH_NAND:
		case BUILTIN_ATOMIC_FETCH_AND:
		case BUILTIN_ATOMIC_FETCH_OR:
		case BUILTIN_ATOMIC_FETCH_XOR:
		case BUILTIN_ATOMIC_FETCH_MAX:
		case BUILTIN_ATOMIC_FETCH_MIN:
		case BUILTIN_ATOMIC_FETCH_SUB:
		case BUILTIN_ATOMIC_FETCH_DEC_WRAP:
			return 5;
		case BUILTIN_MEMCOPY:
		case BUILTIN_MEMCOPY_INLINE:
		case BUILTIN_MEMMOVE:
			return 6;
		case BUILTIN_MEMSET:
		case BUILTIN_MEMSET_INLINE:
			return 5;
		case BUILTIN_COMPARE_EXCHANGE:
			return 8;
		case BUILTIN_NONE:
			UNREACHABLE
	}
	UNREACHABLE
}
