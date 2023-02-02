// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
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
	BA_BOOLINTVEC,
	BA_BOOLINT,
	BA_INTVEC,
	BA_FLOATVEC,
	BA_VEC,
} BuiltinArg;

static bool sema_check_builtin_args_match(Expr **args, size_t arg_len);
static bool sema_check_builtin_args_const(Expr **args, size_t arg_len);
static bool sema_check_builtin_args(Expr **args, BuiltinArg *arg_type, size_t arg_len);
static inline bool sema_expr_analyse_swizzle(SemaContext *context, Expr *expr, bool swizzle_two);
static inline int builtin_expected_args(BuiltinFunction func);

static bool sema_check_builtin_args_match(Expr **args, size_t arg_len)
{
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
		Type *type = type_flatten(args[i]->type->canonical);
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
			case BA_NUMLIKE:
				if (!type_flat_is_numlike(type))
				{
					SEMA_ERROR(args[i], "Expected a number or vector.");
					return false;
				}
				break;
			case BA_FLOATLIKE:
				if (!type_flat_is_floatlike(type))
				{
					SEMA_ERROR(args[i], "Expected a floating point or floating point vector, but was %s.",
					           type_quoted_error_string(type));
					return false;
				}
				break;
			case BA_VEC:
				if (type->type_kind != TYPE_VECTOR)
				{
					SEMA_ERROR(args[i], "Expected a vector.");
					return false;
				}
				break;
			case BA_INTVEC:

				if (type->type_kind != TYPE_VECTOR || !type_flat_is_intlike(type->array.base))
				{
					SEMA_ERROR(args[i], "Expected an integer vector.");
					return false;
				}
				break;
			case BA_BOOLINT:
				if (!type_is_integer_or_bool_kind(type))
				{
					SEMA_ERROR(args[i], "Expected a boolean or integer value.");
					return false;
				}
				break;
			case BA_BOOLINTVEC:

				if (type->type_kind != TYPE_VECTOR || !type_flat_is_boolintlike(type->array.base))
				{
					SEMA_ERROR(args[i], "Expected a boolean or integer vector.");
					return false;
				}
				break;
			case BA_FLOATVEC:
				if (type->type_kind != TYPE_VECTOR || !type_flat_is_floatlike(type->array.base))
				{
					SEMA_ERROR(args[i], "Expected an float vector.");
					return false;
				}
				break;
			case BA_INTLIKE:
				if (!type_flat_is_intlike(type))
				{
					SEMA_ERROR(args[i], "Expected an integer or integer vector.");
					return false;
				}
				break;
			case BA_INTEGER:
				if (!type_is_integer(type))
				{
					SEMA_ERROR(args[i], "Expected an integer.");
					return false;
				}
				break;
			case BA_FLOAT:
				if (!type_is_float(type))
				{
					SEMA_ERROR(args[i], "Expected a float or double.");
					return false;
				}
				break;
		}
	}
	return true;
}

static inline bool sema_expr_analyse_swizzle(SemaContext *context, Expr *expr, bool swizzle_two)
{
	Expr **args = expr->call_expr.arguments;
	unsigned arg_count = vec_size(args);
	bool optional = false;
	int first_mask_value = swizzle_two ? 2 : 1;
	Type *first = NULL;
	for (unsigned i = 0; i < first_mask_value; i++)
	{
		Expr *arg = args[i];
		if (!sema_analyse_expr(context, arg)) return false;
		if (!type_flat_is_vector(arg->type))
		{
			SEMA_ERROR(arg, "A vector was expected here.");
			return false;
		}
		optional = optional || type_is_optional(args[i]->type);
		if (i == 0)
		{
			first = type_no_optional(arg->type)->canonical;
			continue;
		}
		else if (type_no_optional(arg->type->canonical) != first)
		{
			SEMA_ERROR(arg, "Vector type does not match the first vector.");
			return false;
		}
	}
	unsigned components = type_flatten(first)->array.len;
	if (swizzle_two) components *= 2;
	for (unsigned i = first_mask_value; i < arg_count; i++)
	{
		Expr *mask_val = args[i];
		if (!sema_analyse_expr_rhs(context, type_int, mask_val, false)) return false;
		if (!expr_is_const_int(mask_val))
		{
			SEMA_ERROR(mask_val, "The swizzle positions must be compile time constants.");
			return false;
		}
		if (mask_val->const_expr.ixx.i.low >= components)
		{
			if (components == 1)
			{
				SEMA_ERROR(mask_val, "The only possible swizzle position is 0.");
				return false;
			}
			SEMA_ERROR(mask_val, "The swizzle position must be in the range 0-%d.", components - 1);
			return false;
		}
	}
	expr->type = type_add_optional(type_get_vector(type_get_indexed_type(args[0]->type), arg_count - first_mask_value), optional);
	return true;
}

bool is_valid_atomicity(Expr* expr)
{
	if (!expr_is_const_int(expr) || !int_fits(expr->const_expr.ixx, TYPE_U8) || expr->const_expr.ixx.i.low > 6)
	{
		SEMA_ERROR(expr, "Expected a constant integer value < 8.");
		return false;
	}
	return true;
}


bool sema_expr_analyse_compare_exchange(SemaContext *context, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	Expr *pointer = args[0];

	if (!sema_analyse_expr(context, pointer)) return false;
	bool optional = IS_OPTIONAL(args[0]);
	Type *comp_type = type_flatten(args[0]->type);
	if (!type_is_pointer(comp_type))
	{
		SEMA_ERROR(args[0], "Expected a pointer here.");
		return false;
	}
	Type *pointee = comp_type->pointer;
	for (int i = 1; i < 3; i++)
	{
		if (!sema_analyse_expr_rhs(context, pointee, args[i], true)) return false;
		optional = optional || IS_OPTIONAL(args[i]);
	}
	for (int i = 3; i < 5; i++)
	{
		if (!sema_analyse_expr_rhs(context, type_bool, args[i], false)) return false;
		if (!expr_is_const(args[i]))
		{
			SEMA_ERROR(args[i], "Expected a constant boolean value.");
			return false;
		}
	}
	for (int i = 5; i < 7; i++)
	{
		if (!sema_analyse_expr_rhs(context, type_char, args[i], false)) return false;
		if (!is_valid_atomicity(args[i])) return false;
	}
	Expr *align = args[7];
	if (!sema_analyse_expr_rhs(context, type_usz, align, false)) return false;
	if (!expr_is_const_int(align)
	    || !int_fits(align->const_expr.ixx, TYPE_U64)
	    || (!is_power_of_two(align->const_expr.ixx.i.low) && align->const_expr.ixx.i.low))
	{
		SEMA_ERROR(args[7], "Expected a constant power-of-two alignment or zero.");
		return false;
	}
	expr->type = type_add_optional(args[1]->type, optional);
	return true;
}
bool sema_expr_analyse_syscall(SemaContext *context, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	unsigned arg_count = vec_size(args);
	if (arg_count > 7)
	{
		SEMA_ERROR(args[7], "Only 7 arguments supported for $$syscall.");
	}
	bool optional = false;
	for (unsigned i = 0; i < arg_count; i++)
	{
		Expr *arg = args[i];
		if (!sema_analyse_expr_rhs(context, type_uptr, arg, true)) return false;
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
			SEMA_ERROR(expr, "Target does not support $$syscall.");
			return false;
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
			SEMA_ERROR(expr, "Expected %s%d arguments to builtin.", expect_vararg ? "at least " : "", expected_args);
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
			return true;
		case BUILTIN_SYSCLOCK:
			expr->type = type_ulong;
			return true;
		case BUILTIN_GET_ROUNDING_MODE:
			expr->type = type_int;
			return true;
		case BUILTIN_FRAMEADDRESS:
		case BUILTIN_STACKTRACE:
			expr->type = type_voidptr;
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
			if (!sema_check_builtin_args(args,
			                             (BuiltinArg[]) { BA_INTEGER },
			                             arg_count)) return false;
			if (!sema_check_builtin_args_match(args, 1)) return false;
			rtype = type_void;
			break;
		case BUILTIN_SYSCALL:
		case BUILTIN_VECCOMPGE:
		case BUILTIN_VECCOMPEQ:
		case BUILTIN_VECCOMPLE:
		case BUILTIN_VECCOMPGT:
		case BUILTIN_VECCOMPLT:
		case BUILTIN_VECCOMPNE:
			if (!sema_check_builtin_args(args,
			                             (BuiltinArg[]) { BA_VEC, BA_VEC },
			                             arg_count)) return false;
			if (!sema_check_builtin_args_match(args, 2)) return false;
			rtype = type_get_vector(type_bool, type_flatten(args[0]->type)->array.len);
			break;
		case BUILTIN_OVERFLOW_ADD:
		case BUILTIN_OVERFLOW_MUL:
		case BUILTIN_OVERFLOW_SUB:
		{
			if (!sema_check_builtin_args(args,
			                             (BuiltinArg[]) { BA_INTEGER, BA_INTEGER, BA_POINTER },
			                             arg_count)) return false;
			if (!sema_check_builtin_args_match(args, 2)) return false;
			if (type_no_optional(args[0]->type->canonical) != type_no_optional(args[2]->type->canonical->pointer))
			{
				SEMA_ERROR(args[2], "Expected %s, not %s.", type_to_error_string(type_get_ptr(args[0]->type)),
				           type_to_error_string(args[2]->type));
				return false;
			}
			rtype = type_bool;
			break;
		}
		case BUILTIN_EXACT_ADD:
		case BUILTIN_EXACT_DIV:
		case BUILTIN_EXACT_MUL:
		case BUILTIN_EXACT_SUB:
		case BUILTIN_EXACT_MOD:
			if (!sema_check_builtin_args(args,
			                             (BuiltinArg[]) { BA_INTEGER, BA_INTEGER },
			                             arg_count)) return false;
			if (!sema_check_builtin_args_match(args, arg_count)) return false;
			rtype = args[0]->type->canonical;
			break;
		case BUILTIN_EXACT_NEG:
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_INTLIKE }, arg_count)) return false;
			rtype = args[0]->type->canonical;
			break;
		case BUILTIN_MEMCOPY:
		case BUILTIN_MEMCOPY_INLINE:
		case BUILTIN_MEMMOVE:
			if (!sema_check_builtin_args(args,
										 (BuiltinArg[]) { BA_POINTER, BA_POINTER, BA_SIZE, BA_BOOL, BA_SIZE, BA_SIZE },
										 arg_count)) return false;
			if (!sema_check_builtin_args_const(&args[3], 3)) return false;
			rtype = type_void;
			break;
		case BUILTIN_MEMSET:
		case BUILTIN_MEMSET_INLINE:
			if (!sema_check_builtin_args(args,
										 (BuiltinArg[]) { BA_POINTER, BA_CHAR, BA_SIZE, BA_BOOL, BA_SIZE },
										 arg_count)) return false;
			if (!sema_check_builtin_args_const(&args[3], 2)) return false;
			rtype = type_void;
			break;
		case BUILTIN_BITREVERSE:
		case BUILTIN_BSWAP:
		case BUILTIN_CTLZ:
		case BUILTIN_POPCOUNT:
		case BUILTIN_CTTZ:
			if (!sema_check_builtin_args(args,
			                             (BuiltinArg[]) { BA_INTLIKE },
			                             arg_count)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_SAT_SHL:
		case BUILTIN_SAT_SUB:
		case BUILTIN_SAT_ADD:
			if (!sema_check_builtin_args(args,
			                             (BuiltinArg[]) { BA_INTLIKE, BA_INTLIKE },
			                             arg_count)) return false;
			if (!sema_check_builtin_args_match(args, 2)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_REVERSE:
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_VEC }, arg_count)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_EXPECT:
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_BOOLINT, BA_BOOLINT }, arg_count)) return false;
			if (!sema_check_builtin_args_match(args, arg_count)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_EXPECT_WITH_PROBABILITY:
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_BOOLINT, BA_BOOLINT }, 2)) return false;
			if (!cast_implicit(context, args[2], type_double))
			{
				SEMA_ERROR(args[2], "Expected a 'double', but was %s.", type_quoted_error_string(args[2]->type));
				return false;
			}
			if (!expr_is_const(args[2]))
			{
				SEMA_ERROR(args[2], "This value must be a constant.");
				return false;
			}
			else
			{
				Real r = args[2]->const_expr.fxx.f;
				if (r < 0 || r > 1)
				{
					SEMA_ERROR(args[2], "The probability must be between 0 and 1.");
					return false;
				}
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
			if (!sema_check_builtin_args(args,
										 (BuiltinArg[]) { BA_FLOATLIKE },
										 arg_count)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_WASM_MEMORY_SIZE:
			if (!cast_implicit(context, args[0], type_uint)) return false;
			rtype = type_uptr;
			break;
		case BUILTIN_WASM_MEMORY_GROW:
			if (!cast_implicit(context, args[0], type_uint)) return false;
			if (!cast_implicit(context, args[1], type_uptr)) return false;
			rtype = type_iptr;
			break;
		case BUILTIN_PREFETCH:
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_POINTER, BA_INTEGER, BA_INTEGER }, arg_count)) return false;
			for (unsigned i = 1; i < 3; i++)
			{
				if (!expr_is_const(args[i]))
				{
					SEMA_ERROR(args[i], "A constant value is required.");
					return false;
				}
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
			if (!sema_check_builtin_args(args,
										 (BuiltinArg[]) { BA_FLOATLIKE, BA_FLOATLIKE },
										 arg_count)) return false;
			if (!sema_check_builtin_args_match(args, arg_count)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_POW_INT:
			if (!sema_check_builtin_args(args,
			                             (BuiltinArg[]) { BA_FLOATLIKE, BA_INTLIKE },
			                             arg_count)) return false;
			if (!cast_implicit(context, args[1], type_cint)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_REDUCE_FMUL:
		case BUILTIN_REDUCE_FADD:
			if (!sema_check_builtin_args(args,
			                             (BuiltinArg[]) { BA_FLOATVEC, BA_FLOAT },
			                             arg_count)) return false;
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
			if (!sema_check_builtin_args(args,
			                             (BuiltinArg[]) { BA_VEC },
			                             arg_count)) return false;
			rtype = args[0]->type->canonical->array.base;
			break;
		case BUILTIN_REDUCE_ADD:
		case BUILTIN_REDUCE_AND:
		case BUILTIN_REDUCE_OR:
		case BUILTIN_REDUCE_XOR:
		case BUILTIN_REDUCE_MUL:
			if (!sema_check_builtin_args(args,
			                             (BuiltinArg[]) { BA_BOOLINTVEC },
			                             arg_count)) return false;
			rtype = args[0]->type->canonical->array.base;
			break;
		case BUILTIN_ABS:
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_NUMLIKE }, arg_count)) return false;
			if (!sema_check_builtin_args_match(args, arg_count)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_MAX:
		case BUILTIN_MIN:
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_NUMLIKE, BA_NUMLIKE }, arg_count)) return false;
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
		case BUILTIN_FSHL:
		case BUILTIN_FSHR:
			if (!sema_check_builtin_args(args,
			                             (BuiltinArg[]) { BA_INTLIKE, BA_INTLIKE, BA_INTLIKE },
			                             arg_count)) return false;
			if (!sema_check_builtin_args_match(args, arg_count)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_FMULADD:
			if (!sema_check_builtin_args(args,
										 (BuiltinArg[]) { BA_FLOAT, BA_FLOAT, BA_FLOAT },
										 arg_count)) return false;
			if (!sema_check_builtin_args_match(args, arg_count)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_VOLATILE_LOAD:
		{
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_POINTER }, 1)) return false;
			Type *original = type_flatten(args[0]->type);
			if (original == type_voidptr)
			{
				SEMA_ERROR(args[0], "Expected a typed pointer.");
				return false;
			}
			rtype = original->pointer;
			break;
		}
		case BUILTIN_VOLATILE_STORE:
		{
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_POINTER }, 1)) return false;
			Type *original = type_flatten(args[0]->type);
			if (original != type_voidptr)
			{
				if (!cast_implicit(context, args[1], original->pointer)) return false;
			}
			rtype = args[1]->type;
			break;
		}
		case BUILTIN_NONE:
		case BUILTIN_COMPARE_EXCHANGE:
		case BUILTIN_FRAMEADDRESS:
		case BUILTIN_GET_ROUNDING_MODE:
		case BUILTIN_SWIZZLE:
		case BUILTIN_SWIZZLE2:
		case BUILTIN_STACKTRACE:
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
		case BUILTIN_STACKTRACE:
		case BUILTIN_SYSCLOCK:
		case BUILTIN_TRAP:
		case BUILTIN_UNREACHABLE:
		case BUILTIN_FRAMEADDRESS:
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
			return 3;
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
