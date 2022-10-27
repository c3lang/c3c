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
	BA_INTVEC,
	BA_FLOATVEC,
	BA_VEC,
} BuiltinArg;

static bool sema_check_builtin_args_match(Expr **args, size_t arg_len);
static bool sema_check_builtin_args_const(Expr **args, size_t arg_len);
static bool sema_check_builtin_args(Expr **args, BuiltinArg *arg_type, size_t arg_len);
static inline bool sema_expr_analyse_shufflevector(SemaContext *context, Expr *expr);
static inline unsigned builtin_expected_args(BuiltinFunction func);

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
					SEMA_ERROR(args[i], "Expected a floating point or floating point vector.");
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

static inline bool sema_expr_analyse_shufflevector(SemaContext *context, Expr *expr)
{
	Expr **args = expr->call_expr.arguments;
	unsigned arg_count = vec_size(args);
	if (arg_count < 2 || arg_count > 3)
	{
		SEMA_ERROR(expr, "Expected 2 or 3 arguments.");
		return false;
	}
	bool failable = false;
	Expr *mask = args[arg_count - 1];
	unsigned len = 0;
	if (expr_is_const_initializer(mask))
	{
		ConstInitializer *init = mask->const_expr.initializer;
		len = init->kind == CONST_INIT_ARRAY_FULL ? vec_size(init->init_array_full) : 0;
	}
	else if (mask->expr_kind == EXPR_INITIALIZER_LIST)
	{
		len = vec_size(mask->initializer_list);
	}
	if (len)
	{
		if (!sema_analyse_expr_rhs(context, type_get_vector(type_int, len), mask, true)) return false;
	}
	for (unsigned i = 0; i < arg_count; i++)
	{
		if (!sema_analyse_expr(context, args[i])) return false;
		failable = failable || type_is_optional(args[i]->type);
	}

	if (!sema_check_builtin_args(args,
	                             arg_count == 2 ? (BuiltinArg[]) { BA_VEC, BA_INTVEC } : (BuiltinArg[]) { BA_VEC, BA_VEC, BA_INTVEC },
	                             arg_count)) return false;
	if (arg_count == 3 && type_flatten(args[0]->type) != type_flatten(args[1]->type))
	{
		SEMA_ERROR(args[1], "Both vector types must match.");
		return false;
	}
	if (type_flatten(args[0]->type)->array.len != type_flatten(mask->type)->array.len)
	{
		SEMA_ERROR(args[2], "Mask vector length must match operands.");
		return false;
	}
	Type *vec_type = type_flatten(mask->type);
	ArraySize max_size = vec_type->array.len;
	if (arg_count == 3) max_size *= 2;
	if (vec_type->array.base != type_int && vec_type->array.base != type_uint)
	{
		SEMA_ERROR(mask, "Mask must be an int or uint vector.");
		return false;
	}
	if (mask->expr_kind != EXPR_CONST)
	{
		SEMA_ERROR(mask, "The mask must be a compile time constant.");
		return false;
	}
	ConstInitializer *init = mask->const_expr.initializer;
	if (init->kind != CONST_INIT_ARRAY_FULL)
	{
		SEMA_ERROR(mask, "The mask must be a fully specified list.");
		return false;
	}

	FOREACH_BEGIN(ConstInitializer *val, init->init_array_full)
		assert(val->kind == CONST_INIT_VALUE);
		uint64_t index = int_to_u64(val->init_value->const_expr.ixx);
		if (index >= max_size)
		{
			SEMA_ERROR(val->init_value, "Index is out of bounds, expected a value between 0 and %u.", max_size - 1);
			return false;
		}
	FOREACH_END();
	expr->type = type_add_optional(args[0]->type, failable);
	return true;
}

bool sema_expr_analyse_builtin_call(SemaContext *context, Expr *expr)
{
	expr->call_expr.is_builtin = true;
	BuiltinFunction func = exprptr(expr->call_expr.function)->builtin_expr.builtin;
	if (func == BUILTIN_SHUFFLEVECTOR)
	{
		return sema_expr_analyse_shufflevector(context, expr);
	}
	unsigned expected_args = builtin_expected_args(func);
	Expr **args = expr->call_expr.arguments;
	unsigned arg_count = vec_size(args);

	bool is_vararg = func == BUILTIN_SYSCALL;
	// 1. Handle arg count, so at least we know that is ok.
	if (expected_args != arg_count && !is_vararg)
	{
		if (arg_count == 0)
		{
			SEMA_ERROR(expr, "Expected %d arguments to builtin.", expected_args);
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
	if (is_vararg && expected_args > arg_count)
	{
		SEMA_ERROR(expr, "Expected at least %d arguments to builtin.\n", expected_args);
		return false;
	}
	bool failable = false;

	// 2. We can now check all the arguments, since they in general work on the
	//    exact type size, we don't do any forced promotion.
	for (unsigned i = 0; i < arg_count; i++)
	{
		if (!sema_analyse_expr(context, args[i])) return false;
		failable = failable || type_is_optional(args[i]->type);
	}

	Type *rtype = NULL;
	switch (func)
	{
		case BUILTIN_SHUFFLEVECTOR:
			UNREACHABLE;
		case BUILTIN_TRAP:
		case BUILTIN_UNREACHABLE:
			rtype = type_void;
			break;
		case BUILTIN_SYSCLOCK:
			rtype = type_ulong;
			break;
		case BUILTIN_SYSCALL:
			if (arg_count > 7)
			{
				SEMA_ERROR(args[7], "Only 7 arguments supported for $$syscall.");
			}
			rtype = type_uptr;
			for (unsigned i = 0; i < arg_count; i++)
			{
				if (!cast_implicit(context, args[i], type_uptr)) return false;
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
			break;
		case BUILTIN_OVERFLOW_ADD:
		case BUILTIN_OVERFLOW_MUL:
		case BUILTIN_OVERFLOW_SUB:
		{
			if (!sema_check_builtin_args(args,
			                             (BuiltinArg[]) { BA_INTEGER, BA_INTEGER, BA_POINTER },
			                             arg_count)) return false;
			if (!sema_check_builtin_args_match(args, 2)) return false;
			if (args[0]->type->canonical != args[2]->type->canonical->pointer)
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
			rtype = type_no_optional(args[0]->type->canonical);
			break;
		case BUILTIN_EXACT_NEG:
			if (!sema_check_builtin_args(args, (BuiltinArg[]) { BA_INTLIKE }, arg_count)) return false;
			rtype = type_no_optional(args[0]->type->canonical);
			break;
		case BUILTIN_MEMCOPY:
		case BUILTIN_MEMMOVE:
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
				SEMA_ERROR(args[2], "Expected a value between 0 and 3.");
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
			                             (BuiltinArg[]) { BA_INTVEC },
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
			UNREACHABLE
	}
	expr->type = type_add_optional(rtype, failable);
	return true;
}

static inline unsigned builtin_expected_args(BuiltinFunction func)
{
	switch (func)
	{
		case BUILTIN_STACKTRACE:
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
		case BUILTIN_SYSCALL:
		case BUILTIN_TRUNC:
		case BUILTIN_VOLATILE_LOAD:
		case BUILTIN_REDUCE_MUL:
		case BUILTIN_REDUCE_AND:
		case BUILTIN_REDUCE_ADD:
		case BUILTIN_REDUCE_OR:
		case BUILTIN_REDUCE_XOR:
		case BUILTIN_REDUCE_MAX:
		case BUILTIN_REDUCE_MIN:
			return 1;
		case BUILTIN_COPYSIGN:
		case BUILTIN_EXACT_ADD:
		case BUILTIN_EXACT_DIV:
		case BUILTIN_EXACT_MOD:
		case BUILTIN_EXACT_MUL:
		case BUILTIN_EXACT_SUB:
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
			return 2;
		case BUILTIN_FMA:
		case BUILTIN_FSHL:
		case BUILTIN_FSHR:
		case BUILTIN_FMULADD:
		case BUILTIN_OVERFLOW_ADD:
		case BUILTIN_OVERFLOW_MUL:
		case BUILTIN_OVERFLOW_SUB:
		case BUILTIN_PREFETCH:
			return 3;
		case BUILTIN_MEMSET:
			return 5;
		case BUILTIN_MEMCOPY:
		case BUILTIN_MEMMOVE:
			return 6;
		case BUILTIN_SHUFFLEVECTOR:
		case BUILTIN_NONE:
			UNREACHABLE
	}
	UNREACHABLE
}
