// Copyright (c) 2022-2025 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.
#include <math.h>
#include "sema_internal.h"
#include <ctype.h>


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

static bool sema_check_builtin_args_match(SemaContext *context, Expr **args, size_t arg_len);
static bool sema_check_builtin_args_const(SemaContext *context, Expr **args, size_t arg_len);
static bool sema_check_builtin_args(SemaContext *context, Expr **args, BuiltinArg *arg_type, size_t arg_len);
static bool sema_expr_analyse_compare_exchange(SemaContext *context, Expr *expr);
static bool sema_expr_analyse_syscall(SemaContext *context, Expr *expr);
static inline bool sema_expr_analyse_swizzle(SemaContext *context, Expr *expr, bool swizzle_two);
static inline int builtin_expected_args(BuiltinFunction func);
static inline bool is_valid_atomicity(SemaContext *context, Expr *expr);
static bool sema_check_alignment_expression(SemaContext *context, Expr *align);

static bool sema_expr_is_valid_mask_for_value(SemaContext *context, Expr *expr, Expr *value)
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
static bool sema_check_builtin_args_match(SemaContext *context, Expr **args, size_t arg_len)
{
	ASSERT(arg_len > 1);
	Type *first = type_no_optional(args[0]->type->canonical);
	for (size_t i = 1; i < arg_len; i++)
	{
		if (first != type_no_optional(args[i]->type->canonical))
		{
			RETURN_SEMA_ERROR(args[i], "Expected an expression of type %s.", type_quoted_error_string(args[0]->type));
		}
	}
	return true;
}

/**
 * Check the constraint that the arguments are constant.
 */
static bool sema_check_builtin_args_const(SemaContext *context, Expr **args, size_t arg_len)
{
	for (size_t i = 0; i < arg_len; i++)
	{
		if (!sema_cast_const(args[i])) RETURN_SEMA_ERROR(args[i], "Expected a compile time constant value for this argument.");
	}
	return true;
}

static bool sema_check_alignment_expression(SemaContext *context, Expr *align)
{
	if (!sema_analyse_expr_rhs(context, type_usz, align, false, NULL, false)) return false;
	if (!expr_is_const_int(align)
	    || !int_fits(align->const_expr.ixx, TYPE_U64)
	    || (!is_power_of_two(align->const_expr.ixx.i.low) && align->const_expr.ixx.i.low))
	{
		RETURN_SEMA_ERROR(align, "Expected a constant power-of-two alignment or zero.");
	}
	return true;
}

static bool sema_check_builtin_args(SemaContext *context, Expr **args, BuiltinArg *arg_type, size_t arg_len)
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
				if (type_kind_is_real_vector(type->type_kind)) continue;
				RETURN_SEMA_ERROR(arg, "Expected a vector.");
			case BA_PTRVEC:
				if (type_is_pointer_vector(type)) continue;
				RETURN_SEMA_ERROR(arg, "Expected a pointer vector.");
			case BA_NUMVEC:
				if (type_kind_is_real_vector(type->type_kind) && type_is_number_or_bool(type->array.base)) continue;
				RETURN_SEMA_ERROR(arg, "Expected a numeric vector.");
			case BA_INTVEC:
				if (type_kind_is_real_vector(type->type_kind) && type_flat_is_intlike(type->array.base)) continue;
				RETURN_SEMA_ERROR(arg, "Expected an integer vector.");
			case BA_BOOLINT:
				if (type_is_integer_or_bool_kind(type)) continue;
				RETURN_SEMA_ERROR(arg, "Expected a boolean or integer value.");
			case BA_BOOLVEC:
				if (type_flat_is_bool_vector(type)) continue;
				RETURN_SEMA_ERROR(arg, "Expected a boolean vector.");
			case BA_BOOLINTVEC:
				if (type_kind_is_real_vector(type->type_kind) && type_flat_is_boolintlike(type->array.base)) continue;
				RETURN_SEMA_ERROR(arg, "Expected a boolean or integer vector.");
			case BA_FLOATVEC:
				if (type_kind_is_real_vector(type->type_kind) && type_flat_is_floatlike(type->array.base)) continue;
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
		if (!sema_analyse_expr_rvalue(context, arg)) return false;
		// Expect vector
		if (!type_flat_is_vector(arg->type)) RETURN_SEMA_ERROR(arg, "A vector was expected here.");
		// Optional-ness updated
		optional = optional || type_is_optional(args[i]->type);
	}
	// Ensure matching types
	if (swizzle_two && !sema_check_builtin_args_match(context, args, 2)) return false;

	Type *flat = type_flatten(args[0]->type);
	unsigned components = flat->array.len;
	if (swizzle_two) components *= 2;
	for (unsigned i = first_mask_value; i < arg_count; i++)
	{
		Expr *mask_val = args[i];
		if (!sema_analyse_expr_rhs(context, type_int, mask_val, false, NULL, false)) return false;
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
	expr->type = type_add_optional(type_get_vector(type_get_indexed_type(args[0]->type), flat->type_kind, arg_count - first_mask_value), optional);
	return true;
}

static inline bool is_valid_atomicity(SemaContext *context, Expr *expr)
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

	if (!sema_analyse_expr_rvalue(context, pointer)) return false;

	bool optional = IS_OPTIONAL(pointer);
	Type *comp_type = type_flatten(pointer->type);
	if (!type_is_pointer(comp_type)) RETURN_SEMA_ERROR(pointer, "Expected a pointer here.");
	Type *pointee = comp_type->pointer;
	for (int i = 1; i < 3; i++)
	{
		Expr *arg = args[i];
		if (!sema_analyse_expr_rhs(context, type_is_void(pointee) ? NULL : pointee, arg, true, NULL, false)) return false;
		if (type_is_void(pointee)) pointee = arg->type->canonical;
		if (!type_is_atomic(type_flatten(arg->type)))
		{
			RETURN_SEMA_ERROR(arg, "%s may not be used with atomics.", type_quoted_error_string(arg->type));
		}
		optional = optional || IS_OPTIONAL(args[i]);
	}
	for (int i = 3; i < 5; i++)
	{
		if (!sema_analyse_expr_rhs(context, type_bool, args[i], false, NULL, false)) return false;
		if (!sema_cast_const(args[i]))
		{
			RETURN_SEMA_ERROR(args[i], "Expected a constant boolean value.");}
	}
	for (int i = 5; i < 7; i++)
	{
		if (!sema_analyse_expr_rhs(context, type_char, args[i], false, NULL, false)) return false;
		if (!is_valid_atomicity(context, args[i])) return false;
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
		if (!sema_analyse_expr_rhs(context, type_uptr, arg, true, NULL, false)) return false;
		optional = optional || type_is_optional(arg->type);
	}
	switch (compiler.platform.arch)
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

uint64_t rand_u64()
{
	return ((uint64_t)rand() << 48) ^ ((uint64_t)rand() << 32) ^ ((uint64_t)rand() << 16) ^ rand();
}

bool sema_expr_analyse_rnd(SemaContext *context UNUSED, Expr *expr)
{
	uint64_t r = rand_u64();
	uint64_t mantissa = r >> 11;
	double val = (double)mantissa / (double)(1ULL << 53); // Not secure random but...
	expr_rewrite_const_float(expr, type_double, val);
	return true;
}

static bool sema_expr_analyse_str_replace(SemaContext *context, Expr *expr, Expr *arg, Expr *pattern, Expr *replace, Expr *limit)
{
	if (!sema_analyse_expr_rvalue(context, arg)) return false;
	if (!sema_cast_const(arg) || !expr_is_const_string(arg))
	{
		RETURN_SEMA_ERROR(arg, "Expected a constant string replace a pattern in.");
	}
	if (!sema_analyse_expr_rvalue(context, pattern)) return false;
	if (!sema_cast_const(pattern) || !expr_is_const_string(pattern))
	{
		RETURN_SEMA_ERROR(pattern, "Expected a constant pattern to replace.");
	}
	if (!sema_analyse_expr_rvalue(context, replace)) return false;
	if (!sema_cast_const(replace) || !expr_is_const_string(replace))
	{
		RETURN_SEMA_ERROR(replace, "Expected a constant replacement string.");
	}
	if (!sema_analyse_expr_rvalue(context, limit)) return false;
	if (!sema_cast_const(limit) || !expr_is_const_int(limit))
	{
		RETURN_SEMA_ERROR(limit, "Expected a constant limit.");
	}
	const char *inner_str = arg->const_expr.bytes.ptr;
	ArraySize len = arg->const_expr.bytes.len;
	const char *pattern_str = pattern->const_expr.bytes.ptr;
	ArraySize pattern_len = pattern->const_expr.bytes.len;
	const char *replace_str = replace->const_expr.bytes.ptr;
	ArraySize limit_int = int_ucomp(limit->const_expr.ixx, MAX_ARRAY_SIZE, BINARYOP_GT) ? 0 : limit->const_expr.ixx.i.low;
	scratch_buffer_clear();
	ArrayIndex index = 0;
	if (limit_int == 0) limit_int = UINT64_MAX;
	while (index < len)
	{
		const char *end = strstr(inner_str + index, pattern_str);
		if (end == NULL)
		{
			scratch_buffer_append(inner_str + index);
			break;
		}
		scratch_buffer_append_len(inner_str + index, end - inner_str - index);
		scratch_buffer_append(replace_str);
		index = end - inner_str + pattern_len;
		limit_int--;
		if (limit_int == 0)
		{
			scratch_buffer_append(inner_str + index);
			break;
		}
	}
	expr_rewrite_const_string(expr, scratch_buffer_copy());
	return true;
}

static bool sema_expr_analyse_str_hash(SemaContext *context, Expr *expr)
{
	Expr *inner = expr->call_expr.arguments[0];
	if (!sema_analyse_expr_rvalue(context, inner)) return true;
	if (!expr_is_const_string(inner))
	{
		RETURN_SEMA_ERROR(inner, "You need a compile time constant string to take the hash of it.");
	}
	uint32_t hash = (uint32_t)a5hash(inner->const_expr.bytes.ptr, inner->const_expr.bytes.len, 0);
	expr_rewrite_const_int(expr, type_uint, hash);
	return true;
}

bool sema_expr_analyse_str_find(SemaContext *context, Expr *expr)
{
	Expr *inner = expr->call_expr.arguments[0];
	Expr *inner_find = expr->call_expr.arguments[1];
	if (!sema_analyse_expr_rvalue(context, inner) || !sema_analyse_expr_rvalue(context, inner_find)) return true;
	if (!expr_is_const_string(inner))
	{
		RETURN_SEMA_ERROR(inner, "You need a compile time constant string to search.");
	}
	if (!expr_is_const_string(inner_find))
	{
		RETURN_SEMA_ERROR(inner_find, "You need a compile time constant string to search for.");
	}
	const char *inner_str = inner->const_expr.bytes.ptr;
	const char *find_str = inner_find->const_expr.bytes.ptr;
	char *ret = strstr(inner_str, find_str);
	expr_rewrite_const_int(expr, type_isz, (uint64_t)(ret == NULL ? -1 : ret - inner_str));
	return true;
}

bool sema_expr_analyse_str_conv(SemaContext *context, Expr *expr, BuiltinFunction func)
{
	Expr *inner = expr->call_expr.arguments[0];
	if (!sema_analyse_expr_rvalue(context, inner)) return true;
	if (!expr_is_const_string(inner))
	{
		RETURN_SEMA_ERROR(inner, "You need a compile time constant string to take convert.");
	}
	const char *string = inner->const_expr.bytes.ptr;
	ArraySize len = inner->const_expr.bytes.len;
	// Empty string: no conversion
	if (!len)
	{
		expr_replace(expr, inner);
		return true;
	}
	char *new_string;
	if (func == BUILTIN_STR_SNAKECASE)
	{
		int uppers = 0;
		for (ArrayIndex i = 0; i < len; i++)
		{
			if (isupper(string[i])) uppers++;
		}
		new_string = malloc_string(len + 1 + uppers);
	}
	else
	{
		new_string = malloc_string(len + 1);
	}
	switch (func)
	{
		case BUILTIN_STR_LOWER:
			for (ArraySize i = 0; i < len; i++)
			{
				char c = string[i];
				new_string[i] = (char)(char_is_upper(c) ? (c | 0x20) : c);
			}
			break;
		case BUILTIN_STR_SNAKECASE:
		{
			size_t index = 0;
			for (ArraySize i = 0; i < len; i++)
			{
				char c = string[i];
				if (isupper(c))
				{
					if (i > 0 && ((islower(string[i - 1]) || isdigit(string[i - 1])) || (i < len - 1 && islower(string[i + 1]))))
					{
						new_string[index++] = '_';
					}
					new_string[index++] = tolower(c);
					continue;
				}
				new_string[index++] = c;
			}
			len = index;
			break;
		}
		case BUILTIN_STR_PASCALCASE:
		{
			bool capitalize = true;
			size_t j = 0;
			for (ArraySize i = 0; i < len; i++)
			{
				char c = string[i];
				if (!isalpha(c))
				{
					capitalize = true;
					continue;
				}
				if (capitalize)
				{
					new_string[j++] = toupper(c);
					capitalize = false;
					continue;
				}
				new_string[j++] = tolower(c);
			}
			len = j;
			break;
		}
		case BUILTIN_STR_UPPER:
			for (ArraySize i = 0; i < len; i++)
			{
				char c = string[i];
				new_string[i] = (char)(char_is_lower(c) ? (c & ~0x20) : c);
			}
			break;
		default:
			UNREACHABLE
	}
	expr->expr_kind = EXPR_CONST;
	expr->const_expr.const_kind = CONST_STRING;
	new_string[len] = 0;
	expr->const_expr.bytes.ptr = new_string;
	expr->const_expr.bytes.len = len;
	expr->resolve_status = RESOLVE_DONE;
	expr->type = type_string;
	return true;
}

/**
 * Interpret a UTF-8 codepoint and return an integer corresponding to the raw codepoint's value.
 * @param data_ptr pointer to the pointer which scrolls along the input data string
 * @param inc pointer to the integer which will be incremented by the number of bytes consumed
 */
static uint32_t utf8_to_codepoint(const char *data_ptr, int *inc)
{
	int bytes = 1;
	const char *data = data_ptr;
	uint32_t result = 0;

	ASSERT(data);
	ASSERT(inc);

	while ((0xC0 & *(data++)) == 0x80) ++bytes;
	data = data_ptr - 1;

	switch (bytes) {
		case 1: return (uint32_t)(data[0] & 0x7FU);
		case 2:
			result = ((data[0] & 0x1FU) << 6U) | (data[1] & 0x3FU);
			break;
		case 3:
			result = ((data[0] & 0x0FU) << 12U) | ((data[1] & 0x3FU) << 6U) | (data[2] & 0x3FU);
			break;
		case 4:
			result = ((data[0] & 0x07U) << 18U) | ((data[1] & 0x3FU) << 12U) | ((data[2] & 0x3FU) << 6U) | (data[3] & 0x3FU);
			break;
		default: return 0;   // U+10FFFF is the highest codepoint in UTF-8, consisting of a maximum of 4 bytes.
	}

	*inc = bytes;
	return result;
}

bool sema_expr_analyse_str_wide(SemaContext *context, Expr *expr, BuiltinFunction func)
{
	Expr **args = expr->call_expr.arguments;
	Expr *inner = args[0];
	bool zero_terminate = true;
	unsigned arg_count = vec_size(args);
	if (arg_count > 2)
	{
		RETURN_SEMA_ERROR(inner, "Too many arguments, expected 1 or 2 arguments.");
	}
	if (arg_count == 2)
	{
		Expr *zero_term = args[1];
		if (!sema_analyse_expr_rvalue(context, zero_term)) return false;
		if (!sema_cast_const(zero_term) || !expr_is_const_bool(zero_term))
		{
			RETURN_SEMA_ERROR(zero_term, "Expected a boolean value here, to determine zero termination or not.");
		}
		zero_terminate = zero_term->const_expr.b;
	}
	if (!sema_analyse_expr_rvalue(context, inner)) return false;
	if (!sema_cast_const(inner) && !expr_is_const_string(inner))
	{
		RETURN_SEMA_ERROR(inner, "You need a compile time constant string to convert to a wide string.");
	}

	const char *string = inner->const_expr.bytes.ptr;
	ArraySize original_len = inner->const_expr.bytes.len;

	int bytes_per_unit = func == BUILTIN_WIDESTRING_16 ? 2 : 4;

	Type *type = func == BUILTIN_WIDESTRING_16 ? type_ushort : type_uint;
	ArraySize len = (original_len + 1) * bytes_per_unit;   // inflate to unit size +null-term
	ConstInitializer **elements = VECNEW(ConstInitializer*, len);

	const char *data = string;
	const char *end = data + original_len;
	while (data < end)
	{
		char c = *(data++);
		if ((0xC0 & c) != 0xC0)
		{
			// ASCII
			Expr *value = expr_new_const_int(expr->span, type, (c & 0x7F));
			vec_add(elements, const_init_new_value(value));
			continue;
		}

		int increment = 0;
		uint32_t from_codepoint = utf8_to_codepoint(data, &increment);
		data += increment - 1;
		if (!from_codepoint)
		{
			RETURN_SEMA_ERROR(inner, "Unparsable codepoint in string.");
		}
		if (type == type_ushort && from_codepoint & 0xFFFF0000)
		{
			RETURN_SEMA_ERROR(inner, "Invalid codepoint in string: a 16-bit wide-string cannot accommodate codepoints over U+FFFF.");
		}
		Expr *value = expr_new_const_int(expr->span, type, from_codepoint);
		vec_add(elements, const_init_new_value(value));
	}
	if (zero_terminate) vec_add(elements, const_init_new_zero(type));

	Type *array_type = type_get_array(type, vec_size(elements));
	ConstInitializer *init = const_init_new_array_full(array_type, elements);
	expr_rewrite_const_initializer(expr, array_type, init);
	return true;
}

INLINE BinaryOp operator_from_builtin(BuiltinFunction fn)
{
	switch (fn)
	{
		case BUILTIN_VECCOMPLT: return BINARYOP_VEC_LT;
		case BUILTIN_VECCOMPLE: return BINARYOP_VEC_LE;
		case BUILTIN_VECCOMPGT: return BINARYOP_VEC_GT;
		case BUILTIN_VECCOMPGE: return BINARYOP_VEC_GE;
		case BUILTIN_VECCOMPEQ: return BINARYOP_VEC_EQ;
		case BUILTIN_VECCOMPNE: return BINARYOP_VEC_NE;
		default: UNREACHABLE
	}
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
		case BUILTIN_SPRINTF:
			return sema_expr_analyse_sprintf(context, expr, args[0], &args[1], arg_count - 1);
		case BUILTIN_RND:
			return sema_expr_analyse_rnd(context, expr);
		case BUILTIN_STR_HASH:
			return sema_expr_analyse_str_hash(context, expr);
		case BUILTIN_STR_REPLACE:
			return sema_expr_analyse_str_replace(context, expr, args[0], args[1], args[2], args[3]);
		case BUILTIN_STR_UPPER:
		case BUILTIN_STR_LOWER:
		case BUILTIN_STR_PASCALCASE:
		case BUILTIN_STR_SNAKECASE:
			return sema_expr_analyse_str_conv(context, expr, func);
		case BUILTIN_STR_FIND:
			return sema_expr_analyse_str_find(context, expr);
		case BUILTIN_SWIZZLE2:
		case BUILTIN_SWIZZLE:
			return sema_expr_analyse_swizzle(context, expr, func == BUILTIN_SWIZZLE2);
		case BUILTIN_SYSCALL:
			return sema_expr_analyse_syscall(context, expr);
		case BUILTIN_TRAP:
		case BUILTIN_UNREACHABLE:
			expr->call_expr.no_return = true;
			FALLTHROUGH;
		case BUILTIN_BREAKPOINT:
			expr->type = type_void;
			return true;
		case BUILTIN_SYSCLOCK:
			expr->type = type_ulong;
			return true;
		case BUILTIN_GET_ROUNDING_MODE:
			expr->type = type_int;
			return true;
		case BUILTIN_COMPARE_EXCHANGE:
			return sema_expr_analyse_compare_exchange(context, expr);
		case BUILTIN_WIDESTRING_16:
		case BUILTIN_WIDESTRING_32:
			return sema_expr_analyse_str_wide(context, expr, func);
		default:
			break;
	}

	bool optional = false;

	// 2. We can now check all the arguments, since they in general work on the
	//    exact type size, we don't do any forced promotion.
	for (unsigned i = 0; i < arg_count; i++)
	{
		if (!sema_analyse_expr_rvalue(context, args[i])) return false;
		optional = optional || type_is_optional(args[i]->type);
	}

	Type *rtype = NULL;
	switch (func)
	{
		case BUILTIN_SET_ROUNDING_MODE:
			ASSERT(arg_count == 1);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_INTEGER}, 1)) return false;
			rtype = type_void;
			break;
		case BUILTIN_SYSCALL:
		case BUILTIN_STR_HASH:
		case BUILTIN_STR_UPPER:
		case BUILTIN_STR_LOWER:
		case BUILTIN_STR_PASCALCASE:
		case BUILTIN_STR_SNAKECASE:
		case BUILTIN_STR_FIND:
		case BUILTIN_STR_REPLACE:
		case BUILTIN_WIDESTRING_16:
		case BUILTIN_WIDESTRING_32:
		case BUILTIN_SPRINTF:
			UNREACHABLE
		case BUILTIN_VECCOMPGE:
		case BUILTIN_VECCOMPEQ:
		case BUILTIN_VECCOMPLE:
		case BUILTIN_VECCOMPGT:
		case BUILTIN_VECCOMPLT:
		case BUILTIN_VECCOMPNE:
		{
			ASSERT(arg_count == 2);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_NUMVEC, BA_NUMVEC}, 2)) return false;
			if (!sema_check_builtin_args_match(context, args, 2)) return false;
			rtype = type_get_vector_from_vector(type_bool, type_flatten(args[0]->type));
			expr->expr_kind = EXPR_BINARY;
			expr->binary_expr = (ExprBinary) {
				.left = exprid(args[0]),
				.right = exprid(args[1]),
				.operator = operator_from_builtin(func)
			};
			break;
		}
		case BUILTIN_SELECT:
			ASSERT(arg_count == 3);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_BOOLVEC, BA_VEC, BA_VEC}, 3)) return false;
			if (!sema_check_builtin_args_match(context, &args[1], 2)) return false;
			rtype = args[1]->type;
			if (!sema_expr_is_valid_mask_for_value(context, args[0], args[1])) return false;
			break;
		case BUILTIN_OVERFLOW_ADD:
		case BUILTIN_OVERFLOW_MUL:
		case BUILTIN_OVERFLOW_SUB:
			ASSERT(arg_count == 3);
			if (!sema_check_builtin_args(context, args,
						     (BuiltinArg[]) {BA_INTLIKE, BA_INTLIKE, BA_POINTER},
						     3)) return false;
			if (!sema_check_builtin_args_match(context, args, 2)) return false;
			if (type_no_optional(args[0]->type->canonical) != type_no_optional(args[2]->type->canonical->pointer))
			{
				RETURN_SEMA_ERROR(args[2], "Expected %s, not %s.",
								  type_to_error_string(type_get_ptr(args[0]->type)),
								  type_to_error_string(args[2]->type));
			}
			Type *flat_0 = type_flatten(args[0]->type);
			if (type_kind_is_real_vector(flat_0->type_kind))
			{
				rtype = type_get_vector_from_vector(type_bool, flat_0);
				break;
			}
			rtype = type_bool;
			break;
		case BUILTIN_EXACT_ADD:
		case BUILTIN_EXACT_DIV:
		case BUILTIN_EXACT_MUL:
		case BUILTIN_EXACT_SUB:
		case BUILTIN_EXACT_MOD:
			ASSERT(arg_count == 2);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_INTEGER, BA_INTEGER}, 2)) return false;
			if (!sema_check_builtin_args_match(context, args, 2)) return false;
			rtype = args[0]->type->canonical;
			break;
		case BUILTIN_ANY_MAKE:
			ASSERT(arg_count == 2);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_POINTER, BA_TYPEID}, 2)) return false;
			{
				expr->expr_kind = EXPR_MAKE_ANY;
				expr->make_any_expr = (ExprMakeAny) {
					.inner = args[0],
					.typeid = args[1]
				};
				expr->type = type_add_optional(type_any, optional);
				return true;
			}
		case BUILTIN_EXACT_NEG:
			ASSERT(arg_count == 1);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_INTLIKE}, 1)) return false;
			rtype = args[0]->type->canonical;
			break;
		case BUILTIN_MEMCOPY_INLINE:
			ASSERT(arg_count == 6);
			if (!sema_check_builtin_args(context, args,
						     (BuiltinArg[]) {BA_POINTER, BA_POINTER, BA_SIZE, BA_BOOL, BA_SIZE, BA_SIZE},
						     6)) return false;
			if (!sema_check_builtin_args_const(context, &args[2], 4)) return false;
			rtype = type_void;
			break;
		case BUILTIN_MEMCOPY:
		case BUILTIN_MEMMOVE:
			ASSERT(arg_count == 6);
			if (!sema_check_builtin_args(context, args,
						     (BuiltinArg[]) {BA_POINTER, BA_POINTER, BA_SIZE, BA_BOOL, BA_SIZE, BA_SIZE},
						     6)) return false;
			if (!sema_check_builtin_args_const(context, &args[3], 3)) return false;
			rtype = type_void;
			break;
		case BUILTIN_MEMSET:
			ASSERT(arg_count == 5);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_POINTER, BA_CHAR, BA_SIZE, BA_BOOL, BA_SIZE},
						     5)) return false;
			if (!sema_check_builtin_args_const(context, &args[3], 2)) return false;
			rtype = type_void;
			break;
		case BUILTIN_MEMSET_INLINE:
			ASSERT(arg_count == 5);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_POINTER, BA_CHAR, BA_SIZE, BA_BOOL, BA_SIZE},
						     5)) return false;
			if (!sema_check_builtin_args_const(context, &args[2], 3)) return false;
			rtype = type_void;
			break;
		case BUILTIN_BITREVERSE:
		case BUILTIN_BSWAP:
		case BUILTIN_CTLZ:
		case BUILTIN_POPCOUNT:
		case BUILTIN_CTTZ:
			ASSERT(arg_count == 1);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_INTLIKE}, 1)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_MATRIX_TRANSPOSE:
		{
			ASSERT(arg_count == 3);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_VEC, BA_INTEGER, BA_INTEGER}, 3)) return false;
			if (!sema_check_builtin_args_const(context, &args[1], 2)) return false;
			ArraySize vec_len = type_flatten(args[0]->type)->array.len;
			Int sum = int_mul(args[1]->const_expr.ixx, args[2]->const_expr.ixx);
			if (!int_icomp(sum, vec_len, BINARYOP_EQ))
			{
				RETURN_SEMA_ERROR(args[1], "Expected row * col to equal %d.", vec_len);
			}
			rtype = args[0]->type;
			break;
		}
		case BUILTIN_MATRIX_MUL:
			ASSERT(arg_count == 5);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_VEC, BA_VEC, BA_INTEGER, BA_INTEGER, BA_INTEGER }, 5)) return false;
			if (!sema_check_builtin_args_const(context, &args[2], 3)) return false;
			Type *flat1 = type_flatten(args[0]->type);
			Type *flat2 = type_flatten(args[1]->type);
			if (flat1->array.base != flat2->array.base)
			{
				RETURN_SEMA_ERROR(args[1], "Expected both matrices to be of the same type.");
			}
			ArraySize vec_len1 = flat1->array.len;
			ArraySize vec_len2 = flat2->array.len;
			Int128 sum = i128_mult(args[2]->const_expr.ixx.i, args[3]->const_expr.ixx.i);
			args[2]->type = type_int;
			args[3]->type = type_int;
			if (sum.high != 0 || sum.low != vec_len1)
			{
				RETURN_SEMA_ERROR(args[3], "Expected outer row * inner to equal %d.", vec_len1);
			}
			sum = i128_mult(args[3]->const_expr.ixx.i, args[4]->const_expr.ixx.i);
			if (sum.high != 0 || sum.low != vec_len2)
			{
				RETURN_SEMA_ERROR(args[4], "Expected inner * outer col to equal %d.", vec_len2);
			}
			rtype = type_get_vector(flat1->array.base, flat1->type_kind, i128_mult(args[2]->const_expr.ixx.i, args[4]->const_expr.ixx.i).low);
			break;
		case BUILTIN_SAT_SHL:
		case BUILTIN_SAT_SUB:
		case BUILTIN_SAT_ADD:
		case BUILTIN_SAT_MUL:
			ASSERT(arg_count == 2);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_INTLIKE, BA_INTLIKE}, 2)) return false;
			if (!sema_check_builtin_args_match(context, args, 2)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_REVERSE:
			ASSERT(arg_count == 1);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_VEC}, 1)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_EXPECT:
			ASSERT(arg_count == 2);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_BOOLINT, BA_BOOLINT}, 2)) return false;
			if (!sema_check_builtin_args_match(context, args, 2)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_EXPECT_WITH_PROBABILITY:
			ASSERT(arg_count == 3);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_BOOLINT, BA_BOOLINT}, 2)) return false;
			if (!cast_implicit(context, args[2], type_double, false))
			{
				RETURN_SEMA_ERROR(args[2], "Expected a 'double', but was %s.", type_quoted_error_string(args[2]->type));
			}
			if (!sema_cast_const(args[2]))
			{
				RETURN_SEMA_ERROR(args[2], "This value must be a constant.");
			}
			Real r = args[2]->const_expr.fxx.f;
			if (r < 0 || r > 1) RETURN_SEMA_ERROR(args[2], "The probability must be between 0 and 1.");
			if (!sema_check_builtin_args_match(context, args, 2)) return false;
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
			ASSERT(arg_count);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_FLOATLIKE, BA_FLOATLIKE, BA_FLOATLIKE},
						     arg_count)) return false;
			rtype = args[0]->type;
			if (func == BUILTIN_CEIL && expr_is_const_float(args[0]))
			{
				expr_rewrite_const_float(expr, rtype, ceil(args[0]->const_expr.fxx.f));
				return true;
			}
			break;
		case BUILTIN_FRAMEADDRESS:
		case BUILTIN_RETURNADDRESS:
			ASSERT(arg_count);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_INTEGER}, arg_count)) return false;
			if (!cast_implicit(context, args[0], type_int, false)) return false;
			if (!expr_is_const_int(args[0])) RETURN_SEMA_ERROR(args[0], "Expected a compile time constant integer.");
			rtype = type_voidptr;
			break;
		case BUILTIN_WASM_MEMORY_SIZE:
			ASSERT(arg_count == 1);
			if (!cast_implicit(context, args[0], type_uint, false)) return false;
			rtype = type_uptr;
			break;
		case BUILTIN_WASM_MEMORY_GROW:
			ASSERT(arg_count == 2);
			if (!cast_implicit(context, args[0], type_uint, false)) return false;
			if (!cast_implicit(context, args[1], type_uptr, false)) return false;
			rtype = type_iptr;
			break;
		case BUILTIN_PREFETCH:
			ASSERT(arg_count == 3);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_POINTER, BA_INTEGER, BA_INTEGER}, 3)) return false;
			for (unsigned i = 1; i < 3; i++)
			{
				if (!sema_cast_const(args[i])) RETURN_SEMA_ERROR(args[i], "A constant value is required.");
				if (!cast_implicit(context, args[i], type_int, false)) return false;
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
			if (!cast_implicit(context, args[0], type_voidptr, false)) return false;
			rtype = type_void;
			break;
		case BUILTIN_POW:
			ASSERT(arg_count == 2);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_FLOATLIKE, BA_FLOATLIKE}, 2)) return false;
			if (!sema_check_builtin_args_match(context, args, 2)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_POW_INT:
			ASSERT(arg_count == 2);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_FLOATLIKE, BA_INTLIKE}, 2)) return false;
			if (!cast_implicit(context, args[1], type_cint, false)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_REDUCE_FMUL:
		case BUILTIN_REDUCE_FADD:
			ASSERT(arg_count == 2);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_FLOATVEC, BA_FLOAT}, 2)) return false;
			if (!cast_implicit(context, args[1], args[0]->type->canonical->array.base, false)) return false;
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
			ASSERT(arg_count == 1);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_NUMVEC}, 1)) return false;
			rtype = type_get_indexed_type(args[0]->type);
			break;
		}
		case BUILTIN_REDUCE_ADD:
		case BUILTIN_REDUCE_AND:
		case BUILTIN_REDUCE_OR:
		case BUILTIN_REDUCE_XOR:
		case BUILTIN_REDUCE_MUL:
			ASSERT(arg_count == 1);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_BOOLINTVEC}, 1)) return false;
			rtype = type_get_indexed_type(args[0]->type);
			break;
		case BUILTIN_ABS:
			ASSERT(arg_count == 1);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_NUMLIKE}, 1)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_GATHER:
		{
			ASSERT(arg_count == 4);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_PTRVEC, BA_BOOLVEC, BA_VEC, BA_INTEGER}, 4)) return false;
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
								  type_get_vector(pointer_type->pointer, flat_pointer_vec->type_kind, len)),
						  type_quoted_error_string(args[2]->type));
			}
			if (!sema_check_alignment_expression(context, args[3])) return false;
			if (!sema_expr_is_valid_mask_for_value(context, args[1], args[2])) return false;
			rtype = type_get_vector(pointer_type->pointer, flat_pointer_vec->type_kind, len);
			break;
		}
		case BUILTIN_SCATTER:
		{
			ASSERT(arg_count == 4);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_PTRVEC, BA_VEC, BA_BOOLVEC, BA_INTEGER}, 4)) return false;
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
								  type_get_vector_from_vector(pointer_type->pointer, flat_pointer_vec)),
						  type_quoted_error_string(args[2]->type));
			}
			if (!sema_check_alignment_expression(context, args[3])) return false;
			if (!sema_expr_is_valid_mask_for_value(context, args[2], args[1])) return false;
			rtype = type_void;
			break;
		}
		case BUILTIN_MASKED_LOAD:
		{
			ASSERT(arg_count == 4);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_POINTER, BA_BOOLVEC, BA_VEC, BA_INTEGER}, 4)) return false;
			Type *pointer_type = args[0]->type;
			if (!type_is_pointer(pointer_type)) RETURN_SEMA_ERROR(args[0], "Expected a direct pointer.");
			if (pointer_type->pointer->canonical != args[2]->type->canonical)
			{
				RETURN_SEMA_ERROR(args[2], "Expected the value to be of type '%s'.", type_quoted_error_string(pointer_type->pointer));
			}
			if (!sema_check_alignment_expression(context, args[3])) return false;
			if (!sema_expr_is_valid_mask_for_value(context, args[1], args[2])) return false;
			rtype = pointer_type->pointer;
			break;
		}
		case BUILTIN_MASKED_STORE:
		{
			ASSERT(arg_count == 4);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_POINTER, BA_VEC, BA_BOOLVEC, BA_INTEGER}, 4)) return false;
			Type *pointer_type = args[0]->type;
			if (!type_is_pointer(pointer_type)) RETURN_SEMA_ERROR(args[0], "Expected a direct pointer.");
			if (pointer_type->pointer->canonical != args[1]->type->canonical)
			{
				RETURN_SEMA_ERROR(args[2], "Expected the value to be of type %s.", type_quoted_error_string(pointer_type->pointer));
			}
			if (!sema_check_alignment_expression(context, args[3])) return false;
			if (!sema_expr_is_valid_mask_for_value(context, args[2], args[1])) return false;
			rtype = type_void;
			break;
		}
		case BUILTIN_MAX:
		case BUILTIN_MIN:
			ASSERT(arg_count == 2);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_NUMLIKE, BA_NUMLIKE}, 2)) return false;
			if (!sema_check_builtin_args_match(context, args, 2)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_FMA:
			ASSERT(arg_count == 3);
			if (!sema_check_builtin_args(context, args,
						     (BuiltinArg[]) {BA_FLOATLIKE, BA_FLOATLIKE, BA_FLOATLIKE},
						     3)) return false;
			if (!sema_check_builtin_args_match(context, args, 3)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_FSHL:
		case BUILTIN_FSHR:
			ASSERT(arg_count == 3);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_INTLIKE, BA_INTLIKE, BA_INTLIKE},
						     3)) return false;
			if (!sema_check_builtin_args_match(context, args, 3)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_FMULADD:
			ASSERT(arg_count == 3);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_FLOAT, BA_FLOAT, BA_FLOAT},
						     3)) return false;
			if (!sema_check_builtin_args_match(context, args, 3)) return false;
			rtype = args[0]->type;
			break;
		case BUILTIN_FENCE:
			ASSERT(arg_count == 1);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_INTEGER}, 1)) return false;
			if (!sema_cast_const(args[0])) RETURN_SEMA_ERROR(args[0], "Ordering must be a compile time constant.");
			if (!is_valid_atomicity(context, args[0])) return false;
			switch (args[0]->const_expr.ixx.i.low)
			{
				case ATOMIC_NONE:
				case ATOMIC_RELAXED:
				case ATOMIC_UNORDERED:
					RETURN_SEMA_ERROR(args[0], "'none', 'relaxed' and 'unordered' are not valid for fence.");
				default:
					break;
			}
			rtype = type_void;
			break;
		case BUILTIN_ATOMIC_LOAD:
		{
			ASSERT(arg_count == 3);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_POINTER, BA_BOOL, BA_INTEGER}, 3)) return false;
			Type *original = type_flatten(args[0]->type);
			if (original == type_voidptr) RETURN_SEMA_ERROR(args[0], "Expected a typed pointer.");
			if (!sema_cast_const(args[1])) RETURN_SEMA_ERROR(args[1], "'is_volatile' must be a compile time constant.");
			if (!sema_cast_const(args[2])) RETURN_SEMA_ERROR(args[2], "Ordering must be a compile time constant.");
			if (!is_valid_atomicity(context, args[2])) return false;
			switch (args[2]->const_expr.ixx.i.low)
			{
				case ATOMIC_ACQUIRE_RELEASE:
				case ATOMIC_RELEASE:
					RETURN_SEMA_ERROR(args[2], "'release' and 'acquire release' are not valid for atomic loads.");
				default:
					break;
			}
			rtype = original->pointer;
			break;
		}
		case BUILTIN_UNALIGNED_LOAD:
		{
			ASSERT(arg_count == 2);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_POINTER, BA_INTEGER}, 2)) return false;
			Type *original = type_flatten(args[0]->type);
			if (original == type_voidptr) RETURN_SEMA_ERROR(args[0], "Expected a typed pointer.");
			if (!sema_check_alignment_expression(context, args[1])) return false;
			rtype = original->pointer;
			break;
		}
		case BUILTIN_UNALIGNED_STORE:
		{
			ASSERT(arg_count == 3);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_POINTER}, 1)) return false;
			if (!sema_check_builtin_args(context, &args[2], (BuiltinArg[]) {BA_INTEGER}, 1)) return false;
			Type *original = type_flatten(args[0]->type);
			if (!sema_check_alignment_expression(context, args[2])) return false;
			if (original != type_voidptr)
			{
				if (!cast_implicit(context, args[1], original->pointer, false)) return false;
			}
			rtype = args[1]->type;
			break;
		}
		case BUILTIN_VOLATILE_LOAD:
		{
			ASSERT(arg_count == 1);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_POINTER}, 1)) return false;
			Type *original = type_flatten(args[0]->type);
			if (original == type_voidptr) RETURN_SEMA_ERROR(args[0], "Expected a typed pointer.");
			rtype = original->pointer;
			break;
		}
		case BUILTIN_VOLATILE_STORE:
		{
			ASSERT(arg_count == 2);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_POINTER}, 1)) return false;
			Type *original = type_flatten(args[0]->type);
			if (original != type_voidptr)
			{
				if (!cast_implicit(context, args[1], original->pointer, false)) return false;
			}
			rtype = args[1]->type;
			break;
		}
		case BUILTIN_ATOMIC_FETCH_INC_WRAP:
		case BUILTIN_ATOMIC_FETCH_DEC_WRAP:
		{
			ASSERT(arg_count == 5);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_POINTER, BA_INTEGER}, 2)) return false;
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
				if (!cast_implicit(context, args[1], original->pointer, false)) return false;
			}
			if (!sema_cast_const(args[2])) RETURN_SEMA_ERROR(args[2], "'is_volatile' must be a compile time constant.");
			if (!sema_cast_const(args[3])) RETURN_SEMA_ERROR(args[3], "Ordering must be a compile time constant.");
			if (!is_valid_atomicity(context, args[3])) return false;
			if (args[3]->const_expr.ixx.i.low == ATOMIC_UNORDERED) RETURN_SEMA_ERROR(args[3], "'unordered' is not valid ordering.");
			if (!sema_check_alignment_expression(context, args[4])) return false;
			rtype = args[1]->type;
			break;
		}
		case BUILTIN_ATOMIC_FETCH_AND:
		case BUILTIN_ATOMIC_FETCH_NAND:
		case BUILTIN_ATOMIC_FETCH_OR:
		case BUILTIN_ATOMIC_FETCH_XOR:
		{
			ASSERT(arg_count == 5);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_POINTER, BA_BOOLINT}, 2)) return false;
			Type *original = type_flatten(args[0]->type);
			if (original != type_voidptr)
			{
				if (!cast_implicit(context, args[1], original->pointer, false)) return false;
			}
			if (!sema_cast_const(args[2])) RETURN_SEMA_ERROR(args[2], "'is_volatile' must be a compile time constant.");
			if (!sema_cast_const(args[3])) RETURN_SEMA_ERROR(args[3], "Ordering must be a compile time constant.");
			if (!is_valid_atomicity(context, args[3])) return false;
			if (args[3]->const_expr.ixx.i.low == ATOMIC_UNORDERED) RETURN_SEMA_ERROR(args[3], "'unordered' is not valid ordering.");
			if (!sema_check_alignment_expression(context, args[4])) return false;
			rtype = args[1]->type;
			break;
		}
		case BUILTIN_ATOMIC_FETCH_EXCHANGE:
		{
			ASSERT(arg_count == 5);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_POINTER}, 1)) return false;
			Type *original = type_flatten(args[0]->type);
			if (original != type_voidptr)
			{
				if (!cast_implicit(context, args[1], original->pointer, false)) return false;
			}
			Type *val = type_flatten(args[1]->type);
			if (!type_is_atomic(val)) RETURN_SEMA_ERROR(args[1], "%s exceeds pointer size.", val);
			if (!sema_cast_const(args[2])) RETURN_SEMA_ERROR(args[2], "'is_volatile' must be a compile time constant.");
			if (!sema_cast_const(args[3])) RETURN_SEMA_ERROR(args[3], "Ordering must be a compile time constant.");
			if (!is_valid_atomicity(context, args[3])) return false;
			if (args[3]->const_expr.ixx.i.low == ATOMIC_UNORDERED) RETURN_SEMA_ERROR(args[3], "'unordered' is not valid ordering.");
			if (!sema_check_alignment_expression(context, args[4])) return false;
			rtype = args[1]->type;
			break;
		}
		case BUILTIN_ATOMIC_FETCH_ADD:
		case BUILTIN_ATOMIC_FETCH_SUB:
		case BUILTIN_ATOMIC_FETCH_MAX:
		case BUILTIN_ATOMIC_FETCH_MIN:
		{
			ASSERT(arg_count == 5);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_POINTER, BA_NUM}, 2)) return false;
			Type *original = type_flatten(args[0]->type);
			if (original != type_voidptr)
			{
				if (!cast_implicit(context, args[1], original->pointer, false)) return false;
			}
			if (!sema_cast_const(args[2])) RETURN_SEMA_ERROR(args[2], "'is_volatile' must be a compile time constant.");
			if (!sema_cast_const(args[3])) RETURN_SEMA_ERROR(args[3], "Ordering must be a compile time constant.");
			if (!is_valid_atomicity(context, args[3])) return false;
			if (args[3]->const_expr.ixx.i.low == ATOMIC_UNORDERED) RETURN_SEMA_ERROR(args[3], "'unordered' is not valid ordering.");
			if (!sema_check_alignment_expression(context, args[4])) return false;
			rtype = args[1]->type;
			break;
		}
		case BUILTIN_ATOMIC_STORE:
		{
			ASSERT(arg_count == 4);
			if (!sema_check_builtin_args(context, args, (BuiltinArg[]) {BA_POINTER}, 1)) return false;
			if (!sema_check_builtin_args(context, &args[2], (BuiltinArg[]) {BA_BOOL, BA_INTEGER}, 2)) return false;
			Type *original = type_flatten(args[0]->type);
			if (original != type_voidptr)
			{
				if (!cast_implicit(context, args[1], original->pointer, false)) return false;
			}
			if (!sema_cast_const(args[2])) RETURN_SEMA_ERROR(args[2], "'is_volatile' must be a compile time constant.");
			if (!sema_cast_const(args[3])) RETURN_SEMA_ERROR(args[3], "Ordering must be a compile time constant.");
			if (!is_valid_atomicity(context, args[3])) return false;
			switch (args[3]->const_expr.ixx.i.low)
			{
				case ATOMIC_ACQUIRE_RELEASE:
				case ATOMIC_ACQUIRE:
					RETURN_SEMA_ERROR(args[3], "'acquire' and 'acquire release' are not valid for atomic stores.");
				default:
					break;
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
		case BUILTIN_BREAKPOINT:
		case BUILTIN_UNREACHABLE:
		case BUILTIN_RND:
			UNREACHABLE
	}
	expr->type = type_add_optional(rtype, optional);
	return true;
}

static inline int builtin_expected_args(BuiltinFunction func)
{
	switch (func)
	{
		case BUILTIN_SPRINTF:
		case BUILTIN_SYSCALL:
		case BUILTIN_WIDESTRING_16:
		case BUILTIN_WIDESTRING_32:
			return -1;
		case BUILTIN_SWIZZLE:
			return -2;
		case BUILTIN_SWIZZLE2:
			return -3;
		case BUILTIN_GET_ROUNDING_MODE:
		case BUILTIN_SYSCLOCK:
		case BUILTIN_TRAP:
		case BUILTIN_BREAKPOINT:
		case BUILTIN_UNREACHABLE:
		case BUILTIN_RND:
			return 0;
		case BUILTIN_ABS:
		case BUILTIN_BITREVERSE:
		case BUILTIN_BSWAP:
		case BUILTIN_CEIL:
		case BUILTIN_COS:
		case BUILTIN_CTLZ:
		case BUILTIN_CTTZ:
		case BUILTIN_EXACT_NEG:
		case BUILTIN_EXP2:
		case BUILTIN_EXP:
		case BUILTIN_FENCE:
		case BUILTIN_FLOOR:
		case BUILTIN_FRAMEADDRESS:
		case BUILTIN_LLRINT:
		case BUILTIN_LLROUND:
		case BUILTIN_LOG10:
		case BUILTIN_LOG2:
		case BUILTIN_LOG:
		case BUILTIN_LRINT:
		case BUILTIN_LROUND:
		case BUILTIN_NEARBYINT:
		case BUILTIN_POPCOUNT:
		case BUILTIN_REDUCE_ADD:
		case BUILTIN_REDUCE_AND:
		case BUILTIN_REDUCE_MAX:
		case BUILTIN_REDUCE_MIN:
		case BUILTIN_REDUCE_MUL:
		case BUILTIN_REDUCE_OR:
		case BUILTIN_REDUCE_XOR:
		case BUILTIN_RETURNADDRESS:
		case BUILTIN_REVERSE:
		case BUILTIN_RINT:
		case BUILTIN_ROUND:
		case BUILTIN_ROUNDEVEN:
		case BUILTIN_SET_ROUNDING_MODE:
		case BUILTIN_SIN:
		case BUILTIN_SQRT:
		case BUILTIN_STR_HASH:
		case BUILTIN_STR_UPPER:
		case BUILTIN_STR_LOWER:
		case BUILTIN_STR_SNAKECASE:
		case BUILTIN_STR_PASCALCASE:
		case BUILTIN_TRUNC:
		case BUILTIN_VOLATILE_LOAD:
		case BUILTIN_WASM_MEMORY_SIZE:
			return 1;
		case BUILTIN_STR_FIND:
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
		case BUILTIN_SAT_MUL:
		case BUILTIN_VOLATILE_STORE:
		case BUILTIN_VECCOMPNE:
		case BUILTIN_VECCOMPLT:
		case BUILTIN_VECCOMPLE:
		case BUILTIN_VECCOMPGE:
		case BUILTIN_VECCOMPGT:
		case BUILTIN_VECCOMPEQ:
		case BUILTIN_WASM_MEMORY_GROW:
		case BUILTIN_ANY_MAKE:
		case BUILTIN_UNALIGNED_LOAD:
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
		case BUILTIN_UNALIGNED_STORE:
		case BUILTIN_SELECT:
		case BUILTIN_MATRIX_TRANSPOSE:
			return 3;
		case BUILTIN_ATOMIC_STORE:
		case BUILTIN_MASKED_STORE:
		case BUILTIN_MASKED_LOAD:
		case BUILTIN_GATHER:
		case BUILTIN_SCATTER:
		case BUILTIN_STR_REPLACE:
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
		case BUILTIN_MATRIX_MUL:
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
