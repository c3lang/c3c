#include "compiler_internal.h"

#include <math.h>
#include <errno.h>

UNUSED static int precision_bits(TypeKind kind)
{
	switch (kind)
	{
		case TYPE_F16:
			return 11;
		case TYPE_BF16:
			return 8;
		case TYPE_F32:
			return 24;
		case TYPE_F64:
			return 53;
		case TYPE_F128:
			return 113;
		default:
			UNREACHABLE
	}
}

UNUSED static int max_exponent(TypeKind kind)
{
	switch (kind)
	{
		case TYPE_F16:
			return 15;
		case TYPE_BF16:
			return 127;
		case TYPE_F32:
			return 127;
		case TYPE_F64:
			return 1023;
		case TYPE_F128:
			return 16383;
		default:
			UNREACHABLE
	}
}

UNUSED static int min_exponent(TypeKind kind)
{
	switch (kind)
	{
		case TYPE_F16:
			return -14;
		case TYPE_BF16:
			return -126;
		case TYPE_F32:
			return -126;
		case TYPE_F64:
			return -1022;
		case TYPE_F128:
			return -16382;
		default:
			UNREACHABLE
	}
}


Float float_add(Float op1, Float op2)
{
	ASSERT0(op1.type == op2.type);
	return (Float){ op1.f + op2.f, op1.type };
}

Float float_sub(Float op1, Float op2)
{
	ASSERT0(op1.type == op2.type);
	return (Float){ op1.f - op2.f, op1.type };
}

Float float_mul(Float op1, Float op2)
{
	ASSERT0(op1.type == op2.type);
	return (Float){ op1.f * op2.f, op1.type };
}

Float float_div(Float op1, Float op2)
{
	ASSERT0(op1.type == op2.type);
	return (Float){ op1.f / op2.f, op1.type };
}

Float float_rem(Float op1, Float op2)
{
	ASSERT0(op1.type == op2.type);
	return (Float){fmod(op1.f, op2.f), op1.type };
}

Float float_neg(Float op)
{
	op.f = -op.f;
	return op;
}

static char *err_invalid_float_width = "The float width is not valid, it must be one of 16, 32, 64 and 128.";
static char *err_float_out_of_range = "The float value is out of range.";
static char *err_float_format_invalid = "The float format is invalid.";

TypeKind float_suffix(char c, const char **index_ref, char** error_ref)
{
	if (c == 'b' && (*index_ref)[0] == 'f' && (*index_ref)[1] == '1' && (*index_ref)[1] == '6')
	{
		(*index_ref) += 4;
		return TYPE_BF16;
	}
	if (c == 'f')
	{
		int i = 0;
		while ((c = *((*index_ref)++)) && (c >= '0' && c <= '9'))
		{
			if (i > 100)
			{
				if (error_ref) *error_ref = err_invalid_float_width;
				return TYPE_POISONED;
			}
			i = i * 10 + c - '0';
		}
		switch (i)
		{
			case 0:
			case 32:
				return TYPE_F32;
			case 16:
				return TYPE_F16;
			case 64:
				return TYPE_F64;
			case 128:
				return TYPE_F128;
			default:
				if (error_ref) *error_ref = err_invalid_float_width;
				return TYPE_POISONED;
		}
	}
	return TYPE_F64;
}
/**
 * This parses a float from a string. Unfortunately it is limited to parsing doubles as of now.
 *
 * @param string
 * @param error
 * @return the resulting Float, with type = TYPE_POISONED on error.
 */
Float float_from_string(const char *string, char **error)
{
	const char *index = string;
	char c;
	scratch_buffer_clear();
	while ((c = *(index++)) && (c == '_' || (c >= '0' && c <= '9')))
	{
		if (c == '_') continue;
		scratch_buffer_append_char(c);
	}
	if (c == '.')
	{
		scratch_buffer_append_char(c);
		while ((c = *(index++)) && (c == '_' || (c >= '0' && c <= '9')))
		{
			if (c == '_') continue;
			scratch_buffer_append_char(c);
		}
	}
	if (c == 'e' || c == 'E')
	{
		scratch_buffer_append_char(c);
		if (*index == '-')
		{
			scratch_buffer_append_char('-');
			index++;
		}
		else if (*index == '+') index++;
		while ((c = *(index++)) && (c >= '0' && c <= '9'))
		{
			scratch_buffer_append_char(c);
		}
	}
	TypeKind kind = float_suffix(c, &index, error);
	if (kind == TYPE_POISONED) return (Float){ .type = TYPE_POISONED };

	const char *str = scratch_buffer_to_string();
	char *end = NULL;
	errno = 0;
	double d = strtod(str, &end);
	if (d == HUGE_VAL && errno == ERANGE)
	{
		if (error) *error = err_float_out_of_range;
		return (Float){ .type = TYPE_POISONED };
	}
	char *expected_end = scratch_buffer.str + scratch_buffer.len;
	if (d == 0 && end != expected_end)
	{
		if (error) *error = err_float_format_invalid;
		return (Float){ .type = TYPE_POISONED };
	}
	return (Float){ d, kind };
}

/**
 * This parses a float from hex. Unfortunately it is limited to parsing doubles as of now.
 *
 * @param string
 * @param error
 * @return the resulting Float, with type = TYPE_POISONED on error.
 */
Float float_from_hex(const char *string, char **error)
{
	const char *index = string + 2;
	char c;
	scratch_buffer_clear();
	scratch_buffer_append("0x");
	while ((c = *(index++)) && (c == '_' || char_is_hex(c)))
	{
		if (c == '_') continue;
		scratch_buffer_append_char(c);
	}
	if (c == '.')
	{
		scratch_buffer_append_char(c);
		while ((c = *(index++)) && (c == '_' || char_is_hex(c)))
		{
			if (c == '_') continue;
			scratch_buffer_append_char(c);
		}
	}
	if (c == 'p' || c == 'P')
	{
		scratch_buffer_append_char(c);
		if (*index == '-')
		{
			scratch_buffer_append_char('-');
			index++;
		}
		else if (*index == '+') index++;
		while ((c = *(index++)) && (c >= '0' && c <= '9'))
		{
			scratch_buffer_append_char(c);
		}
	}
	TypeKind kind = float_suffix(c, &index, error);
	if (kind == TYPE_POISONED) return (Float){ .type = TYPE_POISONED };

	const char *str = scratch_buffer_to_string();
	char *end = NULL;
	errno = 0;
	double d = strtod(str, &end);
	if (d == HUGE_VAL && errno == ERANGE)
	{
		if (error) *error = err_float_out_of_range;
		return (Float){ .type = TYPE_POISONED };
	}
	if (d == 0 && end != scratch_buffer.str + scratch_buffer.len)
	{
		if (error) *error = err_float_format_invalid;
		return (Float){ .type = TYPE_POISONED };
	}
	return (Float){ d, kind };
}