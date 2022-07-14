module std::io;
import libc;

const int PRINTF_NTOA_BUFFER_SIZE = 256;
const int PRINTF_FTOA_BUFFER_SIZE = 256;
const float PRINTF_MAX_FLOAT = 1e9;
const uint PRINTF_DEFAULT_FLOAT_PRECISION = 6;

fault PrintFault
{
	BUFFER_EXCEEDED,
	INTERNAL_BUFFER_EXCEEDED,
	INVALID_FORMAT_STRING,
	MISSING_ARG,
}
bitstruct PrintFlags : uint
{
	bool zeropad : 0;
	bool left : 1;
	bool plus : 2;
	bool space : 3;
	bool hash : 4;
	bool uppercase : 5;
	bool precision : 6;
	bool adapt_exp : 7;
}

define OutputFn = fn void!(char c, char[] buffer, usize buffer_idx);


private fn usize! out_str(OutputFn out, char[] buffer, usize idx, variant arg, uint prec, uint width, PrintFlags flags)
{
	switch (arg.type.kind)
	{
		case TYPEID:
			return out_substr(out, buffer, idx, "<typeid>", prec, width, flags);
		case VOID:
        	return out_substr(out, buffer, idx, "void", prec, width, flags);
		case ANYERR:
        	return out_substr(out, buffer, idx, "<anyerr>", prec, width, flags);
		case VARIANT:
        	return out_substr(out, buffer, idx, "<variant>", prec, width, flags);
		case ENUM:
        	return out_substr(out, buffer, idx, "<enum>", prec, width, flags);
		case FAULT:
        	return out_substr(out, buffer, idx, "<fault>", prec, width, flags);
		case STRUCT:
        	return out_substr(out, buffer, idx, "<struct>", prec, width, flags);
		case UNION:
        	return out_substr(out, buffer, idx, "<union>", prec, width, flags);
		case BITSTRUCT:
        	return out_substr(out, buffer, idx, "<bitstruct>", prec, width, flags);
		case FUNC:
        	return out_substr(out, buffer, idx, "<func>", prec, width, flags);
		case FAILABLE:
        	return out_substr(out, buffer, idx, "<failable>", prec, width, flags);
		case DISTINCT:
        	return out_substr(out, buffer, idx, "<distinct>", prec, width, flags);
		case POINTER:
			typeid inner = arg.type.inner;
			if (inner.kind == TypeKind.ARRAY && inner.inner == char.typeid)
			{
				char *ptr = *(char**)arg.ptr;
				return out_substr(out, buffer, idx, ptr[0..inner.len - 1], prec, width, flags);
			}
			return ntoa_variant(out, buffer, idx, arg, 16, prec, width, flags);
		case SIGNED_INT:
		case UNSIGNED_INT:
			return ntoa_variant(out, buffer, idx, arg, 10, prec, width, flags);
		case FLOAT:
			return ftoa(out, buffer, idx, float_from_variant(arg), prec, width, flags);
		case ARRAY:
			return out_substr(out, buffer, idx, "[]", prec, width, flags);
		case VECTOR:
			return out_substr(out, buffer, idx, "[]", prec, width, flags);
		case SUBARRAY:
			return out_substr(out, buffer, idx, *(char[]*)arg.ptr, prec, width, flags);
		case BOOL:
			if (*(bool*)arg.ptr)
			{
				return out_substr(out, buffer, idx, "true", prec, width, flags);
			}
			else
			{
				return out_substr(out, buffer, idx, "false", prec, width, flags);
			}
		default:
			return out_substr(out, buffer, idx, "Invalid type", prec, width, flags);
	}
}


private fn uint simple_atoi(char* buf, usize maxlen, usize* len_ptr) @inline
{
	uint i = 0;
	usize len = *len_ptr;
	while (len < maxlen)
    {
        char c = buf[len];
        if (c < '0' || c > '9') break;
        i = i * 10 + c - '0';
        len++;
    }
    *len_ptr = len;
    return i;
}

fault FormattingFault
{
	UNTERMINATED_FORMAT,
	MISSING_ARG,
	INVALID_WIDTH_ARG,
	INVALID_FORMAT_TYPE,
}

private fn void! printf_advance_format(usize format_len, usize *index_ptr) @inline
{
	usize val = ++(*index_ptr);
	if (val >= format_len) return FormattingFault.UNTERMINATED_FORMAT!;
}

private fn variant! next_variant(variant* args_ptr, usize args_len, usize* arg_index_ptr) @inline
{
	if (*arg_index_ptr >= args_len) return FormattingFault.MISSING_ARG!;
	return args_ptr[(*arg_index_ptr)++];
}

private fn int! printf_parse_format_field(variant* args_ptr, usize args_len, usize* args_index_ptr, char* format_ptr, usize format_len, usize* index_ptr) @inline
{
	char c = format_ptr[*index_ptr];
	if (c >= '0' && c <= '9') return simple_atoi(format_ptr, format_len, index_ptr);
	if (c != '*') return 0;
	printf_advance_format(format_len, index_ptr)?;
	variant val = next_variant(args_ptr, args_len, args_index_ptr)?;
    if (!types::kind_is_int(val.type.kind)) return FormattingFault.INVALID_WIDTH_ARG!;
    uint! intval = types::variant_to_int(val, int);
	if (catch intval) return FormattingFault.INVALID_WIDTH_ARG!;
	return intval;
}


private fn void! out_buffer_fn(char c, char[] buffer, usize buffer_idx)
{
	if (buffer_idx >= buffer.len) return PrintFault.BUFFER_EXCEEDED!;
	buffer[buffer_idx] = c;
}

private fn void! out_null_fn(char c @unused, char[] buffer @unused, usize idx @unused)
{
}

private fn void! out_putchar_fn(char c, char[] buffer @unused, usize idx @unused)
{
	libc::putchar(c);
}

private fn usize! out_reverse(OutputFn out, char[] out_buffer, usize buffer_idx, char[] buf, uint width, PrintFlags flags)
{
	usize buffer_start_idx = buffer_idx;
	usize len = buf.len;
    // pad spaces up to given width
    if (!flags.left && !flags.zeropad)
    {
        for (usize i = len; i < width; i++)
        {
            out(' ', out_buffer, buffer_idx++)?;
        }
    }
    // reverse string
    while (len) out(buf[--len], out_buffer, buffer_idx++)?;

	// append pad spaces up to given width
    if (flags.left)
    {
        while (buffer_idx - buffer_start_idx < width)
        {
            out(' ', out_buffer, buffer_idx++)?;
        }
    }
	return buffer_idx;
}

private fn usize! out_char(OutputFn out, char[] buffer, usize idx, variant arg, uint width, PrintFlags flags)
{
	uint l = 1;
	// pre padding
	if (!flags.left)
	{
		while (l++ < width)
		{
			out(' ', buffer, idx++);
		}
	}
    // char output
    Char32 c = types::variant_to_int(arg, uint) ?? 0xFFFD;
	switch (true)
	{
		case c < 0x7f:
			out((char)c, buffer, idx++);
		case c < 0x7ff:
			out((char)(0xC0 | c >> 6), buffer, idx++);
			out((char)(0x80 | (c & 0x3F)), buffer, idx++);
        case c < 0xffff:
			out((char)(0xE0 | c >> 12), buffer, idx++);
			out((char)(0x80 | (c >> 6 & 0x3F)), buffer, idx++);
			out((char)(0x80 | (c & 0x3F)), buffer, idx++);
        default:
			out((char)(0xF0 | c >> 18), buffer, idx++);
			out((char)(0x80 | (c >> 12 & 0x3F)), buffer, idx++);
			out((char)(0x80 | (c >> 6 & 0x3F)), buffer, idx++);
			out((char)(0x80 | (c & 0x3F)), buffer, idx++);
	}
	if (flags.left)
	{
		while (l++ < width)
		{
			out(' ', buffer, idx++);
		}
	}
	return idx;
}

private fn usize! ntoa_format(OutputFn out, char[] out_buffer, usize buffer_idx, char[] buf, usize len, bool negative, uint base, uint prec, uint width, PrintFlags flags)
{
	// pad leading zeros
	if (!flags.left)
	{
		if (width && flags.zeropad && (negative || flags.plus || flags.space)) width--;
		while (len < prec)
		{
			if (len >= buf.len) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			buf[len++] = '0';
		}
		while (flags.zeropad && len < width)
		{
			if (len >= buf.len) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			buf[len++] = '0';
		}
	}

	// handle hash
	if (flags.hash && base != 10)
	{
		if (!flags.precision && len && len == prec && len == width)
		{
			len--;
			if (len) len--;
		}
		if (base != 10)
		{
			if (len + 1 >= buf.len) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			switch (base)
			{
				case 16:
					buf[len++] = flags.uppercase ? 'X' : 'x';
				case 8:
					buf[len++] = flags.uppercase ? 'O' : 'o';
				case 2:
					buf[len++] = flags.uppercase ? 'B' : 'b';
				default:
					unreachable();
			}
			buf[len++] = '0';
		}
	}

	switch (true)
	{
		case negative:
			if (len >= buf.len) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			buf[len++] = '-';
		case flags.plus:
			if (len >= buf.len) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			buf[len++] = '+';
		case flags.space:
			if (len >= buf.len) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			buf[len++] = ' ';
	}
	if (!len) return buffer_idx;
	return out_reverse(out, out_buffer, buffer_idx, buf[0..len - 1], width, flags);
}

$if (env::I128_SUPPORT):
define NtoaType = uint128;
$else:
define NtoaType = ulong;
$endif;

private fn usize! ntoa_variant(OutputFn out, char[] out_buffer, usize buffer_idx, variant arg, uint base, uint prec, uint width, PrintFlags flags)
{
	bool is_neg;
	NtoaType val = int_from_variant(arg, &is_neg);
	return ntoa(out, out_buffer, buffer_idx, val, is_neg, base, prec, width, flags) @inline;
}

private fn usize! ntoa(OutputFn out, char[] out_buffer, usize buffer_idx, NtoaType value, bool negative, uint base, uint prec, uint width, PrintFlags flags)
{
	char[PRINTF_NTOA_BUFFER_SIZE] buf = void;
	usize len = 0;

	// no hash for 0 values
	if (!value) flags.hash = false;

	// write if precision != 0 or value is != 0
	if (!flags.precision || value)
	{
		char past_10 = (flags.uppercase ? 'A' : 'a') - 10;
		do
		{
			if (len >= PRINTF_NTOA_BUFFER_SIZE) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			char digit = (char)(value % base);
			buf[len++] = digit + (digit < 10 ? '0' : past_10);
			value /= base;
		}
		while (value);
	}
	return ntoa_format(out, out_buffer, buffer_idx, buf[..PRINTF_NTOA_BUFFER_SIZE - 1], len, negative, base, prec, width, flags);
}


define FloatType = double;

// internal ftoa for fixed decimal floating point
private fn usize! ftoa(OutputFn out, char[] buffer, usize idx, FloatType value, uint prec, uint width, PrintFlags flags)
{
	char[PRINTF_FTOA_BUFFER_SIZE] buf = void;
	usize len = 0;
	const FloatType[] POW10 = { 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000 };
	FloatType diff = 0.0;

	// powers of 10

	// test for special values
	if (value != value)
	{
		return out_reverse(out, buffer, idx, "nan", width, flags);
	}
	if (value < -FloatType.max)
	{
		return out_reverse(out, buffer, idx, "fni-", width, flags);
	}
	if (value > FloatType.max)
	{
		if (flags.plus)
		{
			return out_reverse(out, buffer, idx, "fni+", width, flags);
		}
		return out_reverse(out, buffer, idx, "fni", width, flags);
	}

	// test for very large values
	// standard printf behavior is to print EVERY whole number digit -- which could be 100s of characters overflowing your buffers == bad
	if (value > PRINTF_MAX_FLOAT || value < -PRINTF_MAX_FLOAT)
	{
		return etoa(out, buffer, idx, value, prec, width, flags);
	}

	// test for negative
	bool negative = value < 0;
	if (negative) value = 0 - value;

	// set default precision, if not set explicitly
	if (!flags.precision) prec = PRINTF_DEFAULT_FLOAT_PRECISION;

	// limit precision to 9, cause a prec >= 10 can lead to overflow errors
	while (prec > 9)
	{
		if (len >= PRINTF_FTOA_BUFFER_SIZE) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
		buf[len++] = '0';
		prec--;
	}

	// Safe due to 1e9 limit.
	int whole = (int)value;
	FloatType tmp = (value - whole) * POW10[prec];
	ulong frac = (ulong)tmp;
	diff = tmp - frac;

	switch (true)
	{
		case diff > 0.5:
			++frac;
			// handle rollover, e.g. case 0.99 with prec 1 is 1.0
			if (frac >= POW10[prec])
			{
				frac = 0;
				++whole;
			}
		case diff < 0.5:
			break;
		case !frac && (frac & 1):
			// if halfway, round up if odd OR if last digit is 0
			++frac;
	}
	if (!prec)
	{
		diff = value - (FloatType)whole;
		if ((!(diff < 0.5) || diff > 0.5) && (whole & 1))
		{
			// exactly 0.5 and ODD, then round up
			// 1.5 -> 2, but 2.5 -> 2
			++whole;
		}
	}
	else
	{
		uint count = prec;
		// now do fractional part, as an unsigned number
		do
		{
			if (len >= PRINTF_FTOA_BUFFER_SIZE) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			--count;
			buf[len++] = (char)(48 + (frac % 10));
		}
		while (frac /= 10);
		// add extra 0s
		while (count-- > 0)
		{
			if (len >= PRINTF_FTOA_BUFFER_SIZE) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			buf[len++] = '0';
		}
		if (len >= PRINTF_FTOA_BUFFER_SIZE) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
		// add decimal
		buf[len++] = '.';
	}

	// do whole part, number is reversed
	do
	{
		if (len >= PRINTF_FTOA_BUFFER_SIZE) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
		buf[len++] = (char)(48 + (whole % 10));
	}
	while (whole /= 10);

	// pad leading zeros
	if (!flags.left && flags.zeropad)
	{
		if (width && (negative || flags.plus || flags.space)) width--;
		while (len < width)
		{
			if (len >= PRINTF_FTOA_BUFFER_SIZE) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			buf[len++] = '0';
		}
	}

	char next = {|
		if (negative) return '-';
		if (flags.plus) return '+';
		if (flags.space) return ' ';
		return 0;
	|};
	if (next)
	{
		if (len >= PRINTF_FTOA_BUFFER_SIZE) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
		buf[len++] = next;
	}
	return out_reverse(out, buffer, idx, buf[..len-1], width, flags);
}

union ConvUnion
{
	ulong u;
	double f;
}

private fn usize! etoa(OutputFn out, char[] buffer, usize idx, FloatType value, uint prec, uint width, PrintFlags flags)
{
	// check for NaN and special values
	if (value != value || value < FloatType.min || value > FloatType.max)
	{
		return ftoa(out, buffer, idx, value, prec, width, flags);
	}

	// determine the sign
	bool negative = value < 0;
	if (negative) value = -value;

	// default precision
	if (!flags.precision)
	{
		prec = PRINTF_DEFAULT_FLOAT_PRECISION;
	}

	// determine the decimal exponent
	// based on the algorithm by David Gay (https://www.ampl.com/netlib/fp/dtoa.c)
	ConvUnion conv;

	conv.f = (double)value;
	int exp2 = (int)(conv.u >> 52 & 0x7FF) - 1023;           // effectively log2
	conv.u = (conv.u & (1u64 << 52 - 1)) | (1023u64 << 52);  // drop the exponent so conv.F is now in [1,2)
	// now approximate log10 from the log2 integer part and an expansion of ln around 1.5
	int expval = (int)(0.1760912590558 + exp2 * 0.301029995663981 + (conv.f - 1.5) * 0.289529654602168);
	// now we want to compute 10^expval but we want to be sure it won't overflow
	exp2 = (int)(expval * 3.321928094887362 + 0.5);
	double z  = expval * 2.302585092994046 - exp2 * 0.6931471805599453;
	double z2 = z * z;
	conv.u = (ulong)(exp2 + 1023) << 52;
	// compute exp(z) using continued fractions, see https://en.wikipedia.org/wiki/Exponential_function#Continued_fractions_for_ex
	conv.f *= 1 + 2 * z / (2 - z + (z2 / (6 + (z2 / (10 + z2 / 14)))));
	// correct for rounding errors
	if (value < conv.f)
	{
		expval--;
		conv.f /= 10;
	}

	// the exponent format is "%+03d" and largest value is "307", so set aside 4-5 characters
	uint minwidth = ((expval < 100) && (expval > -100)) ? 4 : 5;

	// in "%g" mode, "prec" is the number of *significant figures* not decimals
	if (flags.adapt_exp)
	{
		// do we want to fall-back to "%f" mode?
		if (value >= 1e-4 && value < 1e6)
		{
			prec = prec > expval ? prec - expval - 1 : 0;
			flags.precision = true;   // make sure ftoa respects precision
			// no characters in exponent
			minwidth = 0;
			expval = 0;
		}
		else
		{
			// we use one sigfig for the whole part
			if (prec > 0 && flags.precision) prec--;
		}
	}

	// Adjust width
	uint fwidth = width > minwidth ? width - minwidth : 0;

	// if we're padding on the right, DON'T pad the floating part
	if (flags.left && minwidth) fwidth = 0;

	// rescale the float value
	if (expval) value /= conv.f;

	// output the floating part
	usize start_idx = idx;
	PrintFlags ftoa_flags = flags;
	flags.adapt_exp = false;
	idx = ftoa(out, buffer, idx, negative ? -value : value, prec, fwidth, ftoa_flags)?;

	// output the exponent part
	if (minwidth)
	{
		// output the exponential symbol
		out(flags.uppercase ? 'E' : 'e', buffer, idx++);
		// output the exponent value
		idx = ntoa(out, buffer, idx, (NtoaType)(expval < 0 ? -expval : expval), expval < 0, 10, 0, minwidth - 1, { .zeropad = true, .plus = true } )?;
		// might need to right-pad spaces
		if (flags.left)
		{
			while (idx - start_idx < width) out(' ', buffer, idx++);
		}
	}
	return idx;
}

private fn FloatType float_from_variant(variant arg)
{
	$if (env::I128_SUPPORT):
		switch (arg)
		{
			case int128:
				return *arg;
			case uint128:
				return *arg;
		}
	$endif;

	if (arg.type.kind == TypeKind.POINTER)
	{
		return (FloatType)(uptr)(void*)arg.ptr;
	}
	switch (arg)
	{
		case bool:
			return (FloatType)*arg;
		case ichar:
			return *arg;
		case short:
			return *arg;
		case int:
			return *arg;
		case long:
			return *arg;
		case char:
			return *arg;
		case ushort:
			return *arg;
		case uint:
			return *arg;
		case ulong:
			return *arg;
		case float:
			return (FloatType)*arg;
		case double:
			return (FloatType)*arg;
		default:
			return 0;
	}
}

private fn NtoaType int_from_variant(variant arg, bool *is_neg)
{
	*is_neg = false;
	$if (NtoaType.typeid == uint128.typeid):
		switch (arg)
		{
			case int128:
				int128 val = *arg;
				return (*is_neg = val < 0) ? -val : val;
			case uint128:
				return *arg;
		}
	$endif;

	if (arg.type.kind == TypeKind.POINTER)
	{
		return (NtoaType)(uptr)(void*)arg.ptr;
	}
	switch (arg)
	{
		case bool:
			return (NtoaType)*arg;
		case ichar:
			int val = *arg;
			return (NtoaType)((*is_neg = val < 0) ? -val : val);
		case short:
			int val = *arg;
			return (NtoaType)((*is_neg = val < 0) ? -val : val);
		case int:
			int val = *arg;
			return (NtoaType)((*is_neg = val < 0) ? -val : val);
		case long:
			long val = *arg;
			return (NtoaType)((*is_neg = val < 0) ? -val : val);
		case char:
			return *arg;
		case ushort:
			return *arg;
		case uint:
			return *arg;
		case ulong:
			return *arg;
		case float:
			float f = *arg;
			return (NtoaType)((*is_neg = f < 0) ? -f : f);
		case double:
			double d = *arg;
			return (NtoaType)((*is_neg = d < 0) ? -d : d);
		default:
			return 0;
	}
}


fn usize! printf(char[] format, args...)
{
  return vsnprintf(&out_putchar_fn, " ", format, args);
}

private fn usize! out_substr(OutputFn out, char[] buffer, usize idx, char[] str, uint prec, uint width, PrintFlags flags)
{
	usize l = conv::utf8_codepoints(str);
	if (flags.precision && l < prec) l = prec;
	if (!flags.left)
	{
		while (l++ < width)  out(' ', buffer, idx++)?;
	}
	usize index = 0;
	usize chars = str.len;
	char* ptr = str.ptr;
	while (index < chars)
	{
		char c = ptr[index];
		// Break if we have precision set and we ran out...
		if (c & 0xC0 != 0x80 && flags.precision && !prec--) break;
		out(c, buffer, idx++);
        index++;
	}
	if (flags.left)
	{
		while (l++ < width) out(' ', buffer, idx++)?;
    }
    return idx;
}

private fn usize! vsnprintf(OutputFn out, char[] buffer, char[] format, variant[] variants)
{
	usize idx;

	if (!buffer)
	{
		// use null output function
		out = &out_null_fn;
	}
	usize format_len = format.len;
	usize variant_index = 0;
	for (usize i = 0; i < format_len; i++)
	{
		// format specifier?  %[flags][width][.precision][length]
		char c = format[i];
		if (c != '%')
		{
			// no
			out(c, buffer, idx++)?;
			continue;
		}
		i++;
		if (i >= format_len) return PrintFault.INVALID_FORMAT_STRING!;
		c = format[i];
		if (c == '%')
		{
			out(c, buffer, idx++)?;
			continue;
		}
		// evaluate flags
		PrintFlags flags;
		while FLAG_EVAL: (true)
		{
			switch (c)
			{
				case '0': flags.zeropad = true;
				case '-': flags.left = true;
				case '+': flags.plus = true;
				case ' ': flags.space = true;
				case '#': flags.hash = true;
				default: break FLAG_EVAL;
			}
			if (++i >= format_len) return PrintFault.INVALID_FORMAT_STRING!;
			c = format[i];
		}

		// evaluate width field
		int width = printf_parse_format_field(variants.ptr, variants.len, &variant_index, format.ptr, format.len, &i)?;
		c = format[i];
		if (width < 0)
		{
			flags.left = true;
			width = -width;
		}

		// evaluate precision field
		uint precision = 0;
		if (c == '.')
		{
			flags.precision = true;
			if (++i >= format_len) return PrintFault.INVALID_FORMAT_STRING!;
			int prec = printf_parse_format_field(variants.ptr, variants.len, &variant_index, format.ptr, format.len, &i)?;
			precision = prec < 0 ? 0 : prec;
			c = format[i];
		}

		// evaluate specifier
		uint base = 0;
		if (variant_index >= variants.len) return PrintFault.MISSING_ARG!;
		variant current = variants[variant_index++];
		switch (c)
		{
			case 'd':
				base = 10;
				flags.hash = false;
			case 'X' :
				flags.uppercase = true;
				nextcase;
			case 'x' :
				base = 16;
			case 'O':
				flags.uppercase = true;
				nextcase;
			case 'o' :
				base = 8;
			case 'B':
				flags.uppercase = true;
				nextcase;
			case 'b' :
				base = 2;
		    case 'F' :
		        flags.uppercase = true;
		        nextcase;
		    case 'f':
		        idx = ftoa(out, buffer, idx, float_from_variant(current), precision, width, flags)?;
				continue;
			case 'E':
		        flags.uppercase = true;
		        nextcase;
            case 'e':
				idx = etoa(out, buffer, idx, float_from_variant(current), precision, width, flags)?;
				continue;
			case 'G':
				flags.uppercase = true;
				nextcase;
			case 'g':
				flags.adapt_exp = true;
				idx = etoa(out, buffer, idx, float_from_variant(current), precision, width, flags)?;
				continue;
			case 'c':
				idx = out_char(out, buffer, idx, current, width, flags)?;
				continue;
			case 's':
				idx = out_str(out, buffer, idx, current, precision, width, flags)?;
				continue;
			case 'p':
				width = (uint)(void*.sizeof * 2);
				flags.zeropad = true;
				flags.hash = true;
				base = 16;
			default:
				return PrintFault.INVALID_FORMAT_STRING!;
		}
		if (base != 10)
		{
			flags.plus = false;
			flags.space = false;
		}
		// ignore '0' flag when precision is given
		if (flags.precision) flags.zeropad = false;

		bool is_neg;
		NtoaType v = int_from_variant(current, &is_neg);

		ntoa(out, buffer, idx, v, is_neg, base, precision, width, flags)?;
	}
/*

			case 's' : {
				const char* p = va_arg(va, char*);
				unsigned int l = _strnlen_s(p, precision ? precision : (size_t)-1);
				// pre padding
				if (flags & FLAGS_PRECISION) {
					l = (l < precision ? l : precision);
				}
				if (!(flags & FLAGS_LEFT)) {
					while (l++ < width) {
						out(' ', buffer, idx++, maxlen);
					}
				}
				// string output
				while ((*p != 0) && (!(flags & FLAGS_PRECISION) || precision--)) {
					out(*(p++), buffer, idx++, maxlen);
				}
				// post padding
				if (flags & FLAGS_LEFT) {
					while (l++ < width) {
						out(' ', buffer, idx++, maxlen);
					}
				}
				format++;
				break;
			}




	}
*/
	// termination
//	out((char)0, buffer, idx < maxlen ? idx : maxlen - 1U, maxlen);

	// return written chars without terminating \0
	return (int)idx;
}

