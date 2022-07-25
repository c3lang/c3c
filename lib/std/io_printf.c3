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

struct PrintParam
{
	OutputFn outfn;
	char[] buffer;
	PrintFlags flags;
	uint width;
	uint prec;
	usize idx;
}

define OutputFn = fn void!(char c, char[] buffer, usize buffer_idx);

fn void! PrintParam.out(PrintParam* param, char c)
{

	param.outfn(c, param.buffer, param.idx++)?;
}

private fn void! out_str(PrintParam* param, variant arg)
{
	switch (arg.type.kind)
	{
		case TYPEID:
			return out_substr(param, "<typeid>");
		case VOID:
        	return out_substr(param, "void");
		case ANYERR:
        	return out_substr(param, "<anyerr>");
		case VARIANT:
        	return out_substr(param, "<variant>");
		case ENUM:
        	return out_substr(param, arg.type.names[types::variant_to_int(arg, usize)!!]);
		case FAULT:
        	return out_substr(param, "<fault>");
		case STRUCT:
        	return out_substr(param, "<struct>");
		case UNION:
        	return out_substr(param, "<union>");
		case BITSTRUCT:
        	return out_substr(param, "<bitstruct>");
		case FUNC:
        	return out_substr(param, "<func>");
		case FAILABLE:
			unreachable();
		case DISTINCT:
        	return out_str(param, variant { arg.ptr, arg.type.inner });
		case POINTER:
			typeid inner = arg.type.inner;
			if (inner.kind == TypeKind.ARRAY && inner.inner == char.typeid)
			{
				char *ptr = *(char**)arg.ptr;
				return out_substr(param, ptr[:inner.len]);
			}
			return ntoa_variant(param, arg, 16);
		case SIGNED_INT:
		case UNSIGNED_INT:
			return ntoa_variant(param, arg, 10);
		case FLOAT:
			return ftoa(param, float_from_variant(arg));
		case ARRAY:
			// this is SomeType[*] so grab the "SomeType"
			typeid inner = arg.type.inner;
			usize size = inner.sizeof;
			usize len = arg.type.len;
			// Pretend this is a char[]
			void* ptr = (void*)arg.ptr;
			param.out('[')?;
			for (usize i = 0; i < len; i++)
			{
				if (i != 0) out_substr(param, ", ")?;
				out_str(param, variant { ptr, inner })?;
				ptr += size;
			}
			return param.out(']');
		case VECTOR:
			// this is SomeType[*] so grab the "SomeType"
			typeid inner = arg.type.inner;
			usize size = inner.sizeof;
			usize len = arg.type.len;
			// Pretend this is a char[]
			void* ptr = (void*)arg.ptr;
			out_substr(param, "[<")?;
			for (usize i = 0; i < len; i++)
			{
				if (i != 0) out_substr(param, ", ")?;
				out_str(param, variant { ptr, inner })?;
				ptr += size;
			}
			return out_substr(param, ">]");
		case SUBARRAY:
			// this is SomeType[] so grab the "SomeType"
			typeid inner = arg.type.inner;
			if (inner == char.typeid)
			{
				return out_substr(param, *(char[]*)arg);
			}
			usize size = inner.sizeof;
            // Pretend this is a char[]
			char[]* temp = (void*)arg.ptr;
			void* ptr = (void*)temp.ptr;
			usize len = temp.len;
			param.out('[')?;
			for (usize i = 0; i < len; i++)
			{
				if (i != 0) out_substr(param, ", ")?;
				out_str(param, variant { ptr, inner })?;
				ptr += size;
			}
			param.out(']')?;
		case BOOL:
			if (*(bool*)arg.ptr)
			{
				return out_substr(param, "true");
			}
			else
			{
				return out_substr(param, "false");
			}
		default:
			return out_substr(param, "Invalid type");
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

private fn void! PrintParam.out_reverse(PrintParam* param, char[] buf)
{
	usize buffer_start_idx = param.idx;
	usize len = buf.len;
    // pad spaces up to given width
    if (!param.flags.left && !param.flags.zeropad)
    {
        for (usize i = len; i < param.width; i++)
        {
            param.out(' ')?;
        }
    }
    // reverse string
    while (len) param.out(buf[--len])?;

	// append pad spaces up to given width
	return param.left_adjust(param.idx - buffer_start_idx);
}

private fn void! out_char(PrintParam* param, variant arg)
{
	uint l = 1;
	// pre padding
	param.right_adjust(l)?;
    // char output
    Char32 c = types::variant_to_int(arg, uint) ?? 0xFFFD;
	switch (true)
	{
		case c < 0x7f:
			param.out((char)c)?;
		case c < 0x7ff:
			param.out((char)(0xC0 | c >> 6))?;
			param.out((char)(0x80 | (c & 0x3F)))?;
        case c < 0xffff:
			param.out((char)(0xE0 | c >> 12))?;
			param.out((char)(0x80 | (c >> 6 & 0x3F)))?;
			param.out((char)(0x80 | (c & 0x3F)))?;
        default:
			param.out((char)(0xF0 | c >> 18))?;
			param.out((char)(0x80 | (c >> 12 & 0x3F)))?;
			param.out((char)(0x80 | (c >> 6 & 0x3F)))?;
			param.out((char)(0x80 | (c & 0x3F)))?;
	}
	return param.left_adjust(l);
}

private fn void! ntoa_format(PrintParam* param, char[] buf, usize len, bool negative, uint base)
{
	// pad leading zeros
	if (!param.flags.left)
	{
		if (param.width && param.flags.zeropad && (negative || param.flags.plus || param.flags.space)) param.width--;
		while (len < param.prec)
		{
			if (len >= buf.len) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			buf[len++] = '0';
		}
		while (param.flags.zeropad && len < param.width)
		{
			if (len >= buf.len) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			buf[len++] = '0';
		}
	}

	// handle hash
	if (param.flags.hash && base != 10)
	{
		if (!param.flags.precision && len && len == param.prec && len == param.width)
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
					buf[len++] = param.flags.uppercase ? 'X' : 'x';
				case 8:
					buf[len++] = param.flags.uppercase ? 'O' : 'o';
				case 2:
					buf[len++] = param.flags.uppercase ? 'B' : 'b';
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
		case param.flags.plus:
			if (len >= buf.len) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			buf[len++] = '+';
		case param.flags.space:
			if (len >= buf.len) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			buf[len++] = ' ';
	}
	if (!len) return;
	return param.out_reverse(buf[:len]);
}

$if (env::I128_SUPPORT):
define NtoaType = uint128;
$else:
define NtoaType = ulong;
$endif;

private fn void! ntoa_variant(PrintParam* param, variant arg, uint base)
{
	bool is_neg;
	NtoaType val = int_from_variant(arg, &is_neg);
	return ntoa(param, val, is_neg, base) @inline;
}

private fn void! ntoa(PrintParam* param, NtoaType value, bool negative, uint base)
{
	char[PRINTF_NTOA_BUFFER_SIZE] buf = void;
	usize len = 0;

	// no hash for 0 values
	if (!value) param.flags.hash = false;

	// write if precision != 0 or value is != 0
	if (!param.flags.precision || value)
	{
		char past_10 = (param.flags.uppercase ? 'A' : 'a') - 10;
		do
		{
			if (len >= PRINTF_NTOA_BUFFER_SIZE) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			char digit = (char)(value % base);
			buf[len++] = digit + (digit < 10 ? '0' : past_10);
			value /= base;
		}
		while (value);
	}
	return ntoa_format(param, buf[:PRINTF_NTOA_BUFFER_SIZE], len, negative, base);
}


define FloatType = double;

// internal ftoa for fixed decimal floating point
private fn void! ftoa(PrintParam* param, FloatType value)
{
	char[PRINTF_FTOA_BUFFER_SIZE] buf = void;
	usize len = 0;
	const FloatType[] POW10 = { 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000 };
	FloatType diff = 0.0;

	// powers of 10

	// test for special values
	if (value != value)
	{
		return param.out_reverse("nan");
	}
	if (value < -FloatType.max)
	{
		return param.out_reverse("fni-");
	}
	if (value > FloatType.max)
	{
		if (param.flags.plus)
		{
			return param.out_reverse("fni+");
		}
		return param.out_reverse("fni");
	}

	// test for very large values
	// standard printf behavior is to print EVERY whole number digit -- which could be 100s of characters overflowing your buffers == bad
	if (value > PRINTF_MAX_FLOAT || value < -PRINTF_MAX_FLOAT)
	{
		return etoa(param, value);
	}

	// test for negative
	bool negative = value < 0;
	if (negative) value = 0 - value;

	// set default precision, if not set explicitly
	if (!param.flags.precision) param.prec = PRINTF_DEFAULT_FLOAT_PRECISION;

	// limit precision to 9, cause a prec >= 10 can lead to overflow errors
	while (param.prec > 9)
	{
		if (len >= PRINTF_FTOA_BUFFER_SIZE) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
		buf[len++] = '0';
		param.prec--;
	}

	// Safe due to 1e9 limit.
	int whole = (int)value;
	FloatType tmp = (value - whole) * POW10[param.prec];
	ulong frac = (ulong)tmp;
	diff = tmp - frac;

	switch (true)
	{
		case diff > 0.5:
			++frac;
			// handle rollover, e.g. case 0.99 with prec 1 is 1.0
			if (frac >= POW10[param.prec])
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
	if (!param.prec)
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
		uint count = param.prec;
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
	if (!param.flags.left && param.flags.zeropad)
	{
		if (param.width && (negative || param.flags.plus || param.flags.space)) param.width--;
		while (len < param.width)
		{
			if (len >= PRINTF_FTOA_BUFFER_SIZE) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			buf[len++] = '0';
		}
	}

	char next = {|
		if (negative) return '-';
		if (param.flags.plus) return '+';
		if (param.flags.space) return ' ';
		return 0;
	|};
	if (next)
	{
		if (len >= PRINTF_FTOA_BUFFER_SIZE) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
		buf[len++] = next;
	}
	return param.out_reverse(buf[:len]);
}

union ConvUnion
{
	ulong u;
	double f;
}

private fn void! etoa(PrintParam* param, FloatType value)
{
	// check for NaN and special values
	if (value != value || value < FloatType.min || value > FloatType.max)
	{
		return ftoa(param, value);
	}

	// determine the sign
	bool negative = value < 0;
	if (negative) value = -value;

	// default precision
	if (!param.flags.precision)
	{
		param.prec = PRINTF_DEFAULT_FLOAT_PRECISION;
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
	if (param.flags.adapt_exp)
	{
		// do we want to fall-back to "%f" mode?
		if (value >= 1e-4 && value < 1e6)
		{
			param.prec = param.prec > expval ? param.prec - expval - 1 : 0;
			param.flags.precision = true;   // make sure ftoa respects precision
			// no characters in exponent
			minwidth = 0;
			expval = 0;
		}
		else
		{
			// we use one sigfig for the whole part
			if (param.prec > 0 && param.flags.precision) param.prec--;
		}
	}

	// Adjust width
	uint fwidth = param.width > minwidth ? param.width - minwidth : 0;

	// if we're padding on the right, DON'T pad the floating part
	if (param.flags.left && minwidth) fwidth = 0;

	// rescale the float value
	if (expval) value /= conv.f;

	// output the floating part
	usize start_idx = param.idx;
	PrintFlags old = param.flags;
	param.flags.adapt_exp = false;
	param.width = fwidth;
	ftoa(param, negative ? -value : value)?;
	param.flags = old;

	// output the exponent part
	if (minwidth)
	{
		// output the exponential symbol
		param.out(param.flags.uppercase ? 'E' : 'e')?;
		// output the exponent value
		param.flags = { .zeropad = true, .plus = true };
		param.width = minwidth - 1;
		param.prec = 0;
		ntoa(param, (NtoaType)(expval < 0 ? -expval : expval), expval < 0, 10)?;
		param.flags = old;
		// might need to right-pad spaces
		param.left_adjust(param.idx - start_idx)?;
	}
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


fn usize! printf(char[] format, args...) @maydiscard
{
  return vsnprintf(&out_putchar_fn, " ", format, args);
}

private fn void! PrintParam.left_adjust(PrintParam* param, usize len)
{
	if (!param.flags.left) return;
	for (usize l = len; l < param.width; l++) param.out(' ')?;
}

private fn void! PrintParam.right_adjust(PrintParam* param, usize len)
{
	if (param.flags.left) return;
	for (usize l = len; l < param.width; l++) param.out(' ')?;
}

private fn void! out_substr(PrintParam* param, char[] str)
{
	usize l = conv::utf8_codepoints(str);
	uint prec = param.prec;
	if (param.flags.precision && l < prec) l = prec;
	param.right_adjust(' ')?;
	usize index = 0;
	usize chars = str.len;
	char* ptr = str.ptr;
	while (index < chars)
	{
		char c = ptr[index];
		// Break if we have precision set and we ran out...
		if (c & 0xC0 != 0x80 && param.flags.precision && !prec--) break;
		param.out(c)?;
        index++;
	}
	return param.left_adjust(l);
}

private fn usize! vsnprintf(OutputFn out, char[] buffer, char[] format, variant[] variants)
{
	if (!buffer)
	{
		// use null output function
		out = &out_null_fn;
	}
	PrintParam param = { .outfn = out, .buffer = buffer };
	usize format_len = format.len;
	usize variant_index = 0;
	for (usize i = 0; i < format_len; i++)
	{
		// format specifier?  %[flags][width][.precision][length]
		char c = format[i];
		if (c != '%')
		{
			// no
			param.out(c)?;
			continue;
		}
		i++;
		if (i >= format_len) return PrintFault.INVALID_FORMAT_STRING!;
		c = format[i];
		if (c == '%')
		{
			param.out(c)?;
			continue;
		}
		// evaluate flags
		param.flags = {};
		while FLAG_EVAL: (true)
		{
			switch (c)
			{
				case '0': param.flags.zeropad = true;
				case '-': param.flags.left = true;
				case '+': param.flags.plus = true;
				case ' ': param.flags.space = true;
				case '#': param.flags.hash = true;
				default: break FLAG_EVAL;
			}
			if (++i >= format_len) return PrintFault.INVALID_FORMAT_STRING!;
			c = format[i];
		}
		// evaluate width field
		int w = printf_parse_format_field(variants.ptr, variants.len, &variant_index, format.ptr, format.len, &i)?;
		c = format[i];
		if (w < 0)
		{
			param.flags.left = true;
			w = -w;
		}
		param.width = w;
		// evaluate precision field
		param.prec = 0;
		if (c == '.')
		{
			param.flags.precision = true;
			if (++i >= format_len) return PrintFault.INVALID_FORMAT_STRING!;
			int prec = printf_parse_format_field(variants.ptr, variants.len, &variant_index, format.ptr, format.len, &i)?;
			param.prec = prec < 0 ? 0 : prec;
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
				param.flags.hash = false;
			case 'X' :
				param.flags.uppercase = true;
				nextcase;
			case 'x' :
				base = 16;
			case 'O':
				param.flags.uppercase = true;
				nextcase;
			case 'o' :
				base = 8;
			case 'B':
				param.flags.uppercase = true;
				nextcase;
			case 'b' :
				base = 2;
		    case 'F' :
		        param.flags.uppercase = true;
		        nextcase;
		    case 'f':
		        ftoa(&param, float_from_variant(current))?;
				continue;
			case 'E':
		        param.flags.uppercase = true;
		        nextcase;
            case 'e':
				etoa(&param, float_from_variant(current))?;
				continue;
			case 'G':
				param.flags.uppercase = true;
				nextcase;
			case 'g':
				param.flags.adapt_exp = true;
				etoa(&param, float_from_variant(current))?;
				continue;
			case 'c':
				out_char(&param, current)?;
				continue;
			case 's':
				out_str(&param, current)?;
				continue;
			case 'p':
				param.width = (uint)(void*.sizeof * 2);
				param.flags.zeropad = true;
				param.flags.hash = true;
				base = 16;
			default:
				return PrintFault.INVALID_FORMAT_STRING!;
		}
		if (base != 10)
		{
			param.flags.plus = false;
			param.flags.space = false;
		}
		// ignore '0' flag when precision is given
		if (param.flags.precision) param.flags.zeropad = false;

		bool is_neg;
		NtoaType v = int_from_variant(current, &is_neg);

		ntoa(&param, v, is_neg, base)?;
	}
	// termination
//	out((char)0, buffer, idx < maxlen ? idx : maxlen - 1U, maxlen);

	// return written chars without terminating \0
	return param.idx;
}

