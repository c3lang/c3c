module std::io;

private fn void! Formatter.left_adjust(Formatter* this, usz len)
{
	if (!this.flags.left) return;
	for (usz l = len; l < this.width; l++) this.out(' ')?;
}

private fn void! Formatter.right_adjust(Formatter* this, usz len)
{
	if (this.flags.left) return;
	for (usz l = len; l < this.width; l++) this.out(' ')?;
}


private fn NtoaType int_from_variant(variant arg, bool *is_neg)
{
	*is_neg = false;
	$if (NtoaType.typeid == uint128.typeid):
		switch (arg)
		{
			case int128:
				int128 val = *arg;
				return (*is_neg = val < 0) ? (~(NtoaType)val) + 1 : val;
			case uint128:
				return *arg;
		}
	$endif;

	if (arg.type.kindof == TypeKind.POINTER)
	{
		return (NtoaType)(uptr)*(void**)arg.ptr;
	}
	switch (arg)
	{
		case bool:
			return (NtoaType)*arg;
		case ichar:
			int val = *arg;
			return (*is_neg = val < 0) ? (~(NtoaType)val) + 1 : (NtoaType)val;
		case short:
			int val = *arg;
			return (*is_neg = val < 0) ? (~(NtoaType)val) + 1 : (NtoaType)val;
		case int:
			int val = *arg;
			return (*is_neg = val < 0) ? (~(NtoaType)val) + 1 : (NtoaType)val;
		case long:
			long val = *arg;
			return (*is_neg = val < 0) ? (~(NtoaType)val) + 1 : (NtoaType)val;
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
	$if (env::F128_SUPPORT):
		if (arg.type == float128.typeid) return *((float128*)arg.ptr);
	$endif;
	$if (env::F16_SUPPORT):
		if (arg.type == float16.typeid) return *((float16*)arg.ptr);
	$endif;

	if (arg.type.kindof == TypeKind.POINTER)
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


/**
 * Read a simple integer value, typically for formatting.
 *
 * @param [inout] len_ptr "the length remaining."
 * @param [in] buf "the buf to read from."
 * @param maxlen "the maximum len that can be read."
 * @return "The result of the atoi."
 **/
private fn uint simple_atoi(char* buf, usz maxlen, usz* len_ptr) @inline
{
	uint i = 0;
	usz len = *len_ptr;
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


private fn void! Formatter.out_substr(Formatter *this, char[] str)
{
	usz l = conv::utf8_codepoints(str);
	uint prec = this.prec;
	if (this.flags.precision && l < prec) l = prec;
	this.right_adjust(' ')?;
	usz index = 0;
	usz chars = str.len;
	char* ptr = str.ptr;
	while (index < chars)
	{
		char c = ptr[index];
		// Break if we have precision set and we ran out...
		if (c & 0xC0 != 0x80 && this.flags.precision && !prec--) break;
		this.out(c)?;
        index++;
	}
	return this.left_adjust(l);
}


union ConvUnion
{
	ulong u;
	double f;
}

private fn void! Formatter.etoa(Formatter* this, FloatType value)
{
	// check for NaN and special values
	if (value != value || value < FloatType.min || value > FloatType.max)
	{
		return this.ftoa(value);
	}

	// determine the sign
	bool negative = value < 0;
	if (negative) value = -value;

	// default precision
	if (!this.flags.precision)
	{
		this.prec = PRINTF_DEFAULT_FLOAT_PRECISION;
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
	if (this.flags.adapt_exp)
	{
		// do we want to fall-back to "%f" mode?
		if (value >= 1e-4 && value < 1e6)
		{
			this.prec = this.prec > expval ? this.prec - expval - 1 : 0;
			this.flags.precision = true;   // make sure ftoa respects precision
			// no characters in exponent
			minwidth = 0;
			expval = 0;
		}
		else
		{
			// we use one sigfig for the whole part
			if (this.prec > 0 && this.flags.precision) this.prec--;
		}
	}

	// Adjust width
	uint fwidth = this.width > minwidth ? this.width - minwidth : 0;

	// if we're padding on the right, DON'T pad the floating part
	if (this.flags.left && minwidth) fwidth = 0;

	// rescale the float value
	if (expval) value /= conv.f;

	// output the floating part
	usz start_idx = this.idx;
	PrintFlags old = this.flags;
	this.flags.adapt_exp = false;
	this.width = fwidth;
	this.ftoa(negative ? -value : value)?;
	this.flags = old;

	// output the exponent part
	if (minwidth)
	{
		// output the exponential symbol
		this.out(this.flags.uppercase ? 'E' : 'e')?;
		// output the exponent value
		this.flags = { .zeropad = true, .plus = true };
		this.width = minwidth - 1;
		this.prec = 0;
		this.ntoa((NtoaType)(expval < 0 ? -expval : expval), expval < 0, 10)?;
		this.flags = old;
		// might need to right-pad spaces
		this.left_adjust(this.idx - start_idx)?;
	}
}

// internal ftoa for fixed decimal floating point
private fn void! Formatter.ftoa(Formatter* this, FloatType value)
{
	char[PRINTF_FTOA_BUFFER_SIZE] buf = void;
	usz len = 0;
	const FloatType[] POW10 = { 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000 };
	FloatType diff = 0.0;

	// powers of 10

	// test for special values
	if (value != value)
	{
		return this.out_reverse("nan");
	}
	if (value < -FloatType.max)
	{
		return this.out_reverse("fni-");
	}
	if (value > FloatType.max)
	{
		if (this.flags.plus)
		{
			return this.out_reverse("fni+");
		}
		return this.out_reverse("fni");
	}

	// test for very large values
	// standard printf behavior is to print EVERY whole number digit -- which could be 100s of characters overflowing your buffers == bad
	if (value > PRINTF_MAX_FLOAT || value < -PRINTF_MAX_FLOAT)
	{
		return this.etoa(value);
	}

	// test for negative
	bool negative = value < 0;
	if (negative) value = 0 - value;

	// set default precision, if not set explicitly
	if (!this.flags.precision) this.prec = PRINTF_DEFAULT_FLOAT_PRECISION;

	// limit precision to 9, cause a prec >= 10 can lead to overflow errors
	while (this.prec > 9)
	{
		if (len >= PRINTF_FTOA_BUFFER_SIZE) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
		buf[len++] = '0';
		this.prec--;
	}

	// Safe due to 1e9 limit.
	int whole = (int)value;
	FloatType tmp = (value - whole) * POW10[this.prec];
	ulong frac = (ulong)tmp;
	diff = tmp - frac;

	switch (true)
	{
		case diff > 0.5:
			++frac;
			// handle rollover, e.g. case 0.99 with prec 1 is 1.0
			if (frac >= POW10[this.prec])
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
	if (!this.prec)
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
		uint count = this.prec;
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
	if (!this.flags.left && this.flags.zeropad)
	{
		if (this.width && (negative || this.flags.plus || this.flags.space)) this.width--;
		while (len < this.width)
		{
			if (len >= PRINTF_FTOA_BUFFER_SIZE) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			buf[len++] = '0';
		}
	}

	char next = {|
		if (negative) return '-';
		if (this.flags.plus) return '+';
		if (this.flags.space) return ' ';
		return 0;
	|};
	if (next)
	{
		if (len >= PRINTF_FTOA_BUFFER_SIZE) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
		buf[len++] = next;
	}
	return this.out_reverse(buf[:len]);
}

private fn void! Formatter.ntoa(Formatter* this, NtoaType value, bool negative, uint base)
{
	char[PRINTF_NTOA_BUFFER_SIZE] buf = void;
	usz len = 0;

	// no hash for 0 values
	if (!value) this.flags.hash = false;

	// write if precision != 0 or value is != 0
	if (!this.flags.precision || value)
	{
		char past_10 = (this.flags.uppercase ? 'A' : 'a') - 10;
		do
		{
			if (len >= PRINTF_NTOA_BUFFER_SIZE) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			char digit = (char)(value % base);
			buf[len++] = digit + (digit < 10 ? '0' : past_10);
			value /= base;
		}
		while (value);
	}
	return this.ntoa_format(buf[:PRINTF_NTOA_BUFFER_SIZE], len, negative, base);
}

private fn void! Formatter.ntoa_format(Formatter* this, char[] buf, usz len, bool negative, uint base)
{
	// pad leading zeros
	if (!this.flags.left)
	{
		if (this.width && this.flags.zeropad && (negative || this.flags.plus || this.flags.space)) this.width--;
		while (len < this.prec)
		{
			if (len >= buf.len) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			buf[len++] = '0';
		}
		while (this.flags.zeropad && len < this.width)
		{
			if (len >= buf.len) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			buf[len++] = '0';
		}
	}

	// handle hash
	if (this.flags.hash && base != 10)
	{
		if (!this.flags.precision && len && len == this.prec && len == this.width)
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
					buf[len++] = this.flags.uppercase ? 'X' : 'x';
				case 8:
					buf[len++] = this.flags.uppercase ? 'O' : 'o';
				case 2:
					buf[len++] = this.flags.uppercase ? 'B' : 'b';
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
		case this.flags.plus:
			if (len >= buf.len) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			buf[len++] = '+';
		case this.flags.space:
			if (len >= buf.len) return PrintFault.INTERNAL_BUFFER_EXCEEDED!;
			buf[len++] = ' ';
	}
	if (!len) return;
	return this.out_reverse(buf[:len]);
}

$if (env::I128_SUPPORT):
define NtoaType = uint128;
$else:
define NtoaType = ulong;
$endif;

private fn void! Formatter.ntoa_variant(Formatter* this, variant arg, uint base)
{
	bool is_neg;
	NtoaType val = int_from_variant(arg, &is_neg);
	return this.ntoa(val, is_neg, base) @inline;
}

private fn void! Formatter.out_char(Formatter* this, variant arg)
{
	uint l = 1;
	// pre padding
	this.right_adjust(l)?;
    // char output
    Char32 c = types::variant_to_int(arg, uint) ?? 0xFFFD;
	switch (true)
	{
		case c < 0x7f:
			this.out((char)c)?;
		case c < 0x7ff:
			this.out((char)(0xC0 | c >> 6))?;
			this.out((char)(0x80 | (c & 0x3F)))?;
        case c < 0xffff:
			this.out((char)(0xE0 | c >> 12))?;
			this.out((char)(0x80 | (c >> 6 & 0x3F)))?;
			this.out((char)(0x80 | (c & 0x3F)))?;
        default:
			this.out((char)(0xF0 | c >> 18))?;
			this.out((char)(0x80 | (c >> 12 & 0x3F)))?;
			this.out((char)(0x80 | (c >> 6 & 0x3F)))?;
			this.out((char)(0x80 | (c & 0x3F)))?;
	}
	return this.left_adjust(l);
}


private fn void! Formatter.out_reverse(Formatter* this, char[] buf)
{
	usz buffer_start_idx = this.idx;
	usz len = buf.len;
    // pad spaces up to given width
    if (!this.flags.left && !this.flags.zeropad)
    {
        for (usz i = len; i < this.width; i++)
        {
            this.out(' ')?;
        }
    }
    // reverse string
    while (len) this.out(buf[--len])?;

	// append pad spaces up to given width
	return this.left_adjust(this.idx - buffer_start_idx);
}

private fn void! printf_advance_format(usz format_len, usz *index_ptr) @inline
{
	usz val = ++(*index_ptr);
	if (val >= format_len) return FormattingFault.UNTERMINATED_FORMAT!;
}

private fn variant! next_variant(variant* args_ptr, usz args_len, usz* arg_index_ptr) @inline
{
	if (*arg_index_ptr >= args_len) return FormattingFault.MISSING_ARG!;
	return args_ptr[(*arg_index_ptr)++];
}

private fn int! printf_parse_format_field(variant* args_ptr, usz args_len, usz* args_index_ptr, char* format_ptr, usz format_len, usz* index_ptr) @inline
{
	char c = format_ptr[*index_ptr];
	if (c >= '0' && c <= '9') return simple_atoi(format_ptr, format_len, index_ptr);
	if (c != '*') return 0;
	printf_advance_format(format_len, index_ptr)?;
	variant val = next_variant(args_ptr, args_len, args_index_ptr)?;
    if (!val.type.kindof.is_int()) return FormattingFault.INVALID_WIDTH_ARG!;
    uint! intval = types::variant_to_int(val, int);
	if (catch intval) return FormattingFault.INVALID_WIDTH_ARG!;
	return intval;
}
