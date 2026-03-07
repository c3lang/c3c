module std::io;
import std::math;

const char[16] XDIGITS_H = "0123456789ABCDEF";
const char[16] XDIGITS_L = "0123456789abcdef";

faultdef BAD_FORMAT;

fn usz? print_hex_chars(Formatter* f, char[] out, bool uppercase) @inline
{
	char past_10 = (uppercase ? 'A' : 'a') - 10;
	usz len = 0;
	foreach (c : out)
	{
		char digit = c >> 4;
		f.print_char(digit + (digit < 10 ? '0' : past_10))!;
		len++;
		digit = c & 0xf;
		f.print_char(digit + (digit < 10 ? '0' : past_10))!;
		len++;
	}
	return len;
}

macro fault Formatter.first_err(&self, fault f)
{
	if (self.first_fault) return self.first_fault;
	self.first_fault = f;
	return f;
}

fn usz? formatter_adjust(Formatter* f, usz len) @local
{
	if (!f.flags.left) return 0;
	return formatter_pad(f, ' ', f.width, len);
}

fn uint128? int_from_any(any arg, bool *is_neg) @private
{
	switch (arg.type.kindof)
	{
		case FUNC:
		case POINTER:
			*is_neg = false;
			return (uint128)(uptr)*(void**)arg.ptr;
		case TYPEDEF:
		case CONSTDEF:
			return int_from_any(arg.as_inner(), is_neg);
		default:
			break;
	}
	*is_neg = false;
	switch (arg.type)
	{
		case bool:
			return (uint128)*(bool*)arg;
		case ichar:
			int val = *(ichar*)arg;
			return (*is_neg = val < 0) ? (~(uint128)val) + 1 : (uint128)val;
		case short:
			int val = *(short*)arg;
			return (*is_neg = val < 0) ? (~(uint128)val) + 1 : (uint128)val;
		case int:
			int val = *(int*)arg;
			return (*is_neg = val < 0) ? (~(uint128)val) + 1 : (uint128)val;
		case long:
			long val = *(long*)arg;
			return (*is_neg = val < 0) ? (~(uint128)val) + 1 : (uint128)val;
		case int128:
			int128 val = *(int128*)arg;
			return (*is_neg = val < 0) ? (~(uint128)val) + 1 : (uint128)val;
		case char:
			return *(char*)arg;
		case ushort:
			return *(ushort*)arg;
		case uint:
			return *(uint*)arg;
		case ulong:
			return *(ulong*)arg;
		case uint128:
			return *(uint128*)arg;
		case float:
			float f = *(float*)arg;
			return (uint128)((*is_neg = f < 0) ? -f : f);
		case double:
			double d = *(double*)arg;
			return (uint128)((*is_neg = d < 0) ? -d : d);
		default:
			return BAD_FORMAT~;
	}
}

fn FloatType? float_from_any(any arg) @private
{
	$if env::F128_SUPPORT:
		if (arg.type == float128.typeid) return (FloatType)*((float128*)arg.ptr);
	$endif
	if (arg.type.kindof == TYPEDEF || arg.type.kindof == CONSTDEF)
	{
		return float_from_any(arg.as_inner());
	}
	switch (arg.type)
	{
		case bool:
			return (FloatType)*(bool*)arg;
		case ichar:
			return *(ichar*)arg;
		case short:
			return *(short*)arg;
		case int:
			return *(int*)arg;
		case long:
			return *(long*)arg;
		case int128:
			return *(int128*)arg;
		case char:
			return *(char*)arg;
		case ushort:
			return *(ushort*)arg;
		case uint:
			return *(uint*)arg;
		case ulong:
			return *(ulong*)arg;
		case uint128:
			return *(uint128*)arg;
		case float:
			return (FloatType)*(float*)arg;
		case double:
			return (FloatType)*(double*)arg;
		default:
			return BAD_FORMAT~;
	}
}


<*
 Read a simple integer value, typically for formatting.

 @param [inout] len_ptr : "the length remaining."
 @param [in] buf : "the buf to read from."
 @param maxlen : "the maximum len that can be read."
 @return "The result of the atoi."
*>
fn uint simple_atoi(char* buf, usz maxlen, usz* len_ptr) @inline @private
{
	uint i = 0;
	usz len = *len_ptr;
	while (len < maxlen)
	{
		char c = buf[len];
		if (!c.is_digit()) break;
		i = i * 10 + c - '0';
		len++;
	}
	*len_ptr = len;
	return i;
}

fn usz? formatter_out_substr(Formatter* f, String str) @private
{
	usz l = conv::utf8_codepoints(str);
	uint prec = f.prec;
	if (f.flags.precision && l < prec) l = prec;
	usz index = 0;
	usz chars = str.len;
	char* ptr = str.ptr;
	while (index < chars)
	{
		char c = ptr[index];
		// Break if we have precision set and we ran out...
		if (c & 0xC0 != 0x80 && f.flags.precision && !prec--) break;
		f.print_char(c)!;
		index++;
	}
	return index;
}

fn usz? formatter_pad(Formatter* f, char c, isz width, isz len) @inline
{
	isz delta = width - len;
	for (isz i = 0; i < delta; i++) f.print_char(c)!;
	return max(0, delta);
}

fn char* fmt_u(uint128 x, char* s)
{
	for (; x > ulong.max; x /= 10) *--s = '0' + (char)(x % 10);
	for (ulong y = (ulong)x; y; y /= 10) *--s = '0' + (char)(y % 10);
	return s;
}

fn usz? Formatter.out_chars(&self, char[] s)
{
	foreach (c : s) self.print_char(c)!;
	return s.len;
}

enum FloatFormatting
{
	FLOAT,
	EXPONENTIAL,
	ADAPTIVE,
	HEX
}

fn usz? formatter_etoa(Formatter* self, double y) => formatter_floatformat(self, EXPONENTIAL, y);
fn usz? formatter_ftoa(Formatter* self, double y) => formatter_floatformat(self, FLOAT, y);
fn usz? formatter_gtoa(Formatter* self, double y) => formatter_floatformat(self, ADAPTIVE, y);
fn usz? formatter_atoa(Formatter* self, double y) => formatter_floatformat(self, HEX, y);

fn usz? formatter_floatformat_hex(Formatter* self, double y, bool is_neg, isz pl, isz p) @private @inline
{
	double round = 8.0;
	// 0x / 0X
	pl += 2;
	if (p > 0 && p < math::DOUBLE_MANT_DIG / 4 - 1)
	{
		int re = math::DOUBLE_MANT_DIG / 4 - 1 - (int)p;
		round *= 1 << (math::DOUBLE_MANT_DIG % 4);
		while (re--) round *= 16;
		if (is_neg)
		{
			y = -y;
			y -= round;
			y += round;
			y = -y;
		}
		else
		{
			y += round;
			y -= round;
		}
	}
	int e2;
	y = math::frexp(y, &e2) * 2;
	if (y) e2--;
	char[12] ebuf0;
	char* ebuf = &ebuf0[0] + 12;
	char[9 + math::DOUBLE_MANT_DIG / 4] buf_array;
	char* buf = &buf_array[0];

	// Reverse print
	char* estr = fmt_u(e2 < 0 ? (int128)-e2 : (int128)e2, ebuf);
	if (estr == ebuf) *--estr = '0';
	*--estr = (e2 < 0 ? '-' : '+');
	*--estr = self.flags.uppercase ? 'P' : 'p';
	char* s = buf;
	char* xdigits = self.flags.uppercase ? &XDIGITS_H : &XDIGITS_L;
	do
	{
		int x = (int)y;
		*s++ = xdigits[x];
		y = 16 * (y - x);
		if (s - buf == 1 && (y || p > 0 || self.flags.hash)) *s++ = '.';
		if (p >= 0 && (s - buf) >= (2 + p)) break;
	} while (y);
	isz outlen = s - buf;
	isz explen = ebuf - estr;
	if (p > int.max - 2 - explen - pl) return INTERNAL_BUFFER_EXCEEDED~;
	usz len;
	usz l = (usz)(p && outlen - 2 < p
			? p + 2 + explen
			: outlen + explen);
	if (!self.flags.left && !self.flags.zeropad) len += formatter_pad(self, ' ', self.width, pl + l)!;
	if (is_neg || self.flags.plus) len += self.print_char(is_neg ? '-' : '+')!;
	len += self.out_chars(self.flags.uppercase ? "0X" : "0x")!;
	if (self.flags.zeropad) len += formatter_pad(self, '0', self.width, pl + l)!;
	len += self.out_chars(buf[:outlen])!;
	len += formatter_pad(self, '0', (isz)l - outlen - explen, 0)!;
	len += self.out_chars(estr[:explen])!;
	if (self.flags.left) len += formatter_pad(self, ' ', self.width, pl + (isz)l)!;
	return len;
}

fn usz? formatter_floatformat(Formatter* self, FloatFormatting formatting, double y) @private
{
	// This code is heavily based on musl's printf code
	const BUF_SIZE = (math::DOUBLE_MANT_DIG + 28) / 29 + 1
		+ (math::DOUBLE_MAX_EXP + math::DOUBLE_MANT_DIG + 28 + 8) / 9;
	uint[BUF_SIZE] big;
	bool is_neg = false;
	if (math::signbit(y))
	{
		is_neg = true;
		y = -y;
	}
	isz pl = is_neg || self.flags.plus ? 1 : 0;
	// Print inf/nan
	if (!math::is_finite(y))
	{
		usz len;
		// Add padding
		if (!self.flags.left) len += formatter_pad(self, ' ', self.width, 3 + pl)!;
		String s = self.flags.uppercase ? "INF" : "inf";
		if (math::is_nan(y)) s = self.flags.uppercase ? "NAN" : "nan";
		if (pl) len += self.print_char(is_neg ? '-' : '+')!;
		len += self.out_chars(s)!;
		if (self.flags.left) len += formatter_pad(self, ' ', self.width, 3 + pl)!;
		return len;
	}

	isz p = self.flags.precision ? self.prec : -1;
	if (formatting == HEX) return formatter_floatformat_hex(self, y, is_neg, pl, p);

	// Rescale
	int e2;
	y = math::frexp(y, &e2) * 2;
	if (y) e2--;

	if (p < 0) p = 6;
	if (y)
	{
		y *= 0x1p28;
		e2 -= 28;
	}

	uint* a, z, r;
	if (e2 < 0)
	{
		a = r = z = &big;
	}
	else
	{
		a = r = z = (uint*)&big + big.len - math::DOUBLE_MANT_DIG - 1;
	}

	do
	{
		uint v = z++[0] = (uint)y;
		y = 1000000000 * (y - v);
	} while (y);

	while (e2 > 0)
	{
		uint carry = 0;
		int sh = math::min(29, e2);
		for (uint* d = z - 1; d >= a; d--)
		{
			ulong x = (ulong)*d << sh + carry;
			*d = (uint)(x % 1000000000);
			carry = (uint)(x / 1000000000);
		}
		if (carry) *--a = carry;
		while (z > a && !z[-1]) z--;
		e2 -= sh;
	}

	while (e2 < 0)
	{
		uint carry = 0;
		uint* b;
		int sh = math::min(9, -e2);
		int need = (int)(1 + (p + math::DOUBLE_MANT_DIG / 3u + 8) / 9);
		for (uint* d = a; d < z; d++)
		{
			uint rm = *d & ((1 << sh) - 1);
			*d = (*d >> sh) + carry;
			carry = (1000000000 >> sh) * rm;
		}
		if (!a[0]) a++;
		if (carry) z++[0] = carry;
		// Avoid (slow!) computation past requested precision
		b = formatting == FLOAT ? r : a;
		if (z - b > need) z = b + need;
		e2 += sh;
	}

	int e;
	if (a < z)
	{
		for (int i = 10, e = (int)(9 * (r - a)); *a >= i; i *= 10, e++);
	}

	// Perform rounding: j is precision after the radix (possibly neg)
	int j = (int)p;
	if (formatting != FLOAT) j -= e;
	if (formatting == ADAPTIVE && p != 0) j -= 1;

	if (j < 9 * (z - r - 1))
	{
		uint x;
		// We avoid C's broken division of negative numbers
		uint* d = r + 1 + ((j + 9 * math::DOUBLE_MAX_EXP) / 9 - math::DOUBLE_MAX_EXP);
		j += 9 * math::DOUBLE_MAX_EXP;
		j %= 9;
		int i;
		for (i = 10, j++; j < 9; i *= 10, j++);
		x = *d % (uint)i;
		// Are there any significant digits past j?
		if (x || (d + 1) != z)
		{
			double round = 2 / math::DOUBLE_EPSILON;
			double small;
			if (((*d / (uint)i) & 1) || (i == 1000000000 && d > a && (d[-1] & 1)))
			{
				round += 2;
			}
			switch
			{
				case x < i / 2:
					small = 0x0.8p0;
				case x == i / 2 && d + 1 == z:
					small = 0x1.0p0;
				default:
					small = 0x1.8p0;
			}
			if (pl && is_neg)
			{
				round *= -1;
				small *= -1;
			}
			*d -= x;
			// Decide whether to round by probing round+small
			if (round + small != round)
			{
				*d = *d + i;
				while (*d > 999999999)
				{
					*d-- = 0;
					if (d < a) *--a = 0;
					(*d)++;
				}
				for (i = 10, e = (int)(9 * (r - a)); *a >= i; i *= 10, e++);
			}
		}
		if (z > d + 1) z = d + 1;
	}
	for (; z>a && !z[-1]; z--);

	if (formatting == ADAPTIVE)
	{
		if (!p) p++;
		if (p > e && e >= -4)
		{
			formatting = FLOAT;
			p -= (isz)e + 1;
		}
		else
		{
			formatting = EXPONENTIAL;
			p--;
		}
		if (!self.flags.hash)
		{
			// Count trailing zeros in last place
			if (z > a && z[-1])
			{
				for (int i = 10, j = 0; z[-1] % (uint)i == 0; i *= 10, j++);
			}
			else
			{
				j = 9;
			}
			if (formatting == FLOAT)
			{
				p = math::min(p, math::max((isz)0, 9 * (z - r - 1) - j));
			}
			else
			{
				p = math::min(p, math::max((isz)0, 9 * (z - r - 1) + e - j));

			}
		}
	}
	if (p > int.max - 1 - (isz)(p || self.flags.hash)) return INTERNAL_BUFFER_EXCEEDED~;
	int l = (int)(1 + p + (isz)(p || self.flags.hash));
	char[12] ebuf0;
	char* ebuf = &ebuf0[0] + 12;
	char* estr @noinit;
	if (formatting == FLOAT)
	{
		if (e > int.max - l) return INTERNAL_BUFFER_EXCEEDED~;
		if (e > 0) l += e;
	}
	else
	{
		estr = fmt_u((uint128)(e < 0 ? -e : e), ebuf);
		while (ebuf - estr < 2) (--estr)[0] = '0';
		*--estr = (e < 0 ? '-' : '+');
		*--estr = self.flags.uppercase ? 'E' : 'e';
		if (ebuf - estr > (isz)int.max - l) return INTERNAL_BUFFER_EXCEEDED~;
		l += (int)(ebuf - estr);
	}
	if (l > int.max - pl) return INTERNAL_BUFFER_EXCEEDED~;
	usz len;
	if (!self.flags.left && !self.flags.zeropad) len += formatter_pad(self, ' ', self.width, pl + l)!;
	if (is_neg || self.flags.plus) len += self.print_char(is_neg ? '-' : '+')!;
	if (self.flags.zeropad) len += formatter_pad(self, '0', self.width, pl + l)!;

	char[9] buf_array;
	char* buf = &buf_array[0];

	if (formatting == FLOAT)
	{
		if (a > r) a = r;
		uint* d = a;
		for (; d <= r; d++)
		{
			char* s = fmt_u(*d, buf + 9);
			switch
			{
				case d != a:
					while (s > buf) (--s)[0] = '0';
				case s == buf + 9:
					*--s = '0';
			}
			len += self.out_chars(s[:buf + 9 - s])!;
		}
		if (p || self.flags.hash) len += self.print_char('.')!;
		for (; d < z && p > 0; d++, p -= 9)
		{
			char* s = fmt_u(*d, buf + 9);
			while (s > buf) *--s = '0';
			len += self.out_chars(s[:math::min((isz)9, p)])!;
		}
		len += formatter_pad(self, '0', p + 9, 9)!;
	}
	else
	{
		if (z <= a) z = a + 1;
		for (uint* d = a; d < z && p >= 0; d++)
		{
			char* s = fmt_u(*d, buf + 9);
			if (s == buf + 9) (--s)[0] = '0';
			if (d != a)
			{
				while (s > buf) (--s)[0] = '0';
			}
			else
			{
				len += self.print_char(s++[0])!;
				if (p > 0 || self.flags.hash) len += self.print_char('.')!;
			}
			len += self.out_chars(s[:math::min(buf + 9 - s, p)])!;
			p -= buf + 9 - s;
		}
		len += formatter_pad(self, '0', p + 18, 18)!;
		len += self.out_chars(estr[:ebuf - estr])!;
	}

	if (self.flags.left) len += formatter_pad(self, ' ', self.width, pl + l)!;

	return len;
}


fn usz? formatter_out_str_pad(Formatter* self, any arg) @private @inline
{
	usz total;
	if (self.width && !self.flags.left)
	{
		OutputFn out_fn = self.out_fn;
		self.out_fn = (OutputFn)&out_null_fn;
		usz len = formatter_out_str(self, arg)!;
		self.out_fn = out_fn;
		total += formatter_pad(self, ' ', self.width, (isz)len)!;
	}
	usz len = formatter_out_str(self, arg)!;
	total += len;
	if (self.flags.left)
	{
		total += formatter_pad(self, ' ', self.width, (isz)len)!;
	}
	return total;
}

const char[201] DIGIT_PAIRS @private =
  "00102030405060708090"
  "01112131415161718191"
  "02122232425262728292"
  "03132333435363738393"
  "04142434445464748494"
  "05152535455565758595"
  "06162636465666768696"
  "07172737475767778797"
  "08182838485868788898"
  "09192939495969798999";

fn usz? formatter_ntoa(Formatter* f, uint128 value, bool negative, uint base) @private
{
	char[PRINTF_NTOA_BUFFER_SIZE] buf @noinit;
	usz len;

	// no hash for 0 values
	if (!value) f.flags.hash = false;

	// write if precision != 0 or value is != 0
	if (!f.flags.precision || value)
	{
		char past_10 = (f.flags.uppercase ? 'A' : 'a') - 10;
		switch (base)
		{
			case 2:
				do
				{
					if (len >= PRINTF_NTOA_BUFFER_SIZE) return INTERNAL_BUFFER_EXCEEDED~;
					buf[len++] = '0' + (char)value & 1;
					value >>= 1;
				}
				while (value);
			case 10:
				if (!value)
				{
					if (len >= PRINTF_NTOA_BUFFER_SIZE) return INTERNAL_BUFFER_EXCEEDED~;
					buf[len++] = '0';
					break;
				}
				while (value >= 10)
				{
					if (len + 1 >= PRINTF_NTOA_BUFFER_SIZE) return INTERNAL_BUFFER_EXCEEDED~;
					char digit = (char)(value % 100U);
					buf[len:2] = DIGIT_PAIRS[2 * digit:2];
					len += 2;
					value /= 100U;
				}
				if (value > 0)
				{
					if (len >= PRINTF_NTOA_BUFFER_SIZE) return INTERNAL_BUFFER_EXCEEDED~;
					buf[len++] = '0' + (char)value;
				}
			case 16:
				do
				{
					if (len >= PRINTF_NTOA_BUFFER_SIZE) return INTERNAL_BUFFER_EXCEEDED~;
					char digit = (char)value & 0xF;
					buf[len++] = digit + (digit < 10 ? '0' : past_10);
					value >>= 4;
				}
				while (value);
		case 8:
			do
			{
				if (len >= PRINTF_NTOA_BUFFER_SIZE) return INTERNAL_BUFFER_EXCEEDED~;
				buf[len++] = '0' + (char)value & 0x7;
				value >>= 3;
			} while (value);
		default:
			unreachable();
		}
	}
	return formatter_ntoa_format(f, (String)buf[:PRINTF_NTOA_BUFFER_SIZE], len, negative, base);
}

fn usz? formatter_ntoa_format(Formatter* f, String buf, usz len, bool negative, uint base) @private
{
	// pad leading zeros
	if (!f.flags.left)
	{
		if (f.width && f.flags.zeropad && (negative || f.flags.plus || f.flags.space)) f.width--;
		while (len < f.prec)
		{
			if (len >= buf.len) return INTERNAL_BUFFER_EXCEEDED~;
			buf[len++] = '0';
		}
		while (f.flags.zeropad && len < f.width)
		{
			if (len >= buf.len) return INTERNAL_BUFFER_EXCEEDED~;
			buf[len++] = '0';
		}
	}

	// handle hash
	if (f.flags.hash && base != 10)
	{
		if (!f.flags.precision && len && len == f.prec && len == f.width)
		{
			len--;
			if (len) len--;
		}
		if (base != 10)
		{
			if (len + 1 >= buf.len) return INTERNAL_BUFFER_EXCEEDED~;
			switch (base)
			{
				case 16:
					buf[len++] = f.flags.uppercase ? 'X' : 'x';
				case 8:
					buf[len++] = f.flags.uppercase ? 'O' : 'o';
				case 2:
					buf[len++] = f.flags.uppercase ? 'B' : 'b';
				default:
					unreachable();
			}
			buf[len++] = '0';
		}
	}

	switch (true)
	{
		case negative:
			if (len >= buf.len) return INTERNAL_BUFFER_EXCEEDED~;
			buf[len++] = '-';
		case f.flags.plus:
			if (len >= buf.len) return INTERNAL_BUFFER_EXCEEDED~;
			buf[len++] = '+';
		case f.flags.space:
			if (len >= buf.len) return INTERNAL_BUFFER_EXCEEDED~;
			buf[len++] = ' ';
	}
	if (len) formatter_out_reverse(f, buf[:len])!;
	return len;
}


fn usz? formatter_ntoa_any(Formatter* f, any arg, uint base) @private
{
	bool is_neg;
	return formatter_ntoa(f, int_from_any(arg, &is_neg)!!, is_neg, base) @inline;
}

fn usz? formatter_out_char(Formatter* f, any arg) @private
{
	if (!arg.type.kindof.is_int())
	{
		return formatter_out_substr(f, "<NOT CHAR>");
	}
	usz len = 1;
	// pre padding
	if (!f.flags.left)
	{
		len += formatter_pad(f, ' ', f.width, len)!;
	}

	// char output
	Char32 c = types::any_to_int(arg, uint) ?? 0xFFFD;
	switch (true)
	{
		case c < 0x7f:
			f.print_char((char)c)!;
		case c < 0x7ff:
			f.print_char((char)(0xC0 | c >> 6))!;
			f.print_char((char)(0x80 | (c & 0x3F)))!;
		case c < 0xffff:
			f.print_char((char)(0xE0 | c >> 12))!;
			f.print_char((char)(0x80 | (c >> 6 & 0x3F)))!;
			f.print_char((char)(0x80 | (c & 0x3F)))!;
		default:
			f.print_char((char)(0xF0 | c >> 18))!;
			f.print_char((char)(0x80 | (c >> 12 & 0x3F)))!;
			f.print_char((char)(0x80 | (c >> 6 & 0x3F)))!;
			f.print_char((char)(0x80 | (c & 0x3F)))!;
	}
	if (f.flags.left)
	{
		len += formatter_pad(f, ' ', f.width, len)!;
	}
	return len;
}


fn usz? formatter_out_reverse(Formatter* f, char[] buf) @private
{
	usz n;
	usz len = buf.len;
	// pad spaces up to given width
	if (!f.flags.zeropad && !f.flags.left)
	{
		n += formatter_pad(f, ' ', f.width, len)!;
	}
	// reverse string
	while (len) n += f.print_char(buf[--len])!;

	// append pad spaces up to given width
	n += formatter_adjust(f, n)!;
	return n;
}


fn int? printf_parse_format_field(
	any* args_ptr, usz args_len, usz* args_index_ptr,
	char* format_ptr, usz format_len, usz* index_ptr) @inline @private
{
	char c = format_ptr[*index_ptr];
	if (c.is_digit()) return simple_atoi(format_ptr, format_len, index_ptr);
	if (c != '*') return 0;
	usz len = ++(*index_ptr);
	if (len >= format_len) return BAD_FORMAT~;
	if (*args_index_ptr >= args_len) return BAD_FORMAT~;
	any val = args_ptr[(*args_index_ptr)++];
	if (!val.type.kindof.is_int()) return BAD_FORMAT~;
	uint? intval = types::any_to_int(val, int);
	return intval ?? BAD_FORMAT~;
}

fn usz? formatter_out_hex_buffer(Formatter* self, any arg) @private @inline
{
	char[] out @noinit;
	switch (arg.type)
	{
		case char[]:
		case ichar[]:
			out = *(char[]*)arg;
		default:
			if (arg.type.kindof == ARRAY && (arg.type.inner == char.typeid || arg.type.inner == ichar.typeid))
			{
				out = ((char*)arg.ptr)[:arg.type.sizeof];
				break;
			}
			if (arg.type.kindof == POINTER)
			{
				// Maybe there is a more idiomatic way here
				out = ((*(char**)arg.ptr))[:arg.type.inner.sizeof];
				break;
			}
			return formatter_out_substr(self, "<INVALID>");
	}
	usz len = out.len * 2;
	usz total;
	if (self.flags.left)
	{
		total += print_hex_chars(self, out, self.flags.uppercase)!;
		total += formatter_pad(self, ' ', self.width, (isz)total)!;
	}
	else
	{
		if (self.width) total += formatter_pad(self, ' ', self.width, (isz)len)!;
		total += print_hex_chars(self, out, self.flags.uppercase)!;
	}
	return total;
}


fn usz? formatter_out_unknown(Formatter* self, String category, any arg) @private
{
	return formatter_out_substr(self, "<")
		+ formatter_out_substr(self, category)
		+ formatter_out_substr(self, " type:")
		+ formatter_ntoa(self, (iptr)arg.type, false, 16)
		+ formatter_out_substr(self, ", addr:")
		+ formatter_ntoa(self, (iptr)arg.ptr, false, 16)
		+ formatter_out_substr(self, ">");
}

fn usz? formatter_out_collection(Formatter* self, any arg, String open, String close) @private
{
	typeid inner = arg.type.inner;
	if (inner == void.typeid) inner = char.typeid;
	usz size = inner.sizeof;

	usz alen;
	void* data_ptr;
	if (arg.type.kindof == SLICE)
	{
		String* temp = arg.ptr;
		data_ptr = temp.ptr;
		alen = temp.len;
	}
	else
	{
		data_ptr = arg.ptr;
		alen = arg.type.len;
	}

	PrintFlags flags = self.flags;
	uint width = self.width;
	defer
	{
		self.flags = flags;
		self.width = width;
	}
	self.flags = {};
	self.width = 0;

	usz len = formatter_out_substr(self, open)!;
	for (usz i = 0; i < alen; i++)
	{
		if (i != 0) len += formatter_out_substr(self, ", ")!;
		len += formatter_out_str(self, any_make(data_ptr, inner))!;
		data_ptr += size;
	}
	len += formatter_out_substr(self, close)!;
	return len;
}

fn usz? formatter_out_str(Formatter* self, any arg) @private
{
	switch (arg.type.kindof)
	{
		case VOID:
			return formatter_out_substr(self, "void");
		case FAULT:
			fault f = *(fault*)arg.ptr;
			return formatter_out_substr(self, f ? f.nameof : "(empty-fault)");
		case INTERFACE:
			any a = *(any*)arg;
			return a ? formatter_out_str(self, a) : formatter_out_substr(self, "(empty-interface)");
		case ANY:
			any a = *(any*)arg;
			return a ? formatter_out_str(self, a) : formatter_out_substr(self, "(empty-any)");
		case OPTIONAL:
			unreachable();
		case SIGNED_INT:
		case UNSIGNED_INT:
		case FLOAT:
		case FUNC:
		case POINTER:
			PrintFlags flags = self.flags;
			uint width = self.width;
			defer
			{
				self.flags = flags;
				self.width = width;
			}
			self.flags = {};
			self.width = 0;
			switch (arg.type.kindof)
			{
				case SIGNED_INT:
				case UNSIGNED_INT:
					return formatter_ntoa_any(self, arg, 10) ?? formatter_out_substr(self, "<INVALID>");
				case FLOAT:
					return formatter_ftoa(self, float_from_any(arg)) ?? formatter_out_substr(self, "ERR");
				case FUNC:
				case POINTER:
					if (arg.type.kindof == POINTER && arg.type.inner != void.typeid)
					{
						void** pointer = arg.ptr;
						any deref = any_make(*pointer, arg.type.inner);
						usz? n = self.print_with_function((Printable)deref);
						if (try n) return n;
						if (@catch(n) != NOT_FOUND) n!;
					}
					return formatter_out_substr(self, "0x")! + formatter_ntoa_any(self, arg, 16);
				default:
					unreachable();
			}
		case BOOL:
			return formatter_out_substr(self, *(bool*)arg.ptr ? "true" : "false");
		default:
	}
	usz? n = self.print_with_function((Printable)arg);
	if (try n) return n;
	if (@catch(n) != NOT_FOUND) n!;
	switch (arg.type.kindof)
	{
		case TYPEID:
			return formatter_out_substr(self, "typeid[")! + formatter_ntoa(self, (iptr)*(typeid*)arg, false, 16)! + formatter_out_substr(self, "]")!;
		case ENUM:
			usz i = types::any_to_enum_ordinal(arg, usz)!!;
			assert(i < arg.type.names.len, "Illegal enum value found, numerical value was %d.", i);
			return formatter_out_substr(self, arg.type.names[i]);
		case STRUCT:
			return formatter_out_unknown(self, "struct", arg);
		case UNION:
			return formatter_out_unknown(self, "union", arg);
		case BITSTRUCT:
			return formatter_out_unknown(self, "bitstruct", arg);
		case CONSTDEF:
		case TYPEDEF:
			if (arg.type == String.typeid)
			{
				return formatter_out_substr(self, *(String*)arg);
			}
			if (arg.type == ZString.typeid)
			{
				return formatter_out_substr(self, *(ZString*)arg ? ((ZString*)arg).str_view() : "(null)");
			}
			if (arg.type == DString.typeid)
			{
				return formatter_out_substr(self, *(DString*)arg ? ((DString*)arg).str_view() : "(null)");
			}
			return formatter_out_str(self, arg.as_inner());
		case ARRAY:
			return formatter_out_collection(self, arg, "[", "]");
		case VECTOR:
			return formatter_out_collection(self, arg, "[<", ">]");
		case SLICE:
			return formatter_out_collection(self, arg, "[", "]");
		case ANY:
		case INTERFACE:
			unreachable("Already handled");
		default:
	}
	return formatter_out_substr(self, "Invalid type");
}


