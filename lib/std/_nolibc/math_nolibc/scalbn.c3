module std::math::nolibc @feat(NO_LIBC, C3_MATH);

fn double ldexp(double x, int exp) @cname("ldexp") @weak @nostrip => _scalbn(x, exp);
fn float ldexpf(float x, int exp) @cname("ldexpf") @weak @nostrip => _scalbnf(x, exp);

fn double _scalbn(double x, int n) @weak @cname("scalbn") @nostrip
{
	switch
	{
		case n > 1023:
			x *= 0x1p1023;
			n -= 1023;
			if (n > 1023)
			{
				x *= 0x1p1023;
				n -= 1023;
				if (n > 1023) n = 1023;
			}
		case n < -1022:
			x *= 0x1p-1022 * 0x1p53;
			n += 1022 - 53;
			if (n < -1022)
			{
				x *= 0x1p-1022 * 0x1p53;
				n += 1022 - 53;
				if (n < -1022) n = -1022;
			}
	}
	return x * bitcast(((ulong)(0x3ff + n)) << 52, double);
}

fn float _scalbnf(float x, int n) @weak @cname("scalbnf") @nostrip
{
	switch
	{
		case n > 127:
			x *= 0x1p127f;
			n -= 127;
			if (n > 127)
			{
				x *= 0x1p127f;
				n -= 127;
				if (n > 127) n = 127;
			}
		case n < -126:
			x *= 0x1p-126f * 0x1p24f;
			n += 126 - 24;
			if (n < -126)
			{
				x *= 0x1p-126f * 0x1p24f;
				n += 126 - 24;
				if (n < -126) n = -126;
			}
	}
	return x * bitcast((uint)(0x7f + n) << 23, float);
}