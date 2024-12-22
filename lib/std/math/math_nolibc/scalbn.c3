module std::math::nolibc @if(env::NO_LIBC || $feature(C3_MATH));

fn double _scalbn(double x, int n) @weak @extern("scalbn") @nostrip
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
