module std::math::nolibc @if(env::NO_LIBC || $feature(C3_MATH));

union DoubleInternal
{
	double f;
	ulong i;
}

union FloatInternal
{
	float f;
	uint i;
}

// Based on the musl implementation
fn double fmod(double x, double y) @extern("fmod") @weak @nostrip
{
	DoubleInternal ux = { .f = x };
	DoubleInternal uy = { .f = y };
	int ex = (int)((ux.i >> 52) & 0x7ff);
	int ey = (int)((uy.i >> 52) & 0x7ff);
	int sx = (int)(ux.i >> 63);
	ulong uxi = ux.i;
	if (uy.i << 1 == 0 || math::is_nan(y) || ex == 0x7ff) return (x * y)/(x * y);
	if (uxi << 1 <= uy.i << 1)
	{
		if (uxi << 1 == uy.i << 1) return 0 * x;
		return x;
	}

	if (!ex)
	{
		for (ulong i = uxi << 12; i >> 63 == 0; ex--, i <<= 1);
		uxi <<= -ex + 1;
	}
	else
	{
		uxi &= -1UL >> 12;
		uxi |= 1UL << 52;
	}
	if (!ey)
	{
		for (ulong i = uy.i << 12; i >> 63 == 0; ey--, i <<= 1);
		uy.i <<= -ey + 1;
	}
	else
	{
		uy.i &= -1UL >> 12;
		uy.i |= 1UL << 52;
	}

	/* x mod y */
	for (; ex > ey; ex--)
	{
		ulong i = uxi - uy.i;
		if (i >> 63 == 0)
		{
			if (i == 0) return 0 * x;
			uxi = i;
		}
		uxi <<= 1;
	}
	ulong i = uxi - uy.i;
	if (i >> 63 == 0)
	{
		if (i == 0) return 0*x;
		uxi = i;
	}
	for (; uxi>>52 == 0; uxi <<= 1, ex--);

	/* scale result */
	if (ex > 0)
	{
		uxi -= 1UL << 52;
		uxi |= (ulong)ex << 52;
	}
	else
	{
		uxi >>= -ex + 1;
	}
	uxi |= (ulong)sx << 63;
	ux.i = uxi;
	return ux.f;
}

fn float fmodf(float x, float y) @extern("fmodf") @weak @nostrip
{
	FloatInternal ux = { .f = x };
	FloatInternal uy = { .f = y };
	int ex = (int)((ux.i >> 23) & 0xff);
	int ey = (int)((uy.i >> 23) & 0xff);
	int sx = (int)(ux.i >> 31);
	uint uxi = ux.i;
	if (uy.i << 1 == 0 || math::is_nan(y) || ex == 0xff) return (x * y)/(x * y);
	if (uxi << 1 <= uy.i << 1)
	{
		if (uxi << 1 == uy.i << 1) return 0 * x;
		return x;
	}

	if (!ex)
	{
		for (uint i = uxi << 9; i >> 31 == 0; ex--, i <<= 1);
		uxi <<= -ex + 1;
	}
	else
	{
		uxi &= -1U >> 9;
		uxi |= 1U << 23;
	}
	if (!ey)
	{
		for (uint i = uy.i << 9; i >> 31 == 0; ey--, i <<= 1);
		uy.i <<= -ey + 1;
	}
	else
	{
		uy.i &= -1U >> 9;
		uy.i |= 1U << 23;
	}

	/* x mod y */
	for (; ex > ey; ex--)
	{
		uint i = uxi - uy.i;
		if (i >> 31 == 0)
		{
			if (i == 0) return 0 * x;
			uxi = i;
		}
		uxi <<= 1;
	}
	uint i = uxi - uy.i;
	if (i >> 31 == 0)
	{
		if (i == 0) return 0*x;
		uxi = i;
	}
	for (; uxi>>23 == 0; uxi <<= 1, ex--);

	/* scale result */
	if (ex > 0)
	{
		uxi -= 1U << 23;
		uxi |= (uint)ex << 23;
	}
	else
	{
		uxi >>= -ex + 1;
	}
	uxi |= (uint)sx << 31;
	ux.i = uxi;
	return ux.f;
}
