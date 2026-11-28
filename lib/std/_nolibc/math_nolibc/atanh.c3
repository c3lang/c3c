module std::math::nolibc @feat(NO_LIBC, C3_MATH);

/* origin: musl src/math/atanh.c */
/* atanh(x) = log((1+x)/(1-x))/2 = log1p(2x/(1-x))/2 ~= x + x^3/3 + o(x^5) */

fn double _atanh(double x) @weak @cname("atanh") @nostrip
{
	ulong u = bitcast(x, ulong);
	uint e = (uint)(u >> 52 & 0x7ff);
	bool s = (u >> 63) != 0;
	double y = bitcast(u & (ulong::max >> 1), double);
	if (e < 0x3ff - 1)
	{
		if (e < 0x3ff - 32)
		{
			if (e == 0) force_eval((float)y);
		}
		else
		{
			y = 0.5 * _log1p(2 * y + 2 * y * y / (1 - y));
		}
	}
	else
	{
		y = 0.5 * _log1p(2 * (y / (1 - y)));
	}
	return s ? -y : y;
}

/* origin: musl src/math/atanhf.c */
fn float _atanhf(float x) @weak @cname("atanhf") @nostrip
{
	uint u = bitcast(x, uint);
	bool s = (u >> 31) != 0;
	u &= 0x7fffffff;
	float y = bitcast(u, float);
	if (u < 0x3f800000 - (1 << 23))
	{
		if (u < 0x3f800000 - (32 << 23))
		{
			if (u < (1 << 23)) force_eval(y * y);
		}
		else
		{
			y = 0.5f * _log1pf(2 * y + 2 * y * y / (1 - y));
		}
	}
	else
	{
		y = 0.5f * _log1pf(2 * (y / (1 - y)));
	}
	return s ? -y : y;
}
