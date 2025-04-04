module std::math::nolibc @if(env::NO_LIBC || $feature(C3_MATH));

/* origin: FreeBSD usr/src/lib/msun/src/e_atanh.c */
/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunSoft, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 */

fn double _atanh(double x) @weak @extern("atanh") @nostrip
{
	double t @noinit;
	uint hx = x.high_word();
	uint ix = hx & 0x7fffffff;
	uint sign = hx >> 31;
	switch
	{
		/* |x| >= 1 or nan */
		case ix >= 0x3ff00000:
		     	uint lx = x.low_word();
		     	if ((ix - 0x3ff00000 | lx) == 0)
			{
			   	return sign ? -double.inf : double.inf;
			}
			return double.nan;
		/* x<2**-28 */
		case ix < 0x3e300000 && (1e300 + x) > 0.0:
		     	return x;
	}
	x.set_high_word(ix);
	/* |x| < 0.5 */
	if (ix < 0x3fe00000)
	{
		t = x + x;
		t = 0.5 * _log1p(t + t * x / (1.0 - x));
	}
	else
	{
		t = 0.5 * _log1p((x + x) / (1.0 - x));
	}
	return sign ? -t : t;
}

/* origin: FreeBSD usr/src/lib/msun/src/e_atanhf.c */
/*
 * Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.
 */
/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */

fn float _atanhf(float x) @weak @extern("atanhf") @nostrip
{
	float t @noinit;
	uint hx = bitcast(x, uint);
	uint ix = hx & 0x7fffffff;
	uint sign = hx >> 31;
	switch
	{
		/* |x| >= 1 or nan */
		case ix >= 0x3f800000:
		     	if (ix == 0x3f800000)
			{
			   	return sign ? -float.inf : float.inf;
			}
			return float.nan;
		/* x<2**-28 */
		case ix < 0x31800000 && (1e30 + x) > 0.f:
		     	return x;
	}
	x.set_word(ix);
	/* |x| < 0.5 */
	if (ix < 0x3f000000)
	{
		t = x + x;
		t = 0.5f * _log1pf(t + t * x / (1.f - x));
	}
	else
	{
		t = 0.5f * _log1pf((x + x) / (1.f - x));
	}
	return sign ? -t : t;
}
