module std::math::nolibc @if(env::NO_LIBC || $feature(C3_MATH));

/* origin: FreeBSD /usr/src/lib/msun/src/s_log1p.c */
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

/* origin: musl libc /src/math/log1p.c */
/*
 * ====================================================
 * Copyright (c) 2005-2020 Rich Felker, et al.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:

 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.

 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * ====================================================
 */

const LN2_HI @local = 6.93147180369123816490e-01;  /* 3fe62e42 fee00000 */
const LN2_LO @local = 1.90821492927058770002e-10;  /* 3dea39ef 35793c76 */
const LG1 @local = 6.666666666666735130e-01;  /* 3FE55555 55555593 */
const LG2 @local = 3.999999999940941908e-01;  /* 3FD99999 9997FA04 */
const LG3 @local = 2.857142874366239149e-01;  /* 3FD24924 94229359 */
const LG4 @local = 2.222219843214978396e-01;  /* 3FCC71C5 1D8E78AF */
const LG5 @local = 1.818357216161805012e-01;  /* 3FC74664 96CB03DE */
const LG6 @local = 1.531383769920937332e-01;  /* 3FC39A09 D078C69F */
const LG7 @local = 1.479819860511658591e-01;  /* 3FC2F112 DF3E5244 */

fn double _log1p(double x) @weak @extern("log1p") @nostrip
{
	uint hx = x.high_word();
	int k = 1;
	double c @noinit;
	double f @noinit;
	switch
	{
		/* 1+x < sqrt(2)+ */
		case hx < 0x3fda827a || hx >> 31 != 0:
		     	switch
		     	{
				/* x <= -1.0 */
				case hx >= 0xbff00000:
				     	if (x == -1) return -double.inf;
					return double.nan;
				/* |x| < 2**-53 */
				case hx << 1 < 0x3ca00000 << 1:
				     	/* underflow if subnormal */
					if ((hx & 0x7ff00000) == 0)
					{
						(float)@volatile_load(x);
					}
					return x;
				/* sqrt(2)/2- <= 1+x < sqrt(2)+ */
				case hx <= 0xbfd2bec4:
				     	k = 0;
					c = 0;
					f = x;
			}
		case hx >= 0x7ff00000:
		     return x;
	}
	if (k)
	{
		double u = 1 + x;
		uint hu = u.high_word();
		hu += 0x3ff00000 - 0x3fe6a09e;
		k = (int)(hu >> 20) - 0x3ff;
		/* correction term ~ log(1+x)-log(u), avoid underflow in c/u */
		if (k < 54)
		{
			c = (k >= 2) ? 1.0 - (u - x) : x - (u - 1.0);
			c /= u;
		}
		else
		{
			c = 0;
		}
		/* reduce u into [sqrt(2)/2, sqrt(2)] */
		hu = (hu & 0x000fffff) + 0x3fe6a09e;
		u = bitcast(((ulong)hu << 32) | (bitcast(u, ulong) & 0xffffffff) , double);
		f = u - 1.0;
	}
	double hfsq = 0.5 * f * f;
	double s = f / (2.0 + f);
	double z = s * s;
	double w = z * z;
	double t1 = w * (LG2 + w * (LG4 + w * LG6));
	double t2 = z * (LG1 + w * (LG3 + w * (LG5 + w * LG7)));
	double r = t1 + t2;
	double dk = k;
	return s * (hfsq + r) + (dk * LN2_LO + c) - hfsq + f + dk * LN2_HI;
}

/* origin: FreeBSD /usr/src/lib/msun/src/s_log1pf.c */
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

/* origin: musl libc /src/math/log1pf.c */
/*
 * ====================================================
 * Copyright (c) 2005-2020 Rich Felker, et al.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:

 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.

 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * ====================================================
 */

const float LN2_HI_F @local = 6.9313812256e-01; /* 0x3f317180 */
const float LN2_LO_F @local = 9.0580006145e-06; /* 0x3717f7d1 */
/* |(log(1+s)-log(1-s))/s - Lg(s)| < 2**-34.24 (~[-4.95e-11, 4.97e-11]). */
const float LG1_F @local = 0xaaaaaa.0p-24; /* 0.66666662693 */
const float LG2_F @local = 0xccce13.0p-25; /* 0.40000972152 */
const float LG3_F @local = 0x91e9ee.0p-25; /* 0.28498786688 */
const float LG4_F @local = 0xf89e26.0p-26; /* 0.24279078841 */

fn float _log1pf(float x) @weak @extern("log1pf") @nostrip
{
	uint ix = x.word();
	int k = 1;
	float c @noinit;
	float f @noinit;
	switch
	{
		/* 1+x < sqrt(2)+ */
		case ix < 0x3ed413d0 || ix >> 31 != 0:
		     	switch
		     	{
				/* x <= -1.0 */
				case ix >= 0xbf800000:
				     	if (x == -1) return -float.inf;
					return float.nan;
				/* |x| < 2**-24 */
				case ix << 1 < 0x33800000 << 1:
				     	/* underflow if subnormal */
					if ((ix & 0x7f800000) == 0)
					{
						float v = @volatile_load(x);
						v = v * v;
					}
					return x;
				/* sqrt(2)/2- <= 1+x < sqrt(2)+ */
				case ix <= 0xbe95f619:
				     	k = 0;
					c = 0;
					f = x;
			}
		case ix >= 0x7f800000:
		     return x;
	}
	if (k)
	{
		float u = 1 + x;
		uint iu = u.word();
		iu += 0x3f800000 - 0x3f3504f3;
		k = (int)(iu >> 23) - 0x7f;
		/* correction term ~ log(1+x)-log(u), avoid underflow in c/u */
		if (k < 25)
		{
			c = (k >= 2) ? 1.f - (u - x) : x - (u - 1.f);
			c /= u;
		}
		else
		{
			c = 0;
		}
		/* reduce u into [sqrt(2)/2, sqrt(2)] */
		iu = (iu & 0x007fffff) + 0x3f3504f3;
		f = bitcast(iu, float) - 1.f;
	}
	float hfsq = 0.5f * f * f;
	float s = f / (2.f + f);
	float z = s * s;
	float w = z * z;
	float t1 = w * (LG2_F + w * LG4_F);
	float t2 = z * (LG1_F + w * LG3_F);
	float r = t1 + t2;
	float dk = k;
	return s * (hfsq + r) + (dk * LN2_LO_F + c) - hfsq + f + dk * LN2_HI_F;
}