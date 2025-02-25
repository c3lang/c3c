module std::math::nolibc @if(env::NO_LIBC || $feature(C3_MATH));

/* origin: FreeBSD /usr/src/lib/msun/src/e_asin.c */
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

const PIO2_HI @local = 1.57079632679489655800e+00; /* 0x3FF921FB, 0x54442D18 */
const PIO2_LO @local = 6.12323399573676603587e-17; /* 0x3C91A626, 0x33145C07 */
const PS0 @local =  1.66666666666666657415e-01; /* 0x3FC55555, 0x55555555 */
const PS1 @local = -3.25565818622400915405e-01; /* 0xBFD4D612, 0x03EB6F7D */
const PS2 @local =  2.01212532134862925881e-01; /* 0x3FC9C155, 0x0E884455 */
const PS3 @local = -4.00555345006794114027e-02; /* 0xBFA48228, 0xB5688F3B */
const PS4 @local =  7.91534994289814532176e-04; /* 0x3F49EFE0, 0x7501B288 */
const PS5 @local =  3.47933107596021167570e-05; /* 0x3F023DE1, 0x0DFDF709 */
const QS1 @local = -2.40339491173441421878e+00; /* 0xC0033A27, 0x1C8A2D4B */
const QS2 @local =  2.02094576023350569471e+00; /* 0x40002AE5, 0x9C598AC8 */
const QS3 @local = -6.88283971605453293030e-01; /* 0xBFE6066C, 0x1B8D0159 */
const QS4 @local =  7.70381505559019352791e-02; /* 0x3FB3B8C5, 0xB12E9282 */

fn double _r(double z) @local
{
	double p = z * (PS0 + z * (PS1 + z * (PS2 + z * (PS3 + z * (PS4 + z * PS5)))));
	double q = 1.0 + z * (QS1 + z * (QS2 + z * (QS3 + z * QS4)));
	return p / q;
}

fn double _asin(double x) @weak @extern("asin") @nostrip
{
	uint hx = x.high_word();
	uint ix = hx & 0x7fffffff;
	switch
	{
		/* |x| >= 1 or nan */
		case ix >= 0x3ff00000:
			uint lx = x.low_word();
			if ((ix-0x3ff00000 | lx) == 0)
			{
				/* asin(1) = +-pi/2 with inexact */
				return x * PIO2_HI + 0x1p-120f;
			}
			return double.nan;
		/* |x| < 0.5 */
		case ix < 0x3fe00000:
		     	/* if 0x1p-1022 <= |x| < 0x1p-26, avoid raising underflow */
			if (ix < 0x3e500000 && ix >= 0x00100000) return x;
			return x + x * _r(x * x);
		/* 1 > |x| >= 0.5 */
		default:
			double z = (1. - math::abs(x)) * 0.5;
			double s = math::sqrt(z);
			double r = _r(z);
			/* if |x| > 0.975 */
			if (ix >= 0x3fef3333)
			{
				x = PIO2_HI - (2. * (s + s * r) - PIO2_LO);
			}
			else
			{
				double f = s;
				f.set_low_word(0);
				double c = (z - f * f) / (s + f);
				x = 0.5 * PIO2_HI - (2. * s * r - (PIO2_LO - 2. * c) - (0.5 * PIO2_HI - 2. * f));
			}
			if (hx >> 31 != 0) return -x;
			return x;
	}
}

/* origin: FreeBSD /usr/src/lib/msun/src/e_asinf.c */
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

const PIO2 @local = 1.570796326794896558e+00;
const float PS0_F @local =  1.6666586697e-01;
const float PS1_F @local = -4.2743422091e-02;
const float PS2_F @local = -8.6563630030e-03;
const float QS1_F @local = -7.0662963390e-01;

fn float _r_f(float z) @local
{
	float p = z * ( PS0_F + z * (PS1_F + z * PS2_F));
	float q = 1.0f + z * QS1_F;
	return p / q;
}

fn float _asinf(float x) @weak @extern("asinf") @nostrip
{
	uint hx = bitcast(x, uint);
	uint ix = hx & 0x7fffffff;
	switch
	{
		/* |x| >= 1 */
		case ix >= 0x3f800000:
		     	if (ix == 0x3f800000)
			{
				/* asin(+-1) = +-pi/2 with inexact */
				return x * (float)PIO2 + 0x1p-120f;
			}
			return float.nan;
		/* |x| < 0.5 */
		case ix < 0x3f000000:
		     	/* if 0x1p-126 <= |x| < 0x1p-12, avoid raising underflow */
			if (ix < 0x39800000 && ix >= 0x00800000) return x;
			return x + x * _r_f(x * x);
		/* 1 > |x| >= 0.5 */
		default:
			float z = (1.f - math::abs(x)) * 0.5f;
			float s = math::sqrt(z);
			x = (float)PIO2 - 2.f * (s + s * _r_f(z));
			if (hx >> 31) return -x;
			return x;
	}
}