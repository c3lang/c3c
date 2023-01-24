module std::math::nolibc::trig;

$if (!env::COMPILER_LIBC_AVAILABLE):


/* origin: FreeBSD /usr/src/lib/msun/src/s_sinf.c */
/*
 * Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.
 * Optimized by Bruce D. Evans.
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

private const double S1PI2_F = math::PI_2; /* 0x3FF921FB, 0x54442D18 */
private const double S2PI2_F = math::PI; /* 0x400921FB, 0x54442D18 */
private const double S3PI2_F = math::PI + math::PI_2; /* 0x4012D97C, 0x7F3321D2 */
private const double S4PI2_F = math::PI + math::PI; /* 0x401921FB, 0x54442D18 */

fn float _sinf(float x) @weak @extname("sinf")
{
	uint ix = bitcast(x, uint);
	int sign = ix >> 31;
	ix &= 0x7fffffff;
	switch
	{
		case ix <= 0x3f490fda:
			// |x| ~<= pi/4
			if (ix < 0x39800000)
			{
				/* |x| < 2**-12 */
				// raise inexact if x!=0 and underflow if subnormal
				if (ix < 0x00800000)
				{
					@volatile_load(x) / 0x1p120f;
				}
				else
				{
					@volatile_load(x) + 0x1p120f;
				}
				return x;
			}
			return __sindf(x);
		case ix <= 0x407b53d1:
		    // |x| ~<= 5*pi/4
			if (ix <= 0x4016cbe3)
			{
				// |x| ~<= 3pi/4
				if (sign) return -__cosdf(x + S1PI2_F);
				return __cosdf(x - S1PI2_F);
			}
			return __sindf(sign ? -(x + S2PI2_F) : -(x - S2PI2_F));
		case ix <= 0x40e231d5:
		    // |x| ~<= 9*pi/4
			if (ix <= 0x40afeddf)
			{
				// |x| ~<= 7*pi/4
				return sign ? __cosdf(x + S3PI2_F) : -__cosdf(x - S3PI2_F);
			}
			return __sindf(sign ? x + S4PI2_F : x - S4PI2_F);
		case ix >= 0x7f800000:
			// sin(Inf or NaN) is NaN
			return x - x;
	}
	/* general argument reduction needed */
	double y = void;
	int n = rempi::__rem_pio2f(x, &y);
	switch (n & 3)
	{
		case 0: return  __sindf(y);
		case 1: return  __cosdf(y);
		case 2: return  __sindf(-y);
		default: return -__cosdf(y);
	}
	unreachable();
}

/* origin: FreeBSD /usr/src/lib/msun/src/k_sinf.c */
/*
 * Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.
 * Optimized by Bruce D. Evans.
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


/* |sin(x)/x - s(x)| < 2**-37.5 (~[-4.89e-12, 4.824e-12]). */
private const double S1 = -0x15555554cbac77.0p-55; /* -0.166666666416265235595 */
private const double S2 =  0x111110896efbb2.0p-59; /*  0.0083333293858894631756 */
private const double S3 = -0x1a00f9e2cae774.0p-65; /* -0.000198393348360966317347 */
private const double S4 =  0x16cd878c3b46a7.0p-71; /*  0.0000027183114939898219064 */

fn float __sindf(double x)
{
	double z = x * x;
	double w = z * z;
	double r = S3 + z * S4;
	double s = z * x;
	return (float)((x + s * (S1 + z * S2)) +  s * w * r);
}

/* origin: FreeBSD /usr/src/lib/msun/src/k_cosf.c */
/*
 * Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.
 * Debugged and optimized by Bruce D. Evans.
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
/* |cos(x) - c(x)| < 2**-34.1 (~[-5.37e-11, 5.295e-11]). */

private const double C0  = -0x1ffffffd0c5e81.0p-54; /* -0.499999997251031003120 */
private const double C1  =  0x155553e1053a42.0p-57; /*  0.0416666233237390631894 */
private const double C2  = -0x16c087e80f1e27.0p-62; /* -0.00138867637746099294692 */
private const double C3  =  0x199342e0ee5069.0p-68; /*  0.0000243904487962774090654 */

fn float __cosdf(double x)
{
	/* Try to optimize for parallel evaluation as in __tandf.c. */
	double z = x * x;
	double w = z * z;
	double r = C2 + z * C3;
	return (float)(((1.0f + z * C0) + w * C1) + (w * z) * r);
}

fn float cosf_broken(float x) @extname("cosf") @weak
{
	unreachable("'cosf' not supported");
}

fn double sin_broken(double x) @extname("sin") @weak
{
	unreachable("'sin' not supported");
}
fn double cos_broken(double x) @extname("cos") @weak
{
	unreachable("'cos' not supported");
}

fn float roundf_broken(float x) @extname("roundf") @weak
{
	unreachable("'roundf' not supported");
}

fn double round_broken(double x) @extname("round") @weak
{
	unreachable("'round' not supported");
}

fn float powf_broken(float x) @extname("powf") @weak
{
	unreachable("'powf' not supported");
}

$endif;