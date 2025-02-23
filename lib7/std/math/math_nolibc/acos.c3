module std::math::nolibc @if(env::NO_LIBC || $feature(C3_MATH));

/* origin: FreeBSD /usr/src/lib/msun/src/e_acos.c */
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

fn double _acos(double x) @weak @extern("acos") @nostrip
{
	uint hx = x.high_word();
	uint ix = hx & 0x7fffffff;
	switch
	{
		/* |x| >= 1 or nan */
		case ix >= 0x3ff00000:
		     	uint lx = x.low_word();
		     	if ((ix - 0x3ff00000 | lx) == 0)
		     	{
				/* acos(1)=0, acos(-1)=pi */
				if (hx >> 31) return 2. * PIO2_HI + 0x1p-120f;
				return 0.;
			}	
			return double.nan;
		/* |x| < 0.5 */
		case ix < 0x3fe00000:
		     	/* |x| < 2**-57 */
		     	if (ix <= 0x3c600000) return PIO2_HI + 0x1p-120f;
			return PIO2_HI - (x - (PIO2_LO - x * _r(x * x)));
		/* x < -0.5 */
		case (hx >> 31) != 0:
		     	double z = (1. + x) * 0.5;
			double s = math::sqrt(z);
			double w = _r(z) * s - PIO2_LO;
			return 2. * (PIO2_HI - (s + w));
		/* x > 0.5 */
		default:
			double z = (1. - x) * 0.5;
			double s = math::sqrt(z);
			double df = s;
			df.set_low_word(0);
			double c = (z - df * df) / (s + df);
			double w = _r(z) * s + c;
			return 2. * (df + w);	
	}
}

/* origin: FreeBSD /usr/src/lib/msun/src/e_acosf.c */
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

const float PIO2_HI_F @local = 1.5707962513e+00; /* 0x3fc90fda */
const float PIO2_LO_F @local = 7.5497894159e-08; /* 0x33a22168 */
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

fn float _acosf(float x) @weak @extern("acosf") @nostrip
{
	uint hx = bitcast(x, uint);
	uint ix = hx & 0x7fffffff;
	switch
	{
		/* |x| >= 1 or nan */
		case ix >= 0x3f800000:
		     	if (ix == 0x3f800000)
			{
				if (hx >> 31) return 2.f * PIO2_HI_F + 0x1p-120f;
				return 0;
			}
			return float.nan;
		/* |x| < 0.5 */
		case ix < 0x3f000000:
		     	/* |x| < 2**-26 */
		     	if (ix <= 0x32800000) return PIO2_HI_F + 0x1p-120f;
			return PIO2_HI_F - (x - (PIO2_LO_F - x * _r_f(x * x)));
		/* x < -0.5 */
		case (hx >> 31) != 0:
			float z = (1.f + x) * 0.5f;
			float s = math::sqrt(z);
			float w = _r_f(z) * s - PIO2_LO_F;
			return 2.f * (PIO2_HI_F - (s + w));
		/* x > 0.5 */
		default:
			float z = (1.f - x) * 0.5f;
			float s = math::sqrt(z);
			float df = s;
			uint idf = df.word();
			df.set_word(idf & 0xfffff000);
			float c = (z - df * df) / (s + df);
			float w = _r_f(z) * s + c;
			return 2.f * (df + w);
	}
}