module std::math::nolibc @feat(NO_LIBC, C3_MATH);

/* origin: FreeBSD /usr/src/lib/msun/src/s_erf.c, via musl src/math/erf.c */
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

const double ERF_ERX @private  = 8.45062911510467529297e-01; /* 0x3FEB0AC1, 0x60000000 */
// Coefficients for approximation to erf on [0,0.84375]
const double ERF_EFX8 @private =  1.02703333676410069053e+00; /* 0x3FF06EBA, 0x8214DB69 */
const double ERF_PP0 @private  =  1.28379167095512558561e-01; /* 0x3FC06EBA, 0x8214DB68 */
const double ERF_PP1 @private  = -3.25042107247001499370e-01; /* 0xBFD4CD7D, 0x691CB913 */
const double ERF_PP2 @private  = -2.84817495755985104766e-02; /* 0xBF9D2A51, 0xDBD7194F */
const double ERF_PP3 @private  = -5.77027029648944159157e-03; /* 0xBF77A291, 0x236668E4 */
const double ERF_PP4 @private  = -2.37630166566501626084e-05; /* 0xBEF8EAD6, 0x120016AC */
const double ERF_QQ1 @private  =  3.97917223959155352819e-01; /* 0x3FD97779, 0xCDDADC09 */
const double ERF_QQ2 @private  =  6.50222499887672944485e-02; /* 0x3FB0A54C, 0x5536CEBA */
const double ERF_QQ3 @private  =  5.08130628187576562776e-03; /* 0x3F74D022, 0xC4D36B0F */
const double ERF_QQ4 @private  =  1.32494738004321644526e-04; /* 0x3F215DC9, 0x221C1A10 */
const double ERF_QQ5 @private  = -3.96022827877536812320e-06; /* 0xBED09C43, 0x42A26120 */
// Coefficients for approximation to erf in [0.84375,1.25]
const double ERF_PA0 @private  = -2.36211856075265944077e-03; /* 0xBF6359B8, 0xBEF77538 */
const double ERF_PA1 @private  =  4.14856118683748331666e-01; /* 0x3FDA8D00, 0xAD92B34D */
const double ERF_PA2 @private  = -3.72207876035701323847e-01; /* 0xBFD7D240, 0xFBB8C3F1 */
const double ERF_PA3 @private  =  3.18346619901161753674e-01; /* 0x3FD45FCA, 0x805120E4 */
const double ERF_PA4 @private  = -1.10894694282396677476e-01; /* 0xBFBC6398, 0x3D3E28EC */
const double ERF_PA5 @private  =  3.54783043256182359371e-02; /* 0x3FA22A36, 0x599795EB */
const double ERF_PA6 @private  = -2.16637559486879084300e-03; /* 0xBF61BF38, 0x0A96073F */
const double ERF_QA1 @private  =  1.06420880400844228286e-01; /* 0x3FBB3E66, 0x18EEE323 */
const double ERF_QA2 @private  =  5.40397917702171048937e-01; /* 0x3FE14AF0, 0x92EB6F33 */
const double ERF_QA3 @private  =  7.18286544141962662868e-02; /* 0x3FB2635C, 0xD99FE9A7 */
const double ERF_QA4 @private  =  1.26171219808761642112e-01; /* 0x3FC02660, 0xE763351F */
const double ERF_QA5 @private  =  1.36370839120290507362e-02; /* 0x3F8BEDC2, 0x6B51DD1C */
const double ERF_QA6 @private  =  1.19844998467991074170e-02; /* 0x3F888B54, 0x5735151D */
// Coefficients for approximation to erfc in [1.25,1/0.35]
const double ERF_RA0 @private  = -9.86494403484714822705e-03; /* 0xBF843412, 0x600D6435 */
const double ERF_RA1 @private  = -6.93858572707181764372e-01; /* 0xBFE63416, 0xE4BA7360 */
const double ERF_RA2 @private  = -1.05586262253232909814e+01; /* 0xC0251E04, 0x41B0E726 */
const double ERF_RA3 @private  = -6.23753324503260060396e+01; /* 0xC04F300A, 0xE4CBA38D */
const double ERF_RA4 @private  = -1.62396669462573470355e+02; /* 0xC0644CB1, 0x84282266 */
const double ERF_RA5 @private  = -1.84605092906711035994e+02; /* 0xC067135C, 0xEBCCABB2 */
const double ERF_RA6 @private  = -8.12874355063065934246e+01; /* 0xC0545265, 0x57E4D2F2 */
const double ERF_RA7 @private  = -9.81432934416914548592e+00; /* 0xC023A0EF, 0xC69AC25C */
const double ERF_SA1 @private  =  1.96512716674392571292e+01; /* 0x4033A6B9, 0xBD707687 */
const double ERF_SA2 @private  =  1.37657754143519042600e+02; /* 0x4061350C, 0x526AE721 */
const double ERF_SA3 @private  =  4.34565877475229228821e+02; /* 0x407B290D, 0xD58A1A71 */
const double ERF_SA4 @private  =  6.45387271733267880336e+02; /* 0x40842B19, 0x21EC2868 */
const double ERF_SA5 @private  =  4.29008140027567833386e+02; /* 0x407AD021, 0x57700314 */
const double ERF_SA6 @private  =  1.08635005541779435134e+02; /* 0x405B28A3, 0xEE48AE2C */
const double ERF_SA7 @private  =  6.57024977031928170135e+00; /* 0x401A47EF, 0x8E484A93 */
const double ERF_SA8 @private  = -6.04244152148580987438e-02; /* 0xBFAEEFF2, 0xEE749A62 */
// Coefficients for approximation to erfc in [1/.35,28]
const double ERF_RB0 @private  = -9.86494292470009928597e-03; /* 0xBF843412, 0x39E86F4A */
const double ERF_RB1 @private  = -7.99283237680523006574e-01; /* 0xBFE993BA, 0x70C285DE */
const double ERF_RB2 @private  = -1.77579549177547519889e+01; /* 0xC031C209, 0x555F995A */
const double ERF_RB3 @private  = -1.60636384855821916062e+02; /* 0xC064145D, 0x43C5ED98 */
const double ERF_RB4 @private  = -6.37566443368389627722e+02; /* 0xC083EC88, 0x1375F228 */
const double ERF_RB5 @private  = -1.02509513161107724954e+03; /* 0xC0900461, 0x6A2E5992 */
const double ERF_RB6 @private  = -4.83519191608651397019e+02; /* 0xC07E384E, 0x9BDC383F */
const double ERF_SB1 @private  =  3.03380607434824582924e+01; /* 0x403E568B, 0x261D5190 */
const double ERF_SB2 @private  =  3.25792512996573918826e+02; /* 0x40745CAE, 0x221B9F0A */
const double ERF_SB3 @private  =  1.53672958608443695994e+03; /* 0x409802EB, 0x189D5118 */
const double ERF_SB4 @private  =  3.19985821950859553908e+03; /* 0x40A8FFB7, 0x688C246A */
const double ERF_SB5 @private  =  2.55305040643316442583e+03; /* 0x40A3F219, 0xCEDF3BE6 */
const double ERF_SB6 @private  =  4.74528541206955367215e+02; /* 0x407DA874, 0xE79FE763 */
const double ERF_SB7 @private  = -2.24409524465858183362e+01; /* 0xC03670E2, 0x42712D62 */

fn double erfc1(double x) @private
{
	double s = _fabs(x) - 1;
	double p = ERF_PA0 + s * (ERF_PA1 + s * (ERF_PA2 + s * (ERF_PA3 + s * (ERF_PA4 + s * (ERF_PA5 + s * ERF_PA6)))));
	double q = 1 + s * (ERF_QA1 + s * (ERF_QA2 + s * (ERF_QA3 + s * (ERF_QA4 + s * (ERF_QA5 + s * ERF_QA6)))));
	return 1 - ERF_ERX - p / q;
}

fn double erfc2(uint ix, double x) @private
{
	double r @noinit;
	double s @noinit;
	if (ix < 0x3ff40000) return erfc1(x); /* |x| < 1.25 */

	x = _fabs(x);
	double z = 1 / (x * x);
	if (ix < 0x4006db6d)
	{
		/* |x| < 1/.35 ~ 2.85714 */
		r = ERF_RA0 + z * (ERF_RA1 + z * (ERF_RA2 + z * (ERF_RA3 + z * (ERF_RA4 + z * (
			ERF_RA5 + z * (ERF_RA6 + z * ERF_RA7))))));
		s = 1.0 + z * (ERF_SA1 + z * (ERF_SA2 + z * (ERF_SA3 + z * (ERF_SA4 + z * (
			ERF_SA5 + z * (ERF_SA6 + z * (ERF_SA7 + z * ERF_SA8)))))));
	}
	else
	{
		/* |x| > 1/.35 */
		r = ERF_RB0 + z * (ERF_RB1 + z * (ERF_RB2 + z * (ERF_RB3 + z * (ERF_RB4 + z * (
			ERF_RB5 + z * ERF_RB6)))));
		s = 1.0 + z * (ERF_SB1 + z * (ERF_SB2 + z * (ERF_SB3 + z * (ERF_SB4 + z * (
			ERF_SB5 + z * (ERF_SB6 + z * ERF_SB7))))));
	}
	double h = bitcast(bitcast(x, ulong) & 0xffffffff00000000, double);
	return exp(-h * h - 0.5625) * exp((h - x) * (h + x) + r / s) / x;
}

fn double _erf(double x) @weak @cname("erf") @nostrip
{
	uint ix = x.high_word();
	int sign = (int)(ix >> 31);
	ix &= 0x7fffffff;
	if (ix >= 0x7ff00000)
	{
		/* erf(nan)=nan, erf(+-inf)=+-1 */
		return (double)(1 - 2 * sign) + 1 / x;
	}
	double y @noinit;
	if (ix < 0x3feb0000)
	{
		/* |x| < 0.84375 */
		if (ix < 0x3e300000)
		{
			/* |x| < 2**-28, avoid underflow */
			return 0.125 * (8 * x + ERF_EFX8 * x);
		}
		double z = x * x;
		double r = ERF_PP0 + z * (ERF_PP1 + z * (ERF_PP2 + z * (ERF_PP3 + z * ERF_PP4)));
		double s = 1.0 + z * (ERF_QQ1 + z * (ERF_QQ2 + z * (ERF_QQ3 + z * (ERF_QQ4 + z * ERF_QQ5))));
		y = r / s;
		return x + x * y;
	}
	if (ix < 0x40180000)
	{
		/* 0.84375 <= |x| < 6 */
		y = 1 - erfc2(ix, x);
	}
	else
	{
		y = 1 - 0x1p-1022;
	}
	return sign ? -y : y;
}

const float ERFF_ERX @private  =  8.4506291151e-01f; /* 0x3f58560b */
// Coefficients for approximation to erf on [0,0.84375]
const float ERFF_EFX8 @private =  1.0270333290e+00f; /* 0x3f8375d4 */
const float ERFF_PP0 @private  =  1.2837916613e-01f; /* 0x3e0375d4 */
const float ERFF_PP1 @private  = -3.2504209876e-01f; /* 0xbea66beb */
const float ERFF_PP2 @private  = -2.8481749818e-02f; /* 0xbce9528f */
const float ERFF_PP3 @private  = -5.7702702470e-03f; /* 0xbbbd1489 */
const float ERFF_PP4 @private  = -2.3763017452e-05f; /* 0xb7c756b1 */
const float ERFF_QQ1 @private  =  3.9791721106e-01f; /* 0x3ecbbbce */
const float ERFF_QQ2 @private  =  6.5022252500e-02f; /* 0x3d852a63 */
const float ERFF_QQ3 @private  =  5.0813062117e-03f; /* 0x3ba68116 */
const float ERFF_QQ4 @private  =  1.3249473704e-04f; /* 0x390aee49 */
const float ERFF_QQ5 @private  = -3.9602282413e-06f; /* 0xb684e21a */
// Coefficients for approximation to erf in [0.84375,1.25]
const float ERFF_PA0 @private  = -2.3621185683e-03f; /* 0xbb1acdc6 */
const float ERFF_PA1 @private  =  4.1485610604e-01f; /* 0x3ed46805 */
const float ERFF_PA2 @private  = -3.7220788002e-01f; /* 0xbebe9208 */
const float ERFF_PA3 @private  =  3.1834661961e-01f; /* 0x3ea2fe54 */
const float ERFF_PA4 @private  = -1.1089469492e-01f; /* 0xbde31cc2 */
const float ERFF_PA5 @private  =  3.5478305072e-02f; /* 0x3d1151b3 */
const float ERFF_PA6 @private  = -2.1663755178e-03f; /* 0xbb0df9c0 */
const float ERFF_QA1 @private  =  1.0642088205e-01f; /* 0x3dd9f331 */
const float ERFF_QA2 @private  =  5.4039794207e-01f; /* 0x3f0a5785 */
const float ERFF_QA3 @private  =  7.1828655899e-02f; /* 0x3d931ae7 */
const float ERFF_QA4 @private  =  1.2617121637e-01f; /* 0x3e013307 */
const float ERFF_QA5 @private  =  1.3637083583e-02f; /* 0x3c5f6e13 */
const float ERFF_QA6 @private  =  1.1984500103e-02f; /* 0x3c445aa3 */
// Coefficients for approximation to erfc in [1.25,1/0.35]
const float ERFF_RA0 @private  = -9.8649440333e-03f; /* 0xbc21a093 */
const float ERFF_RA1 @private  = -6.9385856390e-01f; /* 0xbf31a0b7 */
const float ERFF_RA2 @private  = -1.0558626175e+01f; /* 0xc128f022 */
const float ERFF_RA3 @private  = -6.2375331879e+01f; /* 0xc2798057 */
const float ERFF_RA4 @private  = -1.6239666748e+02f; /* 0xc322658c */
const float ERFF_RA5 @private  = -1.8460508728e+02f; /* 0xc3389ae7 */
const float ERFF_RA6 @private  = -8.1287437439e+01f; /* 0xc2a2932b */
const float ERFF_RA7 @private  = -9.8143291473e+00f; /* 0xc11d077e */
const float ERFF_SA1 @private  =  1.9651271820e+01f; /* 0x419d35ce */
const float ERFF_SA2 @private  =  1.3765776062e+02f; /* 0x4309a863 */
const float ERFF_SA3 @private  =  4.3456588745e+02f; /* 0x43d9486f */
const float ERFF_SA4 @private  =  6.4538726807e+02f; /* 0x442158c9 */
const float ERFF_SA5 @private  =  4.2900814819e+02f; /* 0x43d6810b */
const float ERFF_SA6 @private  =  1.0863500214e+02f; /* 0x42d9451f */
const float ERFF_SA7 @private  =  6.5702495575e+00f; /* 0x40d23f7c */
const float ERFF_SA8 @private  = -6.0424413532e-02f; /* 0xbd777f97 */
// Coefficients for approximation to erfc in [1/.35,28]
const float ERFF_RB0 @private  = -9.8649431020e-03f; /* 0xbc21a092 */
const float ERFF_RB1 @private  = -7.9928326607e-01f; /* 0xbf4c9dd4 */
const float ERFF_RB2 @private  = -1.7757955551e+01f; /* 0xc18e104b */
const float ERFF_RB3 @private  = -1.6063638306e+02f; /* 0xc320a2ea */
const float ERFF_RB4 @private  = -6.3756646729e+02f; /* 0xc41f6441 */
const float ERFF_RB5 @private  = -1.0250950928e+03f; /* 0xc480230b */
const float ERFF_RB6 @private  = -4.8351919556e+02f; /* 0xc3f1c275 */
const float ERFF_SB1 @private  =  3.0338060379e+01f; /* 0x41f2b459 */
const float ERFF_SB2 @private  =  3.2579251099e+02f; /* 0x43a2e571 */
const float ERFF_SB3 @private  =  1.5367296143e+03f; /* 0x44c01759 */
const float ERFF_SB4 @private  =  3.1998581543e+03f; /* 0x4547fdbb */
const float ERFF_SB5 @private  =  2.5530502930e+03f; /* 0x451f90ce */
const float ERFF_SB6 @private  =  4.7452853394e+02f; /* 0x43ed43a7 */
const float ERFF_SB7 @private  = -2.2440952301e+01f; /* 0xc1b38712 */

fn float erfc1f(float x) @private
{
	float s = _fabsf(x) - 1;
	float p = ERFF_PA0 + s * (ERFF_PA1 + s * (ERFF_PA2 + s * (ERFF_PA3 + s * (ERFF_PA4 + s * (ERFF_PA5 + s * ERFF_PA6)))));
	float q = 1 + s * (ERFF_QA1 + s * (ERFF_QA2 + s * (ERFF_QA3 + s * (ERFF_QA4 + s * (ERFF_QA5 + s * ERFF_QA6)))));
	return 1 - ERFF_ERX - p / q;
}

fn float erfc2f(uint ix, float x) @private
{
	float r @noinit;
	float s @noinit;
	if (ix < 0x3fa00000) return erfc1f(x); /* |x| < 1.25 */

	x = _fabsf(x);
	float z = 1 / (x * x);
	if (ix < 0x4036db6d)
	{
		/* |x| < 1/0.35 */
		r = ERFF_RA0 + z * (ERFF_RA1 + z * (ERFF_RA2 + z * (ERFF_RA3 + z * (ERFF_RA4 + z * (
			ERFF_RA5 + z * (ERFF_RA6 + z * ERFF_RA7))))));
		s = 1.0f + z * (ERFF_SA1 + z * (ERFF_SA2 + z * (ERFF_SA3 + z * (ERFF_SA4 + z * (
			ERFF_SA5 + z * (ERFF_SA6 + z * (ERFF_SA7 + z * ERFF_SA8)))))));
	}
	else
	{
		/* |x| >= 1/0.35 */
		r = ERFF_RB0 + z * (ERFF_RB1 + z * (ERFF_RB2 + z * (ERFF_RB3 + z * (ERFF_RB4 + z * (
			ERFF_RB5 + z * ERFF_RB6)))));
		s = 1.0f + z * (ERFF_SB1 + z * (ERFF_SB2 + z * (ERFF_SB3 + z * (ERFF_SB4 + z * (
			ERFF_SB5 + z * (ERFF_SB6 + z * ERFF_SB7))))));
	}
	float h = bitcast(bitcast(x, uint) & 0xffffe000, float);
	return expf(-h * h - 0.5625f) * expf((h - x) * (h + x) + r / s) / x;
}

fn float _erff(float x) @weak @cname("erff") @nostrip
{
	uint ix = bitcast(x, uint);
	int sign = (int)(ix >> 31);
	ix &= 0x7fffffff;
	if (ix >= 0x7f800000)
	{
		/* erf(nan)=nan, erf(+-inf)=+-1 */
		return (float)(1 - 2 * sign) + 1 / x;
	}
	float y @noinit;
	if (ix < 0x3f580000)
	{
		/* |x| < 0.84375 */
		if (ix < 0x31800000)
		{
			/* |x| < 2**-28, avoid underflow */
			return 0.125f * (8 * x + ERFF_EFX8 * x);
		}
		float z = x * x;
		float r = ERFF_PP0 + z * (ERFF_PP1 + z * (ERFF_PP2 + z * (ERFF_PP3 + z * ERFF_PP4)));
		float s = 1 + z * (ERFF_QQ1 + z * (ERFF_QQ2 + z * (ERFF_QQ3 + z * (ERFF_QQ4 + z * ERFF_QQ5))));
		y = r / s;
		return x + x * y;
	}
	if (ix < 0x40c00000)
	{
		/* |x| < 6 */
		y = 1 - erfc2f(ix, x);
	}
	else
	{
		y = 1 - 0x1p-120f;
	}
	return sign ? -y : y;
}
