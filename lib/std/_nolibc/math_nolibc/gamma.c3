module std::math::nolibc @feat(NO_LIBC, C3_MATH);

/* origin: FreeBSD /usr/src/lib/msun/src/e_lgamma_r.c, via musl src/math/lgamma_r.c */
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


const double LG_PI @private  =  3.14159265358979311600e+00; /* 0x400921FB, 0x54442D18 */
const double LG_A0 @private  =  7.72156649015328655494e-02; /* 0x3FB3C467, 0xE37DB0C8 */
const double LG_A1 @private  =  3.22467033424113591611e-01; /* 0x3FD4A34C, 0xC4A60FAD */
const double LG_A2 @private  =  6.73523010531292681824e-02; /* 0x3FB13E00, 0x1A5562A7 */
const double LG_A3 @private  =  2.05808084325167332806e-02; /* 0x3F951322, 0xAC92547B */
const double LG_A4 @private  =  7.38555086081402883957e-03; /* 0x3F7E404F, 0xB68FEFE8 */
const double LG_A5 @private  =  2.89051383673415629091e-03; /* 0x3F67ADD8, 0xCCB7926B */
const double LG_A6 @private  =  1.19270763183362067845e-03; /* 0x3F538A94, 0x116F3F5D */
const double LG_A7 @private  =  5.10069792153511336608e-04; /* 0x3F40B6C6, 0x89B99C00 */
const double LG_A8 @private  =  2.20862790713908385557e-04; /* 0x3F2CF2EC, 0xED10E54D */
const double LG_A9 @private  =  1.08011567247583939954e-04; /* 0x3F1C5088, 0x987DFB07 */
const double LG_A10 @private =  2.52144565451257326939e-05; /* 0x3EFA7074, 0x428CFA52 */
const double LG_A11 @private =  4.48640949618915160150e-05; /* 0x3F07858E, 0x90A45837 */
const double LG_TC @private  =  1.46163214496836224576e+00; /* 0x3FF762D8, 0x6356BE3F */
const double LG_TF @private  = -1.21486290535849611461e-01; /* 0xBFBF19B9, 0xBCC38A42 */
/* tt = -(tail of tf) */
const double LG_TT @private  = -3.63867699703950536541e-18; /* 0xBC50C7CA, 0xA48A971F */
const double LG_T0 @private  =  4.83836122723810047042e-01; /* 0x3FDEF72B, 0xC8EE38A2 */
const double LG_T1 @private  = -1.47587722994593911752e-01; /* 0xBFC2E427, 0x8DC6C509 */
const double LG_T2 @private  =  6.46249402391333854778e-02; /* 0x3FB08B42, 0x94D5419B */
const double LG_T3 @private  = -3.27885410759859649565e-02; /* 0xBFA0C9A8, 0xDF35B713 */
const double LG_T4 @private  =  1.79706750811820387126e-02; /* 0x3F9266E7, 0x970AF9EC */
const double LG_T5 @private  = -1.03142241298341437450e-02; /* 0xBF851F9F, 0xBA91EC6A */
const double LG_T6 @private  =  6.10053870246291332635e-03; /* 0x3F78FCE0, 0xE370E344 */
const double LG_T7 @private  = -3.68452016781138256760e-03; /* 0xBF6E2EFF, 0xB3E914D7 */
const double LG_T8 @private  =  2.25964780900612472250e-03; /* 0x3F6282D3, 0x2E15C915 */
const double LG_T9 @private  = -1.40346469989232843813e-03; /* 0xBF56FE8E, 0xBF2D1AF1 */
const double LG_T10 @private =  8.81081882437654011382e-04; /* 0x3F4CDF0C, 0xEF61A8E9 */
const double LG_T11 @private = -5.38595305356740546715e-04; /* 0xBF41A610, 0x9C73E0EC */
const double LG_T12 @private =  3.15632070903625950361e-04; /* 0x3F34AF6D, 0x6C0EBBF7 */
const double LG_T13 @private = -3.12754168375120860518e-04; /* 0xBF347F24, 0xECC38C38 */
const double LG_T14 @private =  3.35529192635519073543e-04; /* 0x3F35FD3E, 0xE8C2D3F4 */
const double LG_U0 @private  = -7.72156649015328655494e-02; /* 0xBFB3C467, 0xE37DB0C8 */
const double LG_U1 @private  =  6.32827064025093366517e-01; /* 0x3FE4401E, 0x8B005DFF */
const double LG_U2 @private  =  1.45492250137234768737e+00; /* 0x3FF7475C, 0xD119BD6F */
const double LG_U3 @private  =  9.77717527963372745603e-01; /* 0x3FEF4976, 0x44EA8450 */
const double LG_U4 @private  =  2.28963728064692451092e-01; /* 0x3FCD4EAE, 0xF6010924 */
const double LG_U5 @private  =  1.33810918536787660377e-02; /* 0x3F8B678B, 0xBF2BAB09 */
const double LG_V1 @private  =  2.45597793713041134822e+00; /* 0x4003A5D7, 0xC2BD619C */
const double LG_V2 @private  =  2.12848976379893395361e+00; /* 0x40010725, 0xA42B18F5 */
const double LG_V3 @private  =  7.69285150456672783825e-01; /* 0x3FE89DFB, 0xE45050AF */
const double LG_V4 @private  =  1.04222645593369134254e-01; /* 0x3FBAAE55, 0xD6537C88 */
const double LG_V5 @private  =  3.21709242282423911810e-03; /* 0x3F6A5ABB, 0x57D0CF61 */
const double LG_S0 @private  = -7.72156649015328655494e-02; /* 0xBFB3C467, 0xE37DB0C8 */
const double LG_S1 @private  =  2.14982415960608852501e-01; /* 0x3FCB848B, 0x36E20878 */
const double LG_S2 @private  =  3.25778796408930981787e-01; /* 0x3FD4D98F, 0x4F139F59 */
const double LG_S3 @private  =  1.46350472652464452805e-01; /* 0x3FC2BB9C, 0xBEE5F2F7 */
const double LG_S4 @private  =  2.66422703033638609560e-02; /* 0x3F9B481C, 0x7E939961 */
const double LG_S5 @private  =  1.84028451407337715652e-03; /* 0x3F5E26B6, 0x7368F239 */
const double LG_S6 @private  =  3.19475326584100867617e-05; /* 0x3F00BFEC, 0xDD17E945 */
const double LG_R1 @private  =  1.39200533467621045958e+00; /* 0x3FF645A7, 0x62C4AB74 */
const double LG_R2 @private  =  7.21935547567138069525e-01; /* 0x3FE71A18, 0x93D3DCDC */
const double LG_R3 @private  =  1.71933865632803078993e-01; /* 0x3FC601ED, 0xCCFBDF27 */
const double LG_R4 @private  =  1.86459191715652901344e-02; /* 0x3F9317EA, 0x742ED475 */
const double LG_R5 @private  =  7.77942496381893596434e-04; /* 0x3F497DDA, 0xCA41A95B */
const double LG_R6 @private  =  7.32668430744625636189e-06; /* 0x3EDEBAF7, 0xA5B38140 */
const double LG_W0 @private  =  4.18938533204672725052e-01; /* 0x3FDACFE3, 0x90C97D69 */
const double LG_W1 @private  =  8.33333333333329678849e-02; /* 0x3FB55555, 0x5555553B */
const double LG_W2 @private  = -2.77777777728775536470e-03; /* 0xBF66C16C, 0x16B02E5C */
const double LG_W3 @private  =  7.93650558643019558500e-04; /* 0x3F4A019F, 0x98CF38B6 */
const double LG_W4 @private  = -5.95187557450339963135e-04; /* 0xBF4380CB, 0x8C0FE741 */
const double LG_W5 @private  =  8.36339918996282139126e-04; /* 0x3F4B67BA, 0x4CDAD5D1 */
const double LG_W6 @private  = -1.63092934096575273989e-03; /* 0xBF5AB89D, 0x0B9E43E4 */

<*
 sin(pi*x) assuming x > 2^-100, if sin(pi*x)==0 the sign is arbitrary.
 Shared by lgamma and tgamma (musl's two copies compute the same values).
*>
fn double sin_pi(double x) @private
{
	/* spurious inexact if odd int */
	x = 2.0 * (x * 0.5 - _floor(x * 0.5));  /* x mod 2.0 */

	int n = (int)(x * 4.0);
	n = (n + 1) / 2;
	x -= (double)n * 0.5;
	x *= LG_PI;

	switch (n)
	{
		case 1: return __cos(x, 0.0);
		case 2: return __sin(-x, 0.0, 0);
		case 3: return -__cos(x, 0.0);
		default: return __sin(x, 0.0, 0); /* case 0 and 4 */
	}
}

fn double _lgamma_r(double x, int* signgamp) @weak @cname("lgamma_r") @nostrip
{
	ulong u = bitcast(x, ulong);
	double t @noinit;
	double y @noinit;
	double z @noinit;
	double nadj = 0;
	double p @noinit;
	double p1 @noinit;
	double p2 @noinit;
	double p3 @noinit;
	double q @noinit;
	double r @noinit;
	double w @noinit;
	int i @noinit;

	/* purge off +-inf, NaN, +-0, tiny and negative arguments */
	*signgamp = 1;
	int sign = (int)(u >> 63);
	uint ix = (uint)((u >> 32) & 0x7fffffff);
	if (ix >= 0x7ff00000) return x * x;
	if (ix < ((0x3ff - 70) << 20))
	{
		/* |x|<2**-70, return -log(|x|) */
		if (sign)
		{
			x = -x;
			*signgamp = -1;
		}
		return -log(x);
	}
	if (sign)
	{
		x = -x;
		t = sin_pi(x);
		if (t == 0.0) return 1.0 / (x - x); /* -integer */
		if (t > 0.0)
		{
			*signgamp = -1;
		}
		else
		{
			t = -t;
		}
		nadj = log(LG_PI / (t * x));
	}

	switch
	{
		case (ix == 0x3ff00000 || ix == 0x40000000) && (uint)u == 0:
			/* purge off 1 and 2 */
			r = 0;
		case ix < 0x40000000:
			/* for x < 2.0 */
			if (ix <= 0x3feccccc)
			{
				/* lgamma(x) = lgamma(x+1)-log(x) */
				r = -log(x);
				switch
				{
					case ix >= 0x3FE76944:
						y = 1.0 - x;
						i = 0;
					case ix >= 0x3FCDA661:
						y = x - (LG_TC - 1.0);
						i = 1;
					default:
						y = x;
						i = 2;
				}
			}
			else
			{
				r = 0.0;
				switch
				{
					case ix >= 0x3FFBB4C3:
						/* [1.7316,2] */
						y = 2.0 - x;
						i = 0;
					case ix >= 0x3FF3B4C4:
						/* [1.23,1.73] */
						y = x - LG_TC;
						i = 1;
					default:
						y = x - 1.0;
						i = 2;
				}
			}
			switch (i)
			{
				case 0:
					z = y * y;
					p1 = LG_A0 + z * (LG_A2 + z * (LG_A4 + z * (LG_A6 + z * (LG_A8 + z * LG_A10))));
					p2 = z * (LG_A1 + z * (LG_A3 + z * (LG_A5 + z * (LG_A7 + z * (LG_A9 + z * LG_A11)))));
					p = y * p1 + p2;
					r += (p - 0.5 * y);
				case 1:
					z = y * y;
					w = z * y;
					p1 = LG_T0 + w * (LG_T3 + w * (LG_T6 + w * (LG_T9 + w * LG_T12)));    /* parallel comp */
					p2 = LG_T1 + w * (LG_T4 + w * (LG_T7 + w * (LG_T10 + w * LG_T13)));
					p3 = LG_T2 + w * (LG_T5 + w * (LG_T8 + w * (LG_T11 + w * LG_T14)));
					p = z * p1 - (LG_TT - w * (p2 + y * p3));
					r += LG_TF + p;
				default:
					p1 = y * (LG_U0 + y * (LG_U1 + y * (LG_U2 + y * (LG_U3 + y * (LG_U4 + y * LG_U5)))));
					p2 = 1.0 + y * (LG_V1 + y * (LG_V2 + y * (LG_V3 + y * (LG_V4 + y * LG_V5))));
					r += -0.5 * y + p1 / p2;
			}
		case ix < 0x40200000:
			/* x < 8.0 */
			i = (int)x;
			y = x - (double)i;
			p = y * (LG_S0 + y * (LG_S1 + y * (LG_S2 + y * (LG_S3 + y * (LG_S4 + y * (LG_S5 + y * LG_S6))))));
			q = 1.0 + y * (LG_R1 + y * (LG_R2 + y * (LG_R3 + y * (LG_R4 + y * (LG_R5 + y * LG_R6)))));
			r = 0.5 * y + p / q;
			if (i >= 3)
			{
				/* lgamma(1+s) = log(s) + lgamma(s) */
				z = 1.0;
				for (int k = i; k >= 3; k--) z *= y + (double)(k - 1);
				r += log(z);
			}
		case ix < 0x43900000:
			/* 8.0 <= x < 2**58 */
			t = log(x);
			z = 1.0 / x;
			y = z * z;
			w = LG_W0 + z * (LG_W1 + y * (LG_W2 + y * (LG_W3 + y * (LG_W4 + y * (LG_W5 + y * LG_W6)))));
			r = (x - 0.5) * (t - 1.0) + w;
		default:
			/* 2**58 <= x <= inf */
			r = x * (log(x) - 1.0);
	}
	if (sign) r = nadj - r;
	return r;
}

fn double _lgamma(double x) @weak @cname("lgamma") @nostrip
{
	int sign;
	return _lgamma_r(x, &sign);
}

const float LGF_PI @private  =  3.1415927410e+00f; /* 0x40490fdb */
const float LGF_A0 @private  =  7.7215664089e-02f; /* 0x3d9e233f */
const float LGF_A1 @private  =  3.2246702909e-01f; /* 0x3ea51a66 */
const float LGF_A2 @private  =  6.7352302372e-02f; /* 0x3d89f001 */
const float LGF_A3 @private  =  2.0580807701e-02f; /* 0x3ca89915 */
const float LGF_A4 @private  =  7.3855509982e-03f; /* 0x3bf2027e */
const float LGF_A5 @private  =  2.8905137442e-03f; /* 0x3b3d6ec6 */
const float LGF_A6 @private  =  1.1927076848e-03f; /* 0x3a9c54a1 */
const float LGF_A7 @private  =  5.1006977446e-04f; /* 0x3a05b634 */
const float LGF_A8 @private  =  2.2086278477e-04f; /* 0x39679767 */
const float LGF_A9 @private  =  1.0801156895e-04f; /* 0x38e28445 */
const float LGF_A10 @private =  2.5214456400e-05f; /* 0x37d383a2 */
const float LGF_A11 @private =  4.4864096708e-05f; /* 0x383c2c75 */
const float LGF_TC @private  =  1.4616321325e+00f; /* 0x3fbb16c3 */
const float LGF_TF @private  = -1.2148628384e-01f; /* 0xbdf8cdcd */
/* tt = -(tail of tf) */
const float LGF_TT @private  =  6.6971006518e-09f; /* 0x31e61c52 */
const float LGF_T0 @private  =  4.8383611441e-01f; /* 0x3ef7b95e */
const float LGF_T1 @private  = -1.4758771658e-01f; /* 0xbe17213c */
const float LGF_T2 @private  =  6.4624942839e-02f; /* 0x3d845a15 */
const float LGF_T3 @private  = -3.2788541168e-02f; /* 0xbd064d47 */
const float LGF_T4 @private  =  1.7970675603e-02f; /* 0x3c93373d */
const float LGF_T5 @private  = -1.0314224288e-02f; /* 0xbc28fcfe */
const float LGF_T6 @private  =  6.1005386524e-03f; /* 0x3bc7e707 */
const float LGF_T7 @private  = -3.6845202558e-03f; /* 0xbb7177fe */
const float LGF_T8 @private  =  2.2596477065e-03f; /* 0x3b141699 */
const float LGF_T9 @private  = -1.4034647029e-03f; /* 0xbab7f476 */
const float LGF_T10 @private =  8.8108185446e-04f; /* 0x3a66f867 */
const float LGF_T11 @private = -5.3859531181e-04f; /* 0xba0d3085 */
const float LGF_T12 @private =  3.1563205994e-04f; /* 0x39a57b6b */
const float LGF_T13 @private = -3.1275415677e-04f; /* 0xb9a3f927 */
const float LGF_T14 @private =  3.3552918467e-04f; /* 0x39afe9f7 */
const float LGF_U0 @private  = -7.7215664089e-02f; /* 0xbd9e233f */
const float LGF_U1 @private  =  6.3282704353e-01f; /* 0x3f2200f4 */
const float LGF_U2 @private  =  1.4549225569e+00f; /* 0x3fba3ae7 */
const float LGF_U3 @private  =  9.7771751881e-01f; /* 0x3f7a4bb2 */
const float LGF_U4 @private  =  2.2896373272e-01f; /* 0x3e6a7578 */
const float LGF_U5 @private  =  1.3381091878e-02f; /* 0x3c5b3c5e */
const float LGF_V1 @private  =  2.4559779167e+00f; /* 0x401d2ebe */
const float LGF_V2 @private  =  2.1284897327e+00f; /* 0x4008392d */
const float LGF_V3 @private  =  7.6928514242e-01f; /* 0x3f44efdf */
const float LGF_V4 @private  =  1.0422264785e-01f; /* 0x3dd572af */
const float LGF_V5 @private  =  3.2170924824e-03f; /* 0x3b52d5db */
const float LGF_S0 @private  = -7.7215664089e-02f; /* 0xbd9e233f */
const float LGF_S1 @private  =  2.1498242021e-01f; /* 0x3e5c245a */
const float LGF_S2 @private  =  3.2577878237e-01f; /* 0x3ea6cc7a */
const float LGF_S3 @private  =  1.4635047317e-01f; /* 0x3e15dce6 */
const float LGF_S4 @private  =  2.6642270386e-02f; /* 0x3cda40e4 */
const float LGF_S5 @private  =  1.8402845599e-03f; /* 0x3af135b4 */
const float LGF_S6 @private  =  3.1947532989e-05f; /* 0x3805ff67 */
const float LGF_R1 @private  =  1.3920053244e+00f; /* 0x3fb22d3b */
const float LGF_R2 @private  =  7.2193557024e-01f; /* 0x3f38d0c5 */
const float LGF_R3 @private  =  1.7193385959e-01f; /* 0x3e300f6e */
const float LGF_R4 @private  =  1.8645919859e-02f; /* 0x3c98bf54 */
const float LGF_R5 @private  =  7.7794247773e-04f; /* 0x3a4beed6 */
const float LGF_R6 @private  =  7.3266842264e-06f; /* 0x36f5d7bd */
const float LGF_W0 @private  =  4.1893854737e-01f; /* 0x3ed67f1d */
const float LGF_W1 @private  =  8.3333335817e-02f; /* 0x3daaaaab */
const float LGF_W2 @private  = -2.7777778450e-03f; /* 0xbb360b61 */
const float LGF_W3 @private  =  7.9365057172e-04f; /* 0x3a500cfd */
const float LGF_W4 @private  = -5.9518753551e-04f; /* 0xba1c065c */
const float LGF_W5 @private  =  8.3633989561e-04f; /* 0x3a5b3dd2 */
const float LGF_W6 @private  = -1.6309292987e-03f; /* 0xbad5c4e8 */

<*
 sin(pi*x) assuming x > 2^-100, if sin(pi*x)==0 the sign is arbitrary
*>
fn float sin_pif(float x) @private
{
	/* spurious inexact if odd int */
	x = 2 * (x * 0.5f - _floorf(x * 0.5f));  /* x mod 2.0 */

	int n = (int)(x * 4);
	n = (n + 1) / 2;
	double y = (double)(x - (float)n * 0.5f);
	y *= 3.14159265358979323846;
	switch (n)
	{
		case 1: return __cosdf(y);
		case 2: return __sindf(-y);
		case 3: return -__cosdf(y);
		default: return __sindf(y); /* case 0 and 4 */
	}
}

fn float _lgammaf_r(float x, int* signgamp) @weak @cname("lgammaf_r") @nostrip
{
	uint u = bitcast(x, uint);
	float t @noinit;
	float y @noinit;
	float z @noinit;
	float nadj = 0;
	float p @noinit;
	float p1 @noinit;
	float p2 @noinit;
	float p3 @noinit;
	float q @noinit;
	float r @noinit;
	float w @noinit;
	int i @noinit;

	/* purge off +-inf, NaN, +-0, tiny and negative arguments */
	*signgamp = 1;
	int sign = (int)(u >> 31);
	uint ix = u & 0x7fffffff;
	if (ix >= 0x7f800000) return x * x;
	if (ix < 0x35000000)
	{
		/* |x| < 2**-21, return -log(|x|) */
		if (sign)
		{
			*signgamp = -1;
			x = -x;
		}
		return -logf(x);
	}
	if (sign)
	{
		x = -x;
		t = sin_pif(x);
		if (t == 0.0f) return 1.0f / (x - x); /* -integer */
		if (t > 0.0f)
		{
			*signgamp = -1;
		}
		else
		{
			t = -t;
		}
		nadj = logf(LGF_PI / (t * x));
	}

	switch
	{
		case ix == 0x3f800000 || ix == 0x40000000:
			/* purge off 1 and 2 */
			r = 0;
		case ix < 0x40000000:
			/* for x < 2.0 */
			if (ix <= 0x3f666666)
			{
				/* lgamma(x) = lgamma(x+1)-log(x) */
				r = -logf(x);
				switch
				{
					case ix >= 0x3f3b4a20:
						y = 1.0f - x;
						i = 0;
					case ix >= 0x3e6d3308:
						y = x - (LGF_TC - 1.0f);
						i = 1;
					default:
						y = x;
						i = 2;
				}
			}
			else
			{
				r = 0.0f;
				switch
				{
					case ix >= 0x3fdda618:
						/* [1.7316,2] */
						y = 2.0f - x;
						i = 0;
					case ix >= 0x3F9da620:
						/* [1.23,1.73] */
						y = x - LGF_TC;
						i = 1;
					default:
						y = x - 1.0f;
						i = 2;
				}
			}
			switch (i)
			{
				case 0:
					z = y * y;
					p1 = LGF_A0 + z * (LGF_A2 + z * (LGF_A4 + z * (LGF_A6 + z * (LGF_A8 + z * LGF_A10))));
					p2 = z * (LGF_A1 + z * (LGF_A3 + z * (LGF_A5 + z * (LGF_A7 + z * (LGF_A9 + z * LGF_A11)))));
					p = y * p1 + p2;
					r += p - 0.5f * y;
				case 1:
					z = y * y;
					w = z * y;
					p1 = LGF_T0 + w * (LGF_T3 + w * (LGF_T6 + w * (LGF_T9 + w * LGF_T12)));    /* parallel comp */
					p2 = LGF_T1 + w * (LGF_T4 + w * (LGF_T7 + w * (LGF_T10 + w * LGF_T13)));
					p3 = LGF_T2 + w * (LGF_T5 + w * (LGF_T8 + w * (LGF_T11 + w * LGF_T14)));
					p = z * p1 - (LGF_TT - w * (p2 + y * p3));
					r += (LGF_TF + p);
				default:
					p1 = y * (LGF_U0 + y * (LGF_U1 + y * (LGF_U2 + y * (LGF_U3 + y * (LGF_U4 + y * LGF_U5)))));
					p2 = 1.0f + y * (LGF_V1 + y * (LGF_V2 + y * (LGF_V3 + y * (LGF_V4 + y * LGF_V5))));
					r += -0.5f * y + p1 / p2;
			}
		case ix < 0x41000000:
			/* x < 8.0 */
			i = (int)x;
			y = x - (float)i;
			p = y * (LGF_S0 + y * (LGF_S1 + y * (LGF_S2 + y * (LGF_S3 + y * (LGF_S4 + y * (LGF_S5 + y * LGF_S6))))));
			q = 1.0f + y * (LGF_R1 + y * (LGF_R2 + y * (LGF_R3 + y * (LGF_R4 + y * (LGF_R5 + y * LGF_R6)))));
			r = 0.5f * y + p / q;
			if (i >= 3)
			{
				/* lgamma(1+s) = log(s) + lgamma(s) */
				z = 1.0f;
				for (int k = i; k >= 3; k--) z *= y + (float)(k - 1);
				r += logf(z);
			}
		case ix < 0x5c800000:
			/* 8.0 <= x < 2**58 */
			t = logf(x);
			z = 1.0f / x;
			y = z * z;
			w = LGF_W0 + z * (LGF_W1 + y * (LGF_W2 + y * (LGF_W3 + y * (LGF_W4 + y * (LGF_W5 + y * LGF_W6)))));
			r = (x - 0.5f) * (t - 1.0f) + w;
		default:
			/* 2**58 <= x <= inf */
			r = x * (logf(x) - 1.0f);
	}
	if (sign) r = nadj - r;
	return r;
}

fn float _lgammaf(float x) @weak @cname("lgammaf") @nostrip
{
	int sign;
	return _lgammaf_r(x, &sign);
}

/*
 musl src/math/tgamma.c

 "A Precision Approximation of the Gamma Function" - Cornelius Lanczos (1964)
 "Lanczos Implementation of the Gamma Function" - Paul Godfrey (2001)
 "An Analysis of the Lanczos Gamma Approximation" - Glendon Ralph Pugh (2004)

 approximation method:

                         (x - 0.5)         S(x)
 Gamma(x) = (x + g - 0.5)         *  ----------------
                                     exp(x + g - 0.5)

 with
                  a1      a2      a3            aN
 S(x) ~= [ a0 + ----- + ----- + ----- + ... + ----- ]
                x + 1   x + 2   x + 3         x + N

 with a0, a1, a2, a3,.. aN constants which depend on g.

 for x < 0 the following reflection formula is used:

 Gamma(x)*Gamma(-x) = -pi/(x sin(pi x))

 most ideas and constants are from boost and python
*/

const TG_N @private = 12;
const double TG_PI @private = 3.141592653589793238462643383279502884;
// static const double g = 6.024680040776729583740234375;
const double TG_GMHALF @private = 5.524680040776729583740234375;
const double[TG_N + 1] TG_SNUM @private = {
	23531376880.410759688572007674451636754734846804940,
	42919803642.649098768957899047001988850926355848959,
	35711959237.355668049440185451547166705960488635843,
	17921034426.037209699919755754458931112671403265390,
	6039542586.3520280050642916443072979210699388420708,
	1439720407.3117216736632230727949123939715485786772,
	248874557.86205415651146038641322942321632125127801,
	31426415.585400194380614231628318205362874684987640,
	2876370.6289353724412254090516208496135991145378768,
	186056.26539522349504029498971604569928220784236328,
	8071.6720023658162106380029022722506138218516325024,
	210.82427775157934587250973392071336271166969580291,
	2.5066282746310002701649081771338373386264310793408,
};
const double[TG_N + 1] TG_SDEN @private = {
	0, 39916800, 120543840, 150917976, 105258076, 45995730, 13339535,
	2637558, 357423, 32670, 1925, 66, 1,
};
/* n! for small integer n */
const double[*] TG_FACT @private = {
	1, 1, 2, 6, 24, 120, 720, 5040.0, 40320.0, 362880.0, 3628800.0, 39916800.0,
	479001600.0, 6227020800.0, 87178291200.0, 1307674368000.0, 20922789888000.0,
	355687428096000.0, 6402373705728000.0, 121645100408832000.0,
	2432902008176640000.0, 51090942171709440000.0, 1124000727777607680000.0,
};

<*
 S(x) rational function for positive x
*>
fn double tgamma_s(double x) @private
{
	double num = 0;
	double den = 0;
	/* to avoid overflow handle large x differently */
	if (x < 8)
	{
		for (int i = TG_N; i >= 0; i--)
		{
			num = num * x + TG_SNUM[i];
			den = den * x + TG_SDEN[i];
		}
	}
	else
	{
		for (int i = 0; i <= TG_N; i++)
		{
			num = num / x + TG_SNUM[i];
			den = den / x + TG_SDEN[i];
		}
	}
	return num / den;
}

fn double _tgamma(double x) @weak @cname("tgamma") @nostrip
{
	ulong u = bitcast(x, ulong);
	uint ix = (uint)((u >> 32) & 0x7fffffff);
	int sign = (int)(u >> 63);

	/* special cases */
	if (ix >= 0x7ff00000)
	{
		/* tgamma(nan)=nan, tgamma(inf)=inf, tgamma(-inf)=nan with invalid */
		return x + double::inf;
	}
	if (ix < ((0x3ff - 54) << 20))
	{
		/* |x| < 2^-54: tgamma(x) ~ 1/x, +-0 raises div-by-zero */
		return 1 / x;
	}

	/* integer arguments */
	/* raise inexact when non-integer */
	if (x == _floor(x))
	{
		if (sign) return __math_invalid(x);
		if (x <= TG_FACT.len) return TG_FACT[(int)x - 1];
	}

	/* x >= 172: tgamma(x)=inf with overflow */
	/* x =< -184: tgamma(x)=+-0 with underflow */
	if (ix >= 0x40670000)
	{
		/* |x| >= 184 */
		if (sign)
		{
			float f @noinit;
			@volatile_store(f, (float)(0x1p-126 / x));
			if (_floor(x) * 0.5 == _floor(x * 0.5)) return 0;
			return -0.0;
		}
		x *= 0x1p1023;
		return x;
	}

	double absx = sign ? -x : x;

	/* handle the error of x + g - 0.5 */
	double y = absx + TG_GMHALF;
	double dy @noinit;
	if (absx > TG_GMHALF)
	{
		dy = y - absx;
		dy -= TG_GMHALF;
	}
	else
	{
		dy = y - TG_GMHALF;
		dy -= absx;
	}

	double z = absx - 0.5;
	double r = tgamma_s(absx) * exp(-y);
	if (x < 0)
	{
		/* reflection formula for negative x */
		/* sin_pi(absx) is not 0, integers are already handled */
		r = -TG_PI / (sin_pi(absx) * absx * r);
		dy = -dy;
		z = -z;
	}
	r += dy * (TG_GMHALF + 0.5) * r / y;
	z = pow(y, 0.5 * z);
	y = r * z * z;
	return y;
}

fn float _tgammaf(float x) @weak @cname("tgammaf") @nostrip
{
	return (float)_tgamma(x);
}
