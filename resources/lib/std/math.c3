module std::math;

static union DoubleLong
{
    double f;
    ulong i;
}

func double log10(double x)
{
    const double IVLN10HI = 4.34294481878168880939e-01; /* 0x3fdbcb7b, 0x15200000 */
    const double IVLN10LO = 2.50829467116452752298e-11; /* 0x3dbb9438, 0xca9aadd5 */
    const double LOG10_2HI = 3.01029995663611771306e-01; /* 0x3FD34413, 0x509F6000 */
    const double LOG10_2LO = 3.69423907715893078616e-13; /* 0x3D59FEF3, 0x11F12B36 */
    const double LG1 = 6.666666666666735130e-01;  /* 3FE55555 55555593 */
    const double LG2 = 3.999999999940941908e-01;  /* 3FD99999 9997FA04 */
    const double LG3 = 2.857142874366239149e-01;  /* 3FD24924 94229359 */
    const double LG4 = 2.222219843214978396e-01;  /* 3FCC71C5 1D8E78AF */
    const double LG5 = 1.818357216161805012e-01;  /* 3FC74664 96CB03DE */
    const double LG6 = 1.531383769920937332e-01;  /* 3FC39A09 D078C69F */
    const double LG7 = 1.479819860511658591e-01;  /* 3FC2F112 DF3E5244 */
    DoubleLong u = { .f = x };
	uint hx = (uint)(u.i >> 32);
	int k = 0;
	if (hx < 0x00100000 || hx >> 31)
	{
		if (u.i << 1 == 0) return -1 / (x * x);  /* log(+-0)=-inf */
		if (hx >> 31) return (x - x) / 0.0; /* log(-#) = NaN */
		/* subnormal number, scale x up */
		k -= 54;
		x *= 0x1p54;
		u.f = x;
		hx = (uint)(u.i >> 32);
	}
	else if (hx >= 0x7ff00000)
	{
		return x;
	}
	else if (hx == 0x3ff00000 && u.i << 32 == 0)
	{
	    return 0;
	}
	/* reduce x into [sqrt(2)/2, sqrt(2)] */
	hx += 0x3ff00000 - 0x3fe6a09e;
	k += (int)(hx >> 20) - 0x3ff;
	hx = (hx & 0x000fffff) + 0x3fe6a09e;
	u.i = (ulong)(hx << 32) | (u.i & 0xffffffff);
	x = u.f;

	hx += 0x3ff00000 - 0x3fe6a09e;
	k += (int)(hx >> 20) - 0x3ff;
	hx = (hx & 0x000fffff) + 0x3fe6a09e;
	u.i = (ulong)(hx << 32) | (u.i & 0xffffffff);
	x = u.f;


	double f = x - 1.0;
	double hfsq = 0.5 * f * f;
	double s = f / (2.0+f);
	double z = s * s;
	double w = z * z;
	double t1 = w * (LG2 + w * (LG4 + w * LG6));
	double t2 = z * (LG1 + w * (LG3 + w * (LG5 + w * LG7)));
	double r = t2 + t1;

	/* See log2.c for details. */
	/* hi+lo = f - hfsq + s*(hfsq+R) ~ log(1+f) */
	double hi = f - hfsq;
	u.f = hi;
	// u.i &= (ulong)(-1) << 32;
	u.i &= 0xFFFFFFFF00000000;
	hi = u.f;
	double lo = f - hi - hfsq + s * (hfsq + r);

	/* val_hi+val_lo ~ log10(1+f) + k*log10(2) */
	double val_hi = hi * IVLN10HI;
	double dk = k;
	double y = dk * LOG10_2HI;
	double val_lo = dk * LOG10_2LO + (lo + hi) * IVLN10LO + lo*IVLN10HI;

	/*
	 * Extra precision in for adding y is not strictly needed
	 * since there is no very large cancellation near x = sqrt(2) or
	 * x = 1/sqrt(2), but we do it anyway since it costs little on CPUs
	 * with some parallelism and it reduces the error for many args.
	 */
	w = y + val_hi;
	val_lo += (y - w) + val_hi;
	val_hi = w;

	return val_lo + val_hi;
}

func double cos_limited(double x, double y)
{
    const double C1 = 4.16666666666666019037e-02; /* 0x3FA55555, 0x5555554C */
    const double C2 = -1.38888888888741095749e-03; /* 0xBF56C16C, 0x16C15177 */
    const double C3 = 2.48015872894767294178e-05; /* 0x3EFA01A0, 0x19CB1590 */
    const double C4 = -2.75573143513906633035e-07; /* 0xBE927E4F, 0x809C52AD */
    const double C5 = 2.08757232129817482790e-09; /* 0x3E21EE9E, 0xBDB4B1C4 */
    const double C6 = -1.13596475577881948265e-11; /* 0xBDA8FAE9, 0xBE8838D4 */

	double z = x * x;
	double w = z * z;
	double r = z * (C1+ z * (C2 + z * C3)) + w * w * (C4 + z * (C5 + z * C6));
	double hz = 0.5 * z;
	w  = 1.0 - hz;
	return w + (((1.0 - w) - hz) + (z*r - x*y));
}

static func double sin_limited(double x, double y, bool iy)
{
    const double S1 = -1.66666666666666324348e-01;  // 0xBFC55555, 0x55555549
    const double S2 = 8.33333333332248946124e-03; // 0x3F811111, 0x1110F8A6
    const double S3 = -1.98412698298579493134e-04; // 0xBF2A01A0, 0x19C161D5
    const double S4 = 2.75573137070700676789e-06; // 0x3EC71DE3, 0x57B1FE7D
    const double S5 = -2.50507602534068634195e-08; // 0xBE5AE5E6, 0x8A2B9CEB
    const double S6 = 1.58969099521155010221e-10; // 0x3DE5D93A, 0x5ACFD57C

	double z = x * x;
	double w = z * z;
    double r = S2 + z * (S3 + z * S4) + z * w * (S5 + z * S6);
	double v = z * x;
	if (!iy)
	{
		return x + v * (S1 + z * r);
	}
	else
	{
	    return x - ((z * (0.5 * y - v * r) - y) - v * S1);
	}
}
/+
public func double cos(double x)
{
	double[2] y;
	uint32_t ix;
	unsigned n;

	GET_HIGH_WORD(ix, x);
	ix &= 0x7fffffff;

	/* |x| ~< pi/4 */
	if (ix <= 0x3fe921fb)
	{
		if (ix < 0x3e46a09e)
		{
		    // |x| < 2**-27 * sqrt(2) */
			/* raise inexact if x!=0 */
			FORCE_EVAL(x + 0x1p120f);
			return 1.0;
		}
		return cos_limited(x, 0);
	}

	/* cos(Inf or NaN) is NaN */
	if (ix >= 0x7ff00000) return x - x;

	/* argument reduction */
	n = __rem_pio2(x, y);
	switch (n&3)
	{
	    case 0: return  cos_limited(y[0], y[1]);
	    case 1: return -sin_limited(y[0], y[1], true);
	    case 2: return -cos_limited(y[0], y[1]);
	    default:
		    return  sin_limited(y[0], y[1], true);
	}
}
+/