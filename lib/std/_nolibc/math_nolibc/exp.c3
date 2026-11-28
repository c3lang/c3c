module std::math::nolibc @feat(NO_LIBC, C3_MATH);

/*
 * Double-precision e^x and single-precision e^x, ported from musl's exp.c and expf.c.
 *
 * Copyright (c) 2017-2018, Arm Limited.
 * SPDX-License-Identifier: MIT
 */

// Handle cases that may overflow or underflow when computing the result that
// is scale*(1+TMP) without intermediate rounding. The bit representation of
// scale is in SBITS, however it has a computed exponent that may have
// overflown into the sign bit so that needs to be adjusted before using it as
// a double. (int)KI is the k used in the argument reduction and exponent
// adjustment of scale, positive k here means the result may overflow and
// negative k means the result may underflow.
fn double _exp_specialcase(double tmp, ulong sbits, ulong ki) @private
{
	if ((ki & 0x80000000) == 0)
	{
		// k > 0, the exponent of scale might have overflowed by <= 460.
		sbits -= 1009UL << 52;
		double scale = bitcast(sbits, double);
		return 0x1p1009 * (scale + scale * tmp);
	}
	// k < 0, need special care in the subnormal range.
	sbits += 1022UL << 52;
	double scale = bitcast(sbits, double);
	double y = scale + scale * tmp;
	if (y < 1.0)
	{
		// Round y to the right precision before scaling it into the subnormal
		// range to avoid double rounding that can cause 0.5+E/2 ulp error where
		// E is the worst-case ulp error outside the subnormal range. So this
		// is only useful if the goal is better than 1 ulp worst-case error.
		double lo = scale - y + scale * tmp;
		double hi = 1.0 + y;
		lo = 1.0 - hi + y + lo;
		y = (hi + lo) - 1.0;
		// Avoid -0.0 with downward rounding.
		if (WANT_ROUNDING && y == 0.0) y = 0.0;
		// The underflow exception needs to be signaled explicitly.
		__math_xflow(0, 0x1p-1022);
	}
	return 0x1p-1022 * y;
}

fn double exp(double x) @cname("exp") @nostrip @weak
{
	uint abstop = _top12d(x) & 0x7ff;
	if (abstop - _top12d(0x1p-54) >= _top12d(512.0) - _top12d(0x1p-54)) /* @unlikely */
	{
		// Avoid spurious underflow for tiny x. Note: 0 is common input.
		if (abstop - _top12d(0x1p-54) >= 0x80000000) return WANT_ROUNDING ? 1.0 + x : 1.0;
		if (abstop >= _top12d(1024.0))
		{
			if (bitcast(x, ulong) == bitcast(-double::inf, ulong)) return 0.0;
			if (abstop >= _top12d(double::inf)) return 1.0 + x;
			if (bitcast(x, ulong) >> 63) return __math_uflow(0);
			return __math_oflow(0);
		}
		// Large x is special cased below.
		abstop = 0;
	}

	// exp(x) = 2^(k/N) * exp(r), with exp(r) in [2^(-1/2N),2^(1/2N)].
	// x = ln2/N*k + r, with int k and r in [-ln2/2N, ln2/2N].
	double z = __EXP2_DATA.invln2N * x;
	// z - kd is in [-1, 1] in non-nearest rounding modes.
	double kd = z + __EXP2_DATA.shift;
	ulong ki = bitcast(kd, ulong);
	kd -= __EXP2_DATA.shift;
	double r = x + kd * __EXP2_DATA.negln2hiN + kd * __EXP2_DATA.negln2loN;
	// 2^(k/N) ~= scale * (1 + tail).
	ulong idx = 2 * (ki % EXP_DATA_WIDTH);
	ulong top = ki << (52 - EXP_TABLE_BITS);
	double tail = bitcast(__EXP2_DATA.tab[idx], double);
	// This is only a valid scale when -1023*N < k < 1024*N.
	ulong sbits = __EXP2_DATA.tab[idx + 1] + top;
	// exp(x) = 2^(k/N) * exp(r) ~= scale + scale * (tail + exp(r) - 1).
	// Evaluation is optimized assuming superscalar pipelined execution.
	double r2 = r * r;
	const C2 = __EXP2_DATA.poly[5 - EXP_POLY_ORDER];
	const C3 = __EXP2_DATA.poly[6 - EXP_POLY_ORDER];
	const C4 = __EXP2_DATA.poly[7 - EXP_POLY_ORDER];
	const C5 = __EXP2_DATA.poly[8 - EXP_POLY_ORDER];
	// Without fma the worst case error is 0.25/N ulp larger.
	// Worst case error is less than 0.5+1.11/N+(abs poly error * 2^53) ulp.
	double tmp = tail + r + r2 * (C2 + r * C3) + r2 * r2 * (C4 + r * C5);
	if (abstop == 0) return _exp_specialcase(tmp, sbits, ki); /* @unlikely */
	double scale = bitcast(sbits, double);
	// Note: tmp == 0 or |tmp| > 2^-200 and scale > 2^-739, so there
	// is no spurious underflow here even without fma.
	return scale + scale * tmp;
}

fn float expf(float x) @cname("expf") @nostrip @weak
{
	double xd = (double)x;
	uint abstop = _top12f(x) & 0x7ff;
	if (abstop >= _top12f(88.0f)) /* @unlikely */
	{
		// |x| >= 88 or x is nan.
		if (bitcast(x, uint) == bitcast(-float::inf, uint)) return 0.0f;
		if (abstop >= _top12f(float::inf)) return x + x;
		// x > log(0x1p128) ~= 88.72
		if (x > 0x1.62e42ep6f) return __math_oflowf(0);
		// x < log(0x1p-150) ~= -103.97
		if (x < -0x1.9fe368p6f) return __math_uflowf(0);
	}

	const uint N = 1U << EXP2F_TABLE_BITS;
	// x*N/Ln2 = k + r with r in [-1/2, 1/2] and int k.
	double z = __EXP2F_DATA.invln2_scaled * xd;

	// Round and convert z to int, the result is in [-150*N, 128*N] and
	// ideally ties-to-even rule is used, otherwise the magnitude of r
	// can be bigger which gives larger approximation error.
	double kd = z + __EXP2F_DATA.shift;
	ulong ki = bitcast(kd, ulong);
	kd -= __EXP2F_DATA.shift;
	double r = z - kd;

	// exp(x) = 2^(k/N) * 2^(r/N) ~= s * (C0*r^3 + C1*r^2 + C2*r + 1)
	ulong t = __EXP2F_DATA.tab[ki % N];
	t += ki << (52 - EXP2F_TABLE_BITS);
	double s = bitcast(t, double);
	z = __EXP2F_DATA.poly_scaled[0] * r + __EXP2F_DATA.poly_scaled[1];
	double r2 = r * r;
	double y = __EXP2F_DATA.poly_scaled[2] * r + 1;
	y = z * r2 + y;
	y = y * s;
	return (float)y;
}
