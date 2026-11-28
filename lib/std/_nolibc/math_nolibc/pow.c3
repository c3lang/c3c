module std::math::nolibc @feat(NO_LIBC, C3_MATH);

/*
 * Double-precision x^y function.
 *
 * Copyright (c) 2018, Arm Limited.
 * SPDX-License-Identifier: MIT
 */

/*
Worst-case error: 0.54 ULP (~= ulperr_exp + 1024*Ln2*relerr_log*2^53)
relerr_log: 1.3 * 2^-68 (Relative error of log, 1.5 * 2^-68 without fma)
ulperr_exp: 0.509 ULP (ULP error of exp, 0.511 ULP without fma)
*/

const ulong POW_OFF @private = 0x3fe6955500000000;
const uint POW_SIGN_BIAS @private = 0x800 << EXP_TABLE_BITS;

// Top 12 bits of a double (sign and exponent bits).
macro uint _pow_top12(double x) @private => (uint)(bitcast(x, ulong) >> 52);

<*
 Compute y+TAIL = log(x) where the rounded result is y and TAIL has about
 additional 15 bits precision. IX is the bit representation of x, but
 normalized in the subnormal range using the sign bit for the exponent.
*>
fn double _pow_log_inline(ulong ix, double* tail) @private
{
	const N = 1 << POW_LOG_TABLE_BITS;
	// x = 2^k z; where z is in range [OFF,2*OFF) and exact.
	// The range is split into N subintervals.
	// The ith subinterval contains z and c is near its center.
	ulong tmp = ix - POW_OFF;
	int i = (int)((tmp >> (52 - POW_LOG_TABLE_BITS)) % N);
	int k = (int)(bitcast(tmp, long) >> 52); // arithmetic shift
	ulong iz = ix - (tmp & (0xfffUL << 52));
	double z = bitcast(iz, double);
	double kd = (double)k;

	// log(x) = k*Ln2 + log(c) + log1p(z/c-1).
	double invc = __POW_LOG_DATA.tab[i].invc;
	double logc = __POW_LOG_DATA.tab[i].logc;
	double logctail = __POW_LOG_DATA.tab[i].logctail;

	// Note: 1/c is j/N or j/N/2 where j is an integer in [N,2N) and
	// |z/c - 1| < 1/N, so r = z/c - 1 is exactly representible.
	// Split z such that rhi, rlo and rhi*rhi are exact and |rlo| <= |r|.
	double zhi = bitcast((iz + (1UL << 31)) & (~0UL << 32), double);
	double zlo = z - zhi;
	double rhi = zhi * invc - 1.0;
	double rlo = zlo * invc;
	double r = rhi + rlo;

	// k*Ln2 + log(c) + r.
	double t1 = kd * __POW_LOG_DATA.ln2hi + logc;
	double t2 = t1 + r;
	double lo1 = kd * __POW_LOG_DATA.ln2lo + logctail;
	double lo2 = t1 - t2 + r;

	// Evaluation is optimized assuming superscalar pipelined execution.
	const double[*] A = __POW_LOG_DATA.poly;
	double ar = A[0] * r; // A[0] = -0.5.
	double ar2 = r * ar;
	double ar3 = r * ar2;
	// k*Ln2 + log(c) + r + A[0]*r*r.
	double arhi = A[0] * rhi;
	double arhi2 = rhi * arhi;
	double hi = t2 + arhi2;
	double lo3 = rlo * (ar + arhi);
	double lo4 = t2 - hi + arhi2;
	// p = log1p(r) - r - A[0]*r*r.
	double p = ar3 * (A[1] + r * A[2] + ar2 * (A[3] + r * A[4] + ar2 * (A[5] + r * A[6])));
	double lo = lo1 + lo2 + lo3 + lo4 + p;
	double y = hi + lo;
	*tail = hi - y + lo;
	return y;
}

<*
 Handle cases that may overflow or underflow when computing the result that
 is scale*(1+TMP) without intermediate rounding. The bit representation of
 scale is in SBITS, however it has a computed exponent that may have
 overflown into the sign bit so that needs to be adjusted before using it as
 a double. (int)KI is the k used in the argument reduction and exponent
 adjustment of scale, positive k here means the result may overflow and
 negative k means the result may underflow.
*>
fn double _pow_specialcase(double tmp, ulong sbits, ulong ki) @private
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
	// Note: sbits is signed scale.
	double scale = bitcast(sbits, double);
	double y = scale + scale * tmp;
	if (_fabs(y) < 1.0)
	{
		// Round y to the right precision before scaling it into the subnormal
		// range to avoid double rounding that can cause 0.5+E/2 ulp error where
		// E is the worst-case ulp error outside the subnormal range.  So this
		// is only useful if the goal is better than 1 ulp worst-case error.
		double one = y < 0.0 ? -1.0 : 1.0;
		double lo = scale - y + scale * tmp;
		double hi = one + y;
		lo = one - hi + y + lo;
		y = (hi + lo) - one;
		// Fix the sign of 0.
		if (y == 0.0) y = bitcast(sbits & 0x8000000000000000, double);
		// The underflow exception needs to be signaled explicitly.
		__math_xflow(0, 0x1p-1022);
	}
	return 0x1p-1022 * y;
}

<*
 Computes sign*exp(x+xtail) where |xtail| < 2^-8/N and |xtail| <= |x|.
 The sign_bias argument is POW_SIGN_BIAS or 0 and sets the sign to -1 or 1.
*>
fn double _pow_exp_inline(double x, double xtail, uint sign_bias) @private
{
	uint abstop = _pow_top12(x) & 0x7ff;
	if (abstop - _pow_top12(0x1p-54) >= _pow_top12(512.0) - _pow_top12(0x1p-54)) /* @unlikely */
	{
		if (abstop - _pow_top12(0x1p-54) >= 0x80000000)
		{
			// Avoid spurious underflow for tiny x.
			// Note: 0 is common input.
			double one = WANT_ROUNDING ? 1.0 + x : 1.0;
			return sign_bias ? -one : one;
		}
		if (abstop >= _pow_top12(1024.0))
		{
			// Note: inf and nan are already handled.
			if (bitcast(x, ulong) >> 63) return __math_uflow(sign_bias);
			return __math_oflow(sign_bias);
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
	// The code assumes 2^-200 < |xtail| < 2^-8/N.
	r += xtail;
	// 2^(k/N) ~= scale * (1 + tail).
	ulong idx = 2 * (ki % EXP_DATA_WIDTH);
	ulong top = (ki + sign_bias) << (52 - EXP_TABLE_BITS);
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
	if (abstop == 0 /* @unlikely */) return _pow_specialcase(tmp, sbits, ki);
	double scale = bitcast(sbits, double);
	// Note: tmp == 0 or |tmp| > 2^-200 and scale > 2^-739, so there
	// is no spurious underflow here even without fma.
	return scale + scale * tmp;
}

<*
 Returns 0 if not int, 1 if odd int, 2 if even int. The argument is
 the bit representation of a non-zero finite floating-point value.
*>
fn int _pow_checkint(ulong iy) @private
{
	int e = (int)((iy >> 52) & 0x7ff);
	if (e < 0x3ff) return 0;
	if (e > 0x3ff + 52) return 2;
	if (iy & ((1UL << (0x3ff + 52 - e)) - 1)) return 0;
	if (iy & (1UL << (0x3ff + 52 - e))) return 1;
	return 2;
}

// Returns true if input is the bit representation of 0, infinity or nan.
macro bool _pow_zeroinfnan(ulong i) @private => 2 * i - 1 >= 2 * bitcast(double::inf, ulong) - 1;

fn double pow(double x, double y) @cname("pow") @weak @nostrip
{
	uint sign_bias = 0;
	ulong ix = bitcast(x, ulong);
	ulong iy = bitcast(y, ulong);
	uint topx = _pow_top12(x);
	uint topy = _pow_top12(y);
	if (topx - 0x001 >= 0x7ff - 0x001 || (topy & 0x7ff) - 0x3be >= 0x43e - 0x3be) /* @unlikely */
	{
		// Note: if |y| > 1075 * ln2 * 2^53 ~= 0x1.749p62 then pow(x,y) = inf/0
		// and if |y| < 2^-54 / 1075 ~= 0x1.e7b6p-65 then pow(x,y) = +-1.
		// Special cases: (x < 0x1p-126 or inf or nan) or
		// (|y| < 0x1p-65 or |y| >= 0x1p63 or nan).
		if (_pow_zeroinfnan(iy)) /* @unlikely */
		{
			if (2 * iy == 0) return 1.0;
			if (ix == bitcast(1.0, ulong)) return 1.0;
			if (2 * ix > 2 * bitcast(double::inf, ulong) || 2 * iy > 2 * bitcast(double::inf, ulong)) return x + y;
			if (2 * ix == 2 * bitcast(1.0, ulong)) return 1.0;
			// |x|<1 && y==inf or |x|>1 && y==-inf.
			if ((2 * ix < 2 * bitcast(1.0, ulong)) == !(iy >> 63)) return 0.0;
			return y * y;
		}
		if (_pow_zeroinfnan(ix)) /* @unlikely */
		{
			double x2 = x * x;
			if ((ix >> 63) && _pow_checkint(iy) == 1) x2 = -x2;
			return (iy >> 63) ? 1 / x2 : x2;
		}
		// Here x and y are non-zero finite.
		if (ix >> 63)
		{
			// Finite x < 0.
			int yint = _pow_checkint(iy);
			if (yint == 0) return __math_invalid(x);
			if (yint == 1) sign_bias = POW_SIGN_BIAS;
			ix &= 0x7fffffffffffffff;
			topx &= 0x7ff;
		}
		if ((topy & 0x7ff) - 0x3be >= 0x43e - 0x3be)
		{
			// Note: sign_bias == 0 here because y is not odd.
			if (ix == bitcast(1.0, ulong)) return 1.0;
			if ((topy & 0x7ff) < 0x3be)
			{
				// |y| < 2^-65, x^y ~= 1 + y*log(x).
				return WANT_ROUNDING ? (ix > bitcast(1.0, ulong) ? 1.0 + y : 1.0 - y) : 1.0;
			}
			return (ix > bitcast(1.0, ulong)) == (topy < 0x800) ? __math_oflow(0) : __math_uflow(0);
		}
		if (topx == 0)
		{
			// Normalize subnormal x so exponent becomes negative.
			ix = bitcast(x * 0x1p52, ulong);
			ix &= 0x7fffffffffffffff;
			ix -= 52UL << 52;
		}
	}

	double lo;
	double hi = _pow_log_inline(ix, &lo);
	double yhi = bitcast(iy & (~0UL << 27), double);
	double ylo = y - yhi;
	double lhi = bitcast(bitcast(hi, ulong) & (~0UL << 27), double);
	double llo = hi - lhi + lo;
	double ehi = yhi * lhi;
	double elo = ylo * lhi + y * llo; // |elo| < |ehi| * 2^-25.
	return _pow_exp_inline(ehi, elo, sign_bias);
}

/*
POWF_LOG2_POLY_ORDER = 5
EXP2F_TABLE_BITS = 5

ULP error: 0.82 (~ 0.5 + relerr*2^24)
relerr: 1.27 * 2^-26 (Relative error ~= 128*Ln2*relerr_log2 + relerr_exp2)
relerr_log2: 1.83 * 2^-33 (Relative error of logx.)
relerr_exp2: 1.69 * 2^-34 (Relative error of exp2(ylogx).)
*/

const uint POWF_OFF @private = 0x3f330000;
const uint POWF_SIGN_BIAS @private = 1 << (EXP2F_TABLE_BITS + 11);

// Subnormal input is normalized so ix has negative biased exponent.
fn double _powf_log2_inline(uint ix) @private
{
	const N = 1 << POWF_LOG2_TABLE_BITS;
	// x = 2^k z; where z is in range [OFF,2*OFF] and exact.
	// The range is split into N subintervals.
	// The ith subinterval contains z and c is near its center.
	uint tmp = ix - POWF_OFF;
	int i = (int)((tmp >> (23 - POWF_LOG2_TABLE_BITS)) % N);
	uint top = tmp & 0xff800000;
	uint iz = ix - top;
	int k = bitcast(top, int) >> 23; // arithmetic shift
	double invc = __POWF_LOG2_DATA.tab[i].invc;
	double logc = __POWF_LOG2_DATA.tab[i].logc;
	double z = (double)bitcast(iz, float);

	// log2(x) = log1p(z/c-1)/ln2 + log2(c) + k
	double r = z * invc - 1;
	double y0 = logc + (double)k;

	// Pipelined polynomial evaluation to approximate log1p(r)/ln2.
	const double[*] A = __POWF_LOG2_DATA.poly;
	double r2 = r * r;
	double y = A[0] * r + A[1];
	double p = A[2] * r + A[3];
	double r4 = r2 * r2;
	double q = A[4] * r + y0;
	q = p * r2 + q;
	y = y * r4 + q;
	return y;
}

// The unscaled xd must be in [-1021,1023], sign_bias sets the sign of the result.
fn float _powf_exp2_inline(double xd, uint sign_bias) @private
{
	const N = 1 << EXP2F_TABLE_BITS;
	const SHIFT = __EXP2F_DATA.shift_scaled;
	const double[*] C = __EXP2F_DATA.poly;
	// x = k/N + r with r in [-1/(2N), 1/(2N)]
	double kd = xd + SHIFT;
	ulong ki = bitcast(kd, ulong);
	kd -= SHIFT; // k/N
	double r = xd - kd;

	// exp2(x) = 2^(k/N) * 2^r ~= s * (C0*r^3 + C1*r^2 + C2*r + 1)
	ulong t = __EXP2F_DATA.tab[ki % N];
	ulong ski = ki + sign_bias;
	t += ski << (52 - EXP2F_TABLE_BITS);
	double s = bitcast(t, double);
	double z = C[0] * r + C[1];
	double r2 = r * r;
	double y = C[2] * r + 1;
	y = z * r2 + y;
	y = y * s;
	return (float)y;
}

<*
 Returns 0 if not int, 1 if odd int, 2 if even int. The argument is
 the bit representation of a non-zero finite floating-point value.
*>
fn int _powf_checkint(uint iy) @private
{
	int e = (int)((iy >> 23) & 0xff);
	if (e < 0x7f) return 0;
	if (e > 0x7f + 23) return 2;
	if (iy & ((1U << (0x7f + 23 - e)) - 1)) return 0;
	if (iy & (1U << (0x7f + 23 - e))) return 1;
	return 2;
}

macro bool _powf_zeroinfnan(uint ix) @private => 2 * ix - 1 >= 2U * 0x7f800000 - 1;

fn float powf(float x, float y) @cname("powf") @weak @nostrip
{
	uint sign_bias = 0;
	uint ix = bitcast(x, uint);
	uint iy = bitcast(y, uint);
	if (ix - 0x00800000 >= 0x7f800000 - 0x00800000 || _powf_zeroinfnan(iy)) /* @unlikely */
	{
		// Either (x < 0x1p-126 or inf or nan) or (y is 0 or inf or nan).
		if (_powf_zeroinfnan(iy)) /* @unlikely */
		{
			if (2 * iy == 0) return 1.0f;
			if (ix == 0x3f800000) return 1.0f;
			if (2 * ix > 2U * 0x7f800000 || 2 * iy > 2U * 0x7f800000) return x + y;
			if (2 * ix == 2 * 0x3f800000) return 1.0f;
			// |x|<1 && y==inf or |x|>1 && y==-inf.
			if ((2 * ix < 2 * 0x3f800000) == !(iy & 0x80000000)) return 0.0f;
			return y * y;
		}
		if (_powf_zeroinfnan(ix)) /* @unlikely */
		{
			float x2 = x * x;
			if ((ix & 0x80000000) && _powf_checkint(iy) == 1) x2 = -x2;
			return (iy & 0x80000000) ? 1 / x2 : x2;
		}
		// x and y are non-zero finite.
		if (ix & 0x80000000)
		{
			// Finite x < 0.
			int yint = _powf_checkint(iy);
			if (yint == 0) return __math_invalid(x);
			if (yint == 1) sign_bias = POWF_SIGN_BIAS;
			ix &= 0x7fffffff;
		}
		if (ix < 0x00800000)
		{
			// Normalize subnormal x so exponent becomes negative.
			ix = bitcast(x * 0x1p23f, uint);
			ix &= 0x7fffffff;
			ix -= 23 << 23;
		}
	}
	double logx = _powf_log2_inline(ix);
	double ylogx = y * logx; // cannot overflow, y is single prec.
	if (((bitcast(ylogx, ulong) >> 47) & 0xffff) >= bitcast(126.0, ulong) >> 47) /* @unlikely */
	{
		// |y*log(x)| >= 126.
		if (ylogx > 0x1.fffffffd1d571p+6) return __math_oflowf(sign_bias);
		if (ylogx <= -150.0) return __math_uflowf(sign_bias);
	}
	return _powf_exp2_inline(ylogx, sign_bias);
}
