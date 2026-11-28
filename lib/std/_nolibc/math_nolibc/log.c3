module std::math::nolibc @feat(NO_LIBC, C3_MATH);

/*
 * Double-precision and single-precision log(x), ported from musl's log.c and logf.c.
 *
 * Copyright (c) 2017-2018, Arm Limited.
 * SPDX-License-Identifier: MIT
 */

fn double log(double x) @cname("log") @nostrip @weak
{
	const ulong OFF = 0x3fe6000000000000;
	const ulong N = 1UL << LOG_TABLE_BITS;
	const ulong LO = 0x3fee000000000000; // asuint64(1.0 - 0x1p-4)
	const ulong HI = 0x3ff1090000000000; // asuint64(1.0 + 0x1.09p-4)
	const B = __LOG_DATA.poly1;
	const A = __LOG_DATA.poly;

	ulong ix = bitcast(x, ulong);
	uint top = (uint)(ix >> 48);
	if (ix - LO < HI - LO) /* @unlikely */
	{
		// Handle close to 1.0 inputs separately.
		// Fix sign of zero with downward rounding when x==1.
		if (WANT_ROUNDING && ix == bitcast(1.0, ulong)) return 0;
		double r = x - 1.0;
		double r2 = r * r;
		double r3 = r * r2;
		double y = r3 * (B[1] + r * B[2] + r2 * B[3]
		                 + r3 * (B[4] + r * B[5] + r2 * B[6]
		                         + r3 * (B[7] + r * B[8] + r2 * B[9] + r3 * B[10])));
		// Worst-case error is around 0.507 ULP.
		double w = r * 0x1p27;
		double rhi = r + w - w;
		double rlo = r - rhi;
		w = rhi * rhi * B[0]; // B[0] == -0.5.
		double hi = r + w;
		double lo = r - hi + w;
		lo += B[0] * rlo * (rhi + r);
		y += lo;
		y += hi;
		return y;
	}
	if (top - 0x0010 >= 0x7ff0 - 0x0010) /* @unlikely */
	{
		// x < 0x1p-1022 or inf or nan.
		if (ix * 2 == 0) return __math_divzero(1, double);
		// log(inf) == inf.
		if (ix == bitcast(double::inf, ulong)) return x;
		if ((top & 0x8000) || (top & 0x7ff0) == 0x7ff0) return __math_invalid(x);
		// x is subnormal, normalize it.
		ix = bitcast(x * 0x1p52, ulong);
		ix -= 52UL << 52;
	}

	// x = 2^k z; where z is in range [OFF,2*OFF) and exact.
	// The range is split into N subintervals.
	// The ith subinterval contains z and c is near its center.
	ulong tmp = ix - OFF;
	int i = (int)((tmp >> (52 - LOG_TABLE_BITS)) % N);
	int k = (int)((long)tmp >> 52); // arithmetic shift
	ulong iz = ix - (tmp & (0xfffUL << 52));
	double invc = __LOG_DATA.tab[i].invc;
	double logc = __LOG_DATA.tab[i].logc;
	double z = bitcast(iz, double);

	// log(x) = log1p(z/c-1) + log(c) + k*Ln2.
	// r ~= z/c - 1, |r| < 1/(2*N).
	// rounding error: 0x1p-55/N + 0x1p-66.
	double r = (z - __LOG_DATA.tab2[i].chi - __LOG_DATA.tab2[i].clo) * invc;
	double kd = (double)k;

	// hi + lo = r + log(c) + k*Ln2.
	double w = kd * __LOG_DATA.ln2hi + logc;
	double hi = w + r;
	double lo = w - hi + r + kd * __LOG_DATA.ln2lo;

	// log(x) = lo + (log1p(r) - r) + hi.
	double r2 = r * r; // rounding error: 0x1p-54/N^2.
	// Worst case error if |y| > 0x1p-5:
	// 0.5 + 4.13/N + abs-poly-error*2^57 ULP (+ 0.002 ULP without fma)
	// Worst case error if |y| > 0x1p-4:
	// 0.5 + 2.06/N + abs-poly-error*2^56 ULP (+ 0.001 ULP without fma).
	return lo + r2 * A[0] + r * r2 * (A[1] + r * A[2] + r2 * (A[3] + r * A[4])) + hi;
}

const LOGF_TABLE_BITS @private = 4;
const LOGF_POLY_ORDER @private = 4;

struct LogfData @private
{
	LogInvcLogc[1 << LOGF_TABLE_BITS] tab;
	double ln2;
	<* First order coefficient is 1. *>
	double[LOGF_POLY_ORDER - 1] poly;
}

const LogfData __LOGF_DATA @private = {
	.tab = {
		{ 0x1.661ec79f8f3bep+0, -0x1.57bf7808caadep-2 },
		{ 0x1.571ed4aaf883dp+0, -0x1.2bef0a7c06ddbp-2 },
		{ 0x1.49539f0f010bp+0, -0x1.01eae7f513a67p-2 },
		{ 0x1.3c995b0b80385p+0, -0x1.b31d8a68224e9p-3 },
		{ 0x1.30d190c8864a5p+0, -0x1.6574f0ac07758p-3 },
		{ 0x1.25e227b0b8eap+0, -0x1.1aa2bc79c81p-3 },
		{ 0x1.1bb4a4a1a343fp+0, -0x1.a4e76ce8c0e5ep-4 },
		{ 0x1.12358f08ae5bap+0, -0x1.1973c5a611cccp-4 },
		{ 0x1.0953f419900a7p+0, -0x1.252f438e10c1ep-5 },
		{ 0x1p+0, 0x0p+0 },
		{ 0x1.e608cfd9a47acp-1, 0x1.aa5aa5df25984p-5 },
		{ 0x1.ca4b31f026aap-1, 0x1.c5e53aa362eb4p-4 },
		{ 0x1.b2036576afce6p-1, 0x1.526e57720db08p-3 },
		{ 0x1.9c2d163a1aa2dp-1, 0x1.bc2860d22477p-3 },
		{ 0x1.886e6037841edp-1, 0x1.1058bc8a07ee1p-2 },
		{ 0x1.767dcf5534862p-1, 0x1.4043057b6ee09p-2 },
	},
	.ln2 = 0x1.62e42fefa39efp-1,
	.poly = {
		-0x1.00ea348b88334p-2, 0x1.5575b0be00b6ap-2, -0x1.ffffef20a4123p-2,
	}
};

fn float logf(float x) @cname("logf") @nostrip @weak
{
	const uint OFF = 0x3f330000;
	const uint N = 1U << LOGF_TABLE_BITS;
	const A = __LOGF_DATA.poly;

	uint ix = bitcast(x, uint);
	// Fix sign of zero with downward rounding when x==1.
	if (WANT_ROUNDING && ix == 0x3f800000) return 0; /* @unlikely */
	if (ix - 0x00800000 >= 0x7f800000 - 0x00800000) /* @unlikely */
	{
		// x < 0x1p-126 or inf or nan.
		if (ix * 2 == 0) return __math_divzero(1, float);
		// log(inf) == inf.
		if (ix == 0x7f800000) return x;
		if ((ix & 0x80000000) || ix * 2 >= 0xff000000) return __math_invalid(x);
		// x is subnormal, normalize it.
		ix = bitcast(x * 0x1p23f, uint);
		ix -= 23U << 23;
	}

	// x = 2^k z; where z is in range [OFF,2*OFF] and exact.
	// The range is split into N subintervals.
	// The ith subinterval contains z and c is near its center.
	uint tmp = ix - OFF;
	int i = (int)((tmp >> (23 - LOGF_TABLE_BITS)) % N);
	int k = (int)tmp >> 23; // arithmetic shift
	uint iz = ix - (tmp & 0xff800000);
	double invc = __LOGF_DATA.tab[i].invc;
	double logc = __LOGF_DATA.tab[i].logc;
	double z = (double)bitcast(iz, float);

	// log(x) = log1p(z/c-1) + log(c) + k*Ln2
	double r = z * invc - 1;
	double y0 = logc + (double)k * __LOGF_DATA.ln2;

	// Pipelined polynomial evaluation to approximate log1p(r).
	double r2 = r * r;
	double y = A[1] * r + A[2];
	y = A[0] * r2 + y;
	y = y * r2 + (y0 + r);
	return (float)y;
}
