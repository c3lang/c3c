<*
 float16 and bfloat conversions, which LLVM calls on targets without instructions for them.
 Only bitcasts touch the 16-bit types here: any arithmetic on them would call these functions.
*>
module compiler_rt;

fn float __extendhfsf2(float16 a) @cname("__extendhfsf2") @weak => (float)half_to_double(bitcast(a, ushort), 10);
fn double __extendhfdf2(float16 a) @cname("__extendhfdf2") @weak => half_to_double(bitcast(a, ushort), 10);
fn float16 __truncsfhf2(float a) @cname("__truncsfhf2") @weak => bitcast(double_to_half(a, 10), float16);
fn float16 __truncdfhf2(double a) @cname("__truncdfhf2") @weak => bitcast(double_to_half(a, 10), float16);
fn bfloat __truncsfbf2(float a) @cname("__truncsfbf2") @weak => bitcast(double_to_half(a, 7), bfloat);
fn bfloat __truncdfbf2(double a) @cname("__truncdfbf2") @weak => bitcast(double_to_half(a, 7), bfloat);

<*
 The 16-bit float with a sign bit, `$mant` mantissa bits and the exponent in between, as a double.
*>
macro double half_to_double(ushort h, int $mant) @private
{
	const EXP_BITS = 15 - $mant;
	const int MAX_EXP = (1 << EXP_BITS) - 1;
	const int BIAS = MAX_EXP >> 1;
	ulong sign = (ulong)(h >> 15) << 63;
	int e = (h >> $mant) & MAX_EXP;
	ulong m = h & ((1u << $mant) - 1);
	if (e == MAX_EXP) return bitcast(sign | (0x7FFUL << 52) | (m << (52 - $mant)), double);
	if (e == 0)
	{
		if (m == 0) return bitcast(sign, double);
		// Subnormal: normalize.
		e = 1;
		while (!(m >> $mant))
		{
			m <<= 1;
			e--;
		}
		m &= (1u << $mant) - 1;
	}
	return bitcast(sign | ((ulong)(e - BIAS + 1023) << 52) | (m << (52 - $mant)), double);
}

<*
 `d` rounded to nearest, ties to even, as the bits of a 16-bit float with `$mant` mantissa bits. A NaN stays a NaN,
 made quiet.
*>
macro ushort double_to_half(double d, int $mant) @private
{
	const EXP_BITS = 15 - $mant;
	const int MAX_EXP = (1 << EXP_BITS) - 1;
	ulong bits = bitcast(d, ulong);
	ushort sign = (ushort)((bits >> 63) << 15);
	int de = (int)((bits >> 52) & 0x7FF);
	ulong sig = bits & ((1UL << 52) - 1);
	if (de == 0x7FF)
	{
		ushort nan = sig ? (ushort)((1u << ($mant - 1)) | (sig >> (52 - $mant))) : 0;
		return sign | (ushort)(MAX_EXP << $mant) | nan;
	}
	if (de) sig |= 1UL << 52;
	// The biased exponent of the result; at most 0 is a subnormal, shifted further right.
	int e = (de ? de : 1) - 1023 + (MAX_EXP >> 1);
	if (e >= MAX_EXP) return sign | (ushort)(MAX_EXP << $mant);
	int shift = 52 - $mant + (e < 1 ? 1 - e : 0);
	if (shift > 54) return sign;
	ulong r = sig >> shift;
	ulong rest = sig & ((1UL << shift) - 1);
	ulong half = 1UL << (shift - 1);
	if (rest > half || (rest == half && (r & 1))) r++;
	// A carry out of the mantissa raises the exponent, up to infinity.
	return sign | (ushort)((e > 0 ? (ulong)(e - 1) << $mant : 0) + r);
}
