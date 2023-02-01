module std::math;

fn int128 __divti3(int128 a, int128 b) @extname("__divti3") @weak
{
	int128 sign_a = a >> 127; // -1 : 0
	int128 sign_b = b >> 127; // -1 : 0
	uint128 unsigned_a = (uint128)(a ^ sign_a) + (-sign_a);
	uint128 unsigned_b = (uint128)(b ^ sign_b) + (-sign_b);
	sign_a ^= sign_b; // quotient sign
	return __udivti3(unsigned_a, unsigned_b) @inline ^ sign_a + (-sign_a);
}

fn uint128 __umodti3(uint128 n, uint128 d) @extname("__umodti3") @weak
{
	// Ignore d = 0
    uint128 sr = (d ? $$clz(d) : 128) - (n ? $$clz(n) : 128);
    // If n < d then sr is wrapping.
    // which means we can just return n.
    if (sr > 127) return n;
    // If d == 1 and n = MAX
    if (sr == 127) return 0;
    sr++;
    uint128 r = n >> sr;
    // Follow known algorithm:
    n <<= 128 - sr;
    for (uint128 carry = 0; sr > 0; sr--)
    {
        r = (r << 1) | (n >> 127);
        n = (n << 1) | carry;
        int128 sign = (int128)(d - r - 1) >> 127;
        carry = sign & 1;
        r -= d & sign;
    }
    return r;
}

fn uint128 __udivti3(uint128 n, uint128 d) @extname("__udivti3") @weak
{
	// Ignore d = 0
    uint128 sr = (d ? $$clz(d) : 128) - (n ? $$clz(n) : 128);
    // If n < d then sr is wrapping.
    // which means we can just return 0.
    if (sr > 127) return 0;
    // If d == 1 and n = MAX
    if (sr == 127) return n;
    sr++;
    uint128 r = n >> sr;
    // Follow known algorithm:
    n <<= 128 - sr;
    uint128 carry = 0;
    for (; sr > 0; sr--)
    {
        r = (r << 1) | (n >> 127);
        n = (n << 1) | carry;
        int128 sign = (int128)(d - r - 1) >> 127;
        carry = sign & 1;
        r -= d & sign;
    }
	n = (n << 1) | carry;
    return n;
}

fn int128 __modti3(int128 a, int128 b) @extname("__modti3") @weak
{
	int128 sign = b >> 127;
	uint128 unsigned_b = (uint128)(b ^ sign) + (-sign);
	sign = a >> 127;
	uint128 unsigned_a = (uint128)(a ^ sign) + (-sign);

	return __umodti3(unsigned_a, unsigned_b) ^ sign + (-sign);
}

private union Int128bits
{
	struct
	{
		ulong ulow, uhigh;
	}
	struct
	{
		long ilow, ihigh;
	}
	uint128 all;
}

fn uint128 __lshrti3(uint128 a, uint b) @extname("__lshrti3") @weak
{
    Int128bits result;
    result.all = a;
    if (b >= 64)
    {
        result.ulow = result.uhigh >> (b - 64);
        result.uhigh = 0;
    }
    else
    {
        if (b == 0) return a;
        result.ulow = (result.uhigh << (64 - b)) | (result.ulow >> b);
        result.uhigh  = result.uhigh >> b;
    }
    return result.all;
}

fn int128 __ashrti3(int128 a, uint b) @extern("__ashrti3") @weak
{
    Int128bits result;
    result.all = a;
    if (b >= 64)
    {
        result.ilow = result.ihigh >> (b - 64);
        result.ihigh = result.ihigh >> 63;
    }
    else
    {
        if (b == 0) return a;
        result.ilow = result.ihigh << (64 - b) | (result.ilow >> b);
        result.ihigh  = result.ihigh >> b;
    }
    return result.all;
}

fn int128 __ashlti3(int128 a, uint b) @extern("__ashlti3") @weak
{
    Int128bits result;
    result.all = a;
    if (b >= 64)
    {
        result.ulow = 0;
        result.uhigh = result.ulow << (b - 64);
    }
    else
    {
        if (b == 0) return a;
        result.uhigh = (result.uhigh << b) | (result.ulow >> (64 - b));
        result.ulow  = result.ulow << b;
    }
    return result.all;
}

// Returns: a * b

private fn int128 __mulddi3(ulong a, ulong b)
{
	Int128bits r;
	const ulong LOWER_MASK = 0xffff_ffff;
    r.ulow = (a & LOWER_MASK) * (b & LOWER_MASK);
	ulong t = r.ulow >> 32;
	r.ulow &= LOWER_MASK;
	t += (a >> 32) * (b & LOWER_MASK);
	r.ulow += (t & LOWER_MASK) << 32;
	r.uhigh = t >> 32;
	t = r.ulow >> 32;
	r.ulow &= LOWER_MASK;
	t += (b >> 32) * (a & LOWER_MASK);
	r.ulow += (t & LOWER_MASK) << 32;
	r.uhigh += t >> 32;
	r.uhigh += (a >> 32) * (b >> 32);
	return r.all;
}

fn int128 __multi3(int128 a, int128 b) @extern("__multi3") @weak
{
	Int128bits x = { .all = a };
	Int128bits y = { .all = b };
	Int128bits r = { .all = __mulddi3(x.ulow, y.ulow) };
	r.uhigh += x.uhigh * y.ulow + x.ulow * y.uhigh;
	return r.all;
}

fn float __floattisf(int128 a) @extname("__floattisf") @weak => float_from_i128(float, a);
fn double __floattidf(int128 a) @extname("__floattidf") @weak => float_from_i128(double, a);
fn float __floatuntisf(uint128 a) @extname("__floatuntisf") @weak => float_from_u128(float, a);
fn double __floatuntidf(uint128 a) @extname("__floatuntidf") @weak => float_from_u128(double, a);
fn uint128 __fixunsdfti(double a) @weak @extname("__fixunsdfti") => fixuint(a);
fn uint128 __fixunssfti(float a) @weak @extname("__fixunssfti") => fixuint(a);
fn int128 __fixdfti(double a) @weak @extname("__fixdfti") => fixint(a);
fn int128 __fixsfti(float a) @weak @extname("__fixsfti") => fixint(a);


private macro float_from_i128($Type, a)
{
	var $Rep;
	$switch ($Type):
		$case double:
			$Rep = ulong;
			const MANT_DIG = DOUBLE_MANT_DIG;
			const SIGNIFICANT_BITS = 52;
			const EXP_BIAS = 1023;
			const MANTISSA_MASK = 0xFFFFF_FFFF_FFFFu64;
			const SIGN_BIT = 1u64 << 63;
		$case float:
			$Rep = uint;
			const MANT_DIG = FLOAT_MANT_DIG;
			const EXP_BIAS = 127;
			const SIGNIFICANT_BITS = 23;
			const MANTISSA_MASK = 0x7F_FFFFu32;
			const SIGN_BIT = 1u32 << 31;
		$case float16:
			$Rep = ushort;
			const MANT_DIG = HALF_MANT_DIG;
		$case float128:
			$Rep = uint128;
			const MANT_DIG = QUAD_MANT_DIG;
	$endswitch;
	if (a == 0) return ($Type)0;
	// Grab and remove sign.
    int128 sign = a >> 127;
    a = (a ^ sign) - sign;
	int sd = 128 - (int)$$clz(a); // digits
	int e = sd - 1; // exponent
	if (sd > MANT_DIG)
	{
	    switch (sd)
	    {
            case MANT_DIG + 1:
                a <<= 1;
            case MANT_DIG + 2:
                break;
            default:
                a = (a >> (sd - (MANT_DIG + 2)))
                    | (uint128)((a & ((uint128)(-1) >> ((128 + MANT_DIG + 2) - sd))) != 0);
        }
		a |= (uint128)((a & 4) != 0);
        a++;
        a >>= 2;
        if (a & (1i128 << MANT_DIG))
        {
            a >>= 1;
            e++;
        }
    }
    else
    {
        a <<= (MANT_DIG - sd);
    }
    return bitcast((($Rep)sign & SIGN_BIT) | ((($Rep)e + ($Rep)EXP_BIAS) << SIGNIFICANT_BITS) | (($Rep)a & ($Rep)MANTISSA_MASK), $Type);
}

private macro float_from_u128($Type, a)
{
	var $Rep;
	$switch ($Type):
		$case double:
			$Rep = ulong;
			const MANT_DIG = DOUBLE_MANT_DIG;
			const SIGNIFICANT_BITS = 52;
			const EXP_BIAS = 1023;
			const MANTISSA_MASK = 0xFFFFF_FFFF_FFFFu64;
		$case float:
			$Rep = uint;
			const MANT_DIG = FLOAT_MANT_DIG;
			const EXP_BIAS = 127;
			const SIGNIFICANT_BITS = 23;
			const MANTISSA_MASK = 0x7F_FFFFu32;
		$case float16:
			$Rep = ushort;
			const MANT_DIG = HALF_MANT_DIG;
		$case float128:
			$Rep = uint128;
			const MANT_DIG = QUAD_MANT_DIG;
	$endswitch;
	if (a == 0) return ($Type)0;
	int sd = 128 - (int)$$clz(a); // digits
	int e = sd - 1; // exponent
	if (sd > MANT_DIG)
	{
	    switch (sd)
	    {
            case MANT_DIG + 1:
                a <<= 1;
            case MANT_DIG + 2:
                break;
            default:
                a = (a >> (sd - (MANT_DIG + 2)))
                    | (uint128)((a & ((uint128)(-1) >> ((128 + MANT_DIG + 2) - sd))) != 0);
        }
		a |= (uint128)((a & 4) != 0);
        a++;
        a >>= 2;
        if (a & (1i128 << MANT_DIG))
        {
            a >>= 1;
            e++;
        }
    }
    else
    {
        a <<= (MANT_DIG - sd);
    }
    return bitcast(((($Rep)e + ($Rep)EXP_BIAS) << SIGNIFICANT_BITS) | (($Rep)a & ($Rep)MANTISSA_MASK), $Type);
}


private macro fixuint(a)
{
	var $Rep;
	$switch ($typeof(a)):
		$case double:
			$Rep = ulong;
			const EXPONENT_BITS = 11;
			const SIGNIFICANT_BITS = 52;
		$case float:
			$Rep = uint;
			const EXPONENT_BITS = 8;
			const SIGNIFICANT_BITS = 23;
		$case float16:
			$Rep = ushort;
			const EXPONENT_BITS = 5;
			const SIGNIFICANT_BITS = 10;
		$case float128:
			$Rep = uint128;
			const EXPONENT_BITS = 15;
			const SIGNIFICANT_BITS = 112;
	$endswitch;
	const $Rep MAX_EXPONENT = ($Rep)1 << EXPONENT_BITS - 1u;
	const $Rep EXPONENT_BIAS = MAX_EXPONENT >> 1u;
	const $Rep ONE_REP =EXPONENT_BIAS << SIGNIFICANT_BITS;
	const $Rep SIGN_BIT = ($Rep)1 << (EXPONENT_BITS + SIGNIFICANT_BITS);
	const $Rep ABS_MASK = SIGN_BIT - 1u;
	const $Rep IMPLICIT_BIT = ($Rep)1 << SIGNIFICANT_BITS;
	const $Rep SIGNIFICANT_MASK = IMPLICIT_BIT - 1u;
	const $Rep EXPONENT_MASK = ABS_MASK ^ SIGNIFICANT_MASK;
	const $Rep QUIET_BIT = IMPLICIT_BIT >> 1;
	const $Rep QNAN_REP = EXPONENT_MASK | QUIET_BIT;
	const $Rep INF_REP = EXPONENT_MASK;

	$Rep rep = bitcast(a, $Rep);
	$Rep abs = rep & ABS_MASK;
	int sign = rep & SIGN_BIT ? -1 : 1;
	int exponent = (int)((abs >> SIGNIFICANT_BITS) - EXPONENT_BIAS);
	$Rep significand = (abs & SIGNIFICANT_MASK) | IMPLICIT_BIT;
	if (sign == -1 || exponent < 0) return 0u128;
	if ((uint)exponent >= uint128.sizeof * 8) return ~0u128;
	if (exponent < SIGNIFICANT_BITS) return (uint128)significand >> (SIGNIFICANT_BITS - exponent);
    return (uint128)significand << (exponent - SIGNIFICANT_BITS);
}

private macro fixint(a)
{
	var $Rep;
	$switch ($typeof(a)):
		$case double:
			$Rep = ulong;
			const EXPONENT_BITS = 11;
			const SIGNIFICANT_BITS = 52;
		$case float:
			$Rep = uint;
			const EXPONENT_BITS = 8;
			const SIGNIFICANT_BITS = 23;
		$case float16:
			$Rep = ushort;
			const EXPONENT_BITS = 5;
			const SIGNIFICANT_BITS = 10;
		$case float128:
			$Rep = uint128;
			const EXPONENT_BITS = 15;
			const SIGNIFICANT_BITS = 112;
	$endswitch;
	const $Rep MAX_EXPONENT = ($Rep)1 << EXPONENT_BITS - 1u;
	const $Rep EXPONENT_BIAS = MAX_EXPONENT >> 1u;
	const $Rep ONE_REP = EXPONENT_BIAS << SIGNIFICANT_BITS;
	const $Rep SIGN_BIT = ($Rep)1 << (EXPONENT_BITS + SIGNIFICANT_BITS);
	const $Rep ABS_MASK = SIGN_BIT - 1u;
	const $Rep IMPLICIT_BIT = ($Rep)1 << SIGNIFICANT_BITS;
	const $Rep SIGNIFICANT_MASK = IMPLICIT_BIT - 1u;
	const $Rep EXPONENT_MASK = ABS_MASK ^ SIGNIFICANT_MASK;
	const $Rep QUIET_BIT = IMPLICIT_BIT >> 1;
	const $Rep QNAN_REP = EXPONENT_MASK | QUIET_BIT;
	const $Rep INF_REP = EXPONENT_MASK;

	$Rep rep = bitcast(a, $Rep);
	$Rep abs = rep & ABS_MASK;
	int sign = rep & SIGN_BIT ? -1 : 1;
	int exponent = (int)((abs >> SIGNIFICANT_BITS) - EXPONENT_BIAS);
	$Rep significand = (abs & SIGNIFICANT_MASK) | IMPLICIT_BIT;
	if (exponent < 0) return 0;
	if ((uint)exponent >= uint128.sizeof * 8) return sign == 1 ? int128.max : int128.min;

	if (exponent < SIGNIFICANT_BITS) return sign * ((int128)significand >> (SIGNIFICANT_BITS - exponent));
    return sign * ((int128)significand << (exponent - SIGNIFICANT_BITS));
}
