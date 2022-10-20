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

private union DoubleBits
{
	ulong l;
	double d;
}

private union FloatBits
{
	uint i;
	float f;
}



fn double __floattidf(int128 a) @extname("__floattidf") @weak
{
	if (a == 0) return 0.0;
	// Grab and remove sign.
    int128 sign = a >> 127;
    a = (a ^ sign) - sign;
    int sd = 128 - (int)$$clz(a);
    int e = sd - 1;
    if (sd > DOUBLE_MANT_DIG)
    {
        switch (sd)
        {
            case DOUBLE_MANT_DIG + 1:
                a <<= 1;
            case DOUBLE_MANT_DIG + 2:
                break;
		    default:
                a = ((uint128)a >> (sd - (DOUBLE_MANT_DIG + 2))) |
                     (int128)((a & ((uint128)(-1) >> ((128 + DOUBLE_MANT_DIG + 2) - sd))) != 0);
        }
        a |= (int128)((a & 4) != 0);
        a++;
        a >>= 2;
        if (a & (1i128 << DOUBLE_MANT_DIG))
        {
            a >>= 1;
            e++;
        }
    }
    else
    {
        a <<= DOUBLE_MANT_DIG - sd;
    }
    DoubleBits bits = {
        .l = ((ulong)(
            (((uint)sign & 0x80000000)
            | ((e + 1023) << 20))
            | ((uint)(a >> 32) & 0x000FFFFF)) << 32)
            | (uint)a
	};
    return bits.d;
}

fn double __floattisf(int128 a) @extname("__floattisf") @weak
{
	if (a == 0) return 0.0;
	// Grab and remove sign.
    int128 sign = a >> 127;
    a = (a ^ sign) - sign;
    int sd = 128 - (int)$$clz(a);
    int e = sd - 1;
    if (sd > FLOAT_MANT_DIG)
    {
        switch (sd)
        {
            case FLOAT_MANT_DIG + 1:
                a <<= 1;
            case FLOAT_MANT_DIG + 2:
                break;
		    default:
                a = ((uint128)a >> (sd - (FLOAT_MANT_DIG + 2))) |
                     (int128)((a & ((uint128)(-1) >> ((128 + FLOAT_MANT_DIG + 2) - sd))) != 0);
        }
        a |= (int128)((a & 4) != 0);
        a++;
        a >>= 2;
        if (a & (1i128 << FLOAT_MANT_DIG))
        {
            a >>= 1;
            e++;
        }
    }
    else
    {
        a <<= FLOAT_MANT_DIG - sd;
    }
    FloatBits bits = {
        .i = (((uint)sign & 0x80000000)
             | ((e + 127) << 23))
             | ((uint)a & 0x007FFFFF)
	};
    return bits.f;
}

fn double __floatuntisf(uint128 a) @extname("__floatuntisf") @weak
{
	if (a == 0) return 0.0;
	int sd = 128 - (int)$$clz(a); // digits
	int e = sd - 1; // exponent
	if (sd > FLOAT_MANT_DIG)
	{
	    switch (sd)
	    {
            case FLOAT_MANT_DIG + 1:
                a <<= 1;
            case FLOAT_MANT_DIG + 2:
                break;
            default:
                a = (a >> (sd - (FLOAT_MANT_DIG + 2)))
                    | (uint128)((a & ((uint128)(-1) >> ((128 + FLOAT_MANT_DIG + 2) - sd))) != 0);
        }
		a |= (uint128)((a & 4) != 0);
        a++;
        a >>= 2;
        if (a & (1i128 << FLOAT_MANT_DIG))
        {
            a >>= 1;
            e++;
        }
    }
    else
    {
        a <<= (FLOAT_MANT_DIG - sd);
    }
    FloatBits bits = {
        .i = ((e + 127) << 23) | ((uint)a & 0x007FFFFF)
    };
    return bits.f;
}

fn double __floatuntidf(uint128 a) @extname("__floatuntidf") @weak
{
	if (a == 0) return 0.0;
	int sd = 128 - (int)$$clz(a); // digits
	int e = sd - 1; // exponent
	if (sd > DOUBLE_MANT_DIG)
	{
	    switch (sd)
	    {
            case DOUBLE_MANT_DIG + 1:
                a <<= 1;
            case DOUBLE_MANT_DIG + 2:
                break;
            default:
                a = (a >> (sd - (DOUBLE_MANT_DIG + 2)))
                    | (uint128)((a & ((uint128)(-1) >> ((128 + DOUBLE_MANT_DIG + 2) - sd))) != 0);
        }
		a |= (uint128)((a & 4) != 0);
        a++;
        a >>= 2;
        if (a & (1i128 << DOUBLE_MANT_DIG))
        {
            a >>= 1;
            e++;
        }
    }
    else
    {
        a <<= (DOUBLE_MANT_DIG - sd);
    }
    DoubleBits bits = {
        .l = (ulong)(((e + 1023) << 20) | ((uint)(a >> 32) & 0x000FFFFF)) << 32 + (uint)a
    };
    return bits.d;
}

fn uint128 __fixunsdfti(double a) @extname("__fixunsdfti") = fixuint(a);
fn uint128 __fixunssfti(float a) @extname("__fixunssfti") = fixuint(a);
fn int128 __fixdfti(double a) @extname("__fixdfti") = fixint(a);
fn int128 __fixsfti(float a) @extname("__fixsfti") = fixint(a);

private macro fixuint(a)
{
	$switch ($typeof(a)):
		$case double:
			var $Rep = ulong;
			const EXPONENT_BITS = 11;
			const SIGNIFICANT_BITS = 52;
		$case float:
			var $Rep = uint;
			const EXPONENT_BITS = 8;
			const SIGNIFICANT_BITS = 23;
		$case float16:
			var $Rep = ushort;
			const EXPONENT_BITS = 5;
			const SIGNIFICANT_BITS = 10;
		$case float128:
			var $Rep = uint128;
			const EXPONENT_BITS = 15;
			const SIGNIFICANT_BITS = 112;
	$endswitch;
	const MAX_EXPONENT = 1 << EXPONENT_BITS - 1;
	const EXPONENT_BIAS = MAX_EXPONENT >> 1;
	const ONE_REP = (ulong)EXPONENT_BIAS << SIGNIFICANT_BITS;
	const SIGN_BIT = ($Rep)1 << (EXPONENT_BITS + SIGNIFICANT_BITS);
	const ABS_MASK = SIGN_BIT - 1;
	const IMPLICIT_BIT = ($Rep)1 << SIGNIFICANT_BITS;
	const SIGNIFICANT_MASK = IMPLICIT_BIT - 1;
	const EXPONENT_MASK = ABS_MASK ^ SIGNIFICANT_MASK;
	const QUIET_BIT = IMPLICIT_BIT >> 1;
	const QNAN_REP = EXPONENT_MASK | QUIET_BIT;
	const INF_REP = EXPONENT_MASK;

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
	$switch ($typeof(a)):
		$case double:
			var $Rep = ulong;
			const EXPONENT_BITS = 11;
			const SIGNIFICANT_BITS = 52;
		$case float:
			var $Rep = uint;
			const EXPONENT_BITS = 8;
			const SIGNIFICANT_BITS = 23;
		$case float16:
			var $Rep = ushort;
			const EXPONENT_BITS = 5;
			const SIGNIFICANT_BITS = 10;
		$case float128:
			var $Rep = uint128;
			const EXPONENT_BITS = 15;
			const SIGNIFICANT_BITS = 112;
	$endswitch;
	const MAX_EXPONENT = 1 << EXPONENT_BITS - 1;
	const EXPONENT_BIAS = MAX_EXPONENT >> 1;
	const ONE_REP = (ulong)EXPONENT_BIAS << SIGNIFICANT_BITS;
	const SIGN_BIT = ($Rep)1 << (EXPONENT_BITS + SIGNIFICANT_BITS);
	const ABS_MASK = SIGN_BIT - 1;
	const IMPLICIT_BIT = ($Rep)1 << SIGNIFICANT_BITS;
	const SIGNIFICANT_MASK = IMPLICIT_BIT - 1;
	const EXPONENT_MASK = ABS_MASK ^ SIGNIFICANT_MASK;
	const QUIET_BIT = IMPLICIT_BIT >> 1;
	const QNAN_REP = EXPONENT_MASK | QUIET_BIT;
	const INF_REP = EXPONENT_MASK;

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
