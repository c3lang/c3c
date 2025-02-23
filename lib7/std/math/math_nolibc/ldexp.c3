module std::math::nolibc @if(env::NO_LIBC || $feature(C3_MATH));

fn double ldexp(double x, int exp) @extern("ldexp") 
{
    uint hx = x.high_word();
    int hexp = (int)((hx & 0x7ff00000) >> 20);
    
 
    if (hexp == 0x7ff) return x;
    if (hexp == 0)
    {
        // subnormal number handling
        x *= 0x1p64;
        hx = x.high_word();
        hexp = (int)((hx & 0x7ff00000) >> 20) - 64;
    }

    // new exponent calculation
    hexp += exp;

    if (hexp > 0x7fe) return x * double.inf;
    if (hexp < 1)
    {
        x *= 0x1p-1022;
        hexp += 1022;
        if (hexp < 1) x *= 0x1p-1022;
        return x;
    }

    // set new exponent
    hx = ((ulong)hx & 0x800fffff) | ((ulong)hexp << 20);
    {
        ulong rep = ((ulong)hx << 32) | x.low_word();
        return bitcast(rep, double);
    }
}

fn float ldexpf(float x, int exp) @extern("ldexpf")
{
    uint ix = x.word();
    int hexp = (int)((ix & 0x7f800000) >> 23);

    if (hexp == 0xff) return x;
    if (hexp == 0)
    {
        // subnormal number handling
        x *= 0x1p64f;
        ix = x.word();
        hexp = (int)((ix & 0x7f800000) >> 23) - 64;
    }

    // new exponent calculation
    hexp += exp;

    if (hexp > 0xfe) return x * float.inf;
    if (hexp < 1)
    {
        x *= 0x1p-126f;
        hexp += 126;
        if (hexp < 1) x *= 0x1p-126f;
        return x;
    }

    // set new exponent
    ix = (ix & 0x807fffff) | (hexp << 23);
    return bitcast(ix, float);
}
