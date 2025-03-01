module std::math::nolibc @if(env::NO_LIBC || $feature(C3_MATH));

fn double frexp(double x, int* exp) @extern("frexp")
{
    uint hx = x.high_word();
    uint ix = hx & 0x7fffffff;
    uint lx = x.low_word();

    if (ix >= 0x7ff00000 || (ix | lx) == 0)
    {
        *exp = 0;
        return x;
    }

    // exponent extraction and normalization
    int e = (int)((ix >> 20) & 0x7ff);
    if (e == 0)
    {
        // subnormal number
        x *= 0x1p64;
        hx = x.high_word();
        e = (int)((hx >> 20) & 0x7ff) - 64;
    }
    *exp = e - 1022;

    // set exponent to -1 (fraction in [0.5, 1))
    hx = (hx & 0x800fffff) | 0x3fe00000;
    {
        ulong rep = ((ulong)hx << 32) | lx;
        return bitcast(rep, double);
    }
}

fn float frexpf(float x, int* exp) @extern("frexpf")
{
    uint ix = x.word();
    uint hx = ix & 0x7fffffff;

    if (hx >= 0x7f800000 || hx == 0)
    {
        *exp = 0;
        return x;
    }

    // exponent extraction and normalization
    int e = (int)((hx >> 23) & 0xff);
    if (e == 0)
    {
        // subnormal number
        x *= 0x1p64f;
        ix = x.word();
        e = (int)((ix >> 23) & 0xff) - 64;
    }
    *exp = e - 126;

    // set exponent to -1 (fraction in [0.5, 1))
    ix = (ix & 0x807fffff) | 0x3f000000;
    return bitcast(ix, float);
}
