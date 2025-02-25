module std::math::nolibc @if(env::NO_LIBC || $feature(C3_MATH));

fn double _fabs(double x) @weak @extern("fabs") @nostrip
{
    ulong ix = bitcast(x, ulong);
    ix &= ~(1ul << 63);
    return bitcast(ix, double);
}

fn float _fabsf(float x) @weak @extern("fabsf") @nostrip
{
    uint ix = bitcast(x, uint);
    ix &= 0x7fffffff;
    return bitcast(ix, float);
}
