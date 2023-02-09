module std::math;

struct SimpleRandom
{
    long seed;
}

private const long SIMPLE_RANDOM_MULTIPLIER = 0x5DEECE66D;
private const long SIMPLE_RANDOM_ADDEND = 0xB;
private const long SIMPLE_RANDOM_MASK = (1i64 << 48) - 1;

private fn long simple_random_initial_scramble(long seed)
{
    return (seed ^ SIMPLE_RANDOM_MULTIPLIER) & SIMPLE_RANDOM_MASK;
}

private fn int SimpleRandom.next(SimpleRandom* r, int bits)
{
    long nextseed = (r.seed * SIMPLE_RANDOM_MULTIPLIER + SIMPLE_RANDOM_ADDEND) & SIMPLE_RANDOM_MASK;
    r.seed = nextseed;
    return (int)nextseed >> (48 - bits);
}

fn void SimpleRandom.set_seed(SimpleRandom* r, long seed)
{
    r.seed = simple_random_initial_scramble(seed);
}

fn int SimpleRandom.next_int(SimpleRandom* r)
{
    return r.next(32) @inline;
}

fn bool SimpleRandom.next_bool(SimpleRandom* r)
{
    return r.next(1) != 0;
}

fn float SimpleRandom.next_float(SimpleRandom* r)
{
    return r.next(24) / (float)(1 << 24);
}

fn double SimpleRandom.next_double(SimpleRandom* r)
{
    return (((long)(r.next(26)) << 27) + r.next(27)) * 0x1.0p-53;
}
