module std::math::math_rt;

fn float __roundevenf(float f) @extern("roundevenf") @weak @nostrip
{
	// Slow implementation
	return math::round(f / 2) * 2;
}

fn double __roundeven(double d) @extern("roundeven") @weak @nostrip
{
	// Slow implementation
	return math::round(d / 2) * 2;
}

fn double __powidf2(double a, int b) @extern("__powidf2") @weak @nostrip
{
	bool recip = b < 0;
    double r = 1;
    while (1)
    {
        if (b & 1) r *= a;
        b /= 2;
        if (b == 0) break;
        a *= a;
	}
    return recip ? 1 / r : r;
}