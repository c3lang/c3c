module std::math::complex<Type>;

union Complex
{
	struct
    {
    	Type r, c;
    }
	Type[<2>] v;
}

macro Complex Complex.add(Complex a, Complex b)
{
	return Complex { .v = a.v + b.v };
}

macro Complex Complex.sub(Complex a, Complex b)
{
	return Complex { .v = a.v - b.v };
}

macro Complex Complex.mult(Complex a, Complex b)
{
	return Complex { .r = a.r * b.r - a.c * b.c, .c = a.r * b.c + b.r * a.c };
}

fn Complex Complex.div(Complex a, Complex b)
{
	Type div = b.r * b.r + b.c * b.c;
	return Complex { .r = (a.r * b.r + a.c * b.c) / div, .c = (a.c * b.r - a.r * b.c) / div };
}