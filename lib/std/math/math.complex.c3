module std::math::complex<Real>;

union Complex
{
	struct
    {
    	Real r, c;
    }
	Real[<2>] v;
}


macro Complex identity() => { 1, 0 };
macro Complex Complex.add(Complex a, Complex b) => Complex { .v = a.v + b.v };
macro Complex Complex.add_each(Complex a, Real b) => Complex { .v = a.v + b };
macro Complex Complex.sub(Complex a, Complex b) => Complex { .v = a.v - b.v };
macro Complex Complex.sub_each(Complex a, Real b) => Complex { .v = a.v - b };
macro Complex Complex.scale(Complex a, Real s) => Complex { .v = a.v * s };
macro Complex Complex.mul(Complex a, Complex b) => { a.r * b.r - a.c * b.c, a.r * b.c + b.r * a.c };
macro Complex Complex.div(Complex a, Complex b)
{
	Real div = b.v.dot(b.v);
	return Complex{ (a.r * b.r + a.c * b.c) / div, (a.c * b.r - a.r * b.c) / div };
}
