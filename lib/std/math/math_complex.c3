module std::math::complex(<Real>);

union Complex
{
	struct
	{
		Real r, c;
	}
	Real[<2>] v;
}


const Complex IDENTITY = { 1, 0 };
macro Complex Complex.add(self, Complex b) => Complex { .v = self.v + b.v };
macro Complex Complex.add_each(self, Real b) => Complex { .v = self.v + b };
macro Complex Complex.sub(self, Complex b) => Complex { .v = self.v - b.v };
macro Complex Complex.sub_each(self, Real b) => Complex { .v = self.v - b };
macro Complex Complex.scale(self, Real s) => Complex { .v = self.v * s };
macro Complex Complex.mul(self, Complex b) => { self.r * b.r - self.c * b.c, self.r * b.c + b.r * self.c };
macro Complex Complex.div(self, Complex b)
{
	Real div = b.v.dot(b.v);
	return Complex{ (self.r * b.r + self.c * b.c) / div, (self.c * b.r - self.r * b.c) / div };
}
