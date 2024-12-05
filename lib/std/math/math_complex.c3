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
const Complex IMAGINARY = { 0, 1 };
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
macro Complex Complex.inverse(self)
{
	Real sqr = self.v.dot(self.v);
	return Complex{ self.r / sqr, -self.c / sqr };
}
macro Complex Complex.conjugate(self) => Complex { .r = self.r, .c = -self.c };
macro bool Complex.equals(self, Complex b) => self.v == b.v;
