module std::math::complex{Real};
import std::io;

union Complex (Printable)
{
	struct
	{
		Real r, c;
	}
	Real[<2>] v;
}

const Complex IDENTITY = { 1, 0 };
const Complex IMAGINARY = { 0, 1 };


macro Complex Complex.add(self, Complex b) @operator(+) => { .v = self.v + b.v };
macro Complex Complex.add_real(self, Real r) @operator_s(+) => { .v = self.v + (Real[<2>]) { r, 0 } };
macro Complex Complex.add_each(self, Real b) => { .v = self.v + b };
macro Complex Complex.sub(self, Complex b) @operator(-) => { .v = self.v - b.v };
macro Complex Complex.sub_real(self, Real r) @operator(-) => { .v = self.v - (Real[<2>]) { r, 0 } };
macro Complex Complex.sub_real_inverse(self, Real r) @operator_r(-) => { .v = (Real[<2>]) { r, 0 } - self.v };
macro Complex Complex.sub_each(self, Real b) => { .v = self.v - b };
macro Complex Complex.scale(self, Real r) @operator_s(*) => { .v = self.v * r };
macro Complex Complex.mul(self, Complex b)@operator(*) => { self.r * b.r - self.c * b.c, self.r * b.c + b.r * self.c };
macro Complex Complex.div_real(self, Real r) @operator(/) => { .v = self.v / r };
macro Complex Complex.div_real_inverse(Complex c, Real r) @operator_r(/) => ((Complex) { .r = self }).div(c);
macro Complex Complex.div(self, Complex b) @operator(/)
{
	Real div = b.v.dot(b.v);
	return { (self.r * b.r + self.c * b.c) / div, (self.c * b.r - self.r * b.c) / div };
}
macro Complex Complex.inverse(self)
{
	Real sqr = self.v.dot(self.v);
	return { self.r / sqr, -self.c / sqr };
}
macro Complex Complex.conjugate(self) => { .r = self.r, .c = -self.c };
macro Complex Complex.negate(self) @operator(-) => { .v = -self.v };
macro bool Complex.equals(self, Complex b) @operator(==) => self.v == b.v;
macro bool Complex.not_equals(self, Complex b) @operator(!=) => self.v != b.v;

fn usz? Complex.to_format(&self, Formatter* f) @dynamic
{
	return f.printf("%g%+gi", self.r, self.c);
}