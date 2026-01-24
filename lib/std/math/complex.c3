module std::math;

// Complex number aliases.

alias Complexf = ComplexNumber {float};
alias Complex = ComplexNumber {double};
alias COMPLEX_IDENTITY  @builtin = complex::IDENTITY {double};
alias COMPLEXF_IDENTITY @builtin = complex::IDENTITY {float};
alias IMAGINARY @builtin @deprecated("Use I") = complex::IMAGINARY { double };
alias IMAGINARYF @builtin @deprecated("Use I_F") = complex::IMAGINARY { float };
alias I @builtin = complex::IMAGINARY { double };
alias I_F @builtin = complex::IMAGINARY { float };

<*
 The generic complex number module, for float or double based complex number definitions.

 @require Real.kindof == FLOAT : "A complex number must use a floating type"
*>
module std::math::complex <Real>;
import std::io;

union ComplexNumber (Printable)
{
	struct
	{
		Real r, c;
	}
	Real[<2>] v;
}

const ComplexNumber IDENTITY = { 1, 0 };
const ComplexNumber IMAGINARY = { 0, 1 };

macro ComplexNumber ComplexNumber.add(self, ComplexNumber b) @operator(+) => { .v = self.v + b.v };
macro ComplexNumber ComplexNumber.add_this(&self, ComplexNumber b) @operator(+=) => { .v = self.v += b.v };
macro ComplexNumber ComplexNumber.add_real(self, Real r) @operator_s(+) => { .v = self.v + (Real[<2>]) { r, 0 } };
macro ComplexNumber ComplexNumber.add_each(self, Real b) => { .v = self.v + b };
macro ComplexNumber ComplexNumber.sub(self, ComplexNumber b) @operator(-) => { .v = self.v - b.v };
macro ComplexNumber ComplexNumber.sub_this(&self, ComplexNumber b) @operator(-=) => { .v = self.v -= b.v };
macro ComplexNumber ComplexNumber.sub_real(self, Real r) @operator(-) => { .v = self.v - (Real[<2>]) { r, 0 } };
macro ComplexNumber ComplexNumber.sub_real_inverse(self, Real r) @operator_r(-) => { .v = (Real[<2>]) { r, 0 } - self.v };
macro ComplexNumber ComplexNumber.sub_each(self, Real b) => { .v = self.v - b };
macro ComplexNumber ComplexNumber.scale(self, Real r) @operator_s(*) => { .v = self.v * r };
macro ComplexNumber ComplexNumber.mul(self, ComplexNumber b)@operator(*) => { self.r * b.r - self.c * b.c, self.r * b.c + b.r * self.c };
macro ComplexNumber ComplexNumber.div_real(self, Real r) @operator(/) => { .v = self.v / r };
macro ComplexNumber ComplexNumber.div_real_inverse(ComplexNumber c, Real r) @operator_r(/) => ((ComplexNumber) { .r = r }).div(c);
macro ComplexNumber ComplexNumber.div(self, ComplexNumber b) @operator(/)
{
	Real div = b.v.dot(b.v);
	return { (self.r * b.r + self.c * b.c) / div, (self.c * b.r - self.r * b.c) / div };
}
macro ComplexNumber ComplexNumber.inverse(self)
{
	Real sqr = self.v.dot(self.v);
	return { self.r / sqr, -self.c / sqr };
}
macro ComplexNumber ComplexNumber.conjugate(self) => { .r = self.r, .c = -self.c };
macro ComplexNumber ComplexNumber.negate(self) @operator(-) => { .v = -self.v };
macro bool ComplexNumber.equals(self, ComplexNumber b) @operator(==) => self.v == b.v;
macro bool ComplexNumber.equals_real(self, Real r) @operator_s(==) => self.v == { r, 0 };
macro bool ComplexNumber.not_equals(self, ComplexNumber b) @operator(!=) => self.v != b.v;

fn usz? ComplexNumber.to_format(&self, Formatter* f) @dynamic
{
	return f.printf("%g%+gi", self.r, self.c);
}