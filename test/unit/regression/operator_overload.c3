module operator_overload;
import std;

struct Abc
{ int a; }

int b;
fn bool Abc.neq(self, Abc abc) @operator(!=) => self.a != abc.a;
fn bool Abc.eq(self, Abc abc) @operator(==) => self.a == abc.a;
fn Abc Abc.plus(self, Abc abc) @operator(+) => { self.a + abc.a };
fn Abc Abc.plus2(self, int i) @operator(+) => { self.a + i };
fn Abc Abc.minus2(self, int i) @operator(-) => { self.a - i };
fn Abc Abc.plus_abc(Abc abc, int self) @operator_r(+) => { self + abc.a };
fn int Abc.mul_abc(Abc abc, int self) @operator_s(*) => self * abc.a;
fn Abc Abc.negate(self) @operator(~) => { ~self.a };
fn Abc Abc.negate2(self) @operator(-) => { -self.a };
fn Abc Abc.shr2(self, int x) @operator(>>) => { self.a >> x };
fn Abc Abc.shl2(self, int x) @operator(<<) @local  { b++; return { self.a << x }; }
fn Abc Abc.shl(self, Abc x) @operator(<<) => { self.a << x.a };
fn Abc Abc.div(self, Abc abc) @operator(/) => { self.a / abc.a };
fn Abc Abc.rem(self, Abc abc) @operator(%) => { self.a % abc.a };
fn Abc Abc.xor(self, Abc abc) @operator(^) => { self.a ^ abc.a };
fn Abc Abc.or(self, Abc abc) @operator(|) => { self.a | abc.a };
fn Abc Abc.and(self, Abc abc) @operator(&) => { self.a & abc.a };

fn void test_struct_overload() @test
{
	Abc x = { 2 };
	Abc y = { 3 };
	assert(x != y);
	assert(x == x);
	assert(x + 2 == { 4 });
	assert(x - 2 == { 0 });
	assert(2 + x == { 4 });
	assert(3 * x == 6);
	assert(~x == { -3 });
	assert(-x == { -2 });
	assert(+x == { 2 });
	assert(y >> 1 == { 1 });
	assert(y << x == { 12 });
	assert(x << 4 == { 32 });
	assert((Abc) { 123 } / x == { 61 });
	assert((Abc) { 123 } % x == { 1 });
	assert(x ^ y == { 1 });
	assert(x | y == { 3 });
	assert(x & y == { 2 });
}