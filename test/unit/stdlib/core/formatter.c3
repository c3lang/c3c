module std::core::formatter_test;

import std;

struct Foo (Printable) { int a; }
fn usz? Foo.to_format(&self, Formatter *f) @dynamic
{
	return f.printf("Foo[%d]", self.a);
}

fn void test_ref() @test
{
	Foo* f = &&(Foo){ 8 };
	Foo* f0 = null;
	int* a = (void*)(uptr)0x40;
	int* b = null;
	String s = string::format(mem, "%s %s %s %s %s", a, b, f0, f, *f);
	defer free(s);
	assert(s == "0x40 0x0 (null) Foo[8] Foo[8]");

	double d1 = 6.62607015e-34d;
	double d2 = 0.0d;
	double d3 = -0.0d;
	double d4 = double.inf;
	double d5 = -double.inf;
	double d6 = -4.932096661796888e-226d;
	double d7 = 3.439070283483335e+35d;
	double d8 = 6.606854224493745e-17d;
	double d9 = 6.079537928711555e+61d;
	String s2 = string::format(mem, "%.9g %g %g %g %g %.16g %.16g %.16g %.16g", d1, d2, d3, d4, d5, d6, d7, d8, d9);
	defer free(s2);
	assert(s2 == "6.62607015e-34 0 -0 inf -inf -4.932096661796888e-226 3.439070283483335e+35 6.606854224493745e-17 6.079537928711555e+61");
}
