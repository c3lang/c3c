module std::core::formatter_test;

import std;

struct Foo (Printable) { int a; }
fn usz! Foo.to_format(&self, Formatter *f) @dynamic
{
	return f.printf("Foo[%d]", self.a);
}

fn void test_ref() @test
{
	Foo* f = &&Foo{ 8 };
	Foo* f0 = null;
	int* a = (void*)(uptr)0x40;
	int* b = null;
	String s = string::new_format("%s %s %s %s %s", a, b, f0, f, *f);
	defer free(s);
	assert(s == "0x40 0x0 (null) Foo[8] Foo[8]");
}
