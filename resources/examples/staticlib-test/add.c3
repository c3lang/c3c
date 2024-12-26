module add;
import std;
struct Foo(Printable)
{
	int a;
}

fn usz! Foo.to_format(&self, Formatter* f) @dynamic
{
	return f.printf("Foo[%d]", self.a);
}

fn int add(int a, int b) @export("adder")
{
	io::printn("In adder");
	Foo x = { a };
	io::printfn("Print foo: %s", x);
	return a + b;
}