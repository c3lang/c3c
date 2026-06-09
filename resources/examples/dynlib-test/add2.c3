import std, libc;
struct Bar (Printable)
{
	int b;
}

fn sz? Bar.to_format(&self, Formatter* f) @dynamic
{
	return f.printf("Bar[%d]", self.b);
}

fn void init_x2() @init
{
	libc::puts("Init x2");
}

fn int adder2(int a, int b) @export("adder2")
{
	// Exercise add2's own @dynamic registration; host greps for "Bar[10]".
	Bar y = { a };
	io::printfn("Print bar: %s", y);
	return a + b;
}
