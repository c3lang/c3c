import std, libc;
struct Foo(Printable)
{
	int a;
}

fn sz? Foo.to_format(&self, Formatter* f) @dynamic
{
	return f.printf("Foo[%d]", self.a);
}

// Runtime-init state, read back by the host through the getters below.
int init_value;
char[8] init_order;
int order_len;

fn void record(char c)
{
	init_order[order_len] = c;
	order_len++;
}

// Lower priority runs first, so the host must observe "AB".
fn void init_a() @init(100) { record('A'); }
fn void init_b() @init(200) { record('B'); init_value = 42; }

fn void init_x() @init
{
	libc::puts("Init x");
}
fn void shutdown() @finalizer
{
	io::printn("add: finalizer ran");
}

fn int add(int a, int b) @export("adder")
{
	io::printn("In adder");
	Foo x = { a };
	io::printfn("Print foo: %s", x);
	return a + b;
}

fn int get_init_value() @export("add_init_value") { return init_value; }
fn char* get_init_order() @export("add_init_order") { return &init_order[0]; }
