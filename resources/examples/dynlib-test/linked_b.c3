module linked_b;
import std::io, linked_foo;

fn void init_1() @init(1) { linked_foo::record("b1"); }
fn void init_2() @init(50) { linked_foo::record("b2"); }

fn void run() @export("linked_b_run")
{
	Foo f = { 1 };
	Hello h = &f;
	io::printfn("linked_b: %s", h.hello());
}

fn ulong foo_typeid() @export("linked_b_typeid")
{
	Foo f = { 1 };
	any a = &f;
	return (ulong)(uptr)a.type;
}
