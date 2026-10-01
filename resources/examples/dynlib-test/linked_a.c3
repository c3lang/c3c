module linked_a;
import std::io, linked_foo;

fn void init_1() @init(5) { linked_foo::record("a1"); }
fn void init_2() @init(100) { linked_foo::record("a2"); }

fn void run() @export("linked_a_run")
{
	Foo f = { 1 };
	Hello h = &f;
	io::printfn("linked_a: %s", h.hello());
}

fn ulong foo_typeid() @export("linked_a_typeid")
{
	Foo f = { 1 };
	any a = &f;
	return (ulong)(uptr)a.type;
}
