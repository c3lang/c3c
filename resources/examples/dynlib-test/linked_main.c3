module linked_main;
import std::io, std::os::env, linked_foo;

extern fn void linked_a_run();
extern fn void linked_b_run();
extern fn ulong linked_a_typeid();
extern fn ulong linked_b_typeid();

// Regression: two C3 dynamic libraries linked (not dlopen'd) into a C3 executable.
// - Each library must register its own @dynamic methods before the executable's @init runs.
// - The weak type data of a type they share must resolve to a single instance.
// - The @init functions of all images run ordered by priority, regardless of which image they are in.
fn void init_middle() @init(75) { linked_foo::record("m"); }

fn void early() @init
{
	linked_a_run();
	linked_b_run();
}

fn int main()
{
	String order = env::tget_var("LINKED_ORDER") ?? "";
	if (order != "b1 a1 b2 m a2 ")
	{
		io::printfn("Unexpected order of @init: '%s'", order);
		return 1;
	}
	if (linked_a_typeid() != linked_b_typeid())
	{
		io::printn("The type data of Foo differs between the libraries.");
		return 1;
	}
	return 0;
}
