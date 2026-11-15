module test;

// A pointer wrapped in an inline distinct type converts to `any` and to an interface
// the same way the plain pointer does.
typedef IntPtr = inline int*;

fn void inline_distinct_ptr_to_any()
{
	int x = 5;
	IntPtr p = (IntPtr)&x;
	any a = p;
}

interface Foo
{
	fn void bar();
}

struct Impl (Foo)
{
	int x;
}

fn void Impl.bar(&self) @dynamic {}

typedef ImplPtr = inline Impl*;

fn void inline_distinct_ptr_to_interface()
{
	Impl i;
	ImplPtr p = (ImplPtr)&i;
	Foo f = p;
}
