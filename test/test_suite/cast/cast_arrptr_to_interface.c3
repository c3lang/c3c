module test;

interface Foo
{
	fn void bar();
}

// Arrays and vectors can't implement an interface, so a pointer to one must not convert implicitly.
fn void array_ptr_to_interface()
{
	int[4] x;
	Foo f = &x; // #error: cannot be implicitly cast to 'Foo'
}

fn void vector_ptr_to_interface()
{
	int[<4>] x;
	Foo f = &x; // #error: cannot be implicitly cast to 'Foo'
}

// This is the existing behaviour for any other pointer that doesn't implement the interface.
fn void int_ptr_to_interface()
{
	int x;
	Foo f = &x; // #error: cannot be implicitly cast to 'Foo'
}

// The explicit cast is still allowed.
fn void explicit_array_ptr_to_interface()
{
	int[4] x;
	Foo f = (Foo)&x;
}
