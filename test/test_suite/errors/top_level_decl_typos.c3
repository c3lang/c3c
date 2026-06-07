module foo;

struct Foo
{
	int bar;
}

ptr p; // #error: This is not a valid top level declaration. Did you perhaps want 'iptr' or 'uptr'?
