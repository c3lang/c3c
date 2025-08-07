

macro $typeof({}) foo2() { return {}; }

struct Foo
{
    int a;
}

fn $typeof(Foo.a) foo3() {} // #error: may not return a member reference

fn $typeof({}) foo() {} // #error: A function may not return an untyped list

fn int main()
{
	foo2();
	return 0;
}