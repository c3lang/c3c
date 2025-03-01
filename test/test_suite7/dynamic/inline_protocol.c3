interface Foo
{
	fn int foo();
}

struct Abc (Foo)
{
	int a;
}

struct Bcd
{
	inline Abc a;
}

struct Def
{
	Abc a;
}

fn int Abc.foo(&self) @dynamic => 1;

distinct Foob = inline Abc;

fn void test1()
{
	Foob b;
	Abc x;
	Bcd y;
	Def d;
	Foo f = &x;
	f = &b;
	f = &y;
	f = &d; // #error: assume the interface is implemented
}