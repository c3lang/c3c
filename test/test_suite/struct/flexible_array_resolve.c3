struct Abc
{
	Foo[4] x; // #error: Arrays of structs with flexible array members
}

struct Foo
{
	int a;
	int[?] x;
}

struct Foo2
{
	int a;
	int[?] x, y; // #error: must be the last element
}
