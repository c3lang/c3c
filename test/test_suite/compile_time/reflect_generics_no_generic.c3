import std;

alias Foo = List{List{int}};
struct Abc
{ int a; }

fn void test1()
{
	bool is_gen1 = Foo::is_generic(int); // #error:  'is_generic' requires a generic type name
}

fn void test2()
{
	bool is_gen2 = Foo::is_generic(Abc); // #error: 'Abc' is not a generic type
}

fn void test3()
{
	bool is_gen3 = Foo::is_generic($Typeof(Abc)); // #error:  'is_generic' requires a generic type name.
}

