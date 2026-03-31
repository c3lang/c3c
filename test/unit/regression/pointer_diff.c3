import std;
struct Foo
{
	int a,b,c,d;
}

fn void pointer_diff() @test
{
	Foo* foo;
	sz offset = &foo[1] - &foo[0];
	assert(offset == 1);
}

fn void pointer_add() @test
{
	Foo* foo;
	Foo* bar = foo + 2;
	sz offset = bar - foo;
	assert(offset == 2);
}