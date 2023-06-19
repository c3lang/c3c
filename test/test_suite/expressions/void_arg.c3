import std::io;

fn void test1()
{
	foo(test1()); // #error: A 'void' value cannot be passed as a parameter
}

fn void test2()
{
	bar(test2()); // #error: You cannot cast 'void' into 'int' even with an explicit cast
}

fn void test3()
{
	baz(test3()); // #error: A value of type 'void' cannot be passed as a variadic argumen
}

fn void test4()
{
	abc(test4());
}

macro void abc(...) {}

extern fn void baz(...);

macro void foo(x)
{
}

fn void bar(int x)
{}

