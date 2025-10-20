import std;
fn int test1()
{
	@test1();
}

fn int test2()
{
	@test2();
}

fn int test3()
{
	@test3();
}

fn int test4()
{
	@test4();
}

fn void test5()
{
	@test5();
}

fn void test6()
{
	@test6();
}

macro @test1($Foo = ...)
{
	return $Foo.typeid; // #error: The parameter '$Foo' was not provided by the caller.
}

macro @test2(#foo = ...)
{
	return #foo; // #error: The parameter '#foo' was not provided by the caller
}

macro @test3(foo = ...)
{
	return foo; // #error: The parameter 'foo' was not provided by the caller.
}

macro @test4($foo = ...)
{
	return $foo; // #error: The parameter '$foo' was not provided by the caller
}

macro @test5($Foo = ...)
{
	$Foo = int; // #error: The parameter '$Foo' was not provided by the caller
	return $Foo.typeid;
}

macro @test6(foo = ...)
{
	foo = 1; // #error: The parameter 'foo' was not provided by the caller
	return foo;
}
