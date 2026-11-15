enum Col { RED, GREEN }
struct Foo { int x; }
const int BAR = 1;

fn void test1()
{
	bool b = $defined($eval("Col::GREEN")); // #error: 'Col::GREEN' could not be resolved to a valid identifier
}

fn void test2()
{
	int x = $eval("Col.GREEN"); // #error: 'Col.GREEN' could not be resolved to a valid identifier
}

fn void test3()
{
	int x = $eval("Foo::x"); // #error: 'Foo::x' could not be resolved to a valid identifier
}

fn void test4()
{
	int x = $eval("int::max"); // #error: 'int::max' could not be resolved to a valid identifier
}

fn void test5()
{
	var $Type = int;
	int x = $eval("$Type::max"); // #error: '$Type::max' could not be resolved to a valid identifier
}

fn void test6()
{
	int x = $eval("Foo::typeid"); // #error: 'Foo::typeid' could not be resolved to a valid identifier
}

fn void test7()
{
	bool b = $defined($eval("Foo")); // #error: 'Foo' looks like a type name, use '$Typefrom'
}

fn void test8()
{
	int x = $eval("int"); // #error: 'int' looks like a type name, use '$Typefrom'
}

fn void test8b()
{
	int x = $eval("Nope"); // #error: 'Nope' looks like a type name, use '$Typefrom'
}

fn void test9()
{
	int x = $eval("if"); // #error: 'if' could not be resolved to a valid identifier
}

fn void test10()
{
	int x = $eval("std::if::foo"); // #error: 'std::if::foo' could not be resolved to a valid identifier
}

fn void test11()
{
	int x = $eval("BAR "); // #error: 'BAR ' could not be resolved to a valid identifier
}

fn void test12()
{
	int x = $eval("1abc"); // #error: '1abc' could not be resolved to a valid identifier
}

fn void test13()
{
	int x = $eval("a:::b"); // #error: 'a:::b' could not be resolved to a valid identifier
}

fn void test14()
{
	$assert !$defined($eval("nosuchmodule::foo"));
	$assert !$defined($eval("nosuchmodule::FOO"));
	$assert !$defined($eval("nosuch_ident_at_all"));
}

fn void test15()
{
	bool b = $defined($eval("foo::bar::$bar")); // #error: Unexpected path in 'foo::bar::$bar'
}

macro void @hash_path(#hash)
{
	bool b = $defined($eval("foo::bar::#hash")); // #error: Unexpected path in 'foo::bar::#hash'
}

fn void test16()
{
	int x;
	@hash_path(x);
}

fn void test17()
{
	int $cv = 1;
	bool b = $defined($eval("eval_type_path::$cv")); // #error: Unexpected path in 'eval_type_path::$cv'
}

fn void test18()
{
	Foo f;
	bool b = $defined(f.$eval("foo::$x")); // #error: Unexpected path in 'foo::$x'
}

fn void test19()
{
	var $Tt = int;
	bool b = $defined($Typefrom("foo::$Tt")); // #error: Unexpected path in 'foo::$Tt'
}

fn void test20()
{
	bool b = $defined($Typefrom("foo::int")); // #error: Unexpected path in 'foo::int'
}
