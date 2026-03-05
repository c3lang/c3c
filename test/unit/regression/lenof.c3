module regression;
import std;

struct Foo
{
	inline int[2] a;
}

fn void lengthof_aggregates() @test
{
	Foo g;
	g.len;
	String foo = "abc";
	test::eq(lengthof(g), 2);
	test::eq(lengthof(foo), 3);
}

fn void lenof_test() @test
{
	List{int} l;
	l.push(123);
	l.push(222);
	l.push(111);
	test::eq(lengthof(l), 3);
	int[] x = { 1, 2 };
	test::eq(lengthof(x), 2);
	l.push(111);
	test::eq(lengthof(l), 4);
	assert(!$defined(lengthof(1)));
	assert($defined(lengthof(x)));
	int* zz;
	assert(!$defined(lengthof(zz)));
}