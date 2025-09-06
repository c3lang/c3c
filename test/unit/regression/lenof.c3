module regression;
import std;


fn void lenof_test() @test
{
	List{int} l;
	l.push(123);
	l.push(222);
	l.push(111);
	test::eq(lenof(l), 3);
	int[] x = { 1, 2 };
	test::eq(lenof(x), 2);
	l.push(111);
	test::eq(lenof(l), 4);
	assert(!$defined(lenof(1)));
	assert($defined(lenof(x)));
	int* zz;
	assert(!$defined(lenof(zz)));
}