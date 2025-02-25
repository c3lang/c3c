module test;

struct Abc
{
	int x;
	int[?] y;
}


fn void test()
{
	Abc x;
	Abc y;
	bool same = x.y == y.y; // #error: does not support comparisons
}