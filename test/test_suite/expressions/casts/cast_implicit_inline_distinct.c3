typedef DistinctInt = inline int;
typedef DistinctInt2 = inline DistinctInt;

fn void test()
{
	DistinctInt2 y;
	DistinctInt x = y;
	DistinctInt2 z = x; // #error: Implicitly casting
}