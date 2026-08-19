<* @require [a] a > 0 : "test" *> // #error: Invalid constraint - '@require' with variable references is only valid for functions, macros and function types.
struct Foo <Test>
{
	int a;
}
macro int test(int x, int $a)
{
	return $a + $a;
}

fn int main()
{
	int doekf = 1;
	test(3, 12);
	return 0;
}