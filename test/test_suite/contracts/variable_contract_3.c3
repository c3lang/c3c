<* @require [a] a < 0 *>
fn int test(int x, int a)
{
	return a + a;
}

fn int main()
{
	int doekf = 1;
	test(3, 1); // #error: @require "a < 0" violated.
	return 0;
}