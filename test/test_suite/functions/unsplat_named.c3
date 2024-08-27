fn void test(int... abc, int a)
{  }

fn void main()
{
	int[] b = { 1, 2 };
	test(...b, a: 123);
}