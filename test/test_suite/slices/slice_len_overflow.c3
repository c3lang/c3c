fn void test1()
{
	int[4] y;
	int[] x = y[1:9_223_372_036_854_775_807]; // #error: End index out of bounds, was 9223372036854775807, exceeding max index 3.
}

fn void test2()
{
	int[4] y;
	int[] x = y[2:9_223_372_036_854_775_807]; // #error: End index out of bounds, was 9223372036854775808, exceeding max index 3.
}
