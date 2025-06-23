module test_2212;

macro test(int a = 123, int $b = 10, int c = -9,)
{
}

fn int main()
{
	test(a: 5, c: 5, $b: 5); // #error: Named arguments must always be declared
	return 0;
}
