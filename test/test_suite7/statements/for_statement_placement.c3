fn void test(int x)
{}

fn void main()
{
	for (int i = 0; i < 10; i++) test(
		i
	);
	for (int i = 0; i < 10; i++)
		test( // #error: A single statement after
		i
	);
}