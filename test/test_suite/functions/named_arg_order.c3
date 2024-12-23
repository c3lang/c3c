import std;
macro void test(int a, int $baz)
{
	io::printn(a + $baz);
}

fn void main()
{
	test($baz: 1, a: 4); // #error: Named arguments must always
}