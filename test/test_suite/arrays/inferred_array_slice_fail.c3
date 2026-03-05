import std;
macro  test(int[1][1] a) {
	var b = a;
}

fn int main()
{
	int[][1] b;
	test(b); // #error:  It is not possible to cast 'int[][1]' to 'int[1][1]'
	return 12;
}