module test;

fn int main()
{
	static char[(1024 * mem::KB) + 1] a;
	tlocal char[(1024 * mem::KB) + 1] b;
	return 0;
}