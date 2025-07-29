import std;
fn void main()
{
	(void)test();
}


fn ulong? test() @naked @noinline
{
	int? z;
	z!; // #error: Rethrow is not allowed in a '@naked' function
	unreachable();
}

fn ulong test2(int a) @naked @noinline
{
	int x = a; // #error: Parameters may not be directly accessed in '@naked' functions
	unreachable();
}