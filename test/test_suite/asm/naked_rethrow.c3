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