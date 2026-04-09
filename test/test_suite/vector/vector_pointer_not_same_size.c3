int a;

fn void main()
{
	void*[<2>] x = { &a, &a };
	int[<1>] y = { 1 };
	x + y; // #error: It is not possible to cast 'int[<1>]' to 'sz[<2>]' (long[<2>])
}
