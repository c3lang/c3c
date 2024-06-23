fn Abcd test(Test a) @export
{
	return Abcd.ABC;
}

fn void test2(Abc a) @export // #error: must also be an exported type
{
}

fault Abcd @export
{
	ABC,
	EFG
}
enum Abc
{
	ABC,
	DEF
}
struct Test @export
{
	struct
	{
		int a;
		int b;
	}
	bitstruct : int
	{
		int fa : 1..6;
	}
	int[*] x;

}

struct Test2 @export
{
	struct
	{
		Abc d; // #error: must also be an exported type
	}
}