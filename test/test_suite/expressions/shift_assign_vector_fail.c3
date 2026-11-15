fn void test()
{
	int x = 1;
	int[<2>] v = { 1, 2 };
	x <<= v; // #error: The shift cannot be a vector of type 'int[<2>]' when shifting a variable of type 'int'.
}

fn void test2()
{
	long y = 1;
	char[<4>] w = { 1, 2, 3, 4 };
	y >>= w; // #error: The shift cannot be a vector of type 'char[<4>]' when shifting a variable of type 'long'.
}
