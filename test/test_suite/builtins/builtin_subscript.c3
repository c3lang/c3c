fn void main()
{
	$$veccompgt[1]; // #error: Builtins cannot be subscripted.
}
fn void test()
{
	$$veccompeq[1] = 33; // #error: Builtins cannot be subscripted.
}

fn void test2()
{
	$$veccomple[..] = 52; // #error: A builtin cannot be sliced.
}