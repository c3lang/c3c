module std::test;
fn void main()
{
	$$veccompgt[1]; // #error:   Builtin functions cannot be subscripted
}
fn void test()
{
	$$veccompeq[1] = 33; // #error:  Builtin functions cannot be subscripted
}

fn void test2()
{
	$$veccomple[..] = 52; // #error: Builtin functions cannot be sliced.
}