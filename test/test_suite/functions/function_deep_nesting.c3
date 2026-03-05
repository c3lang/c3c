fn void test(int a = test()) // #error: Resolution failed due to too deeply nested scopes (257).
{
}
fn void main()
{
	test();
}