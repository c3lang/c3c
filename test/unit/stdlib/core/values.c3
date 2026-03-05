module std::core::values @test;

fn void test_select()
{
	const int X = true ??? 1 : "Hello";
	const String Y = false ??? 1 : "Hello";
	test::eq(X, 1);
	test::eq(Y, "Hello");
}