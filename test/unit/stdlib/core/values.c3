module std::core::values @test;

fn void test_select()
{
	const int X = @select(true, 1, "Hello");
	const String Y = @select(false, 1, "Hello");
	test::eq(X, 1);
	test::eq(Y, "Hello");
}