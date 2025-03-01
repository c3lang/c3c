macro @test(int x = 1; @body())
{
	if (x > 0) @body();
	return x;
}

fn int foo(int x) => @test(x) // #error: The macro itself returns 'int' here
{
	return 2;
}
