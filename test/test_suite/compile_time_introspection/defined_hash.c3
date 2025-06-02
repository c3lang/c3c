macro @test(#a)
{
	$defined(#a);
}

fn int main()
{
	@test(0);
	return 0;
}