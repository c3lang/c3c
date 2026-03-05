module lambda;

macro @test(int $xd = ...)
{
	return fn int() {
		static int tx = 0;
		$if $defined($xd):
			return $xd + tx++;
		$else
			return 0;
		$endif
	};
}

fn void test_lambda_in_macro() @test
{
	assert(@test(2)() == 2);
	assert(@test(2)() == 3);
	assert(@test(2)() == 4);
	assert(@test(2)() == 5);
	assert(@test(4)() == 4);
	assert(@test()() == 0);
}
