module subscripting_tests @test;

fn void subscript_ct()
{
	$if int.nameof[0] == 'i':
		assert(true);
	$else
		assert(false);
	$endif
	$if int.nameof[^1] == 't':
		assert(true);
	$else
		assert(false);
	$endif
}