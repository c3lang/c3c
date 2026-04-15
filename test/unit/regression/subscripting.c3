module subscripting_tests @test;

fn void subscript_ct()
{
	$if int::name[0] == 'i':
		assert(true);
	$else
		assert(false);
	$endif
	$if int::name[^1] == 't':
		assert(true);
	$else
		assert(false);
	$endif
}