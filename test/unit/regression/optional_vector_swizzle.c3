module optional_vector_swizzle;

faultdef BAD;

fn int[<4>]? get_vector(bool fail) @noinline
{
	if (fail) return BAD~;
	return { 1, 2, 3, 4 };
}

fn void optional_vector_swizzle() @test
{
	int[<4>]? vector = { 1, 2, 3, 4 };
	int[<2>]? xy = vector.xy;
	assert(xy!! == { 1, 2 });

	int[<3>]? repeated = vector.wxx;
	assert(repeated!! == { 4, 1, 1 });

	int[<2>]? chained = get_vector(false).wzyx.xy;
	assert(chained!! == { 4, 3 });

	int[<2>]? failed = get_vector(true).xy;
	assert(@catch(failed) == BAD);

	int[<4>]? bad = BAD~;
	failed = bad.zw;
	assert(@catch(failed) == BAD);
}
