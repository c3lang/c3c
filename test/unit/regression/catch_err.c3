module catch_err @test;

fn void test()
{
	anyfault a;
	int! z = {|
		const ABC = 4;
		int! x = SearchResult.MISSING?;
		defer (catch err) a = err;
		return x;
	|};
	assert(a == SearchResult.MISSING);
	anyfault y;
	z = {|
		const ABC = 4;
		int! x = 1;
		defer (catch err) y = err;
		return x;
	|};
	assert(!y);
}