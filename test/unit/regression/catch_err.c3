module catch_err @test;

fn void test()
{
	fault a;
	int? z = fn int?(fault* a) {
		const ABC = 4;
		int? x = NOT_FOUND~;
		defer (catch err) *a = err;
		return x;
	}(&a);
	assert(a == NOT_FOUND);
	fault y;
	z = fn int?(fault* y) {
		const ABC = 4;
		int? x = 1;
		defer (catch err) *y = err;
		return x;
	}(&y);
	assert(!y);
}