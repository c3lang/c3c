module catch_err @test;

fn void test()
{
	anyfault a;
	int? z = fn int?(anyfault* a) {
		const ABC = 4;
		int? x = NOT_FOUND?;
		defer (catch err) *a = err;
		return x;
	}(&a);
	assert(a == NOT_FOUND);
	anyfault y;
	z = fn int?(anyfault* y) {
		const ABC = 4;
		int? x = 1;
		defer (catch err) *y = err;
		return x;
	}(&y);
	assert(!y);
}