import std::math;

fn void vector_method_reduce() @test
{
	float[<3>] x = { 1, 2.0, 4.0 };
	int[<*>] y = { -23, 1, 4 };
	assert(y.sum() == -18);
	assert(y.product() == -92);
	assert(y.max() == 4);
	assert(y.and() == 0);
	assert(y.xor() == -20);
	assert(y.min() == -23);
	assert(y.or() == -19);
	assert(x.sum(1.2) - 8.2 < 0.000001);
	assert(x.product(1.2) - 9.6 < 0.000001);
	assert(x.min() == 1.0);
	assert(x.max() == 4.0);
}