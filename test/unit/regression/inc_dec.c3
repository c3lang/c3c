fn void vector_inc_dec() @test
{
	int[<3>] x;
	int[<3>] y;
	int z = ++x[0];
	int zz = y[0]++;
	x[1]++;
	++y[2];
	int[<3>] g = x--;
	assert(x == { 0, 0, -1 });
	assert(y == { 1, 0, 1 });
	assert(z == 0 && zz == 1);
	assert(g == { 1, 1, 0 });
}

fn void int_inc_dec() @test
{
	int x;
	assert(x++ == 0);
	assert(x == 1);
	assert(++x == 2);
	assert(x-- == 2);
	assert(--x == 0);
}

fn void float_inc_dec() @test
{
	double x;
	assert(x++ == 0);
	assert(x == 1.0);
	assert(++x == 2.0);
	assert(x-- == 2.0);
	assert(--x == 0);
}
