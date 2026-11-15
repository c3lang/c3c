module int128_min_to_float;

fn void int128_min_to_float() @test
{
	int128 m = int128::min;
	int128 x = -5;
	int128 big = -((int128)1 << 100) - 1;
	assert((double)m == -0x1p127);
	assert((float)m == -0x1p127f);
	assert((double)x == -5.0);
	assert((float)x == -5.0f);
	assert((double)big == -0x1p100);
}
