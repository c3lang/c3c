module comparison @test;

fn void compare_long()
{
	assert(compare_to(long.max, long.min) == 1);
	assert(compare_to(long.min, long.max) == -1);
	assert(compare_to(long.min, long.min) == 0);
}

fn void compare_ulong()
{
	assert(compare_to(ulong.max, ulong.min) == 1);
	assert(compare_to(ulong.min, ulong.max) == -1);
	assert(compare_to(ulong.min, ulong.min) == 0);
}

fn void compare_int128()
{
	assert(compare_to(int128.max, int128.min) == 1);
	assert(compare_to(int128.min, int128.max) == -1);
	assert(compare_to(int128.min, int128.min) == 0);
}
