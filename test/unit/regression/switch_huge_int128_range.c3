module switch_huge_int128_range;
fn String u(uint128 x)
{
	switch (x)
	{
		case 0..uint128::max / 2: return "low";
		case uint128::max / 2 + 1..uint128::max: return "high";
	}
	return "none";
}
fn String s(int128 x)
{
	switch (x)
	{
		case -5..int128::max: return "in";
		default: return "out";
	}
}
fn String full(uint128 x)
{
	switch (x)
	{
		case 0..uint128::max: return "all";
	}
	return "none";
}
fn void huge_int128_ranges() @test
{
	assert(u(5) == "low");
	assert(u(uint128::max) == "high");
	assert(s(-6) == "out");
	assert(s(-5) == "in");
	assert(s(int128::max) == "in");
	assert(full(123) == "all");
}
