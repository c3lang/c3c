module const_int_div_overflow;

fn void const_min_div_minus_one() @test
{
	const int V = int::min / -1;
	const long L = long::min / -1;
	const int128 W = int128::min / -1;
	bool v_pos = V > 0;
	bool l_pos = L > 0;
	bool w_pos = W > 0;
	assert(!v_pos && !l_pos && !w_pos);
	assert(V == int::min);
	assert(L == long::min);
	assert(W == int128::min);
}

fn void const_min_rem_minus_one() @test
{
	const int V = int::min % -1;
	const long L = long::min % -1;
	assert(V == 0);
	assert(L == 0);
}
