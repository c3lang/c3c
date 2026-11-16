module defer_macro_rethrow_copy;

faultdef BAD;
int macro_defers;
int ok_values;

fn int? opt(int x) => x > 5 ? x : BAD~;

macro int? @mac(int r)
{
	defer macro_defers++;
	int v = opt(r)!;
	ok_values += v;
	return v;
}

fn void one_exit(int r)
{
	defer (void)@mac(r);
}

fn int two_exits(int r)
{
	defer (void)@mac(r);
	if (r > 100) return 1;
	return 0;
}

fn void rethrow_in_macro_called_from_defer() @test
{
	macro_defers = 0;
	ok_values = 0;
	one_exit(0);
	assert(macro_defers == 1 && ok_values == 0);
	one_exit(7);
	assert(macro_defers == 2 && ok_values == 7);
	two_exits(3);
	assert(macro_defers == 3 && ok_values == 7);
	two_exits(200);
	assert(macro_defers == 4 && ok_values == 207);
}
