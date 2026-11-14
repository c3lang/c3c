module array_compare_order;

fn int[4] arr_bump(int[4]* p) { int[4] old = *p; (*p)[0] = 99; return old; }
fn int[2][2] nested_bump(int[2][2]* p) { int[2][2] old = *p; (*p)[1][1] = 99; return old; }
fn double[3] float_bump(double[3]* p) { double[3] old = *p; (*p)[2] = 0.5; return old; }

fn void array_eq_reads_left_first() @test
{
	int[4] a = { 1, 2, 3, 4 };
	assert(a == arr_bump(&a));
	assert(a[0] == 99);
}

fn void array_ne_reads_left_first() @test
{
	int[4] a = { 1, 2, 3, 4 };
	assert(!(a != arr_bump(&a)));
}

fn void nested_array_eq_reads_left_first() @test
{
	int[2][2] n = { { 1, 2 }, { 3, 4 } };
	assert(n == nested_bump(&n));
}

fn void float_array_eq_reads_left_first() @test
{
	double[3] d = { 1.0, 2.0, 3.0 };
	assert(d == float_bump(&d));
}

fn void array_eq_through_pointer_reads_left_first() @test
{
	int[4] a = { 1, 2, 3, 4 };
	int[4]* p = &a;
	assert(*p == arr_bump(p));
}
