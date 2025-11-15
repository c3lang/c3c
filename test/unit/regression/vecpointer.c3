module vecpointer @test;

fn void pointer_npot2_size()
{
	int[<9>][3] a;
	assert((usz)&a[1] - (usz)&a[0] == 9 * 4);
}

fn void pointer_add_sub_diff()
{
	int[5] a;
	void*[<2>] x = { &a[0], &a[4] };
	int*[<2>] y = x;
	assert(x[0] == y[0] && x[1] == y[1]);
	int*[2] y2;
	y2 = y;
	assert(y2[0] == x[0] && y2[1] == x[1]);
	y = { null, null };
	assert(y[0] == null && y[0] == null);
	y = y2;
	assert(x[0] == y[0] && x[1] == y[1]);
	int[<2>] z = { 1, -1 };
	y = y + z;
	assert(y[0] == &a[1] && y[1] == &a[3]);
	y = y - z;
	assert(y[0] == &a[0] && y[1] == &a[4]);
	int*[<2>] yy = { &a[1], &a[2] };
	isz[<2>] w;
	w = y - yy;
	assert(w == { -1, 2 });
	int*[<2>] zz = y - (y - yy);
	assert(zz[0] == &a[1] && zz[1] == &a[2]);
	int[*]*[<2>] g = (int[2]*[<2>]) { null, null };
	int[*]*[<*>] g2 = (int[2]*[<2>]) { null, null };
}