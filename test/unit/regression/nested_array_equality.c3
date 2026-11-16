module nested_array_equality;

fn void nested_float_array() @test
{
	double[2][2] a = { { -0.0, 1.0 }, { 2.0, 3.0 } };
	double[2][2] b = { { 0.0, 1.0 }, { 2.0, 3.0 } };
	assert(a == b);
	b[1][1] = 4.0;
	assert(a != b);
	switch (a)
	{
		case { { 0.0, 1.0 }, { 2.0, 3.0 } }: break;
		default: unreachable();
	}
}

fn void nested_string_array() @test
{
	char[2] buf = { 'a', 'b' };
	String[1][2] s = { { (String)buf[..] }, { "c" } };
	String[1][2] t = { { "ab" }, { "c" } };
	assert(s == t);
	t[1][0] = "d";
	assert(s != t);
}

fn void slice_of_float_arrays() @test
{
	float[1][2] a = { { -0.0 }, { 1.0 } };
	float[1][2] b = { { 0.0 }, { 1.0 } };
	assert(a[..] == b[..]);
}
