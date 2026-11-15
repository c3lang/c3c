module empty_vaarg_splat_init;

struct Foo { int a; int b; }

macro usz @slice_len(...)
{
	int[] x = { ...$vaarg[^0..] };
	return x.len;
}

macro usz @slice_all(...)
{
	int[] x = { ...$vaarg };
	return x.len;
}

macro int @slice_sum(...)
{
	int[] x = { ...$vaarg };
	int sum;
	foreach (v : x) sum += v;
	return sum;
}

macro int[3] @array(...)
{
	int[3] x = { ...$vaarg };
	return x;
}

macro Foo @strukt(...)
{
	Foo f = { ...$vaarg };
	return f;
}

macro usz @string_slice(...)
{
	String[] s = { ...$vaarg };
	return s.len;
}

macro usz @with_prefix(...)
{
	int[] x = { 7, ...$vaarg };
	return x.len;
}

macro usz @forward(...)
{
	return @slice_all(...$vaarg);
}

fn void empty_vaarg_splat() @test
{
	assert(@slice_len() == 0);
	assert(@slice_len(1, 2, 3) == 0);
	assert(@slice_all() == 0);
	assert(@slice_all(1, 2) == 2);
	assert(@slice_sum() == 0);
	assert(@slice_sum(1, 2, 3) == 6);
	assert(@array()[2] == 0);
	assert(@array(4, 5, 6)[2] == 6);
	assert(@strukt().a == 0);
	assert(@strukt(1, 2).b == 2);
	assert(@string_slice() == 0);
	assert(@string_slice("a", "b") == 2);
	assert(@with_prefix() == 1);
	assert(@with_prefix(1, 2) == 3);
	assert(@forward() == 0);
	assert(@forward(1, 2, 3) == 3);
}

fn void runtime_splat() @test
{
	int[2] two = { 1, 2 };
	int[] y = { ...two };
	test::eq(y.len, 2);
	test::eq(y[1], 2);
}
