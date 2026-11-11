import std::io;

fn void test_int_mod() @test
{
	int[<2>] y = { 10, 99 };
	int[<2>] z = { 3, 5 };
	assert(y % z == { 1, 4 });
	assert(y / z == { 3, 19 });
	assert((int[<2>]){ 10, 99 } % { 3, 5 } == { 1, 4 });
	assert((int[<2>]){ 10, 99 } / { 3, 5 } == { 3, 19 });
}

fn void test_float_mod() @test
{
	float[<2>] v = { 7.5, -3.5 };
	float[<2>] w = { 2, 2 };
	assert(v % w == { 1.5, -1.5 });
	assert(v % 2 == { 1.5, -1.5 });
	v %= 2;
	assert(v == { 1.5, -1.5 });
	double[<2>] d = { 7.5, 9 };
	d %= { 2, 4 };
	assert(d == { 1.5, 1 });
}

fn void test_swizzle_assign() @test
{
	int[<3>] abc;
	abc.xy = { 3, 4 };
	assert(abc == { 3, 4, 0 });
	abc.xz += { 2, 7 };
	assert(abc == { 5, 4, 7 });
	abc.xy += { 4, -100 };
	assert(abc == { 9, -96, 7 });
	abc.xz = { 0, 0 };
	assert(abc == { 0, -96, 0});
}

fn void test_swizzle_assign_bool() @test
{
	bool[<3>] abc;
	abc.xy = { true, true };
	assert(abc == { true, true, false });
	abc.yz ^= { true, true };
	assert(abc == { true, false, true });
	abc ^= true;
	assert(abc == { false, true, false });
	abc.xy ^= true;
	assert(abc == { true, false, false });
	assert((abc.yx ^= true) == { true, false });
	assert(abc == { false, true, false });
}

fn void test_conv() @test
{
    float[<4>] y = { 1, 2, 3, 4 };
    float[<4>] z = { 0, 2, 2, -100 };
	int[<4>] w = { -1, 2, 3, 4 };
	float[<4>] yy = w;
	assert(yy == { -1.0, 2.0, 3.0, 4.0 });
	ulong[<4>] ww = (ulong[<4>])w;
	assert(ww == { (ulong)-1, 2, 3, 4 });
	int[<4>] g = (int[<4>])ww;
	assert(g == w);
	ww = (ulong[<4>])y;
	assert(ww == { 1, 2, 3, 4 });
	bool[<2>] b = { true, false };
	int[<2>] gh = b;
	assert(gh == { -1, 0 });
	var $k = (bool[<2>]){ true, false };
	var $gh = (int[<2>])$k;
	$assert $gh[0] == -1;
	var $gh2 = (char[<2>])$gh;
	$assert $gh2[0] == 255;
	b = (bool[<2>])gh;
	assert(b == { true, false });
}

fn void testf() @test
{
	float[<4>] x = { 4, 0, -1, 33 };
	assert({ true, false, true, true} == (bool[<4>])x);
    float[<4>] y = { 1, 2, 3, 4 };
    float[<4>] z = { 2, 2, 2, -100 };
    float[<4>] w = y + z;
    assert(w == { 3, 4, 5, -96 });
    w = y * z;
    assert(w == { 2, 4, 6, -400 });
    w = y / z;
    assert(w == { 0.5, 1.0, 1.5, -0.04 });
    w = y - z;
    assert(w == { -1, 0, 1, 104 });
    int[<4>] ww = y.comp_lt(z);
    assert(ww == { -1, 0, 0, 0 });
    ww = y.comp_le(z);
    assert(ww == { -1, -1, 0, 0 });
    ww = y.comp_gt(z);
    assert(ww == { 0, 0, -1, -1 });
    ww = y.comp_ge(z);
    assert(ww == { 0, -1, -1, -1 });
    ww = y.comp_eq(z);
    assert(ww == { 0, -1, 0, 0 });
    ww = y.comp_ne(z);
    assert(ww == { -1, 0, -1, -1 });
}

fn void testb() @test
{
    bool[<4>] y = { true, false, true, true };
    bool[<4>] z = { false, false, true, true };
    ichar[<4>] ww = y.comp_lt(z);
    assert(ww == { 0, 0, 0, 0 });
    ww = y.comp_le(z);
    assert(ww == { 0, -1, -1, -1 });
    ww = y.comp_gt(z);
	assert(ww == { -1, 0, 0, 0 });
    ww = y.comp_ge(z);
    assert(ww == { -1, -1, -1, -1 });
    ww = y.comp_eq(z);
    assert(ww == { 0, -1, -1, -1 });
    ww = y.comp_ne(z);
	assert(ww == { -1, 0, 0, 0 });
}

fn void testi() @test
{
	int[<4>] x = { 4, 0, -1, 33 };
	assert({ true, false, true, true} == (bool[<4>])x);
    int[<4>] y = { 1, 2, 3, 4 };
    int[<4>] z = { 2, 2, 2, -100 };
    int[<4>] w = y + z;
    assert(w == { 3, 4, 5, -96 });
    w = y * z;
    assert(w == { 2, 4, 6, -400 });
    w = y / z;
    assert(w == { 0, 1, 1, 0 });
    w = y - z;
    assert(w == { -1, 0, 1, 104 });
    w = z >> y;
    assert(w == { 1, 0, 0, -7});
    w = z << y;
    assert(w == { 4, 8, 16, -1600 });
    w = z.comp_gt(y);
    assert(w == { -1, 0, 0, 0});
    w = z.comp_ge(y);
    assert(w == { -1, -1, 0, 0 });
    w = z.comp_lt(y);
    assert(w == { 0, 0, -1, -1 });
    w = z.comp_le(y);
    assert(w == { 0, -1, -1, -1 });
    w = z.comp_eq(y);
    assert(w == { 0, -1, 0, 0 });
    w = y.comp_ne(z);
    assert(w == { -1, 0, -1, -1 });
}

fn void test_unsigned_vector_div() @test
{
	uint[<4>] a = { 1, 0x8000_0000, 0xFFFF_FFFF, 5 };
	assert(a / 3 == { 0, 0x2AAA_AAAA, 0x5555_5555, 1 });
	assert(a % 3 == { 1, 2, 0, 2 });
	assert(a >> 4 == { 0, 0x0800_0000, 0x0FFF_FFFF, 0 });
	a /= 3;
	assert(a == { 0, 0x2AAA_AAAA, 0x5555_5555, 1 });
	char[<2>] c = { 200, 255 };
	assert(c / 2 == { 100, 127 });
	assert(c >> 1 == { 100, 127 });
}

fn void test_vector_index_from_end() @test
{
	int[<4>] v = { 1, 2, 3, 4 };
	int i = 1;
	v[^i] = 70;
	assert(v == { 1, 2, 3, 70 });
	i = 4;
	v[^i] += 10;
	assert(v == { 11, 2, 3, 70 });
	i = 2;
	v[^i]++;
	assert(v == { 11, 2, 4, 70 });
}

fn int write_last(int[<4>]* v)
{
	(*v)[3] = 99;
	return 5;
}

fn int write_first_index(int[<4>]* v)
{
	(*v)[0] = 42;
	return 1;
}

fn int[<2>] write_last2(int[<4>]* v)
{
	(*v)[3] = 77;
	return { 5, 6 };
}

fn void test_vector_assign_side_effects() @test
{
	int[<4>] v = { 1, 2, 3, 4 };
	v[0] = write_last(&v);
	assert(v == { 5, 2, 3, 99 });
	v = { 1, 2, 3, 4 };
	v[1] += write_last(&v);
	assert(v == { 1, 7, 3, 99 });
	v = { 1, 2, 3, 4 };
	v[write_first_index(&v)] = 9;
	assert(v == { 42, 9, 3, 4 });
	v = { 1, 2, 3, 4 };
	v[write_first_index(&v)]++;
	assert(v == { 42, 3, 3, 4 });
	v = { 1, 2, 3, 4 };
	v.xy = write_last2(&v);
	assert(v == { 5, 6, 3, 77 });
	v = { 1, 2, 3, 4 };
	v.xy += write_last2(&v);
	assert(v == { 6, 8, 3, 77 });
}

fn void test_swizzle_inc_dec() @test
{
	int[<3>] f = { 1, 2, 3 };
	f.yz++;
	assert(f == { 1, 3, 4 });
	++f.xz;
	assert(f == { 2, 3, 5 });
	f.zx--;
	assert(f == { 1, 3, 4 });
	int[<2>] r = f.xy++;
	assert(r == { 1, 3 });
	assert(f == { 2, 4, 4 });
	r = --f.yx;
	assert(r == { 3, 1 });
	assert(f == { 1, 3, 4 });
	float[<2>] g = { 1.5, 2.5 };
	g.yx++;
	assert(g == { 2.5, 3.5 });
}

struct NotVecHolder
{
	bool[<4>] b;
	int guard;
}

fn void test_vector_not() @test
{
	int[<4>] a = { 1, 0, 5, 0 };
	bool[<4>] b = !a;
	assert(b == { false, true, false, true });
	float[<2>] f = { 0.0f, 2.5f };
	bool[<2>] nf = !f;
	assert(nf == { true, false });
	double[<2>] d = { 3.0, 0.0 };
	assert(!d == { false, true });
	long[<2>] l = { 0, 1L << 40 };
	assert(!l == { true, false });
	char[<4>] c = { 0, 1, 0, 255 };
	assert(!c == { true, false, true, false });
	bool[<4>] bb = { true, false, false, true };
	assert(!bb == { false, true, true, false });
	int[<4>] mask = !a;
	assert(mask == { 0, -1, 0, -1 });
	NotVecHolder h = { .guard = 0x1234_5678 };
	h.b = !a;
	assert(h.b == { false, true, false, true });
	assert(h.guard == 0x1234_5678);
}

faultdef NOT_VEC_FAULT;

fn int[<2>]? not_vec_source(bool fail)
{
	if (fail) return NOT_VEC_FAULT~;
	return { 0, 3 };
}

fn bool[<2>]? bool_vec_source(bool fail)
{
	if (fail) return NOT_VEC_FAULT~;
	return { true, false };
}

fn void test_vector_not_optional() @test
{
	bool[<2>]? r = !not_vec_source(false);
	assert(r!! == { true, false });
	r = !not_vec_source(true);
	assert(@catch(r) == NOT_VEC_FAULT);
	r = !bool_vec_source(false);
	assert(r!! == { false, true });
	r = !bool_vec_source(true);
	assert(@catch(r) == NOT_VEC_FAULT);
}
