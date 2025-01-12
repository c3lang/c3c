import std::io;

fn void test_int_mod() @test
{
	int[<2>] y = { 10, 99 };
	int[<2>] z = { 3, 5 };
	assert(y % z == { 1, 4 });
	assert(y / z == { 3, 19 });
	assert(int[<2>]{ 10, 99 } % int[<2>]{ 3, 5 } == { 1, 4 });
	assert(int[<2>]{ 10, 99 } / int[<2>]{ 3, 5 } == { 3, 19 });
}

fn void test_conv() @test
{
    float[<4>] y = { 1, 2, 3, 4 };
    float[<4>] z = { 0, 2, 2, -100 };
	int[<4>] w = { -1, 2, 3, 4 };
	float[<4>] yy = w;
	assert(yy == { -1.0, 2.0, 3.0, 4.0 });
	ulong[<4>] ww = w;
	assert(ww == { (ulong)-1, 2, 3, 4 });
	int[<4>] g = (int[<4>])ww;
	assert(g == w);
	ww = (long[<4>])y;
	assert(ww == { 1, 2, 3, 4 });
	bool[<2>] b = { true, false };
	int[<2>] gh = b;
	assert(gh == { -1, 0 });
	var $k = bool[<2>] { true, false };
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
    int[<4>] ww = $$veccomplt(y, z);
    assert(ww == { -1, 0, 0, 0 });
    ww = $$veccomple(y, z);
    assert(ww == { -1, -1, 0, 0 });
    ww = $$veccompgt(y, z);
    assert(ww == { 0, 0, -1, -1 });
    ww = $$veccompge(y, z);
    assert(ww == { 0, -1, -1, -1 });
    ww = $$veccompeq(y, z);
    assert(ww == { 0, -1, 0, 0 });
    ww = $$veccompne(y, z);
    assert(ww == { -1, 0, -1, -1 });
}

fn void testb() @test
{
    bool[<4>] y = { true, false, true, true };
    bool[<4>] z = { false, false, true, true };
    ichar[<4>] ww = $$veccomplt(y, z);
    assert(ww == { 0, 0, 0, 0 });
    ww = $$veccomple(y, z);
    assert(ww == { 0, -1, -1, -1 });
    ww = $$veccompgt(y, z);
	assert(ww == { -1, 0, 0, 0 });
    ww = $$veccompge(y, z);
    assert(ww == { -1, -1, -1, -1 });
    ww = $$veccompeq(y, z);
    assert(ww == { 0, -1, -1, -1 });
    ww = $$veccompne(y, z);
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
    w = $$veccompgt(z, y);
    assert(w == { -1, 0, 0, 0});
    w = $$veccompge(z, y);
    assert(w == { -1, -1, 0, 0 });
    w = $$veccomplt(z, y);
    assert(w == { 0, 0, -1, -1 });
    w = $$veccomple(z, y);
    assert(w == { 0, -1, -1, -1 });
    w = $$veccompeq(z, y);
    assert(w == { 0, -1, 0, 0 });
    w = $$veccompne(z, y);
    assert(w == { -1, 0, -1, -1 });
}
