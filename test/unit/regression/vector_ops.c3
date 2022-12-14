import libc;

fn void! test_div() @test
{
    float[<4>] y = { 1, 2, 3, 4 };
    float[<4>] z = { 0, 2, 2, -100 };

}
fn void! testf() @test
{
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

fn void! testb() @test
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

fn void! testi() @test
{
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
    assert(w == { -1, 0, 0, -1});
    w = $$veccompge(z, y);
    assert(w == { -1, -1, 0, -1 });
    w = $$veccomplt(z, y);
    assert(w == { 0, 0, -1, 0 });
    w = $$veccomple(z, y);
    assert(w == { 0, -1, -1, 0 });
    w = $$veccompeq(z, y);
    assert(w == { 0, -1, 0, 0 });
    w = $$veccompne(z, y);
    assert(w == { -1, 0, -1, -1 });
}
