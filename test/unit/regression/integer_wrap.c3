module integer_wrap;

fn void signed_wraps() @test
{
	int i = int::max;
	i += 1;
	assert(i == int::min);
	i--;
	assert(i == int::max);
	i++;
	assert(i == int::min);
	assert(-i == int::min);
	long l = long::max;
	assert(l * 2 == -2);
	assert(-(l + 1) == long::min);
	ichar c = 127;
	c += 1;
	assert(c == -128);
	short s = short::min;
	s -= 1;
	assert(s == short::max);
	int one = 1;
	assert(one << 31 == int::min);
	assert(int::min << one == 0);
}

fn void unsigned_narrow_wraps() @test
{
	ushort a = 65535;
	ushort b = 65535;
	assert((ushort)(a * b) == 1);
	char x = 200;
	assert((char)(x * x) == 64);
	ushort h = 0x8000;
	assert((ushort)(h << 1) == 0);
}

fn void vector_wraps() @test
{
	int[<4>] v = { int::max, int::min, 1, -1 };
	int[<4>] w = v + (int[<4>]){ 1, -1, 0, 0 };
	assert(w[0] == int::min && w[1] == int::max);
	int[<4>] n = -v;
	assert(n[1] == int::min && n[3] == 1);
	int[<4>] m = v * 2;
	assert(m[0] == -2 && m[1] == 0);
}
