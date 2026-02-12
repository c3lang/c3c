module array_zero_filled;

fn void test_array_lenght() @test 
{
	const char[128] SCANCODE_ASCII_MAP =
	{
		0, 27, '1','2','3','4','5','6','7','8','9','0','-','=', 8,
		'\t','q','w','e','r','t','y','u','i','o','p','[',']','\n',
		0, 'a','s','d','f','g','h','j','k','l',';','\'','`',
		0, '\\','z','x','c','v','b','n','m',',','.','/',
		0, '*', 0, ' ',
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	};

	assert(SCANCODE_ASCII_MAP.len == 128);
}

fn void test_zero_fill_explicit() @test 
{
	int[5] arr = {1, 2};
	assert(arr[0] == 1);
	assert(arr[1] == 2);
	assert(arr[2] == 0);
	assert(arr[3] == 0);
	assert(arr[4] == 0);
}

fn void test_multidim_array() @test 
{
	int[4][2] mat = { {1, 2}, {3} };
	assert(mat[0][0] == 1);
	assert(mat[0][1] == 2);
	assert(mat[0][2] == 0);
	assert(mat[0][3] == 0);

	assert(mat[1][0] == 3);
	assert(mat[1][1] == 0);
	assert(mat[1][2] == 0);
	assert(mat[1][3] == 0);
}

struct Foo { int[3] values; }

fn void test_struct_with_array() @test 
{
	Foo foo = { {1, 2} };
	assert(foo.values[0] == 1);
	assert(foo.values[1] == 2);
	assert(foo.values[2] == 0);
}

fn void test_const_array_access() @test 
{
	const int[6] X = { 10, 20, 30 };
	int x3 = X[3];
	int x4 = X[4];
	int x5 = X[5];

	assert(x3 == 0);
	assert(x4 == 0);
	assert(x5 == 0);
}

enum MyEnum 
{
	SLOW,
	FAST
}

struct Bar 
{
	MyEnum en;
	int x;
}

fn void test_const_struct_with_enum() @test
{
	const Bar BAR = {MyEnum.FAST, 1};
	const Bar[3] BARS = {BAR};

	assert(BARS[0].en == MyEnum.FAST);
	assert(BARS[0].x == 1);

	assert(BARS[1].en == MyEnum.SLOW);
	assert(BARS[1].x == 0);

	assert(BARS[2].en == MyEnum.SLOW);
	assert(BARS[2].x == 0);
}