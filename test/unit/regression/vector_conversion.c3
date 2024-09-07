module test @test;

fn void vector_array_inferred()
{
	int[<2>] x = { 4, 7 };
	int[2] y = x;
	int[*] y1 = y;
	int[*] y2 = x;
	int[<*>] z = x;
	int[<*>] w = y;
	double[<2>] ww = x;
	assert((int[<2>])y == int[<2>]{ 4, 7});
	assert((int[<2>])y1 == int[<2>] { 4, 7 });
	assert((int[<2>])y2 == int[<2>] { 4, 7 });
	assert(z == int[<2>] { 4, 7 });
	assert(w == int[<2>] { 4, 7 });
}

fn void vector_convert_slice()
{
	int[<2>] x = { 1, 2 };
	int[2] y = { 4, 4 };
	x *= y[:2];
	assert(x == { 4, 8 });
}
fn void vector_convert_bool()
{
	bool[<2>] a = { true, false };
	assert(int[<2>] { -1, 0 } == (int[<2>])a);
	assert(float[<2>] { 1.0, 0 } == (float[<2>])a);
}