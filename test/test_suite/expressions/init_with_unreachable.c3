fn int a() => 0;
bitstruct Bs : int { int lo : 0..7; int hi : 8..15; }
bitstruct Ba : char[2] { int lo : 0..7; int hi : 8..15; }
bitstruct Bbe : int @bigendian { int lo : 0..7; int hi : 8..15; }

fn void init_inferred()
{
	int[<*>]? x = { unreachable(), a() };
}

fn void init_sized()
{
	int[<2>]? x = { unreachable(), a() };
}

fn void init_float()
{
	float[<3>]? x = { unreachable(), (float)a(), 1.0f };
}

fn void assign()
{
	int[<2>]? x;
	x = { unreachable(), a() };
}

fn void unreachable_last()
{
	int[<2>]? x = { a(), unreachable() };
}

fn void bitstruct_init()
{
	Bs b = { unreachable(), a() };
}

fn void bitstruct_designated()
{
	Bs b = { .lo = unreachable(), .hi = a() };
}

fn void bitstruct_optional()
{
	Bs? b = { unreachable(), a() };
}

fn void bitstruct_splat()
{
	Bs b;
	Bs c = { ...b, .lo = unreachable() };
}

fn void bitstruct_array()
{
	Ba x = { unreachable(), a() };
}

fn void bitstruct_array_designated()
{
	Ba y = { .lo = unreachable(), .hi = a() };
}

fn void bitstruct_bigendian()
{
	Bbe x = { .lo = unreachable(), .hi = a() };
}

fn int main()
{
	if (a())
	{
		init_inferred();
		init_sized();
		init_float();
		assign();
		unreachable_last();
		bitstruct_init();
		bitstruct_designated();
		bitstruct_optional();
		bitstruct_splat();
		bitstruct_array();
		bitstruct_array_designated();
		bitstruct_bigendian();
	}
	int[<2>]? x = { a(), a() };
	return x[0]!!;
}
