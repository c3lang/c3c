module subscript_swizzle_lvalue;

struct SslHolder { float[<3>][3] arr; }

fn void swizzle_of_subscript_update() @test
{
	SslHolder h;
	h.arr[1].x++;
	h.arr[1].y += 2;
	h.arr[2].z = 3;
	assert(h.arr[1].x == 1 && h.arr[1].y == 2 && h.arr[2].z == 3);
	assert(h.arr[0] == { 0, 0, 0 });
}
