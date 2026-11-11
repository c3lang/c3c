// Pointer casts (sema's EXPR_RVALUE) keep their target type: scalars, pointer arithmetic on a cast, pointer vectors.
module pointer_cast_values;

fn void pointer_casts() @test
{
	uint[8] big;
	big[5] = 55;
	uint* p = (uint*)&big + 5;
	assert(*p == 55);
	void* v = p;
	char* c = (char*)v;
	assert(c == (char*)&big[5]);
	int a = 1;
	int b = 2;
	void*[<2>] vp = { &a, &b };
	int*[<2>] ip = (int*[<2>])vp;
	assert(*ip[0] == 1 && *ip[1] == 2);
}
