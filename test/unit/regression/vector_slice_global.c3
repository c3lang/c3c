module vector_slice_global_test @test;

int[<2>][] g_int2 = { { 19, 4 }, { 1, 2 } };
float[<3>][] g_float3 = { { 1.5, 2.5, 3.5 } };

struct Design
{
	int[<2>][] asleep;
}

const Design[*] DESIGNS = {
	{ .asleep = { { 19, 4 } } },
	{ .asleep = { { 10, 20 }, { 35, 17 } } },
};

fn void global_vector_slice()
{
	assert(g_int2.len == 2);
	assert(g_int2[0] == { 19, 4 });
	assert(g_int2[1] == { 1, 2 });
	assert(g_float3[0] == { 1.5, 2.5, 3.5 });
}

fn void vector_slice_in_const_struct()
{
	assert(DESIGNS[0].asleep.len == 1);
	assert(DESIGNS[0].asleep[0] == { 19, 4 });
	assert(DESIGNS[1].asleep[1] == { 35, 17 });
}

fn void static_vector_slice()
{
	static int[<2>][] s = { { 7, 8 } };
	assert(s[0] == { 7, 8 });
}
