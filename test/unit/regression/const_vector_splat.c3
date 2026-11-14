module const_vector_splat_test @test;

// Primitive types
bool[<4>] g_bool = (bool[<4>])true;
char[<16>] g_char = (char[<16>])'Z';
short[<8>] g_short = (short[<8>])0x1234;
float[<3>] g_float = (float[<3>])-1.5f;
double[<2>] g_double = (double[<2>])3.1415926535;

// Non-power-of-two vectors
int[<3>] g_npot3 = (int[<3>])7;
int[<5>] g_npot5 = (int[<5>])42;

// Expressions in splat
const int BASE_VAL = 100;
int[<4>] g_expr = (int[<4>])(BASE_VAL * 2 + 5);

// Array of vectors
int[<4>][2] g_arr_vec = { (int[<4>])1, (int[<4>])2 };

// Nested structs containing vectors
struct InnerStruct
{
	int[<4>] v;
	float x;
}

struct OuterStruct
{
	InnerStruct inner;
	int[<2>] v2;
}

OuterStruct g_outer = {
	.inner = { .v = (int[<4>])9, .x = 1.0f },
	.v2 = (int[<2>])7
};

// Distinct vector type
typedef MyVec = inline int[<4>];
struct WrapperStruct
{
	MyVec v;
}
WrapperStruct g_wrapper = { .v = (MyVec)5 };

// Vector to array conversion
const int[4] ARR_FROM_VEC = (int[4])((int[<4>])55);

fn void test_const_vector_splat()
{
	assert(g_bool[0] == true && g_bool[3] == true);
	assert(g_char[0] == 'Z' && g_char[15] == 'Z');
	assert(g_short[0] == 0x1234 && g_short[7] == 0x1234);
	assert(g_float[0] == -1.5f && g_float[2] == -1.5f);
	assert(g_double[0] == 3.1415926535 && g_double[1] == 3.1415926535);

	assert(g_npot3[0] == 7 && g_npot3[2] == 7);
	assert(g_npot5[0] == 42 && g_npot5[4] == 42);

	assert(g_expr[0] == 205 && g_expr[3] == 205);

	assert(g_arr_vec[0][0] == 1 && g_arr_vec[0][3] == 1);
	assert(g_arr_vec[1][0] == 2 && g_arr_vec[1][3] == 2);

	assert(g_outer.inner.v[0] == 9 && g_outer.inner.v[3] == 9);
	assert(g_outer.inner.x == 1.0f);
	assert(g_outer.v2[0] == 7 && g_outer.v2[1] == 7);

	assert(g_wrapper.v[0] == 5 && g_wrapper.v[3] == 5);

	assert(ARR_FROM_VEC[0] == 55 && ARR_FROM_VEC[3] == 55);
}
