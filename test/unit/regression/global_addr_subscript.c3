module global_addr_subscript;

struct GasInner { char c; long l; }
struct GasOuter { int a; GasInner[2] in; }

GasOuter g_outer;
GasInner[3] g_inners;
int[4] g_arr;
int[2][3] g_matrix;

GasInner* p_outer_in = &g_outer.in[1];
long* p_inner_field = &g_inners[1].l;
long* p_outer_in_field = &g_outer.in[1].l;
int* p_arr = &g_arr[2];
int* p_matrix = &g_matrix[2][1];
int[2]* p_matrix_row = &g_matrix[1];

fn void global_addr_through_subscript() @test
{
	assert(p_outer_in == &g_outer.in[1]);
	assert(p_inner_field == &g_inners[1].l);
	assert(p_outer_in_field == &g_outer.in[1].l);
	assert(p_arr == &g_arr[2]);
	assert(p_matrix == &g_matrix[2][1]);
	assert(p_matrix_row == &g_matrix[1]);
	*p_inner_field = 42;
	assert(g_inners[1].l == 42);
	*p_matrix = 7;
	assert(g_matrix[2][1] == 7);
}

fn void static_local_addr_through_subscript() @test
{
	static GasInner[2] s_inners;
	static long* p = &s_inners[1].l;
	*p = 5;
	assert(s_inners[1].l == 5);
}

struct GasPair { int[2] a; }

int[4] g_arr4 = { 1, 2, 3, 4 };
const int[4] C_ARR4 = { 5, 6, 7, 8 };
const int* C_PTR = &g_arr4[1];
int[<4>] g_vec4 = { 1, 2, 3, 4 };
GasPair g_pair;
const GasPair* C_PAIR = &g_pair;

int* q_const_ptr_index = &C_PTR[1];
int* q_const_ptr_deref = &(*C_PTR);
int* q_vector_elem = &g_vec4[2];
int* q_const_array = &C_ARR4[2];
int* q_from_end = &g_arr4[^1];
int* q_member_array = &g_pair.a[1];
int* q_const_ptr_member = &C_PAIR.a[1];

fn void global_addr_corner_cases() @test
{
	assert(q_const_ptr_index == &g_arr4[2]);
	assert(*q_const_ptr_index == 3);
	assert(q_const_ptr_deref == &g_arr4[1]);
	assert(q_vector_elem == &g_vec4[2]);
	assert(*q_vector_elem == 3);
	assert(*q_const_array == 7);
	assert(q_from_end == &g_arr4[3]);
	assert(q_member_array == &g_pair.a[1]);
	assert(q_const_ptr_member == &g_pair.a[1]);
}
