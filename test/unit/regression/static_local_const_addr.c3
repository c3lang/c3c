module static_local_const_addr;

struct SlcaIn { char c; long l; }
struct SlcaOut { int a; SlcaIn[2] in; struct { int x; int y; } }

SlcaOut g_out;
int[4] g_arr;
const int* C_P = &g_arr[1];

int* g_anon = &g_out.y;
int* g_deref = &(*C_P);
long* g_nested = &g_out.in[1].l;

fn void global_const_addr() @test
{
	assert(g_anon == &g_out.y);
	assert(g_deref == &g_arr[1]);
	assert(g_nested == &g_out.in[1].l);
}

fn void static_local_const_addr() @test
{
	static SlcaOut s_out;
	static int[3] s_arr;
	static long* p_nested = &s_out.in[1].l;
	static int* p_elem = &s_arr[2];
	static int* p_anon = &s_out.y;
	static int* p_global = &g_arr[3];
	static SlcaIn* p_struct = &s_out.in[1];
	static int* p_const_ptr = &C_P[1];
	assert(p_nested == &s_out.in[1].l);
	assert(p_elem == &s_arr[2]);
	assert(p_anon == &s_out.y);
	assert(p_global == &g_arr[3]);
	assert(p_struct == &s_out.in[1]);
	assert(p_const_ptr == &g_arr[2]);
	*p_anon = 7;
	assert(s_out.y == 7);
}
