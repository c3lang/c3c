module taddr_const_copy;

const int[3] CA = { 1, 2, 3 };
struct Pt { int x; }
const Pt CP = { 7 };

macro int[] @take() => &&CA;

fn void taddr_of_constant_is_a_copy() @test
{
	int[] sl = &&CA;
	sl[1] = 30;
	int[3]* ap = &&CA;
	(*ap)[0] = 10;
	Pt* p = &&CP;
	p.x = 70;
	int[] m = @take();
	m[2] = 90;
	assert(sl[1] == 30 && (*ap)[0] == 10 && p.x == 70 && m[2] == 90);
	assert(CA[0] == 1 && CA[1] == 2 && CA[2] == 3 && CP.x == 7);
}

enum Foo : (Pt pt) { A { { 5 } } }
const int[] CS = { 7, 8 };
int[3]* g_arr = &&CA;
int* g_elem = &&CA[1];
int* g_member = &&CP.x;
int[]* g_slice = &&CS;
Pt* g_assoc = &&Foo.A.pt;

fn void global_taddr_of_constant_is_a_copy() @test
{
	static int* s_elem = &&CA[2];
	(*g_arr)[0] = 10;
	*g_elem = 20;
	*g_member = 70;
	*g_slice = {};
	g_assoc.x = 50;
	*s_elem = 30;
	assert((*g_arr)[0] == 10 && *g_elem == 20 && *g_member == 70 && g_slice.len == 0 && g_assoc.x == 50 && *s_elem == 30);
	assert(CA[0] == 1 && CA[1] == 2 && CA[2] == 3 && CP.x == 7 && CS.len == 2 && Foo.A.pt.x == 5);
}

fn void address_into_const_slice_is_not_a_copy() @test
{
	int i = 1;
	int[] v = CS;
	int[] t = CS[i..];
	assert(&CS[1] == &v[1] && &CS[1] == &CS[i] && &t[0] == &v[1]);
}
