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

struct PtrBox { int* p; }
enum Tv : (int* p, PtrBox b) { A { &&3, { &&4 } } }
const int* HP = &&5;
int* g_tv = &&6;

fn void taddr_in_constants_is_shared() @test
{
	Tv t = Tv.A;
	assert(Tv.A.p == Tv.A.p && Tv.A.p == t.p && Tv.A.b.p == t.b.p && HP == HP);
	assert(*Tv.A.p == 3 && *Tv.A.b.p == 4 && *HP == 5);
	*g_tv = 60;
	assert(*g_tv == 60 && g_tv == g_tv);
}

int g_any_target = 41;
any* g_any = &&(any)&g_any_target;

fn void taddr_of_runtime_constant_global() @test
{
	assert(g_any == g_any && *(int*)g_any.ptr == 41);
}

const any C_ANY = &g_any_target;
any* g_any_copy = &&C_ANY;

fn void taddr_of_any_constant_global() @test
{
	assert(*(int*)g_any_copy.ptr == 41);
}

int*[3] g_nested = { &&5, NESTED_B[0], NESTED_B[1] };
const int*[2] NESTED_B = { &&7, &&8 };
constset TaddrSet : int* { TA = &&3, TB = &&4 }
int taddr0 = 77;
enum Tv2 : (int* p) { A { &&9 } }
fn int Tv2.taddr0(self) => 42;

fn void taddr_owner_claims_only_its_own() @test
{
	*g_nested[0] = 99;
	assert(*g_nested[0] == 99 && g_nested[1] == NESTED_B[0] && *NESTED_B[1] == 8);
	assert((int*)TaddrSet.TA == (int*)TaddrSet.TA && *(int*)TaddrSet.TB == 4 && taddr0 == 77);
	assert(*Tv2.A.p == 9 && Tv2.A.taddr0() == 42);
}
