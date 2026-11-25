module taddr_copies;

faultdef BAD;
int g = 7;
struct Bar { int a; int b; }
typedef IntPtr = inline int*;

fn void nullit(int** pp) { *pp = null; }
fn int? opt(bool fail) => fail ? BAD~ : 5;
fn Bar make() => { 1, 2 };

fn void taddr_local_copies() @test
{
	int* p = null;
	int** pp = &&p;
	*pp = &g;
	assert(p == null);
	assert(*pp == &g);
}

fn void taddr_cast_copies() @test
{
	IntPtr ip = (IntPtr)&g;
	nullit(&&(int*)ip);
	assert(ip == (IntPtr)&g);
}

fn void taddr_member_copies() @test
{
	Bar b = { 3, 4 };
	int* x = &&b.b;
	*x = 9;
	assert(b.b == 4);
	assert(*x == 9);
}

fn void taddr_element_copies() @test
{
	int[3] arr = { 1, 2, 3 };
	int* e = &&arr[1];
	*e = 0;
	assert(arr[1] == 2);
	int*[1] pa = { &g };
	int** q = &&pa[0];
	*q = null;
	assert(pa[0] == &g);
}

fn void taddr_rvalues() @test
{
	int* y = &&make().b;
	assert(*y == 2);
	int* c = &&5;
	assert(*c == 5);
	int*? z = &&opt(false);
	assert(*z!! == 5);
	int*? w = &&opt(true);
	assert(!@ok(w));
}
