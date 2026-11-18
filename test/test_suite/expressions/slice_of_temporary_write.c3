struct Foo { int[3] a; }
fn Foo rf() => {};
fn int[3] ra() => { 1, 2, 3 };
struct Rec { int x; }
fn Rec[2] rs() => { { 1 }, { 2 } };

fn void assign_call()
{
	ra()[0:2][0] = 1; // #error: An assignable expression
}

fn void incdec_call()
{
	ra()[..][1]++; // #error: An assignable expression
}

fn void ternary(bool c)
{
	int[3] a;
	int[3] b;
	(c ? a : b)[0:2][1] = 5; // #error: An assignable expression
}

fn void call_member()
{
	rf().a[1:2][0] += 3; // #error: An assignable expression
}

fn void addr_call()
{
	int* p = &ra()[0:2][1]; // #error: To take the address of a temporary value
}

fn void addr_nested_slice()
{
	int* p = &ra()[..][..][1]; // #error: To take the address of a temporary value
}

fn void addr_nested_slice_member()
{
	int* p = &rs()[..][..][1].x; // #error: To take the address of a temporary value
}

fn void assign_nested_slice()
{
	ra()[..][..][1] = 3; // #error: An assignable expression
}

fn void valid(int[] s, int* p)
{
	int[3] a;
	Foo f;
	a[0:2][0] = 7;
	a[..][1]++;
	s[0:2][1] = 4;
	p[0:2][1] = 5;
	f.a[1:2][0] = 9;
	int* q = &a[0:2][1];
	int* r = &s[1:1][0];
	int x = ra()[0:2][1];
	int[] w = ra()[..];
	int* t = &&ra()[0:2][1];
	int* u = &a[..][..][1];
	int* v = &s[..][..][0];
	$assert !$defined(ra()[0:2][0] = 1);
	$assert !$defined(&ra()[0:2][1]);
	$assert $defined(a[0:2][0] = 1);
	$assert $defined(&a[0:2][1]);
}
