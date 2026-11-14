struct Foo { int[3] arr; int a; }
fn Foo rf() => {};
fn int[3] ra() => {};
fn int[<3>] rv() => {};

fn void main()
{
	int[3] a;
	int[3] b;
	bool c = true;
	(c ? a : b)[0] = 99; // #error: An assignable expression
	rf().arr[0] = 7; // #error: An assignable expression
	ra()[1]++; // #error: An assignable expression
	(c ? a : b)[..] = 0; // #error: An assignable expression
	rv().x = 1; // #error: An assignable expression
	int* p = &ra()[0]; // #error: To take the address of a temporary value
	$assert !$defined((c ? a : b)[0] = 1);
	$assert !$defined(&rf().arr[0]);
}
