struct Foo { int[<3>] v; }
fn Foo rf() => {};
fn int[<3>] rv() => { 1, 2, 3 };

fn void ternary(bool c)
{
	int[<3>] a;
	int[<3>] b;
	int* p = &(c ? a : b).y; // #error: To take the address of a temporary value
}

fn void call_result()
{
	int* p = &rv().y; // #error: To take the address of a temporary value
}

fn void call_member()
{
	int* p = &rf().v.z; // #error: To take the address of a temporary value
}

fn void still_valid()
{
	int[<3>] a;
	Foo f;
	int* p = &a.y;
	int* q = &f.v.z;
	int* r = &&rv().y;
	$assert !$defined(&rf().v.z);
	$assert $defined(&a.y);
}
