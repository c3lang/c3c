fn void test1()
{
	int a;
	var $x;
	$x = a; // #error: only assign constants to a compile time
}
fn void test2()
{
	int a;
	int[2] $x = { 1, 2 };
	$x[0] = a; // #error: argument must be a constant value
}

int g;
fn void test3()
{
	int*[2] $x = { &g + 1, null };
	int* $y = &g + 1;
	$x[0] = &g + 1;
}
