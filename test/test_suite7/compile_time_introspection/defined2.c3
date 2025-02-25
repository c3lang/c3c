fn void main()
{
	int[<2>] ab = { 11, 22 };
	$assert !$defined(&1);
	int! a;
	$assert $defined(a!!);
	$assert !$defined(ab!!);
	$assert $defined(a!);
	$assert !$defined(ab!);
	$assert !$defined(*ab);
	int* z = &&1;
	$assert $defined(*z);
	void* g;
	$assert !$defined(*g);
}