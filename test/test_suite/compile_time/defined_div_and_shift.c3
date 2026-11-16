
const int FOO = 0;
const int BAR = 2;
fn void main()
{
	int x = 5; uint u = 5; int[<2>] iv = { 1, 2 }; long l;
	var $d = $defined(1 / FOO);
	$assert !$d;
	$assert $defined(1 / BAR);
	$assert !$defined(x / 0);
	$assert !$defined(x % 0);
	$assert !$defined(x << 100);
	$assert !$defined(1 << 64);
	$assert !$defined(x >> -1);
	$assert !$defined(x /= 0);
	$assert !$defined(x %= FOO);
	$assert !$defined(x <<= 40);
	$assert !$defined(x >>= -2);
	$assert $defined(x << 3);
	$assert $defined(x /= 2);
	$assert $defined(l << 40);
	$assert $defined(1.0 / 0);
}
