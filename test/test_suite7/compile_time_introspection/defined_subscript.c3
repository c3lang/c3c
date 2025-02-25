fn void main()
{
	int[<2>] ab = { 11, 22 };
	$assert !$defined(ab[3]);
	$assert $defined(ab[1]);
	$assert !$defined(ab[0][0]);
}