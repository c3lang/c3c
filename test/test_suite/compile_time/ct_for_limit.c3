fn int main()
{
	$for var $i = 0; $i < 1000000000; $i++: // #error: Too many iterations in
	$endfor
	return 0;
}