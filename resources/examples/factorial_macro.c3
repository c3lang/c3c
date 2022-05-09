macro int factorial($n)
{
	$if ($n == 0):
		return 1;
	$else:
		return $n * factorial($n - 1);
	$endif;
}

extern fn void printf(char *fmt, ...);

fn void main()
{
	int x = factorial(12);
	printf("12! = %d\n", x);
}