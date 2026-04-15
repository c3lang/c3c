fn int main()
{
	var $foo = $reflect(main).type;
	$typefrom($foo) a;
	a = &main;
	return 0;
}