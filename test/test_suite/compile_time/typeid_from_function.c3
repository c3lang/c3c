fn int main()
{
	var $foo = $reflect(main).type;
	$Typefrom($foo) a;
	a = &main;
	return 0;
}