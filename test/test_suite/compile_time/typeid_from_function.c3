fn int main()
{
	var $foo = main.typeid;
	$typefrom($foo) a;
	a = &main;
	return 0;
}