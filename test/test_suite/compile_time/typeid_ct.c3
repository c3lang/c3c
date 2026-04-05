fn void main()
{
	typeid t = $typeof({1, "hts"}).typeid; // #error: cannot take the typeid
}

fn void main2()
{
	$typeof({1, "hts"}).nameof; // #error: does not have a property or method
}