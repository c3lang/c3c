fn void main()
{
	typeid t = $typeof({1, "hts"}).typeid; // #error: cannot take the typeid
	$typeof({1, "hts"}).nameof; // #error: does not have a property or method
}