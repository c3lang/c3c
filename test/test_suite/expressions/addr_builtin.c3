module std::test;

fn void testb()
{
	$sizeof(&$$veccompeq); // #error: You cannot take the address of a builtin function.
}