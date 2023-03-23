fault Abc
{
	ABC,
	DEF,
	ZED
}


fault Foo
{
	XYZ
}
/**
 *
 * @return! Abc.DEF "if x is blurb"
 * @return! Foo, Abc.ABC, Abc
 * @return! Foo, Abc.ABC
 * hello world
 **/
fn void! abc(int a, int b, int z)
{
	return Abc.ZED?; // #error: This value does not match declared optional returns
}

/**
 * @return! Abc.DEF "if x is blurb"
 * @return! Foo, Abc.ABC, Abc
 * @return! Foo, Abc.ABC
 * hello world
 **/
macro void! @abc(int a, int b, int z)
{
	return Abc.ZED?; // #error: This value does not match declared optional returns
}

fn void main()
{
	@abc(2,3,3)!!;
}