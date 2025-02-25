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
<*

 hello world
 @return! Foo, Abc.ABC
 *>
fn void! abc(int a, int b, int z)
{
	return Abc.ZED?; // #error: This value does not match declared optional returns
}

<*
hello world
 @return! Foo, Abc.ABC
 *>
macro void! @abc(int a, int b, int z)
{
	return Abc.ZED?; // #error: This value does not match declared optional returns
}

fn void main()
{
	@abc(2,3,3)!!;
}