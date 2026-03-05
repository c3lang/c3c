faultdef
	ABC,
	DEF,
	ZED;


faultdef XYZ;
<*

 hello world
 @return? XYZ, ABC
 *>
fn void? abc(int a, int b, int z)
{
	return ZED~; // #error: This value does not match declared optional returns
}

<*
hello world
 @return? XYZ, ABC
 *>
macro void? @abc(int a, int b, int z)
{
	return ZED~; // #error: This value does not match declared optional returns
}

fn void main()
{
	@abc(2,3,3)!!;
}