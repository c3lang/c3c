interface TestProto
{
}
struct Test
{
	void* abc;
}
fn void main()
{
	TestProto z = mem::alloc(Test?); // #error:  "$Type.kindof != OPTIONAL" violated: 'Expected a non-optional type'.
}