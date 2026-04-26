interface TestProto
{
}
struct Test
{
	void* abc;
}
fn void main()
{
	TestProto z = mem::alloc(Test?); // #error:  "$Type::kind != OPTIONAL" violated: 'Expected a non-optional type'.
}