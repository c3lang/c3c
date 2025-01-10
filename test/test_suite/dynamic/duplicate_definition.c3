module test;

interface Abc
{
	fn void abc();
	fn void abc(); // #error: Duplicate definition
}
fn void main()
{
	Abc* g;
	g.abc();
}
