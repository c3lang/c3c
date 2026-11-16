interface Ifc
{
	fn void foo(int& a); // #error: Interface methods have an implicit self
}
