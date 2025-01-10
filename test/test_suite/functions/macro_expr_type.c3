macro void @foo(int #a)
{
}
fn void main()
{
	@foo(1.0); // #error: cannot implicitly be converted
}