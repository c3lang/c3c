fn void a()
{
	void? $y; // #error: Expected a runtime or compile time type with a well-defined zero value
}
fn int main()
{
	void $x; // #error: Expected a runtime or compile time type with a well-defined zero value

}