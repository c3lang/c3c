import std;
enum Foo
{
	ABC,
	DEF,
	GHI
}
fn int main()
{
	Foo f = (Foo)5; // #error: The value '5' exceeds the max ordinal '2'
	return 0;
}