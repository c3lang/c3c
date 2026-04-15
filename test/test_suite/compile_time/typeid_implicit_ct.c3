module test;
import std;
macro test(x)
{
	var $Type = $typeof(x);
}
fn int main()
{
	test($typeof($reflect(main))); // #error: You cannot take the typeid of a compile time type
	return 0;
}