module test;
import std;
macro test(x)
{
	var $Type = $Typeof(x);
}
fn int main()
{
	test($Typeof($reflect(main))); // #error: You cannot take the typeid of a compile time type
	return 0;
}