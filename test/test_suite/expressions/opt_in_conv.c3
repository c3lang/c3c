module test;
import std;
fn void main()
{
	String s = "Hello";
	Char16* title = String.to_utf16(s, mem).ptr; // #error: It is not possible to cast from
	int! a;
	float b = (float)a; // #error: It is not possible to cast from
}