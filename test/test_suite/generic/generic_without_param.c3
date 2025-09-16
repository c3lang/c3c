module test;
import std;
import abc;
fn void main()
{
	abc::test(); // #error: but no parameters where given
}
module abc{Type};
fn void test() {}