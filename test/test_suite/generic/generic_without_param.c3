module test;
import std;
import abc;
fn void main()
{
	abc::test(); // #error: {...}
}
module abc{Type};
fn void test() {}