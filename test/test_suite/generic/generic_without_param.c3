module test;
import std;
import abc;
fn void main()
{
	abc::test(); // #error: 'test' is a generic function, did you forget the parameters '{ ... }'?
}
module abc <Type>;
fn void test() {}