// #target: macos-x64
module test;
import std;

enum Abc
{
	ABCD,
	OKFEOFKE,
	OFKEOFK
}

typedef Bob = Abc;
fn void main()
{
	Abc y;
	y ^= 1; // #error: Expected an integer here, not a value of type
	y += 1.0; // #error: The right side was 'double'
	Bob gh;
	gh += 1; // #error: value of type 'Bob'
}
