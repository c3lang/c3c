module test;
import std;

enum Abc
{
	ABCD,
	OKFEOFKE,
	OFKEOFK
}

typedef Bob = inline Abc;
fn void main()
{
	Abc y;
	y += 1; // #error:  Expected a numeric type here
	Bob gh;
	gh += 1; // #error:  Expected a numeric type here
}

