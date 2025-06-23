// #target: macos-x64
module test;
import std;

fn void main()
{
	DString y;

	y ^= 1; // #error: Expected an integer here
	y += 1.0; // #error: Expected a numeric type here
	y += 1; // #error: Expected a numeric type here
}
