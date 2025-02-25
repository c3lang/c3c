// #target: macos-x64
module test;
import std;

fn void main()
{
	DString y;

	y ^= 1; // #error: Expected an integer here
	y += 1.0; // #error: A value of type 'DString'
	y += 1; // #error: A value of type 'DString'
}
