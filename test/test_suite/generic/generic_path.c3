module main;
import std::io;

fn void main()
{
	int x = to_integer{int}("394")!!; // #error: Functions from other modules must be prefixed with the module name, please use string::to_integer{int}{...} instead.
}
