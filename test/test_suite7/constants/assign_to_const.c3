module test;
import std::io;

fn void main()
{
	const NUM = 4;
	NUM += 1; // #error: You cannot assign to a constant
}