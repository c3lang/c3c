module test;
import std::io;
fn void main()
{
	int[<2>] x = { 4, 7 };
	int[2] y = x;
	int[?] y1 = y;
	int[?] y2 = x;
	int[<?>] z = x;
	int[<?>] w = y;
	double[<2>] ww = x;
	short[<2>] www = y; // #error: Implicitly casting 'int[2]' to 'short[<2>]'
}