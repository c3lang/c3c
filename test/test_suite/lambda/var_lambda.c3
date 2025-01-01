module test;
import std;
fn void main()
{
	var x = fn int(int y) { return y * 2; };
	io::printn(x(4));
	io::printn(x(3));
}