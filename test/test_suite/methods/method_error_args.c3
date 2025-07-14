module test;
import std;

fn void String.hello(self, int a, int b) {}
fn void main()
{
	String s = "foek";
	s.hello();  // #error: 'hello' expects 2 parameter(s), but none was provided
	s.hello(1); // #error: 1 more argument was expected after this one, did you forget it?
}
