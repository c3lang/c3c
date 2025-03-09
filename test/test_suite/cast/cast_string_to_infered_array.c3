import std;
fn void main()
{
	char[*]* x = "abc"; // #error: You cannot cast 'String' to 'char[*]*'
	io::printn($typeof(x).nameof);
}