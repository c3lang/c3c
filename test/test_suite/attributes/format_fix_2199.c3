import std;
fn void main()
{
	io::printfn("%*d", 4, 4);
	io::printfn("%.*0s", 5, "123"); // #error: Unexpected escape sequence '%0' in format declaration
}
