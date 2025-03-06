import std;

fn void main()
{
	char[] cd = {};
    char[*] b = x""; // #error: must be at least 1 byte
    io::printfn("%d", b.len);
}