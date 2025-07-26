import std;
fn int main()
{
	char[] a = "hello";
	char[*] b = *(char[*]*)&a; // #error: cannot be used to infer the length
	return 0;
}