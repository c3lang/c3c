import std;
fn int main()
{
	char[] a = "hello";
	char[*] b = *(char[5]*)a.ptr;
	return 0;
}