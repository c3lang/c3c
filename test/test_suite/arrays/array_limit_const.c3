const FOO = (int[0x80000000][1]) {}; // #error: This type would exceed max type size of 2 GB.

fn int main()
{
	return 0;
}
