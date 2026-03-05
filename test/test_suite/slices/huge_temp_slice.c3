module main;

char[] one_mb2 = { [0..(1024*1024)-1] = 0xA5 };
fn int main()
{
	char[] one_mb;
	one_mb = { [0..(1024*1024)-1] = 0xA5 }; // #error: The size of this stack allocated expression
	one_mb2 = { [0..(1024*1024)-1] = 0xA5 }; // #error: The size of this stack allocated expression
	return 0;
}
