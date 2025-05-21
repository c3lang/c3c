import std;

fn int main()
{
	// This works
	int** my_super_cool_pointer = &&(&&1);
	// This doesn't
	int** my_super_cool_pointer2 = &&&&1; // #error: '&&&' is the compile time '&&' operator
	return 0;
}
