import std;
fn int[] hello()
{
	return { 1, 2 }; // #error: will be invalid
}
fn void main()
{
	hello();
}