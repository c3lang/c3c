import std;
fn int main()
{
	Colour col = {255, 255, 0}; // #error: Too few elements in initializer
	return 0;
}
struct Colour
{
	char a;
	char r;
	char g;
	char b;
}