typedef Foo = double;
typedef Bar = void*;
fn int main()
{
	float f = 1;
	Foo foo = f; // #error: explicit cast
	Bar bar = f; // #error: 'float' to 'Bar'
	return 1;
}