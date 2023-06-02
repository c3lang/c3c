def Foo = distinct double;
def Bar = distinct void*;
fn int main()
{
	float f = 1;
	Foo foo = f; // #error: you may use a cast
	Bar bar = f; // #error: It is not possible to convert
	return 1;
}