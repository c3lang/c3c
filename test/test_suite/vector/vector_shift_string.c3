
module test;
typedef Foo = inline int;
fn int main()
{
	Foo? u = 2;
	int? f4 = (int[<2>]) { 1, 2 } << $reflect(u).name; // #error: Expected a numeric type here.
}