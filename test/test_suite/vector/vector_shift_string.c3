
module test;
typedef Foo = inline int;
fn int main()
{
	Foo? u = 2;
	int? f4 = (int[<2>]) { 1, 2 } << $reflect(u).name; // #error: << is not defined in the expression 'int[<2>]' << 'String'
}