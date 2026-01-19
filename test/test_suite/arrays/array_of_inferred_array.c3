import std;
macro void test(int[*][6] a)
{
}
fn void main()
{
	test({ { 1, 2 } }); // #error: Too few elements in initializer
}