module lvalue_handling;
import std;
struct Foo
{
	int a;
}
def IntList = List(<Foo>);
fn void subscript_overload() @test
{
	IntList x;
	x.push({ 3 });
	int* a = &x[0].a;
	assert(*a == 3);
	assert(x[0].a == 3);
	*a = 4;
	assert(x[0].a == 4);
	x[0].a = 5;
	assert(x[0].a == 5);
}