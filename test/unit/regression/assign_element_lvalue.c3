module assign_element_lvalue;

struct Foo { int[3] arr; int[<3>] v; }
const int[3] CA = { 1, 2, 3 };

fn Foo* pass(Foo* f) => f;
macro @set(#x) { #x[0] = 5; return &#x[1]; }

fn void assign_element_lvalue() @test
{
	Foo f;
	Foo* p = &f;
	int[3] a;
	pass(p).arr[0] = 1;
	p.arr[1]++;
	(*p).arr[2] = 3;
	pass(p).v.x = 4;
	int* q = &pass(p).arr[0];
	int* r = @set(a);
	*r = 9;
	assert(f.arr == { 1, 1, 3 });
	assert(f.v == { 4, 0, 0 });
	assert(q == &f.arr[0]);
	assert(a == { 5, 9, 0 });
	assert(*&CA[1] == 2);
}
