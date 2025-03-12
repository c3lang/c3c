// Repro issue #2039
module foo;
struct Foo { void* x; }
fn void main()
{
	Foo* foo;
	float[<2>] a = foo[0..1]; // #error: It is not possible to cast
}