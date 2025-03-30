alias Abc = fn void();
typedef Foo = inline Abc;
struct Bar
{
	inline Abc a;
}
fn void test()
{
}
fn void main()
{
	Foo f = &test;
	Bar b = { &test };
	f();
	b(); // #error: may be invoked
}