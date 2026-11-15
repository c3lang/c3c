alias FooFn = fn FooFn(); // #error: Recursive definition of 'FooFn'
enum Foo : (FooFn f)
{
	FOO {fn () => 0},
	BAR {0},
}
constset Bar : FooFn
{
	FOO = fn int() => 0,
	BAR = &foo,
}