module test2;
import test3;
struct Bar @if($defined(Foo.b)) // #error: There might be a method 'b' for 'Foo', but methods for the type have not yet been completely registered, so this yields an error
{
	int a;
}

struct Bar2 @if($defined((Foo){}.b)) // #error: There might be a method 'b' for 'Foo', but methods have not yet been completely registered, so analysis fails.
{
	int a;
}

module test3;
struct Foo
{
	int a;
}
module test;

import test2;
import test3;

fn int main()
{
	Bar b;
	return 0;
}