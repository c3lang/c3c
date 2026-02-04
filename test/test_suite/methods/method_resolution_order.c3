module test2;
import test3;
struct Bar @if($defined(Foo.b))
{
	int a;
}

struct Bar2 @if($defined((Foo){}.b))
{
	int a;
}

module test3;
struct Foo
{
	int a;
}
fn void Foo.b(&self) {} // #error: A method was added to 'Foo' which already was checked for method availability

module test;

import test2;
import test3;

fn int main()
{
	Bar b;
	return 0;
}