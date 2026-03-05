module test;
import std;

struct Foo
{
	<* hello *>
	int a;
	<* test *>
	union
	{
		<* test2 *>
		int b;
	}
	struct foo
	{
		<* baz *>
		int g;
	}
}

faultdef <* hello *> FOO;
enum Abc
{
	<* test *>
	XYZ
}

faultdef <* @require a > 0 *> FOO2; // #error: No constraints are allowed on faults
enum Abc2
{
	<* @require a > 0 *> // #error: No constraints are allowed on enum values
	XYZ
}
struct Test
{
	<* @require a > 0 *> // #error: No constraints are allowed on struct/union members
	int a;
}

fn void main()
{}