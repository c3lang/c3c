module get_environ;
import std;

enum Foo : (String x, int val)
{
	ABC { "Hello", 3 },
	DEF { "World", -100 },
}

fn int main()
{
	var $member1 = Foo::members[0];
	Foo.ABC.$member1 = "Hello"; // #error: You cannot assign to a constant expression.
	$member1.offset; // #error: There is no property 'offset' available for the reflected expression
	return 0;
}