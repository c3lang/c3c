module get_environ;
import std;

enum Foo : (String x, int val)
{
	ABC = { "Hello", 3 },
	DEF = { "World", -100 },
}

fn int main()
{
	Foo.membersof[0].set(Foo.ABC, "Hello"); // #error: Enum associated values cannot be set
	Foo.membersof[0].offsetof; // #error: No member or property 'offsetof' was found
	return 0;
}