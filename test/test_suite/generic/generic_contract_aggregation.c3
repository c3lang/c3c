import std::io;

<* @require Type::kind == SIGNED_INT *>
struct Foo <Type>
{
	Type a;
}

fn int main()
{
	Foo{double} d; // #error:  Parameter(s) failed validation: @require "Type::kind == SIGNED_INT"
	return 0;
}
