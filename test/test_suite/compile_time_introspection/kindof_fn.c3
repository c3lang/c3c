<* @require Type::kind == SIGNED_INT *>
struct Foo <Type>
{}

fn int main()
{
	Foo{double} d; // #error: Parameter(s) failed validation: @require "Type::kind == SIGNED_INT" violated
}