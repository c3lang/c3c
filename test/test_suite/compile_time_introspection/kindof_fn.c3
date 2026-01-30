<* @require main.kindof == SIGNED_INT *>
struct Foo <Type>
{}

fn int main()
{
	Foo{double} d; // #error: Parameter(s) failed validation: @require "main.kindof == SIGNED_INT" violated
}