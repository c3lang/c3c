<* @require THE_CONST < 5 *>
module the_generic <THE_CONST, Type>;
struct Abc
{ int a; }

module the_generic <THE_CONST, Type>;

struct TheStruct
{
	Type[THE_CONST] el;
}

module test;
import the_generic;

fn int main()
{
	TheStruct{5, double} s; // #error: Parameter(s) failed validation: @require "THE_CONST < 5" violated
	return x;
}