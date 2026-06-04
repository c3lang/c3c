module test;
import std;
fn void main()
{
	HashMap{String, String} my_map;
	my_map.@each(;module, migration) // #error:  'module' is a keyword, and can't be used
	{
	};
}