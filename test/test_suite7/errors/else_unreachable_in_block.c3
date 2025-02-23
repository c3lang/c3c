// #deprecation: no
module test;
import std;
fn void main()
{
	char[] bytes = file::load_temp("config.json") ?? {| abort("Unable to open config.json file"); // #error: Cannot find a common type for 'char[]' and 'void'
		return {}; |}; // #warning: This code will never execute.
	char[] bytes2;
}
