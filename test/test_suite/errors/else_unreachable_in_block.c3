// #deprecation: no
module test;
import std;

macro unreachable_abort()
{
	abort("Unable to open config.json file");
}
fn void main()
{
	char[] bytes = file::load_temp("config.json") ??
		unreachable_abort();// #error: Cannot find a common type for 'char[]' and 'void'
	char[] bytes2;
}
