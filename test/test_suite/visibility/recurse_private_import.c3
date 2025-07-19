module abc;

module abc::baz;

struct Abc @private
{
	int x;
}

fn void test() @private
{

}

module test;
import abc @public;

fn int main()
{
	Abc x;
	abc::baz::test();
	return 0;
}

module test2;
import abc @norecurse @public;

fn int test()
{
	{ Abc x; } // #error: is '@private'
	abc::baz::test(); // #error: is '@private'
	return 0;
}