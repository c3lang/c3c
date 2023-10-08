module test;

protocol Abc : Def
{}

protocol Def
{}

fn void! test()
{
	any* x;
	Abc* d = x; // #error: cannot implicitly be converted to 'Abc*'
}


fn void! test2()
{
	Abc* x;
	any* d = x;
	Def* e = x;
	x = e; // #error: is not a parent protocol of 'Def'
}
