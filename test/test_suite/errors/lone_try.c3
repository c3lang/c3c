module test;

fn void main()
{
	try a; // #error: try?
}

fn void hello()
{
	catch a; // #error: catch?
}
