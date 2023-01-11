module test;

macro test($baz, $Type)
{
	$baz = 123;
	$Type = int;
}
fn void main()
{
	var $foo = 1;
	$foo = 4;
	test(4, double);
}
