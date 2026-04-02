module test;

macro check_type($Type)
{
	var $b = ($Type[4][4]) {};
	$Type z = $b[2][2];
}
enum Blurb { } // #error:  An enum must have at least one value.
fn int main()
{
	check_type(Blurb);
	return 0;
}
