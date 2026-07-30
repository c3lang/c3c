module main;

fn int main()
{
	$echo m();
	return 0;
}

macro int m()
{
	var $x = do_nothing(); // #error: This expression is not a real value, so you cannot assign it to a compile time variable
	return 3;
}

macro void do_nothing()
{
	$echo "nothing";
}
