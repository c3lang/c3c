
fn void test1()
{
	var $d = { {1}, {2, 3} };
	var $e = (int[2][*])$d; // #error:  This untyped list contained an element
}

fn void test2()
{
	var $d = { {1}, {3}, {2} };
	var $e = (int[1][*])$d;
}

fn void test3()
{
	var $d = { {1}, {} };
	var $e = (int[1][*])$d;
}
