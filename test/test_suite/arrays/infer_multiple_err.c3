
fn void test1()
{
	var $d = { {1}, {2, 3} };
	var $e = (int[*][*])$d; // #error: contains elements that have different lengths
}

fn void test2()
{
	var $d = { {1}, {3}, {2} };
	var $e = (int[*][*])$d;
}

fn void test3()
{
	var $d = { {1}, {} };
	var $e = (int[*][*])$d; // #error: This untyped list contained an element of type
}
