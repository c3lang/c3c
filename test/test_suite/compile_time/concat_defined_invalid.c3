enum Col { RED }
faultdef BAD;

fn void test()
{
	$assert !$defined("a" +++ 1.0);
	$assert !$defined("a" +++ true);
	$assert !$defined("a" +++ -1);
	$assert !$defined("a" +++ Col.RED);
	$assert !$defined("a" +++ BAD);
	$assert !$defined(x"01" +++ 300);
	$assert !$defined((int[]){1} +++ 1.5);
	$assert !$defined((int[]){1} +++ "a");
	$assert $defined("a" +++ "b");
	$assert $defined("a" +++ 'x');
	$assert $defined(x"01" +++ 2);
	$assert $defined((int[]){1} +++ 2);
}

fn void test_non_const(String s)
{
	$assert !$defined(s +++ "a");
	$assert !$defined("a" +++ s);
	var $str = "a";
	$assert !$defined($str +++= 1.0);
	$assert !$defined($str +++= s);
	$assert !$defined($str +++= { 1.0 });
	$assert !$defined($str +++= { .a = 1 });
	var $arr = (int[]){ 1 };
	$assert !$defined($arr +++= { 1.0 });
	$assert !$defined($arr +++= { .a = 1 });
	$assert $arr.len == 1;
	$assert $str == "a";
}

fn void test3(String s)
{
	String t = "a" +++ s; // #error: Expected this to evaluate to a constant value
}

fn void test4()
{
	var $str = "a";
	$str +++= 1.0; // #error: Concatenating 'String' with double is not possible
}

fn void test2()
{
	String s = "a" +++ 1.0; // #error: Concatenating 'String' with double is not possible
}
