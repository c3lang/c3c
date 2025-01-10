module ct_slice @test;

fn void slice_bytes()
{
	char[4] $a = x'aabbccdd';
	var $b = $a[1..2];
	char[2] y = $b;
	assert($b == char[] { 187, 204 });
	assert(y == char[2] { 187, 204 });
}

fn void slice_string()
{
	String $a = "abcd";
	assert($a == "abcd");
	var $c = $a[1:0];
	String c = $c;
	assert($c == "");
	assert(c == "");
	var $d = $a[1:2];
	String d = $d;
	assert($d == "bc");
	assert(d == "bc");
	assert($a[..] == "abcd");
}

fn void slice_untyped()
{
	var $v = { 1, "hello", 3 };
	int[1] v = $v[0:1];
	assert(v == int[1] { 1 });
	int[1] v2 = $v[2:1];
	assert(v2 == int[1] { 3 });
	String[1] v3 = $v[1:1];
	assert(v3 == String[1] { "hello" });
	int[] v4 = $v[0:1];
	assert(v4 == int[] { 1 });
}