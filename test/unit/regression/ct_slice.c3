module ct_slice @test;

fn void! slice_string()
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