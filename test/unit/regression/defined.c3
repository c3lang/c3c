module defined_tests @test;

fn void binary()
{

	assert(!$defined(1 == "abc"));
	assert(!$defined(1 + "ofke"));
	assert(!$defined(1 << "ofke"));
	assert(!$defined(1 / "ofke"));
    assert(!$defined(1 * "ofke"));
	assert(!$defined(1 ^ "ofke"));
	assert(!$defined(1 | "ofke"));
	assert(!$defined(1 ?? 1));
	assert(!$defined(~"ofke"));
	assert(!$defined(*1));
	assert(!$defined(&1));
	assert(!$defined(1 = "ofek"));
	String[] a;
	assert(!$defined(a[0] = 1));
	int x;
	assert(!$defined(x[0]));
}