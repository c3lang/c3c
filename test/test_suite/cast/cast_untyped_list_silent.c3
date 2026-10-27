module test;

// A silent probe of an untyped list that cannot convert must answer false and print nothing.
fn void silent_bad_list()
{
	var $bad = { 1, "a" };
	$assert(!$defined(int[*] y = $bad));
}

// These already work, and must keep working.
fn void silent_good_list()
{
	var $ok = { 1, 2 };
	$assert($defined(int[*] y = $ok));
}

fn void loud_bad_list()
{
	var $bad = { 1, "a" };
	int[*] y = $bad; // #error: This untyped list contained an element of type
}
