module ct_switch_typedef_range;

typedef CtMyInt = int;
typedef CtMyChar = char;

macro String classify($x)
{
	$switch $x:
		$case (CtMyInt)1..(CtMyInt)5: return "low";
		$case (CtMyInt)6..(CtMyInt)10: return "high";
		$default: return "other";
	$endswitch
}

fn void ct_case_range_on_typedef_int() @test
{
	assert(classify((CtMyInt)3) == "low");
	assert(classify((CtMyInt)7) == "high");
	assert(classify((CtMyInt)11) == "other");
}

fn void ct_case_range_on_typedef_char() @test
{
	const CtMyChar C = (CtMyChar)'m';
	$switch C:
		$case (CtMyChar)'a'..(CtMyChar)'z': assert(true);
		$default: assert(false);
	$endswitch
}
