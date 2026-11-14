module test;

// A silent probe must answer false, and print nothing, for a conversion that is an error
// when written out. Each probe is in its own function so one failure doesn't hide the other.
alias Vec4 = int[<4>] @simd;

fn void slice_to_ptr_alignment_increase()
{
	int[4]*[] s;
	$assert(!$defined(Vec4** p = s));
}

fn void arrptr_to_slice_alignment_increase()
{
	int[4] a;
	int[4]*[2] arr = { &a, &a };
	$assert(!$defined(Vec4*[] t = &arr));
}

// And the safe direction must still be answered with true.
fn void arrptr_to_slice_alignment_decrease()
{
	Vec4 v;
	Vec4*[2] arr = { &v, &v };
	$assert($defined(int[4]*[] t = &arr));
}
