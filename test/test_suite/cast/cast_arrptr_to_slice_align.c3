module test;

// The element pointers' pointee alignment goes up (4 -> 16): an implicit conversion must be rejected.
// The matched text is deliberately generic: both the alignment error and the generic
// "you may do an explicit cast" error start with it. What an explicit cast does is not covered here.
alias Vec4 = int[<4>] @simd;

fn void arrptr_to_slice_alignment_increase()
{
	int[4] a;
	int[4]*[2] arr = { &a, &a };
	Vec4*[] s = &arr; // #error: Implicitly casting
}

// Going the other way the alignment goes down (16 -> 4), which is safe.
fn void arrptr_to_slice_alignment_decrease()
{
	Vec4 v;
	Vec4*[2] arr = { &v, &v };
	int[4]*[] s = &arr;
}
