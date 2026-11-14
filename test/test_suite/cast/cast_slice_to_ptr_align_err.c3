module test;

// The alignment of the pointee goes up (4 -> 16), so an implicit conversion must be rejected.
// The matched text is deliberately generic: both the alignment error and the generic
// "you may do an explicit cast" error start with it. What an explicit cast does is not covered here.
alias Vec4 = int[<4>] @simd;

fn void slice_to_ptr_alignment_increase()
{
	int[4]*[] s;
	Vec4** p = s; // #error: Implicitly casting
}
