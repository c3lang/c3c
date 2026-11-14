module test;

// The alignment of the pointee goes down (16 -> 4), which is always safe, so this must compile.
alias Vec4 = int[<4>] @simd;

fn void slice_to_ptr_alignment_decrease()
{
	Vec4*[] s;
	int[4]** p = s;
}
