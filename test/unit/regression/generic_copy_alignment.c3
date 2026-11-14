module generic_copy_alignment <ALIGN>;

typedef AlignedInt = int @align(ALIGN);

module generic_copy_alignment_test;
import generic_copy_alignment;

fn usz alignment4() => AlignedInt{4}::alignment;
fn usz alignment8() => AlignedInt{8}::alignment;

fn void test_distinct_alignment_is_copied() @test
{
	assert(alignment4() == 4);
	assert(alignment8() == 8);
}
