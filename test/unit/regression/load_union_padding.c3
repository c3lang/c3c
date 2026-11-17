module load_union_padding;

import std::core::mem;

union PaddedUnion
{
	struct padded { ulong lo; ushort hi; }
	ulong[2] wide;
}

union GapUnion
{
	struct gap { char first; ulong last; }
	ulong[2] wide;
}

struct Wrapper
{
	char tag;
	PaddedUnion value;
}

struct NestedPad { ulong lo; ushort hi; }
struct NestedRep { NestedPad[2] values; }

union NestedArrayUnion
{
	NestedRep rep;
	ulong[4] wide;
}

fn PaddedUnion padded_value()
{
	PaddedUnion value;
	value.wide = { 0x1111_2222_3333_4444, 0xAAAA_BBBB_CCCC_DDDD };
	return value;
}

fn void volatile_load_preserves_union_padding() @test
{
	PaddedUnion source = padded_value();
	PaddedUnion copy = @volatile_load(source);
	source.wide[1] = 0;
	assert(copy.wide == { 0x1111_2222_3333_4444, 0xAAAA_BBBB_CCCC_DDDD });

	GapUnion gap;
	gap.wide = { 0x1111_2222_3333_4444, 0xAAAA_BBBB_CCCC_DDDD };
	GapUnion gap_copy = @volatile_load(gap);
	assert(gap_copy.wide == gap.wide);
}

fn void unaligned_load_preserves_union_padding() @test
{
	PaddedUnion source = padded_value();
	PaddedUnion copy = mem::load(&source, 1);
	source.wide[1] = 0;
	assert(copy.wide == { 0x1111_2222_3333_4444, 0xAAAA_BBBB_CCCC_DDDD });

	source = padded_value();
	PaddedUnion volatile_copy = mem::load(&source, 1, true);
	assert(volatile_copy.wide == source.wide);

	char[PaddedUnion::size + 1] storage;
	PaddedUnion* unaligned = (PaddedUnion*)&storage[1];
	mem::store(unaligned, source, 1);
	PaddedUnion unaligned_copy = mem::load(unaligned, 1);
	assert(unaligned_copy.wide == source.wide);

	PaddedUnion unaligned_volatile_copy = mem::load(unaligned, 1, true);
	assert(unaligned_volatile_copy.wide == source.wide);
}

fn void aggregate_load_preserves_nested_union_padding() @test
{
	Wrapper source = { .tag = 7, .value = padded_value() };
	Wrapper volatile_copy = @volatile_load(source);
	assert(volatile_copy.tag == 7 && volatile_copy.value.wide == source.value.wide);

	Wrapper unaligned_copy = mem::load(&source, 1);
	assert(unaligned_copy.tag == 7 && unaligned_copy.value.wide == source.value.wide);

	PaddedUnion[2] array = { padded_value(), padded_value() };
	array[1].wide[1] = 0x5555_6666_7777_8888;
	PaddedUnion[2] array_copy = @volatile_load(array);
	assert(array_copy[0].wide[1] == 0xAAAA_BBBB_CCCC_DDDD);
	assert(array_copy[1].wide[1] == 0x5555_6666_7777_8888);
}

fn void load_preserves_padding_nested_through_array() @test
{
	NestedArrayUnion source = { .wide = { 1, 2, 3, 4 } };
	NestedArrayUnion volatile_copy = @volatile_load(source);
	NestedArrayUnion unaligned_copy = mem::load(&source, 1);
	assert(volatile_copy.wide == { 1, 2, 3, 4 });
	assert(unaligned_copy.wide == { 1, 2, 3, 4 });
}
