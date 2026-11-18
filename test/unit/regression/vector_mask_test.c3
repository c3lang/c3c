module vector_mask @test;
import std::io, std::math, vector_mask_lanes;

fn void to_from_mask()
{
	int x = (bool[<9>]){ true, false, true, false, false, false, false, false, true }.mask_to_int();
	test::eq(x, 0b100000101);
	bool[<10>] mask = vector::mask_from_int(bool[<10>], x);
	test::eq(mask, (bool[<10>]){ true, false, true, false, false, false, false, false, true, false });
}

fn void mask_to_int_fewer_lanes_than_bits()
{
	bool[<4>] mask = { true, false, true, true };
	test::eq(vector_mask_lanes::pick(mask, false), 0b1101);
	test::eq(vector_mask_lanes::add(mask, 16), 29);
}

module vector_mask_lanes;
import std::math;

fn char pick(bool[<4>] mask, bool other) => other ? 7 : mask.mask_to_int();

fn char add(bool[<4>] mask, char sum)
{
	sum += mask.mask_to_int();
	return sum;
}
