module vector_mask @test;
import std::io, std::math;

fn void to_from_mask()
{
	int x = (bool[<9>]){ true, false, true, false, false, false, false, false, true }.mask_to_int();
	test::eq(x, 0b100000101);
	bool[<10>] mask = vector::mask_from_int(bool[<10>], x);
	test::eq(mask, (bool[<10>]){ true, false, true, false, false, false, false, false, true, false });
}