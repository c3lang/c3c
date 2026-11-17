module int_float_rounding;

fn float runtime_float(uint128 value) @noinline => (float)value;
fn double runtime_double(uint128 value) @noinline => (double)value;

fn void integer_to_float_rounding() @test
{
	// Just above a float midpoint, but rounded to the midpoint by a double intermediate.
	assert(bitcast((float)0x8000_0080_0000_0001UL, uint) == 0x5f000001);
	assert(bitcast((float)-0x4000_0040_0000_0001L, uint) == 0xde800001);
	// Exact halfway cases round to an even significand in either direction.
	assert(bitcast((float)0x8000_0080_0000_0000UL, uint) == 0x5f000000);
	assert(bitcast((float)0x8000_017f_ffff_ffffUL, uint) == 0x5f000001);
	assert(bitcast((float)0x8000_0180_0000_0000UL, uint) == 0x5f000002);
	assert((float)0 == 0.0f);
	assert((float)int128::min == runtime_float((uint128)1 << 127) * -1.0f);
	assert((float)uint128::max == runtime_float(uint128::max));
	assert((float)0x8000_0080_0000_0001UL == runtime_float(0x8000_0080_0000_0001UL));
}

fn void integer_to_double_rounding() @test
{
	// The high word alone is halfway; the low word makes the exact integer greater.
	const uint128 VALUE = ((uint128)1 << 127) + ((uint128)1 << 74) + 1;
	assert(bitcast((double)VALUE, ulong) == 0x47e0000000000001);
	assert((double)VALUE == runtime_double(VALUE));
	assert(bitcast((double)uint128::max, ulong) == 0x47f0000000000000);
	assert((double)int128::min == -runtime_double((uint128)1 << 127));
	assert((double)0 == 0.0);
}

fn void integer_to_small_float_rounding() @test
{
	assert((float)(float16)2049 == 2048.0f);
	assert((float)(float16)2051 == 2052.0f);
	assert((float)(float16)-2051 == -2052.0f);
	assert(bitcast((bfloat)0x8080_0000_0000_0001UL, ushort) == 0x5f01);
	assert(bitcast((bfloat)0x8080_0000_0000_0000UL, ushort) == 0x5f00);
}
