module ldexp_types_test @test;
import std::math;

fn void ldexp_scalar_types()
{
	assert(math::ldexp(3, 2) == 12.0);
	assert($Typeof(math::ldexp((float16)1, 0)) == float16);
	assert($Typeof(math::ldexp(1.0f, 0)) == float);
	assert($Typeof(math::ldexp(1.0, 0)) == double);
	assert($Typeof(math::ldexp((float128)1, 0)) == float128);
	assert(math::ldexp((float16)1.5, 4) == (float16)24);
	assert(bitcast(math::ldexp((float16)1.5, -24), ushort) == 2);
	assert(bitcast(math::ldexp(bitcast((ushort)1, float16), 24), ushort) == 0x3c00);
	assert(bitcast(math::ldexp((float128)1.5, -16494), uint128) == 2);
	assert(bitcast(math::ldexp(bitcast((uint128)1, float128), 16494), uint128) == ((uint128)16383 << 112));
	assert(bitcast(math::ldexp((float128)1.5, 4), uint128) == (((uint128)16387 << 112) | ((uint128)1 << 111)));
}
