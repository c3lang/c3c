module copysign @test;
import std::math;
fn void copysign_float()
{
	float a = 3;
	float b = -4;
	float c = math::copysign(a, b);
	assert(c == -3);
	float d = math::copysign(a, 3);
	assert(d == 3);
	assert(math::copysign(a, -3) == -3);
	float e = math::copysign(3, a);
	assert(e == 3);
	assert(math::copysign(3, b) == -3);
}