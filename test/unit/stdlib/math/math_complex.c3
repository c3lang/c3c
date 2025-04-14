module math_tests @test;
import std::math;
import std::math::complex;


fn void complex_mul_imaginary()
{
	Complex c = math::I;
	assert(c * c == -1);
	assert(c * c * c == -math::I);
}

fn void complex_add()
{
	Complex a = { 3, 4 };
	Complex b = { 1, 2 };
	assert(a + b == (Complex){ 4, 6 });
	assert(a.add_each(1).equals({4, 5}));
	//a += b;
	//assert(a == (Complex){ 4, 6 });
}

fn void complex_sub()
{
	Complex a = { 3, 4 };
	Complex b = { 1, 2 };
	assert(a - b == (Complex){ 2, 2 });
	assert(a.sub_each(1).equals({2, 3}));
	//a -= b;
	//assert(a == (Complex){ 2, 2 });
}

fn void complex_scale()
{
	Complex a = {2, 1};
	assert(a * 2 == (Complex) {4, 2});
}

fn void complex_conjugate()
{
	Complex a = {3, 4};
	assert(a.conjugate() == (Complex) {3, -4});
}

fn void complex_inverse()
{
	Complex a = {3, 4};
	assert(a.inverse() * a == 1);
}

fn void complex_div()
{
	Complex a = {2, 5};
	Complex b = {4, -1};
	assert(a / b == (Complex) {3.0/17.0, 22.0/17.0});
	//a /= b;
	//assert(a == (Complex) {3.0/17.0, 22.0/17.0});
}

