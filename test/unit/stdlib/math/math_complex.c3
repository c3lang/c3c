module math_tests @test;
import math_tests::complex;

def ComplexDouble = ComplexType(<double>) @local;
def ComplexInt = ComplexType(<int>) @local;

module math_tests::complex(<ElementType>) @test;
import std::math;

def ComplexType = Complex(<ElementType>);

fn void complex_mul_imaginary()
{
	ComplexType i = complex::IMAGINARY(<ElementType>);
	assert(i.mul(i).equals(ComplexType{-1, 0}));
	assert(i.mul(i).mul(i).equals(ComplexType{0, -1}));
}

fn void complex_add()
{
	ComplexType a = {3, 4};
	ComplexType b = {1, 2};
	assert(a.add(b).equals(ComplexType{4, 6}));
	assert(a.add_each(1).equals(ComplexType{4, 5}));
}

fn void complex_sub()
{
	ComplexType a = {3, 4};
	ComplexType b = {1, 2};
	assert(a.sub(b).equals(ComplexType{2, 2}));
	assert(a.sub_each(1).equals(ComplexType{2, 3}));
}

fn void complex_scale()
{
	ComplexType a = {2, 1};
	assert(a.scale(2).equals(ComplexType{4, 2}));
}

fn void complex_conjugate()
{
	ComplexType a = {3, 4};
	assert(a.conjugate().equals(ComplexType{3, -4}));
}

fn void complex_inverse() @if(types::is_float(ElementType))
{
	ComplexType a = {3, 4};
	assert(a.inverse().mul(a).equals(complex::IDENTITY(<ElementType>)));
}

fn void complex_div() @if(types::is_float(ElementType))
{
	ComplexType a = {2, 5};
	ComplexType b = {4, -1};
	assert(a.div(b).equals(ComplexType{3.0/17.0, 22.0/17.0}));
}

