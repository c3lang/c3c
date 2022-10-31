// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::math;
import std::math::complex;

// TODO Define these using quad precision.
const E = 2.718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427427466;
const LOG2E = 1.44269504088896340735992468100189214; // log2(e)
const LOG10E = 0.434294481903251827651128918916605082; // log10(e)
const LN2 = 0.693147180559945309417232121458176568; // ln(2)
const LN10 = 2.30258509299404568401799145468436421; // ln(10)
const PI = 3.14159265358979323846264338327950288419716939937510; // pi
const PI_2 = 1.57079632679489661923132169163975144; // pi / 2
const PI_4 = 0.785398163397448309615660845819875721; // pi / 4
const DIV_PI = 0.318309886183790671537767526745028724; // 1 / pi
const DIV_2_PI = 0.636619772367581343075535053490057448; // 2 / pi
const DIV_2_SQRTPI = 1.12837916709551257389615890312154517;   // 2/sqrt(pi)
const SQRT2 = 1.41421356237309504880168872420969808; // sqrt(2)
const double DIV_1_SQRT2 = 0.707106781186547524400844362104849039; // 1 / sqrt(2)

const HALF_MAX = 6.5504e+4;
const HALF_MIN = 6.103515625e-5;
const HALF_DENORM_MIN = 5.9604644775390625e-8;
const HALF_DIG = 3;
const HALF_DEC_DIGITS = 5;
const HALF_MANT_DIG = 11;
const HALF_MAX_10_EXP = 4;
const HALF_MIN_10_EXP = -4;
const HALF_MAX_EXP = 16;
const HALF_MIN_EXP = -13;
const HALF_EPSILON = 9.765625e-4;

const FLOAT_MAX = 0x1.fffffep+127;
const FLOAT_MIN = 1.17549435e-38;
const FLOAT_DENORM_MIN = 1.40129846432481707092e-45;
const FLOAT_DIG = 6;
const FLOAT_DEC_DIGITS = 9;
const FLOAT_MANT_DIG = 24;
const FLOAT_MAX_10_EXP = 38;
const FLOAT_MIN_10_EXP = -37;
const FLOAT_MAX_EXP = 128;
const FLOAT_MIN_EXP = -125;
const FLOAT_EPSILON = 1.1920928955078125e-07;

const DOUBLE_MAX = 1.79769313486231570815e+308;
const DOUBLE_MIN = 2.2250738585072014e-308;
const DOUBLE_DENORM_MIN = 4.94065645841246544177e-324;
const DOUBLE_DIG = 15;
const DOUBLE_DEC_DIGITS = 17;
const DOUBLE_MANT_DIG = 53;
const DOUBLE_MAX_10_EXP = 308;
const DOUBLE_MIN_10_EXP = -307;
const DOUBLE_MAX_EXP = 1024;
const DOUBLE_MIN_EXP = -1021;
const DOUBLE_EPSILON = 2.22044604925031308085e-16;

const QUAD_MANT_DIG = 113;

/*
const QUAD_MAX = 1.18973149535723176508575932662800702e+4932;
const QUAD_MIN = 3.36210314311209350626267781732175260e-4932;
const QUAD_DENORM_MIN = 6.47517511943802511092443895822764655e-4966;
const QUAD_DIG = 33;
const QUAD_DEC_DIGITS = 36;
const QUAD_MAX_10_EXP = 4932;
const QUAD_MIN_10_EXP = -4931;
const QUAD_MAX_EXP = 16384;
const QUAD_MIN_EXP = -16481;
const QUAD_EPSILON = 1.92592994438723585305597794258492732e-34;
*/

define Complex32 = Complex<float>;
define Complex64 = Complex<double>;

/**
 * @require types::is_numerical($typeof(x)) `The input must be a numerical value or numerical vector`
 **/
macro abs(x) = $$abs(x);

/**
 * @require types::is_numerical($typeof(x)) `The input must be a numerical value or numerical vector`
 * @require types::@has_same(x, lower, upper) `The input types must be equal`
 **/
macro clamp(x, lower, upper) = $$max(lower, $$min(x, upper));

/**
 * @require types::is_floatlike($typeof(mag)) `The input must be a floating point value or float vector`
 * @require types::is_same($typeof(mag), $typeof(sgn)) `The input types must be equal`
 **/
macro copysign(mag, sgn) = $$copysign(mag, sgn);

/**
 * @require types::is_floatlike($typeof(x)) `The input must be a floating point value or float vector`
 **/
macro cos(x) = $$cos(x);

macro cosec(x) = 1 / sin(x);

macro cosech(x) = 2 / (exp(x) - exp(-x));

/**
 * @require types::is_floatlike($typeof(x)) `The input must be a floating point value or float vector`
 **/
macro cosh(x) = (exp(x) + exp(-x)) / 2.0;

macro cotan(x) = cos(x) / sin(x);

macro cotanh(x) = (exp(2.0 * x) + 1.0) / (exp(2.0 * x) - 1.0);

/**
 * @require types::is_floatlike($typeof(x)) `The input must be a floating point value or float vector`
 **/
macro exp(x) = $$exp(x);

/**
 * @require types::is_floatlike($typeof(x)) `The input must be a floating point value or float vector`
 **/
macro exp2(x) = $$exp2(x);

/**
 * @require types::is_floatlike($typeof(a)) `The input must be a floating point value or float vector`
 * @require types::@has_same(a, b, c) `The input types must be equal`
 **/
macro fma(a, b, c) = $$fma(a, b, c);

/**
 * @require types::is_floatlike($typeof(x)) `The input must be a floating point value or float vector`
 * @require types::is_floatlike($typeof(y)) `The input must be a floating point value or float vector`
 **/
macro hypot(x, y) = sqrt(sqr(x) + sqr(y));

/**
 * @require types::is_floatlike($typeof(x)) `The input must be a floating point value or float vector`
 **/
macro log(x) = $$log(x);

/**
 * @require types::is_floatlike($typeof(x)) `The input must be a floating point value or float vector`
 **/
macro log2(x) = $$log2(x);

/**
 * @require types::is_floatlike($typeof(x)) `The input must be a floating point value or float vector`
 **/
macro log10(x) = $$log10(x);

/**
 * @require types::is_numerical($typeof(x)) `The input must be a floating point value or float vector`
 * @require types::is_same($typeof(x), $typeof(y)) `The input types must be equal`
 **/
macro max(x, y) = $$max(x, y);

/**
 * @require types::is_numerical($typeof(x)) `The input must be a numerical value or numerical vector`
 * @require types::is_same($typeof(x), $typeof(y)) `The input types must be equal`
 **/
macro min(x, y) = $$min(x, y);

/**
 * @require types::is_float($typeof(a)) `The input must be a floating point value`
 * @require types::@has_same(a, b, c) `The input types must be equal`
 **/
macro muladd(a, b, c) = $$fmuladd(a, b, c);

/**
 * @require types::is_floatlike($typeof(x)) `The input must be a floating point value or float vector`
 * @require types::is_floatlike($typeof(exp)) || types::is_int($typeof(exp)) `The input must be an integer, a floating point value or float vector`
 **/
macro pow(x, exp)
{
    $if (types::is_floatlike($typeof(exp))):
	return $$pow(x, exp);
    $else:
	return $$pow_int(x, exp);
    $endif;
}

macro sec(x) = 1 / cos(x);

macro sech(x) = 2 / (exp(x) + exp(-x));

/**
 * @require types::is_floatlike($typeof(x)) `The input must be a floating point value or float vector`
 **/
macro sin(x) = $$sin(x);

/**
 * @require types::is_floatlike($typeof(x)) `The input must be a floating point value or float vector`
 **/
macro sinh(x) = (exp(x) - exp(-x)) / 2.0;

/**
 * @require types::is_floatlike($typeof(x)) `The input must be a floating point value or float vector`
 **/
macro sqr(x) = x * x;

/**
 * @require types::is_floatlike($typeof(x)) `The input must be a floating point value or float vector`
 **/
macro sqrt(x) = $$sqrt(x);

macro tan(x) = sin(x) / cos(x);

/**
 * @require types::is_floatlike($typeof(x)) `The input must be a floating point value or float vector`
 **/
macro tanh(x) = (exp(2.0 * x) - 1.0) / (exp(2.0 * x) + 1.0);

macro float float.ceil(float x) = $$ceil(x);
macro float float.clamp(float x, float lower, float upper) = $$max(lower, $$min(x, upper));
macro float float.copysign(float mag, float sgn) = $$copysign(mag, sgn);
macro float float.floor(float x) = $$floor(x);
macro float float.fma(float a, float b, float c) = $$fma(a, b, c);
macro float float.muladd(float a, float b, float c) = $$fmuladd(a, b, c);
macro float float.nearbyint(float x) = $$nearbyint(x);
macro float float.pow(float x, exp) = pow(x, exp);
macro float float.rint(float x) = $$rint(x);
macro float float.round(float x) = $$round(x);
macro float float.roundeven(float x) = $$roundeven(x);
macro float float.trunc(float x) = $$trunc(x);

macro float[<*>] float[<*>].ceil(float[<*>] x) = $$ceil(x);
macro float[<*>] float[<*>].clamp(float[<*>] x, float[<*>] lower, float[<*>] upper) = $$max(lower, $$min(x, upper));
macro float[<*>] float[<*>].copysign(float[<*>] mag, float[<*>] sgn) = $$copysign(mag, sgn);
macro float[<*>] float[<*>].fma(float[<*>] a, float[<*>] b, float[<*>] c) = $$fma(a, b, c);
macro float[<*>] float[<*>].floor(float[<*>] x) = $$floor(x);
macro float[<*>] float[<*>].nearbyint(float[<*>] x) = $$nearbyint(x);
macro float[<*>] float[<*>].pow(float[<*>] x, exp) = pow(x, exp);
macro float[<*>] float[<*>].rint(float[<*>] x) = $$rint(x);
macro float[<*>] float[<*>].round(float[<*>] x) = $$round(x);
macro float[<*>] float[<*>].roundeven(float[<*>] x) = $$roundeven(x);
macro float[<*>] float[<*>].trunc(float[<*>] x) = $$trunc(x);

macro double double.ceil(double x) = $$ceil(x);
macro double double.clamp(double x, double lower, double upper) = $$max(lower, $$min(x, upper));
macro double double.copysign(double mag, double sgn) = $$copysign(mag, sgn);
macro double double.floor(double x) = $$floor(x);
macro double double.fma(double a, double b, double c) = $$fma(a, b, c);
macro double double.muladd(double a, double b, double c) = $$fmuladd(a, b, c);
macro double double.nearbyint(double x) = $$nearbyint(x);
macro double double.pow(double x, exp) = pow(x, exp);
macro double double.rint(double x) = $$rint(x);
macro double double.round(double x) = $$round(x);
macro double double.roundeven(double x) = $$roundeven(x);
macro double double.trunc(double x) = $$trunc(x);

macro double[<*>] double[<*>].ceil(double[<*>] x) = $$ceil(x);
macro double[<*>] double[<*>].clamp(double[<*>] x, double[<*>] lower, double[<*>] upper) = $$max(lower, $$min(x, upper));
macro double[<*>] double[<*>].copysign(double[<*>] mag, double[<*>] sgn) = $$copysign(mag, sgn);
macro double[<*>] double[<*>].floor(double[<*>] x) = $$floor(x);
macro double[<*>] double[<*>].fma(double[<*>] a, double[<*>] b, double[<*>] c) = $$fma(a, b, c);
macro double[<*>] double[<*>].nearbyint(double[<*>] x) = $$nearbyint(x);
macro double[<*>] double[<*>].pow(double[<*>] x, exp) = pow(x, exp);
macro double[<*>] double[<*>].rint(double[<*>] x) = $$rint(x);
macro double[<*>] double[<*>].round(double[<*>] x) = $$round(x);
macro double[<*>] double[<*>].roundeven(double[<*>] x) = $$roundeven(x);
macro double[<*>] double[<*>].trunc(double[<*>] x) = $$trunc(x);

/**
 * @checked x & 1
 */
macro bool is_power_of_2(x)
{
	return x != 0 && (x & (x - 1)) == 0;
}

macro next_power_of_2(x)
{
	$typeof(x) y = 1;
	while (y < x) y += y;
	return y;
}
