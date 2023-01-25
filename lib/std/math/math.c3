// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::math;
import std::math::complex;
import std::math::matrix;
import std::math::quaternion;

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

enum RoundingMode : int
{
	TOWARD_ZERO,
	TO_NEAREST,
	TOWARD_INFINITY,
	TOWARD_NEG_INFINITY
}

fault MatrixError
{
    MATRIX_INVERSE_DOESNT_EXIST,
}

define Complexf = Complex<float>;
define Complex = Complex<double>;
define complexf_identity = complex::identity<float>;
define complex_identity = complex::identity<double>;

define Quaternionf = Quaternion<float>;
define Quaternion = Quaternion<double>;
define quaternionf_identity = quaternion::identity<float>;
define quaternion_identity = quaternion::identity<double>;

define Matrix2f = Matrix2x2<float>;
define Matrix2 = Matrix2x2<double>;
define Matrix3f = Matrix3x3<float>;
define Matrix3 = Matrix3x3<double>;
define Matrix4f = Matrix4x4<float>;
define Matrix4 = Matrix4x4<double>;
define matrix4_ortho = matrix::ortho<double>;
define matrix4_perspective = matrix::perspective<double>;
define matrix4f_ortho = matrix::ortho<float>;
define matrix4f_perspective = matrix::perspective<float>;

/**
 * @require types::is_numerical($typeof(x)) `The input must be a numerical value or numerical vector`
 **/
macro abs(x) => $$abs(x);

/**
 * @require values::@is_int(x) `The input must be an integer`
 **/
macro sign(x)
{
	if (!x) return ($typeof(x))0;
	return ($typeof(x))(x < 0 ? -1 : 1);
}


/**
 * @require values::@is_int(x) || values::@is_float(x) "Expected an integer or floating point value"
 * @checked x + y
 **/
macro atan2(x, y)
{
	$if ($typeof(x).typeid == float.typeid && $typeof(y).typeid == float.typeid):
		return _atan2f(x, y);
	$else:
		return _atan2(x, y);
	$endif;
}

/**
 * @require values::@is_int(x) || values::@is_float(x) "Expected an integer or floating point value"
 * @checked x + y
 **/
macro atan(x)
{
	$if ($typeof(x).typeid == float.typeid):
		return _atanf(x);
	$else:
		return _atan(x);
	$endif;
}

/**
 * @require values::@is_floatlike(x) `The input must be a floating point value or float vector`
 **/
macro ceil(x) => $$ceil(x);

/**
 * @require types::is_numerical($typeof(x)) `The input must be a numerical value or numerical vector`
 * @require types::@has_same(x, lower, upper) `The input types must be equal`
 **/
macro clamp(x, lower, upper) => $$max(lower, $$min(x, upper));

/**
 * @require values::@is_promotable_to_floatlike(mag) `The input must be a number value or float vector`
 * @require values::@is_same_vector_type(mag, sgn) `The input types must match`
 **/
macro copysign(mag, sgn) => $$copysign(mag, sgn);

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number value or float vector`
 **/
macro cos(x) => $$cos(x);

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number value or float vector`
 **/
macro cosec(x) => 1 / sin(x);

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number value or float vector`
 **/
macro cosech(x) => 2 / (exp(x) - exp(-x));

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number value or float vector`
 **/
macro cosh(x) => (exp(x) + exp(-x)) / 2.0;

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number value or float vector`
 **/
macro cotan(x) => cos(x) / sin(x);

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number value or float vector`
 **/
macro cotanh(x) => (exp(2.0 * x) + 1.0) / (exp(2.0 * x) - 1.0);

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number value or float vector`
 **/
macro exp(x) => $$exp(values::promote_int(x));

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number value or float vector`
 **/
macro exp2(x) => $$exp2(values::promote_int(x));

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number value or float vector`
 **/
macro floor(x) => $$floor(values::promote_int(x));

/**
 * @require values::@is_promotable_to_floatlike(a) `The input must be a number or float vector`
 * @require values::@is_promotable_to_floatlike(b) `The input must be a number or float vector`
 * @require values::@is_promotable_to_floatlike(c) `The input must be a number or float vector`
 * @require types::@is_same_vector_type(a, b) `The input types must be equal`
 * @require types::@is_same_vector_type(a, c) `The input types must match`
 **/
macro fma(a, b, c) => $$fma(a, b, c);


/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 * @require values::@is_promotable_to_floatlike(y) `The input must be a number or a float vector`
 * @require types::@is_same_vector_type(x, y) `The input types must match`
 **/
macro hypot(x, y) => sqrt(sqr(x) + sqr(y));

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro log(x) => $$log(values::promote_int(x));

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro log2(x) => $$log2(values::promote_int(x));

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro log10(x) => $$log10(values::promote_int(x));

/**
 * @require types::is_numerical($typeof(x)) `The input must be a floating point value or float vector`
 * @require types::is_same($typeof(x), $typeof(y)) `The input types must be equal`
 **/
macro max(x, y, ...)
{
	$if ($vacount == 0):
		return $$max(x, y);
	$else:
		var m = $$max(x, y);
		$for (var $i = 0; $i < $vacount; $i++):
			m = $$max(m, $vaarg($i));
		$endfor;
		return m;
	$endif;
}

/**
 * @require types::is_numerical($typeof(x)) `The input must be a numerical value or numerical vector`
 * @require types::is_same($typeof(x), $typeof(y)) `The input types must be equal`
 **/
macro min(x, y, ...)
{
	$if ($vacount == 0):
		return $$min(x, y);
    $else:
    	var m = $$min(x, y);
    	$for (var $i = 0; $i < $vacount; $i++):
    		m = $$min(m, $vaarg($i));
    	$endfor;
    	return m;
    $endif;
}

/**
 * @require types::@is_float(a) `The input must be a floating point value`
 * @require types::@has_same(a, b, c) `The input types must be equal`
 **/
macro muladd(a, b, c) => $$fmuladd(a, b, c);

/**
 * @require values::@is_floatlike(x) `The input must be a floating point value or float vector`
 **/
macro nearbyint(x) => $$nearbyint(x);

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 * @require values::@convertable_to(exp, x) || values::@is_int(exp) `The input must be an integer, castable to the type of x`
 **/
macro pow(x, exp)
{
    $if (types::is_floatlike($typeof(exp))):
		return $$pow(x, ($typeof(x))exp);
    $else:
		return $$pow_int(x, exp);
    $endif;
}

/**
 * @require values::@is_floatlike(x) `The input must be a number or a float vector`
 **/
macro rint(x) => $$rint(x);

/**
 * @require values::@is_floatlike(x) `The input must be a floating point value or float vector`
 **/
macro round(x) => $$round(x);

/**
 * @require values::@is_floatlike(x) `The input must be a floating point value or float vector`
 **/
macro roundeven(x) => $$roundeven(x);

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro sec(x) => 1 / cos(x);

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro sech(x) => 2 / (exp(x) + exp(-x));

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro sin(x) => $$sin(values::promote_int(x));

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro sinh(x) => (exp(x) - exp(-x)) / 2.0;

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro sqr(x) => values::promote_int(x) * values::promote_int(x);

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro sqrt(x) => $$sqrt(values::promote_int(x));

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro tan(x) => sin(x) / cos(x);

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro tanh(x) => (exp(2.0 * x) - 1.0) / (exp(2.0 * x) + 1.0);

/**
 * @require values::@is_floatlike(x) `The input must be a floating point value or float vector`
 **/
macro trunc(x) => $$trunc(x);

private macro lerp(x, y, amount) => x + (y - x) * amount;
private macro reflect(x, y)
{
	var dot = x.dot(y);
	return x - 2 * y * dot;
}
private macro normalize(x)
{
	var len = x.length();
	if (len == 0) return x;
	return x * (1 / len);
}

macro float float.ceil(float x) => $$ceil(x);
macro float float.clamp(float x, float lower, float upper) => $$max(lower, $$min(x, upper));
macro float float.copysign(float mag, float sgn) => $$copysign(mag, sgn);
macro float float.floor(float x) => $$floor(x);
macro float float.fma(float a, float b, float c) => $$fma(a, b, c);
macro float float.muladd(float a, float b, float c) => $$fmuladd(a, b, c);
macro float float.nearbyint(float x) => $$nearbyint(x);
macro float float.pow(float x, exp) => pow(x, exp);
macro float float.rint(float x) => $$rint(x);
macro float float.round(float x) => $$round(x);
macro float float.roundeven(float x) => $$roundeven(x);
macro float float.trunc(float x) => $$trunc(x);

macro float float[<*>].sum(float[<*>] x, float start = 0.0) => $$reduce_fadd(x, start);
macro float float[<*>].product(float[<*>] x, float start = 1.0) => $$reduce_fmul(x, start);
macro float float[<*>].max(float[<*>] x) => $$reduce_max(x);
macro float float[<*>].min(float[<*>] x) => $$reduce_min(x);
macro float[<*>] float[<*>].ceil(float[<*>] x) => $$ceil(x);
macro float[<*>] float[<*>].clamp(float[<*>] x, float[<*>] lower, float[<*>] upper) => $$max(lower, $$min(x, upper));
macro float[<*>] float[<*>].copysign(float[<*>] mag, float[<*>] sgn) => $$copysign(mag, sgn);
macro float[<*>] float[<*>].fma(float[<*>] a, float[<*>] b, float[<*>] c) => $$fma(a, b, c);
macro float[<*>] float[<*>].floor(float[<*>] x) => $$floor(x);
macro float[<*>] float[<*>].nearbyint(float[<*>] x) => $$nearbyint(x);
macro float[<*>] float[<*>].pow(float[<*>] x, exp) => pow(x, exp);
macro float[<*>] float[<*>].rint(float[<*>] x) => $$rint(x);
macro float[<*>] float[<*>].round(float[<*>] x) => $$round(x);
macro float[<*>] float[<*>].roundeven(float[<*>] x) => $$roundeven(x);
macro float[<*>] float[<*>].trunc(float[<*>] x) => $$trunc(x);
macro float float[<*>].dot(float[<*>] x, float[<*>] y) => (x * y).sum();
macro float float[<*>].length(float[<*>] x) => $$sqrt(x.dot(x));
macro float float[<*>].distance(float[<*>] x, float[<*>] y) => (x - y).length();
macro float[<*>] float[<*>].normalize(float[<*>] x) => normalize(x);
macro float[<*>] float[<*>].lerp(float[<*>] x, float[<*>] y, float amount) => lerp(x, y, amount);
macro float[<*>] float[<*>].reflect(float[<*>] x, float[<*>] y) => reflect(x, y);
macro bool float[<*>].equals(float[<*>] x, float[<*>] y) => equals_vec(x, y);

macro bool[<*>] float[<*>].comp_lt(float[<*>] x, float[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] float[<*>].comp_le(float[<*>] x, float[<*>] y) => $$veccomple(x, y);
macro bool[<*>] float[<*>].comp_eq(float[<*>] x, float[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] float[<*>].comp_gt(float[<*>] x, float[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] float[<*>].comp_ge(float[<*>] x, float[<*>] y) => $$veccompge(x, y);
macro bool[<*>] float[<*>].comp_ne(float[<*>] x, float[<*>] y) => $$veccompne(x, y);

macro double double.ceil(double x) => $$ceil(x);
macro double double.clamp(double x, double lower, double upper) => $$max(lower, $$min(x, upper));
macro double double.copysign(double mag, double sgn) => $$copysign(mag, sgn);
macro double double.floor(double x) => $$floor(x);
macro double double.fma(double a, double b, double c) => $$fma(a, b, c);
macro double double.muladd(double a, double b, double c) => $$fmuladd(a, b, c);
macro double double.nearbyint(double x) => $$nearbyint(x);
macro double double.pow(double x, exp) => pow(x, exp);
macro double double.rint(double x) => $$rint(x);
macro double double.round(double x) => $$round(x);
macro double double.roundeven(double x) => $$roundeven(x);
macro double double.trunc(double x) => $$trunc(x);

macro double double[<*>].sum(double[<*>] x, double start = 0.0) => $$reduce_fadd(x, start);
macro double double[<*>].product(double[<*>] x, double start = 1.0) => $$reduce_fmul(x, start);
macro double double[<*>].max(double[<*>] x) => $$reduce_fmax(x);
macro double double[<*>].min(double[<*>] x) => $$reduce_fmin(x);
macro double[<*>] double[<*>].ceil(double[<*>] x) => $$ceil(x);
macro double[<*>] double[<*>].clamp(double[<*>] x, double[<*>] lower, double[<*>] upper) => $$max(lower, $$min(x, upper));
macro double[<*>] double[<*>].copysign(double[<*>] mag, double[<*>] sgn) => $$copysign(mag, sgn);
macro double[<*>] double[<*>].floor(double[<*>] x) => $$floor(x);
macro double[<*>] double[<*>].fma(double[<*>] a, double[<*>] b, double[<*>] c) => $$fma(a, b, c);
macro double[<*>] double[<*>].nearbyint(double[<*>] x) => $$nearbyint(x);
macro double[<*>] double[<*>].pow(double[<*>] x, exp) => pow(x, exp);
macro double[<*>] double[<*>].rint(double[<*>] x) => $$rint(x);
macro double[<*>] double[<*>].round(double[<*>] x) => $$round(x);
macro double[<*>] double[<*>].roundeven(double[<*>] x) => $$roundeven(x);
macro double[<*>] double[<*>].trunc(double[<*>] x) => $$trunc(x);
macro double double[<*>].dot(double[<*>] x, double[<*>] y) => (x * y).sum();
macro double double[<*>].length(double[<*>] x) => $$sqrt(x.dot(x));
macro double double[<*>].distance(double[<*>] x, double[<*>] y) => (x - y).length();
macro double[<*>] double[<*>].normalize(double[<*>] x) => normalize(x);
macro double[<*>] double[<*>].reflect(double[<*>] x, double[<*>] y) => reflect(x, y);
macro double[<*>] double[<*>].lerp(double[<*>] x, double[<*>] y, double amount) => lerp(x, y, amount);
macro bool double[<*>].equals(double[<*>] x, double[<*>] y) => equals_vec(x, y);

macro bool[<*>] double[<*>].comp_lt(double[<*>] x, double[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] double[<*>].comp_le(double[<*>] x, double[<*>] y) => $$veccomple(x, y);
macro bool[<*>] double[<*>].comp_eq(double[<*>] x, double[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] double[<*>].comp_gt(double[<*>] x, double[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] double[<*>].comp_ge(double[<*>] x, double[<*>] y) => $$veccompge(x, y);
macro bool[<*>] double[<*>].comp_ne(double[<*>] x, double[<*>] y) => $$veccompne(x, y);

macro ichar ichar[<*>].sum(ichar[<*>] x) => $$reduce_add(x);
macro ichar ichar[<*>].product(ichar[<*>] x) => $$reduce_mul(x);
macro ichar ichar[<*>].and(ichar[<*>] x) => $$reduce_and(x);
macro ichar ichar[<*>].or(ichar[<*>] x) => $$reduce_or(x);
macro ichar ichar[<*>].xor(ichar[<*>] x) => $$reduce_xor(x);
macro ichar ichar[<*>].max(ichar[<*>] x) => $$reduce_max(x);
macro ichar ichar[<*>].min(ichar[<*>] x) => $$reduce_min(x);

macro short short[<*>].sum(short[<*>] x) => $$reduce_add(x);
macro short short[<*>].product(short[<*>] x) => $$reduce_mul(x);
macro short short[<*>].and(short[<*>] x) => $$reduce_and(x);
macro short short[<*>].or(short[<*>] x) => $$reduce_or(x);
macro short short[<*>].xor(short[<*>] x) => $$reduce_xor(x);
macro short short[<*>].max(short[<*>] x) => $$reduce_max(x);
macro short short[<*>].min(short[<*>] x) => $$reduce_min(x);

macro bool[<*>] int[<*>].comp_lt(int[<*>] x, int[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] int[<*>].comp_le(int[<*>] x, int[<*>] y) => $$veccomple(x, y);
macro bool[<*>] int[<*>].comp_eq(int[<*>] x, int[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] int[<*>].comp_gt(int[<*>] x, int[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] int[<*>].comp_ge(int[<*>] x, int[<*>] y) => $$veccompge(x, y);
macro bool[<*>] int[<*>].comp_ne(int[<*>] x, int[<*>] y) => $$veccompne(x, y);

macro int int[<*>].sum(int[<*>] x) => $$reduce_add(x);
macro int int[<*>].product(int[<*>] x) => $$reduce_mul(x);
macro int int[<*>].and(int[<*>] x) => $$reduce_and(x);
macro int int[<*>].or(int[<*>] x) => $$reduce_or(x);
macro int int[<*>].xor(int[<*>] x) => $$reduce_xor(x);
macro int int[<*>].max(int[<*>] x) => $$reduce_max(x);
macro int int[<*>].min(int[<*>] x) => $$reduce_min(x);

macro long long[<*>].sum(long[<*>] x) => $$reduce_add(x);
macro long long[<*>].product(long[<*>] x) => $$reduce_mul(x);
macro long long[<*>].and(long[<*>] x) => $$reduce_and(x);
macro long long[<*>].or(long[<*>] x) => $$reduce_or(x);
macro long long[<*>].xor(long[<*>] x) => $$reduce_xor(x);
macro long long[<*>].max(long[<*>] x) => $$reduce_max(x);
macro long long[<*>].min(long[<*>] x) => $$reduce_min(x);

macro int128 int128[<*>].sum(int128[<*>] x) => $$reduce_add(x);
macro int128 int128[<*>].product(int128[<*>] x) => $$reduce_mul(x);
macro int128 int128[<*>].and(int128[<*>] x) => $$reduce_and(x);
macro int128 int128[<*>].or(int128[<*>] x) => $$reduce_or(x);
macro int128 int128[<*>].xor(int128[<*>] x) => $$reduce_xor(x);
macro int128 int128[<*>].max(int128[<*>] x) => $$reduce_max(x);
macro int128 int128[<*>].min(int128[<*>] x) => $$reduce_min(x);

macro bool[<*>] bool[<*>].comp_lt(bool[<*>] x, bool[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] bool[<*>].comp_le(bool[<*>] x, bool[<*>] y) => $$veccomple(x, y);
macro bool[<*>] bool[<*>].comp_eq(bool[<*>] x, bool[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] bool[<*>].comp_gt(bool[<*>] x, bool[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] bool[<*>].comp_ge(bool[<*>] x, bool[<*>] y) => $$veccompge(x, y);
macro bool[<*>] bool[<*>].comp_ne(bool[<*>] x, bool[<*>] y) => $$veccompne(x, y);

macro bool bool[<*>].sum(bool[<*>] x) => $$reduce_add(x);
macro bool bool[<*>].product(bool[<*>] x) => $$reduce_mul(x);
macro bool bool[<*>].and(bool[<*>] x) => $$reduce_and(x);
macro bool bool[<*>].or(bool[<*>] x) => $$reduce_or(x);
macro bool bool[<*>].xor(bool[<*>] x) => $$reduce_xor(x);
macro bool bool[<*>].max(bool[<*>] x) => $$reduce_max(x);
macro bool bool[<*>].min(bool[<*>] x) => $$reduce_min(x);

macro char char[<*>].sum(char[<*>] x) => $$reduce_add(x);
macro char char[<*>].product(char[<*>] x) => $$reduce_mul(x);
macro char char[<*>].and(char[<*>] x) => $$reduce_and(x);
macro char char[<*>].or(char[<*>] x) => $$reduce_or(x);
macro char char[<*>].xor(char[<*>] x) => $$reduce_xor(x);
macro char char[<*>].max(char[<*>] x) => $$reduce_max(x);
macro char char[<*>].min(char[<*>] x) => $$reduce_min(x);


macro ushort ushort[<*>].sum(ushort[<*>] x) => $$reduce_add(x);
macro ushort ushort[<*>].product(ushort[<*>] x) => $$reduce_mul(x);
macro ushort ushort[<*>].and(ushort[<*>] x) => $$reduce_and(x);
macro ushort ushort[<*>].or(ushort[<*>] x) => $$reduce_or(x);
macro ushort ushort[<*>].xor(ushort[<*>] x) => $$reduce_xor(x);
macro ushort ushort[<*>].max(ushort[<*>] x) => $$reduce_max(x);
macro ushort ushort[<*>].min(ushort[<*>] x) => $$reduce_min(x);

macro uint uint[<*>].sum(uint[<*>] x) => $$reduce_add(x);
macro uint uint[<*>].product(uint[<*>] x) => $$reduce_mul(x);
macro uint uint[<*>].and(uint[<*>] x) => $$reduce_and(x);
macro uint uint[<*>].or(uint[<*>] x) => $$reduce_or(x);
macro uint uint[<*>].xor(uint[<*>] x) => $$reduce_xor(x);
macro uint uint[<*>].max(uint[<*>] x) => $$reduce_max(x);
macro uint uint[<*>].min(uint[<*>] x) => $$reduce_min(x);

macro ulong ulong[<*>].sum(ulong[<*>] x) => $$reduce_add(x);
macro ulong ulong[<*>].product(ulong[<*>] x) => $$reduce_mul(x);
macro ulong ulong[<*>].and(ulong[<*>] x) => $$reduce_and(x);
macro ulong ulong[<*>].or(ulong[<*>] x) => $$reduce_or(x);
macro ulong ulong[<*>].xor(ulong[<*>] x) => $$reduce_xor(x);
macro ulong ulong[<*>].max(ulong[<*>] x) => $$reduce_max(x);
macro ulong ulong[<*>].min(ulong[<*>] x) => $$reduce_min(x);

macro uint128 uint128[<*>].sum(uint128[<*>] x) => $$reduce_add(x);
macro uint128 uint128[<*>].product(uint128[<*>] x) => $$reduce_mul(x);
macro uint128 uint128[<*>].and(uint128[<*>] x) => $$reduce_and(x);
macro uint128 uint128[<*>].or(uint128[<*>] x) => $$reduce_or(x);
macro uint128 uint128[<*>].xor(uint128[<*>] x) => $$reduce_xor(x);
macro uint128 uint128[<*>].max(uint128[<*>] x) => $$reduce_max(x);
macro uint128 uint128[<*>].min(uint128[<*>] x) => $$reduce_min(x);

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

private macro equals_vec(v1, v2)
{
	var $elements = v1.len;
	var abs_diff = math::abs(v1 - v2);
	var abs_v1 = math::abs(v1);
	var abs_v2 = math::abs(v2);
    $typeof(abs_v2) eps = 1;
	return abs_diff.comp_le(FLOAT_EPSILON * math::max(abs_v1, abs_v2, eps)).and();
}

macro uint double.high_word(double d) => (uint)(bitcast(d, ulong) >> 32);
macro uint double.low_word(double d) => (uint)bitcast(d, ulong);
macro uint float.word(float d) => bitcast(d, uint);

macro is_nan(x)
{
	$switch ($typeof(x)):
	$case float:
		return bitcast(x, uint) & 0x7fffffff > 0x7f800000;
	$case double:
		return bitcast(x, ulong) & (((ulong)-1) >> 1) > (0x7ffu64 << 52);
	$default:
		$assert(false, "Type cannot be used with is_nan");
	$endswitch;
}


extern fn double _atan(double x) @extname("atan");
extern fn float _atanf(float x) @extname("atanf");
extern fn double _atan2(double, double) @extname("atan2");
extern fn float _atan2f(float, float) @extname("atan2f");
