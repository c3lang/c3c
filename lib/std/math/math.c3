// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::math;
import std::math::complex;
import std::math::matrix;
import std::math::quaternion;

attrdef @MathLibc(name) = @cname(name), @link(env::POSIX, "m");

const E = 2.718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427427466;
const LOG2E = 1.44269504088896340735992468100189214; // log2(e)
const LOG10E = 0.434294481903251827651128918916605082; // log10(e)
const LN2 = 0.693147180559945309417232121458176568; // ln(2)
const LN10 = 2.30258509299404568401799145468436421; // ln(10)
const PI = 3.14159265358979323846264338327950288419716939937510; // pi
const HALF_PI = 1.57079632679489661923132169163975144; // pi / 2
const QUARTER_PI = 0.785398163397448309615660845819875721; // pi / 4
const INV_PI = 0.318309886183790671537767526745028724; // 1 / pi
const TWO_OVER_PI = 0.636619772367581343075535053490057448; // 2 / pi
const TWO_OVER_SQRT_PI = 1.12837916709551257389615890312154517;   // 2/sqrt(pi)
const SQRT2 = 1.41421356237309504880168872420969808; // sqrt(2)
const double INV_SQRT2 = 0.707106781186547524400844362104849039; // 1 / sqrt(2)

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
const FLOAT_VECTOR_EPSILON = 2e-6;

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
const DOUBLE_VECTOR_EPSILON = 2e-12;

enum RoundingMode : int
{
	TOWARD_ZERO,
	TO_NEAREST,
	TOWARD_INFINITY,
	TOWARD_NEG_INFINITY
}

faultdef OVERFLOW, MATRIX_INVERSE_DOESNT_EXIST, NO_INTERSECTION;

<*
 @require values::@is_promotable_to_floatlike(degrees) : `The input must be a numerical value or numerical vector`
*>
macro deg_to_rad(degrees)
{
	var val = values::promote_int(degrees);
	return val * ($typeof(val))(PI / 180);
}

<*
 @require values::@is_promotable_to_floatlike(radians) : `The input must be a numerical value or numerical vector`
*>
macro rad_to_deg(radians)
{
	var val = values::promote_int(radians);
	return val * ($typeof(val))(180 / PI);
}

<*
 @require types::is_numerical($typeof(x)) : `The input must be a numerical value or numerical vector`
*>
macro abs(x) => $$abs(x);

<*
 @require $defined($x < 0, -$x) : `The input must be a numerical value which can be negated`
*>
macro @abs($x) => $x >= 0 ??? $x : -$x;

<*
 @require values::@is_int(x) || values::@is_float(x) : "Expected an integer or floating point value"
 @require values::@is_int(y) || values::@is_float(y) : "Expected an integer or floating point value"
*>
macro bool is_approx(x, y, eps)
{
	return fn bool($typeof(x) a, $typeof(y) b, $typeof(eps) eps)
	{
		if (a == b) return true;
		if (is_nan(a) || is_nan(b)) return false;
		return abs(a - b) <= eps;
	}(x, y, eps);
}

<*
 @require values::@is_int(x) || values::@is_float(x) : "Expected an integer or floating point value"
 @require values::@is_int(y) || values::@is_float(y) : "Expected an integer or floating point value"
*>
macro bool is_approx_rel(x, y, eps)
{
	return fn bool($typeof(x) a, $typeof(y) b, $typeof(eps) eps)
	{
		if (a == b) return true;
		if (is_nan(a) || is_nan(b)) return false;
		return abs(a - b) <= eps * max(abs(a), abs(b));
	}(x, y, eps);
}

<*
 @require @kindof(x).is_int() : `The input must be an integer`
*>
macro sign(x)
{
	var $Type = $typeof(x);
	$if $Type::kind == UNSIGNED_INT:
		return ($Type)(x > 0);
	$else
		return ($Type)(x > 0) - ($Type)(x < 0);
	$endif
}

<*
 @require values::@is_int(x) || values::@is_float(x) : "Expected an integer or floating point value"
*>
macro erf(x)
{
	$if $typeof(x) == float:
		return _erff(x);
	$else
		return _erf(x);
	$endif
}

<*
 @require values::@is_int(x) || values::@is_float(x) : "Expected an integer or floating point value"
*>
macro tgamma(x)
{
	$if $typeof(x) == float:
		return _tgammaf(x);
	$else
		return _tgamma(x);
	$endif
}

<*
 @require values::@is_int(x) || values::@is_float(x) : "Expected an integer or floating point value"
*>
macro lgamma(x)
{
	$if $typeof(x) == float:
		return _lgammaf(x);
	$else
		return _lgamma(x);
	$endif
}

<*
 @require values::@is_int(x) || values::@is_float(x) : "Expected an integer or floating point value"
 @require values::@is_int(y) || values::@is_float(y) : "Expected an integer or floating point value"
*>
macro atan2(x, y)
{
	$if $typeof(x) == float &&& $typeof(y) == float:
		return _atan2f(x, y);
	$else
		return _atan2(x, y);
	$endif
}

<*
 @require values::@is_int(x) || values::@is_float(x) : "Expected an integer or floating point value"
 @require @kindof(sinp) == POINTER : "Expected sinp to be a pointer"
 @require @typematch(sinp, cosp) : "Expected sinp and cosp to have the same type"
 @require $defined(*sinp = x) : "Expected x and *sinp/*cosp to have the same type"
*>
macro void sincos_ref(x, sinp, cosp)
{
	$if $typeof(sinp) == float*:
		_sincosf(x, sinp, cosp);
	$else
		_sincos(x, sinp, cosp);
	$endif
}

<*
 Return a vector with sin / cos of the given angle.

 @param x : `the angle in radians`
 @require values::@is_int(x) || values::@is_float(x) : "Expected an integer or floating point value"
*>
macro sincos(x)
{
	$if $typeof(x) == float:
		float[<2>] v @noinit;
		_sincosf(x, &v[0], &v[1]);
	$else
		double[<2>] v @noinit;
		_sincos(x, &v[0], &v[1]);
	$endif
	return v;
}

<*
 @require values::@is_int(x) || values::@is_float(x) : "Expected an integer or floating point value"
*>
macro atanh(x)
{
	$if $typeof(x) == float:
		return _atanhf(x);
	$else
		return _atanh(x);
	$endif
}

<*
 @require values::@is_int(x) || values::@is_float(x) : "Expected an integer or floating point value"
*>
macro acos(x) => $$acos(values::promote_int(x));

<*
 @require values::@is_int(x) || values::@is_float(x) : "Expected an integer or floating point value"
*>
macro acosh(x)
{
	$if $typeof(x) == float:
		return _acoshf(x);
	$else
		return _acosh(x);
	$endif
}

<*
 @require values::@is_int(x) || values::@is_float(x) : "Expected an integer or floating point value"
*>
macro asin(x) => $$asin(values::promote_int(x));

<*
 @require values::@is_int(x) || values::@is_float(x) : "Expected an integer or floating point value"
*>
macro asinh(x)
{
	$if $typeof(x) == float:
		return _asinhf(x);
	$else
		return _asinh(x);
	$endif
}

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number value or float vector`
*>
macro atan(x) => $$atan(values::promote_int(x));

<*
 @require values::@is_floatlike(x) : `The input must be a floating point value or float vector`
*>
macro ceil(x) => $$ceil(x);

<*
 Ceil for compile time evaluation.

 @require @kindof($input) == FLOAT : "Only float and double may be used"
*>
macro @ceil($input) @const => $$ceil($input);

<*
 Constrain the value to lie within the given interval.

 @param x : "the value to clamp, may be a number or a numerical vector."
 @param lower : "the lower bounds"
 @param upper : "the upper bounds"
 @return "lower if x < lower, upper if x > upper, otherwise return x."

 @require types::is_numerical($typeof(x)) : `The input must be a numerical value or numerical vector`
 @require $defined(x = lower) : `The lower bound must be convertible to the value type.`
 @require $defined(x = upper) : `The upper bound must be convertible to the value type.`
*>
macro clamp(x, lower, upper) => $$max(($typeof(x))lower, $$min(x, ($typeof(x))upper));

<*
 @require values::@is_promotable_to_floatlike(mag) : `The input must be a number value or float vector`
 @require $defined(($typeof(values::promote_int(mag)))mag) : `It's not possible to cast the sign to the type of the magnitude`
*>
macro copysign(mag, sgn) => $$copysign(values::promote_int_same(mag, sgn), ($typeof(values::promote_int_same(mag, sgn)))sgn);

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number value or float vector`
*>
macro cos(x) => $$cos(values::promote_int(x));

<*
 Cosecant

 @require values::@is_promotable_to_floatlike(x) : `The input must be a number value or float vector`
*>
macro csc(x) => 1 / sin(x);

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number value or float vector`
*>
macro csch(x) => 2 / (exp(x) - exp(-x));

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number value or float vector`
*>
macro cosh(x) => $$cosh(values::promote_int(x));

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number value or float vector`
*>
macro cot(x) => cos(x) / sin(x);

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number value or float vector`
*>
macro coth(x) => (exp(2.0 * x) + 1.0) / (exp(2.0 * x) - 1.0);

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number value or float vector`
*>
macro exp(x) => $$exp(values::promote_int(x));

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number value or float vector`
*>
macro exp2(x) => $$exp2(values::promote_int(x));

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number value or float vector`
*>
macro exp10(x)
{
	$if env::POSIX:
		return $$exp10(values::promote_int(x));
	$else
		var val = values::promote_int(x);
		return $$exp2(val * ($typeof(val))0x1.a934f0979a371p+1);
	$endif
}

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number value or float vector`
*>
macro floor(x) => $$floor(values::promote_int(x));

<*
 Fused multiply-add. Also consider mad if looser guarantees on fusing the operation is desired.

 @require values::@is_promotable_to_floatlike(a) : `The input must be a number or float vector`
 @require values::@is_promotable_to_floatlike(b) : `The input must be a number or float vector`
 @require values::@is_promotable_to_floatlike(c) : `The input must be a number or float vector`
 @require values::@is_same_vector_type(a, b) : `The input types must be equal`
 @require values::@is_same_vector_type(a, c) : `The input types must match`
*>
macro fma(a, b, c) => $$fma(a, b, c);


<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number or a float vector`
 @require values::@is_promotable_to_floatlike(y) : `The input must be a number or a float vector`
 @require values::@is_same_vector_type(x, y) : `The input types must match`
*>
macro hypot(x, y) => sqrt(sqr(x) + sqr(y));

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number or a float vector`
*>
macro ln(x) => $$log(values::promote_int(x));

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number or a float vector`
 @require values::@is_promotable_to_floatlike(base) : `The base must be a number or a float vector`
*>
macro log(x, base)
{
	return $$log(values::promote_int_same(x, base)) / $$log(values::promote_int_same(base, x));
}

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number or a float vector`
*>
macro log2(x) => $$log2(values::promote_int(x));

<*
 @require values::@is_int($x) : `The input value must be an integer.`
 @require $x >= 0 : `The input value must be a positive integer.`
 @return `A floored base-2 log of an input integer value.`
*>
macro @intlog2($x)
{
	$if $x <= 1:
		return 0;
	$endif

	$typeof($x) $z = 0;
	$for var $y = $x; $y > 0; $y >>= 1, ++$z: $endfor

	return $z - 1;
}

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number or a float vector`
*>
macro log10(x) => $$log10(values::promote_int(x));

<*
 @require types::is_numerical($typeof(x)) : `The input must be a floating point value or float vector`
 @require @typematch(x, y) : `The input types must be equal`
*>
macro max(x, y, ...)
{
	$if $vaarg.len == 0:
		return $$max(x, y);
	$else
		var m = $$max(x, y);
		$for var $i = 0; $i < $vaarg.len; $i++:
			m = $$max(m, $vaarg[$i]);
		$endfor
		return m;
	$endif
}

<*
 @require types::is_numerical($typeof(x)) : `The input must be a numerical value or numerical vector`
 @require types::is_same($typeof(x), $typeof(y)) : `The input types must be equal`
*>
macro min(x, y, ...)
{
	$if $vaarg.len == 0:
		return $$min(x, y);
	$else
		var m = $$min(x, y);
		$for var $i = 0; $i < $vaarg.len; $i++:
			m = $$min(m, $vaarg[$i]);
		$endfor
		return m;
	$endif
}

<*
 Multiply and add - possibly fused

 @require types::@is_float(a) : `The input must be a floating point value`
 @require types::@has_same(a, b, c) : `The input types must be equal`
*>
macro mad(a, b, c) => $$fmuladd(a, b, c);

<*
 @require values::@is_floatlike(x) : `The input must be a floating point value or float vector`
*>
macro nearbyint(x) => $$nearbyint(x);

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number or a float vector`
 @require $defined($typeof(values::promote_int(x)) v = exp) || values::@is_int(exp) : `The input must be an integer, castable to the type of x`
*>
macro pow(x, exp)
{
	$if types::is_floatlike($typeof(exp)):
		return $$pow(values::promote_int_same(x, exp), ($typeof(values::promote_int_same(x, exp)))exp);
	$else
		return $$pow_int(values::promote_int(x), (int)exp);
	$endif
}

<*
 @require values::@is_promotable_to_float(x) : `The input must be integer or floating type`
*>
macro frexp(x, int* e)
{
	$switch $typeof(x):
	$case float:
	$case float16:
		return _frexpf((float)x, e);
	$default:
		return _frexp((double)x, e);
	$endswitch
}

<*
 @require values::@is_promotable_to_float(x) : `The input must be integer or floating type`
*>
macro int signbit(x)
{
	$switch $typeof(x):
	$case float:
	$case float16:
		return (int)(bitcast((float)x, uint) >> 31);
	$default:
		return (int)(bitcast((double)x, ulong) >> 63);
	$endswitch
}

<*
 @require values::@is_floatlike(x) : `The input must be a number or a float vector`
*>
macro rint(x) => $$rint(x);

<*
 @require values::@is_floatlike(x) : `The input must be a floating point value or float vector`
*>
macro round(x) => $$round(x);


<*
 @require values::@is_floatlike(x) : `The input must be a floating point value or float vector`
*>
macro round_to_decimals(x, int decimal_places)
{
  var div = $$pow_int(($typeof(x))10, decimal_places);
  return $$round(div * x) / div;
}

<*
 @require values::@is_floatlike(x) : `The input must be a floating point value or float vector`
*>
macro roundeven(x) => $$roundeven(x);

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number or a float vector`
*>
macro sec(x) => 1 / cos(x);

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number or a float vector`
*>
macro sech(x) => 2 / (exp(x) + exp(-x));

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number or a float vector`
*>
macro sin(x) => $$sin(values::promote_int(x));

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number or a float vector`
*>
macro sinh(x) => $$sinh(values::promote_int(x));

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number or a float vector`
*>
macro sqr(x) => values::promote_int(x) * values::promote_int(x);

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number or a float vector`
*>
macro sqrt(x) => $$sqrt(values::promote_int(x));

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number or a float vector`
*>
macro tan(x) => $$tan(values::promote_int(x));

<*
 @require values::@is_promotable_to_floatlike(x) : `The input must be a number or a float vector`
*>
macro tanh(x) => $$tanh(values::promote_int(x));

<*
 @require values::@is_promotable_to_float(x) : `The input must be a float`
*>
macro bool is_finite(x)
{
	$switch $typeof(x):
		$case float:
		$case float16:
			return bitcast((float)x, uint) & 0x7fffffff < 0x7f800000;
		$default:
			return bitcast((double)x, ulong) & (~0UL >> 1) < 0x7ffUL << 52;
	$endswitch
}

<*
 @require values::@is_promotable_to_float(x) : `The input must be a float`
*>
macro bool is_nan(x)
{
	$switch $typeof(x):
	$case float:
	$case float16:
		return bitcast((float)x, uint) & 0x7fffffff > 0x7f800000;
	$default:
		return bitcast((double)x, ulong) & (~0UL >> 1) > 0x7ffUL << 52;
	$endswitch
}

<*
 @require values::@is_promotable_to_float(x) : `The input must be a float`
*>
macro bool is_inf(x)
{
	$switch $typeof(x):
	$case float:
	$case float16:
		return bitcast((float)x, uint) & 0x7fffffff == 0x7f800000;
	$default:
		return bitcast((double)x, ulong) & (~0UL >> 1) == 0x7ffUL << 52;
	$endswitch
}

<*
 @require values::@is_floatlike(x) : `The input must be a floating point value or float vector`
*>
macro trunc(x) => $$trunc(x);

<*
 Use a mask to select values from either "then" or "else" vectors.

 @param mask : "The mask to use for the select, 'true' will pick the then_value, 'false' the else_value"
 @param then_value : "The vector to get elements from where the mask is 'true'"
 @param else_value : "The vector to get elements from where the mask is 'false'"
 @require values::@is_vector(then_value) && values::@is_vector(else_value) : "'Then' and 'else' must be vectors."
 @require @typematch(then_value, else_value) : "'Then' and 'else' vectors must be of the same type."
 @require then_value.len == mask.len : "Mask and selected vectors must be of the same width."

 @return "a vector of the same type as then/else"
*>
macro select(bool[<*>] mask, then_value, else_value)
{
	return $$select(mask, then_value, else_value);
}

macro float float.ceil(float x) => $$ceil(x);
macro float float.clamp(float x, float lower, float upper) => $$max(lower, $$min(x, upper));
macro float float.copysign(float mag, float sgn) => $$copysign(mag, sgn);
macro float float.floor(float x) => $$floor(x);
macro float float.fma(float a, float b, float c) => $$fma(a, b, c);
macro float float.mad(float a, float b, float c) => $$fmuladd(a, b, c);
macro float float.nearbyint(float x) => $$nearbyint(x);
macro float float.pow(float x, exp) => pow(x, exp);
macro float float.rint(float x) => $$rint(x);
macro float float.round(float x) => $$round(x);
macro float float.roundeven(float x) => $$roundeven(x);
macro float float.trunc(float x) => $$trunc(x);
macro float float.to_degrees(float radians) => radians * (float)(180 / PI);
macro float float.to_radians(float degrees) => degrees * (float)(PI / 180);

macro double double.ceil(double x) => $$ceil(x);
macro double double.clamp(double x, double lower, double upper) => $$max(lower, $$min(x, upper));
macro double double.copysign(double mag, double sgn) => $$copysign(mag, sgn);
macro double double.floor(double x) => $$floor(x);
macro double double.fma(double a, double b, double c) => $$fma(a, b, c);
macro double double.mad(double a, double b, double c) => $$fmuladd(a, b, c);
macro double double.nearbyint(double x) => $$nearbyint(x);
macro double double.pow(double x, exp) => pow(x, exp);
macro double double.rint(double x) => $$rint(x);
macro double double.round(double x) => $$round(x);
macro double double.roundeven(double x) => $$roundeven(x);
macro double double.trunc(double x) => $$trunc(x);
macro double double.to_degrees(double radians) => radians * (180 / PI);
macro double double.to_radians(double degrees) => degrees * (PI / 180);

macro char char.sat_add(char x, char y) => $$sat_add(x, y);
macro char char.sat_sub(char x, char y) => $$sat_sub(x, y);
macro char char.sat_mul(char x, char y) => $$sat_mul(x, y);
macro char char.sat_shl(char x, char y) => $$sat_shl(x, y);
macro char? char.overflow_add(char x, char y) => overflow_add_helper(x, y);
macro char? char.overflow_sub(char x, char y) => overflow_sub_helper(x, y);
macro char? char.overflow_mul(char x, char y) => overflow_mul_helper(x, y);


macro ichar ichar.sat_add(ichar x, ichar y) => $$sat_add(x, y);
macro ichar ichar.sat_sub(ichar x, ichar y) => $$sat_sub(x, y);
macro ichar ichar.sat_mul(ichar x, ichar y) => $$sat_mul(x, y);
macro ichar ichar.sat_shl(ichar x, ichar y) => $$sat_shl(x, y);
macro ichar? ichar.overflow_add(ichar x, ichar y) => overflow_add_helper(x, y);
macro ichar? ichar.overflow_sub(ichar x, ichar y) => overflow_sub_helper(x, y);
macro ichar? ichar.overflow_mul(ichar x, ichar y) => overflow_mul_helper(x, y);

macro ushort ushort.sat_add(ushort x, ushort y) => $$sat_add(x, y);
macro ushort ushort.sat_sub(ushort x, ushort y) => $$sat_sub(x, y);
macro ushort ushort.sat_mul(ushort x, ushort y) => $$sat_mul(x, y);
macro ushort ushort.sat_shl(ushort x, ushort y) => $$sat_shl(x, y);
macro ushort? ushort.overflow_add(ushort x, ushort y) => overflow_add_helper(x, y);
macro ushort? ushort.overflow_sub(ushort x, ushort y) => overflow_sub_helper(x, y);
macro ushort? ushort.overflow_mul(ushort x, ushort y) => overflow_mul_helper(x, y);

macro short short.sat_add(short x, short y) => $$sat_add(x, y);
macro short short.sat_sub(short x, short y) => $$sat_sub(x, y);
macro short short.sat_mul(short x, short y) => $$sat_mul(x, y);
macro short short.sat_shl(short x, short y) => $$sat_shl(x, y);
macro short? short.overflow_add(short x, short y) => overflow_add_helper(x, y);
macro short? short.overflow_sub(short x, short y) => overflow_sub_helper(x, y);
macro short? short.overflow_mul(short x, short y) => overflow_mul_helper(x, y);

macro uint uint.sat_add(uint x, uint y) => $$sat_add(x, y);
macro uint uint.sat_sub(uint x, uint y) => $$sat_sub(x, y);
macro uint uint.sat_mul(uint x, uint y) => $$sat_mul(x, y);
macro uint uint.sat_shl(uint x, uint y) => $$sat_shl(x, y);
macro uint? uint.overflow_add(uint x, uint y) => overflow_add_helper(x, y);
macro uint? uint.overflow_sub(uint x, uint y) => overflow_sub_helper(x, y);
macro uint? uint.overflow_mul(uint x, uint y) => overflow_mul_helper(x, y);

macro int int.sat_add(int x, int y) => $$sat_add(x, y);
macro int int.sat_sub(int x, int y) => $$sat_sub(x, y);
macro int int.sat_mul(int x, int y) => $$sat_mul(x, y);
macro int int.sat_shl(int x, int y) => $$sat_shl(x, y);
macro int? int.overflow_add(int x, int y) => overflow_add_helper(x, y);
macro int? int.overflow_sub(int x, int y) => overflow_sub_helper(x, y);
macro int? int.overflow_mul(int x, int y) => overflow_mul_helper(x, y);

macro ulong ulong.sat_add(ulong x, ulong y) => $$sat_add(x, y);
macro ulong ulong.sat_sub(ulong x, ulong y) => $$sat_sub(x, y);
macro ulong ulong.sat_mul(ulong x, ulong y) => $$sat_mul(x, y);
macro ulong ulong.sat_shl(ulong x, ulong y) => $$sat_shl(x, y);
macro ulong? ulong.overflow_add(ulong x, ulong y) => overflow_add_helper(x, y);
macro ulong? ulong.overflow_sub(ulong x, ulong y) => overflow_sub_helper(x, y);
macro ulong? ulong.overflow_mul(ulong x, ulong y) => overflow_mul_helper(x, y);

macro long long.sat_add(long x, long y) => $$sat_add(x, y);
macro long long.sat_sub(long x, long y) => $$sat_sub(x, y);
macro long long.sat_mul(long x, long y) => $$sat_mul(x, y);
macro long long.sat_shl(long x, long y) => $$sat_shl(x, y);
macro long? long.overflow_add(long x, long y) => overflow_add_helper(x, y);
macro long? long.overflow_sub(long x, long y) => overflow_sub_helper(x, y);
macro long? long.overflow_mul(long x, long y) => overflow_mul_helper(x, y);

macro uint128 uint128.sat_add(uint128 x, uint128 y) => $$sat_add(x, y);
macro uint128 uint128.sat_sub(uint128 x, uint128 y) => $$sat_sub(x, y);
macro uint128 uint128.sat_mul(uint128 x, uint128 y) => $$sat_mul(x, y);
macro uint128 uint128.sat_shl(uint128 x, uint128 y) => $$sat_shl(x, y);
macro uint128? uint128.overflow_add(uint128 x, uint128 y) => overflow_add_helper(x, y);
macro uint128? uint128.overflow_sub(uint128 x, uint128 y) => overflow_sub_helper(x, y);
macro uint128? uint128.overflow_mul(uint128 x, uint128 y) => overflow_mul_helper(x, y);

macro int128 int128.sat_add(int128 x, int128 y) => $$sat_add(x, y);
macro int128 int128.sat_sub(int128 x, int128 y) => $$sat_sub(x, y);
macro int128 int128.sat_mul(int128 x, int128 y) => $$sat_mul(x, y);
macro int128 int128.sat_shl(int128 x, int128 y) => $$sat_shl(x, y);
macro int128? int128.overflow_add(int128 x, int128 y) => overflow_add_helper(x, y);
macro int128? int128.overflow_sub(int128 x, int128 y) => overflow_sub_helper(x, y);
macro int128? int128.overflow_mul(int128 x, int128 y) => overflow_mul_helper(x, y);
<*
 @require values::@is_int(x) : `The input must be an integer`
*>
macro bool is_odd(x) => (bool)(x & ($typeof(x))1);

<*
 @require values::@is_int(x) : `The input must be an integer`
*>
macro bool is_even(x) => !is_odd(x);

macro bool char.is_even(char x) => is_even(x);
macro bool char.is_odd(char x) => is_odd(x);

macro bool ichar.is_even(ichar x) => is_even(x);
macro bool ichar.is_odd(ichar x) => is_odd(x);

macro bool ushort.is_even(ushort x) => is_even(x);
macro bool ushort.is_odd(ushort x) => is_odd(x);

macro bool short.is_even(short x) => is_even(x);
macro bool short.is_odd(short x) => is_odd(x);

macro bool uint.is_even(uint x) => is_even(x);
macro bool uint.is_odd(uint x) => is_odd(x);

macro bool int.is_even(int x) => is_even(x);
macro bool int.is_odd(int x) => is_odd(x);

macro bool ulong.is_even(ulong x) => is_even(x);
macro bool ulong.is_odd(ulong x) => is_odd(x);

macro bool long.is_even(long x) => is_even(x);
macro bool long.is_odd(long x) => is_odd(x);

macro bool uint128.is_even(uint128 x) => is_even(x);
macro bool uint128.is_odd(uint128 x) => is_odd(x);

macro bool int128.is_even(int128 x) => is_even(x);
macro bool int128.is_odd(int128 x) => is_odd(x);

<*
 @require types::is_underlying_int($typeof(x)) : `is_power_of_2 may only be used on integer types`
*>
macro bool is_power_of_2(x)
{
	return x != 0 && (x & (x - 1)) == 0;
}

<*
 Compile time version.
 @require types::is_underlying_int($typeof($x)) : `is_power_of_2 may only be used on integer types`
*>
macro bool @is_power_of_2($x)
{
	return $x != 0 && ($x & ($x - 1)) == 0;
}

<*
 Returns the next power of two that is greater than or equal to x.

 @require types::is_int($typeof(x)) : "Input must be an integer type"
 @require x >= 0 : "Input must be non-negative"
*>
macro next_power_of_2(x)
{
	var $Type = $typeof(x);
	if (x <= 1) return ($Type)1;
	if (x == 2) return ($Type)2;

	$Type v = x - ($Type)1;

	v |= v >> 1;
	v |= v >> 2;
	v |= v >> 4;

	$if (@sizeof(x) >= 2):  v |= v >> 8;  $endif;
	$if (@sizeof(x) >= 4):  v |= v >> 16; $endif;
	$if (@sizeof(x) >= 8):  v |= v >> 32; $endif;
	$if (@sizeof(x) >= 16): v |= v >> 64; $endif;

	return v + ($Type)1;
}

<*
 Compile time version.
 Returns the next power of two that is greater than or equal to x.

 @require types::is_int($typeof($x)) : "Input must be an integer type"
 @require $x >= 0 : "Input must be non-negative"
*>
macro @next_power_of_2($x)
{
	$if $x <= 1: return 1; $endif
	$if $x == 2: return 2; $endif

	$typeof($x) $v = $x - 1;

	$v |= $v >> 1;
	$v |= $v >> 2;
	$v |= $v >> 4;

	$if (@sizeof($x) >= 2):  $v |= $v >> 8;  $endif;
	$if (@sizeof($x) >= 4):  $v |= $v >> 16; $endif;
	$if (@sizeof($x) >= 8):  $v |= $v >> 32; $endif;
	$if (@sizeof($x) >= 16): $v |= $v >> 64; $endif;

	return $v + 1;
}


macro uint double.high_word(double d) => (uint)(bitcast(d, ulong) >> 32);
macro uint double.low_word(double d) => (uint)bitcast(d, ulong);
macro uint float.word(float d) => bitcast(d, uint);

macro void double.set_high_word(double* d, uint u)
{
	ulong rep = bitcast(*d, ulong);
	rep = ((ulong)u << 32) | (rep & 0xffffffff);
	*d = bitcast(rep, double);
}

macro void double.set_low_word(double* d, uint u)
{
	ulong rep = bitcast(*d, ulong);
	rep = (rep & 0xffffffff00000000) | (ulong)u;
	*d = bitcast(rep, double);
}

macro void float.set_word(float* f, uint u) => *f = bitcast(u, float);

macro double scalbn(double x, int n) => _scalbn(x, n);

extern fn double _atan(double x) @MathLibc("atan");
extern fn float _atanf(float x) @MathLibc("atanf");
extern fn double _atan2(double, double) @MathLibc("atan2");
extern fn float _atan2f(float, float) @MathLibc("atan2f");

extern fn void _sincos(double, double*, double*) @MathLibc("__sincos") @if(env::DARWIN);
extern fn void _sincosf(float, float*, float*) @MathLibc("__sincosf") @if(env::DARWIN);

extern fn void _sincos(double, double*, double*) @MathLibc("sincos") @if(!env::DARWIN && !env::WIN32);
extern fn void _sincosf(float, float*, float*) @MathLibc("sincosf") @if(!env::DARWIN && !env::WIN32);

fn void _sincos(double a, double* s, double* c) @cname("sincos") @if(env::WIN32) { *s = sin(a); *c = cos(a); }
fn void _sincosf(float a, float* s, float* c) @cname("sincosf") @if(env::WIN32)  { *s = sin(a); *c = cos(a); }

extern fn double _tan(double x) @MathLibc("tan");
extern fn float _tanf(float x) @MathLibc("tanf");
extern fn double _scalbn(double x, int n) @MathLibc("scalbn");
extern fn double _acos(double x) @MathLibc("acos");
extern fn double _asin(double x) @MathLibc("asin");
extern fn double _acosh(double x) @MathLibc("acosh");
extern fn double _asinh(double x) @MathLibc("asinh");
extern fn double _atanh(double x) @MathLibc("atanh");
extern fn float _acosf(float x) @MathLibc("acosf");
extern fn float _asinf(float x) @MathLibc("asinf");
extern fn float _acoshf(float x) @MathLibc("acoshf");
extern fn float _asinhf(float x) @MathLibc("asinhf");
extern fn float _atanhf(float x) @MathLibc("atanhf");

extern fn double _erf(double x) @MathLibc("erf");
extern fn double _lgamma(double x) @MathLibc("lgamma");
extern fn double _tgamma(double x) @MathLibc("tgamma");

extern fn float _erff(float x) @MathLibc("erf");
extern fn float _lgammaf(float x) @MathLibc("lgammaf");
extern fn float _tgammaf(float x) @MathLibc("tgammaf");


fn double _frexp(double x, int* e)
{
	ulong i = bitcast(x, ulong);
	int ee = (int)((i >> 52) & 0x7ff);
	switch
	{
		case !ee:
			if (!x)
			{
				*e = 0;
				return x;
			}
			x = _frexp(x * 0x1p64, e);
			*e -= 64;
			return x;
		case ee == 0x7ff:
			return x;
		default:
			*e = ee - 0x3fe;
			i &= 0x800fffffffffffffUL;
			i |= 0x3fe0000000000000UL;
			return bitcast(i, double);
	}
}

fn float _frexpf(float x, int* e)
{
	uint i = bitcast(x, uint);
	int ee = (int)(i >> 23) & 0xff;

	switch
	{
		case !ee:
			if (!x)
			{
				*e = 0;
				return x;
			}
			x = _frexpf(x * 0x1p64, e);
			*e -= 64;
			return x;
		case ee == 0xff:
			return x;
		default:
			*e = ee - 0x7e;
			i &= 0x807fffffU;
			i |= 0x3f000000U;
			return bitcast(i, float);
	}
}

macro overflow_add_helper(x, y) @local
{
	$typeof(x) res @noinit;
	if ($$overflow_add(x, y, &res)) return OVERFLOW~;
	return res;
}

macro overflow_sub_helper(x, y) @local
{
	$typeof(x) res @noinit;
	if ($$overflow_sub(x, y, &res)) return OVERFLOW~;
	return res;
}

macro overflow_mul_helper(x, y) @local
{
	$typeof(x) res @noinit;
	if ($$overflow_mul(x, y, &res)) return OVERFLOW~;
	return res;
}

<*
 @param [&out] out : "Where the result of the addition is stored"
 @return "Whether the addition resulted in an integer overflow"
 @require @typematch(a, b) : "a and b must be the same type"
 @require values::@is_flat_intlike(a) &&& values::@is_flat_intlike(b) : "a and b must both be integer or integer vector based"
 @require $defined(*out) &&& @typematch(*out, a) : "out must be a pointer of the same type as a and b"
*>
macro bool overflow_add(a, b, out) => $$overflow_add(a, b, out);

<*
 @param [&out] out : "Where the result of the subtraction is stored"
 @return "Whether the subtraction resulted in an integer overflow"
 @require @typematch(a, b) : "a and b must be the same type"
 @require values::@is_flat_intlike(a) &&& values::@is_flat_intlike(b) : "a and b must both be integer or integer vector based"
 @require $defined(*out) &&& @typematch(*out, a) : "out must be a pointer of the same type as a and b"
*>
macro bool overflow_sub(a, b, out) => $$overflow_sub(a, b, out);

<*
 @param [&out] out : "Where the result of the multiplication is stored"
 @return "Whether the multiplication resulted in an integer overflow"
 @require @typematch(a, b) : "a and b must be the same type"
 @require values::@is_flat_intlike(a) &&& values::@is_flat_intlike(b) : "a and b must both be integer or integer vector based"
 @require $defined(*out) &&& @typematch(*out, a) : "out must be a pointer of the same type as a and b"
*>
macro overflow_mul(a, b, out) => $$overflow_mul(a, b, out);

<*
 @require types::is_vector($Type) || ($Type::kind == ARRAY &&& types::is_numerical($typefrom($Type::inner)))
*>
macro iota($Type)
{
	$Type $val = {};
	$for var $i = 0; $i < $Type::len; $i++:
		$val[$i] = $i;
	$endfor
	return $val;
}

macro char char.muldiv(self, char mul, char div) => mul_div_helper(self, mul, div);
macro ichar ichar.muldiv(self, ichar mul, ichar div) => mul_div_helper(self, mul, div);
macro short short.muldiv(self, short mul, short div) => mul_div_helper(self, mul, div);
macro ushort ushort.muldiv(self, ushort mul, ushort div) => mul_div_helper(self, mul, div);
macro int int.muldiv(self, int mul, int div) => mul_div_helper(self, mul, div);
macro uint uint.muldiv(self, uint mul, uint div) => mul_div_helper(self, mul, div);
macro long long.muldiv(self, long mul, long div) => mul_div_helper(self, mul, div);
macro ulong ulong.muldiv(self, ulong mul, ulong div) => mul_div_helper(self, mul, div);

<*
 Calculate the least common multiple for the provided arguments.

 @require $vaarg.len >= 2 : "At least two arguments are required."
*>
macro lcm(...)
{
	$typeof($vaarg[0]) result = $vaarg[0];
	$for var $i = 1; $i < $vaarg.len; $i++:
		$if $defined(result.lcm):
			result = result.lcm($vaarg[$i]);
		$else
			result = (abs($vaarg[$i]) * abs(result)) / (_gcd($vaarg[$i], result));
		$endif
	$endfor
	return result;
}

<*
 Calculate the greatest common divisor for the provided arguments.

 @require $vaarg.len >= 2 : "At least two arguments are required."
*>
macro gcd(...)
{
	$typeof($vaarg[0]) result = $vaarg[0];
	$for var $i = 1; $i < $vaarg.len; $i++:
		$if $defined(result.gcd):
			result = result.gcd($vaarg[$i]);
		$else
			result = _gcd($vaarg[$i], result);
		$endif
	$endfor
	return result;
}

module std::math @private;

<*
 @require types::is_int($typeof(a)) : `The input must be an integer`
 @require types::is_int($typeof(b)) : `The input must be an integer`
*>
macro _gcd(a, b)
{
	if (a == 0) return b;
	if (b == 0) return a;

	var $Type = $typeof(a);
	$Type r, aa, ab;
	aa = abs(a);
	ab = abs(b);
	while (ab != 0)
	{
		r = aa % ab;
		aa = ab;
		ab = r;
	}
	return aa;
}

macro lerp(x, y, amount) => x + (y - x) * amount;


macro mul_div_helper(val, mul, div)
{
	var $Type = $typeof(val);
	return ($Type)(($Type)mul * (val / ($Type)div) + ($Type)mul * (val % ($Type)div) / ($Type)div);
}
