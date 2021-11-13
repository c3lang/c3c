module std::math;

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
const DIV_1_SQRT2 = 0.707106781186547524400844362104849039; // 1 / sqrt(2)

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
/*
const QUAD_MAX = 1.18973149535723176508575932662800702e+4932;
const QUAD_MIN = 3.36210314311209350626267781732175260e-4932;
const QUAD_DENORM_MIN = 6.47517511943802511092443895822764655e-4966;
const QUAD_DIG = 33;
const QUAD_DEC_DIGITS = 36;
const QUAD_MANT_DIG = 113;
const QUAD_MAX_10_EXP = 4932;
const QUAD_MIN_10_EXP = -4931;
const QUAD_MAX_EXP = 16384;
const QUAD_MIN_EXP = -16481;
const QUAD_EPSILON = 1.92592994438723585305597794258492732e-34;
*/

fn double log10(double x) @inline
{
    return $$log10(x);
}

fn double log2(double x) @inline
{
    return $$log2(x);
}

fn double log(double x) @inline
{
    return $$log(x);
}

fn double cos(double x) @inline
{
    return $$cos(x);
}

fn double sin(double x) @inline
{
    return $$sin(x);
}

fn double exp(double x) @inline
{
    return $$exp(x);
}

fn double pow(double x, double y) @inline
{
    return $$pow(x, y);
}

fn double fabs(double x) @inline
{
    return $$fabs(x);
}

fn double trunc(double x) @inline
{
    return $$trunc(x);
}

fn double ceil(double x) @inline
{
    return $$ceil(x);
}
