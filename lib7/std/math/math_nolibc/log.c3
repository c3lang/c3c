module std::math::nolibc @if(env::NO_LIBC || $feature(C3_MATH));

const double LOG_LN2_HI = 6.93147180369123816490e-01;
const double LOG_LN2_LO = 1.90821492927058770002e-10;
const double LOG_L1 = 6.666666666666735130e-01;
const double LOG_L2 = 3.999999999940941908e-01;
const double LOG_L3 = 2.857142874366239149e-01;
const double LOG_L4 = 2.222219843214978396e-01;
const double LOG_L5 = 1.818357216161805012e-01;
const double LOG_L6 = 1.531383769920937332e-01;
/*--------------------------------------------*/
const float LOGF_LN2_HI = 6.9313812256e-01f;
const float LOGF_LN2_LO = 9.0580006145e-06f;
const float LOGF_L1 = 6.6666662693e-01f;
const float LOGF_L2 = 4.0000972152e-01f;
const float LOGF_L3 = 2.8498786688e-01f;
const float LOGF_L4 = 2.4279078841e-01f;

const double SQRT2 = 1.41421356237309504880;
const float SQRT2F = 1.41421356237309504880f;

fn double log(double x) @extern("log")
{
    if (x != x) return x;
    if (x < 0.0) return double.nan;
    if (x == 0.0) return -double.inf;
    if (x == double.inf) return double.inf;

    int k;
    double f = frexp(x, &k);
    if (f < SQRT2 * 0.5)
    {
        f *= 2.0;
        k--;
    }

    // polynomial approximation of log(1 + f), with f in [0, sqrt(2) - 1]
    f -= 1.0;
    double s = f / (2.0 + f);
    double z = s * s;
    double w = z * z;

    // even-part polynomial terms (t1) and odd-part polynomial terms (t2)
    double t1 = w * (LOG_L1 + w * (LOG_L3 + w * LOG_L5));
    double t2 = z * (LOG_L2 + w * (LOG_L4 + w * LOG_L6));
    double r = t1 + t2;

    double hfsq = 0.5 * f * f;

    return k * LOG_LN2_HI - ((hfsq - (s * (hfsq + r) + k * LOG_LN2_LO)) - f);
}

fn float logf(float x) @extern("logf")
{
    if (x != x) return x;
    if (x < 0.0f) return float.nan;
    if (x == 0.0f) return -float.inf;
    if (x == float.inf) return float.inf;

    int k;
    float f = frexpf(x, &k);
    if (f < SQRT2F * 0.5f)
    {
        f *= 2.0f;
        k--;
    }

    // polynomial approximation for log(1 + f)
    f -= 1.0f;
    float s = f / (2.0f + f);
    float z = s * s;
    float w = z * z;

    /*
        logf uses fewer terms in its polynomial approximation
        compared to the double precision version because
        single-precision floating point doesn't benefit from the additional terms.
        LOGF_L1, ... ,LOGF_L4 provide sufficient accuracy for 32-bit float calculations.
    */
    float t1 = w * (LOGF_L1 + w * LOGF_L3);
    float t2 = z * (LOGF_L2 + w * LOGF_L4);
    float r = t1 + t2;

    float hfsq = 0.5f * f * f;
    return k * LOGF_LN2_HI - ((hfsq - (s * (hfsq + r) + k * LOGF_LN2_LO)) - f);
}