module std::math::nolibc @if(env::NO_LIBC || $feature(C3_MATH));

const double EXP_LN2_HI = 6.93147180369123816490e-01;
const double EXP_LN2_LO = 1.90821492927058770002e-10;
const double EXP_INV_LN2 = 1.44269504088896338700e+00;
const double EXP_P1 = 1.66666666666666019037e-01;
const double EXP_P2 = -2.77777777770155933842e-03;
const double EXP_P3 = 6.61375632143793436117e-05;
const double EXP_P4 = -1.65339022054652515390e-06;
const double EXP_P5 = 4.13813679705723846039e-08;
/*--------------------------------------------*/
const float EXPF_LN2_HI = 6.9314575195e-01f;
const float EXPF_LN2_LO = 1.4286067653e-06f;
const float EXPF_INV_LN2 = 1.4426950216e+00f;
const float EXPF_P1 = 1.6666667163e-01f;
const float EXPF_P2 = -2.7777778450e-03f;
const float EXPF_P3 = 6.6137559770e-05f;
const float EXPF_P4 = -1.6533901999e-06f;

fn double exp(double x) @extern("exp")
{
    if (x != x) return x;
    if (x == double.inf) return double.inf;
    if (x == -double.inf) return 0.0; // IEEE 754 spec
    // Overflow threshold for exp (approx +709.78 for double)
    if (x > 709.782712893384) return double.inf;
    // Underflow threshold for exp (approx -745.13 for double)
    if (x < -745.133219101941) return 0.0;

    double px = x * EXP_INV_LN2;
    double k = _floor(px + 0.5);
    double r = x - k * EXP_LN2_HI - k * EXP_LN2_LO;

    double r2 = r * r;
    double p = r2 * (EXP_P1 + r2 * (EXP_P2 + r2 * (EXP_P3 + r2 * (EXP_P4 + r2 * EXP_P5))));
    double exp_r = 1.0 + r + r * p;

    return ldexp(exp_r, (int)k);
}

fn float expf(float x) @extern("expf")
{
    if (x != x) return x;
    if (x == float.inf) return float.inf;
    if (x == -float.inf) return 0.0f; // IEEE 754 spec
    // Overflow threshold (approx +88.72 for float)
    if (x > 88.7228f) return float.inf;
    // Underflow threshold (approx -103.97 for float)
    if (x < -103.972084f) return 0.0f;

    float px = x * EXPF_INV_LN2;
    float k = _floorf(px + 0.5f);
    float r = x - k * EXPF_LN2_HI - k * EXPF_LN2_LO;

    float r2 = r * r;
    float p = r2 * (EXPF_P1 + r2 * (EXPF_P2 + r2 * (EXPF_P3 + r2 * EXPF_P4)));
    float exp_r = 1.0f + r + r * p;

    return ldexpf(exp_r, (int)k);
}
