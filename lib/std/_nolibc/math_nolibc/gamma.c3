module std::math::nolibc @if(env::NO_LIBC || $feature(C3_MATH));

// Constants
const double SQRT_PI = 1.7724538509055160272981674833411451;
const double LN_SQRT_2PI = 0.9189385332046727417803297364056176;

<*
 Natural logarithm of the gamma function using the Lanczos approximation.
 @require x > 0.0 : "x must be positive."
*>
fn double lgamma(double x)
{
	// Lanczos approximation coefficients (g = 7, n = 9)
	const double[*] LANCZOS_COEF = {
	    0.99999999999980993,
	    676.5203681218851,
	    -1259.1392167224028,
	    771.32342877765313,
	    -176.61502916214059,
	    12.507343278686905,
	    -0.13857109526572012,
	    9.9843695780195716e-6,
	    1.5056327351493116e-7
	};
	const double LANCZOS_G = 7.0;

	// For small x, use the reflection formula.
	if (x < 0.5)
	{
	    return math::ln(math::PI) - math::ln(math::sin(math::PI * x)) - lgamma(1.0 - x);
	}

	// Shift to use approximation around x >= 1.5
	x -= 1.0;

	double base = x + LANCZOS_G + 0.5;
	double sum = LANCZOS_COEF[0];

	for (int i = 1; i < 9; i++)
	{
	    sum += LANCZOS_COEF[i] / (x + (double)i);
	}

	return LN_SQRT_2PI + math::ln(sum) + (x + 0.5) * math::ln(base) - base;
}

<*
 Gamma function.
 Valid for x > 0 and some negative non-integer values.
*>
fn double tgamma(double x)
{
	// Handle special cases.
	if (x == 0.0) return double.inf;

	// Check for negative integers (poles).
	if (x < 0.0 && x == math::floor(x))
	{
	    return double.nan;
	}

	// For positive values, use exp(lgamma(x)).
	if (x > 0.0)
	{
	    return math::exp(lgamma(x));
	}

	// For negative non-integer values, use the reflection formula.
	return math::PI / (math::sin(math::PI * x) * tgamma(1.0 - x));
}
