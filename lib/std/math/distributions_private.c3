// Copyright (c) 2026 Koni Marti. All rights reserved.
// Use of this source code is governed by the MIT license.
module std::math::distributions @private;
import std::math::random;

struct ConvergenceControl
{
	usz max_iter;
	double epsilon;
}

const DEFAULT_CONV = (ConvergenceControl){ 600, 1e-12 };
const RELAXED_CONV = (ConvergenceControl){ 600, 1e-6 };

faultdef NOT_CONVERGED;

<*
 Compute binomial coefficient C(n, k)
*>
fn double binomial_coefficient(int n, int k)
{
	if (k < 0 || k > n) return 0.0;
	if (k == 0 || k == n) return 1.0;

	// Use symmetry.
	if (k > n - k) k = n - k;

	double result = 1.0;
	for (int i = 0; i < k; i++)
	{
		result *= (double)(n - i);
		result /= (double)(i + 1);
	}
	return result;
}

<*
 Natural logarithm of factorial.
*>
fn double ln_factorial(int n)
{
	if (n < 0) return double.nan;
	if (n <= 1) return 0.0;
	return math::lgamma((double)(n + 1));
}

<*
 Beta function B(a, b)
 @require a > 0
 @require b > 0
*>
fn double beta_function(double a, double b)
{
	return math::exp(math::lgamma(a) + math::lgamma(b) - math::lgamma(a + b));
}

<*
 Regularized incomplete beta function I_x(a, b)
 Based on https://github.com/codeplea/incbeta/blob/master/incbeta.c
*>
fn double incomplete_beta(double x, double a, double b, ConvergenceControl conv = DEFAULT_CONV)
{
	if (x < 0.0 || x > 1.0) return double.nan;
	if (x == 0.0) return 0.0;
	if (x == 1.0) return 1.0;

	if (x > (a + 1.0)/(a + b + 2.0)) return 1.0 - incomplete_beta(1.0 - x, b,a);

	const double TINY = 1e-30;

	// Find the first part before the continued fraction.
	double lbeta_ab = math::lgamma(a) + math::lgamma(b) - math::lgamma(a + b);
	double front = math::exp(math::ln(x) * a + math::ln(1.0 - x) * b - lbeta_ab) / a;

	// Use Lentz's algorithm to evaluate the continued fraction.
	double f = 1.0;
	double c = 1.0;
	double d = 0.0;

	usz m;
	for (usz i = 0; i <= conv.max_iter; ++i)
	{
		m = i/2;

		double numerator;

		if (i == 0)
		{
			numerator = 1.0;
		}
		else if (i % 2 == 0)
		{
			numerator = (m * (b - m) * x) / ((a + 2.0 * m - 1.0) * (a + 2.0 * m));
		}
		else
		{
			numerator = -((a + m) * (a + b + m) * x) / ((a + 2.0 * m) * (a + 2.0 * m + 1));
		}

		d = 1.0 + numerator * d;
		if (math::abs(d) < TINY) d = TINY;
		d = 1.0 / d;

		c = 1.0 + numerator / c;
		if (math::abs(c) < TINY) c = TINY;

		double cd = c*d;
		f *= cd;

		if (math::abs(1.0 - cd) < conv.epsilon) return front * (f - 1.0);
	}

	return double.nan; // Not convered.
}

<*
 Calculates the p-th quantile for a continuous distribution using bisection.
 @return? NOT_CONVERGED
*>
fn double? bisection_search(ContinuousDistribution dist, double low, double high, double p,
	ConvergenceControl conv = DEFAULT_CONV)
{
	// Expand upper bound if needed.
	while (dist.cdf(high) < p) high *= 2.0;

	// Bisection search.
	for (usz i = 0; i < conv.max_iter; i++)
	{
		double mid = (low + high) * 0.5;
		if (high - low < conv.epsilon) return mid;

		if (dist.cdf(mid) < p)
		{
			low = mid;
		}
		else
		{
			high = mid;
		}
	}
	return NOT_CONVERGED~;
}

<*
 Calculates the p-th quantile for a continuous distribution using Newton-Raphson.
 @return? NOT_CONVERGED
*>
fn double? newton_raphson(ContinuousDistribution dist, double x, double p,
	ConvergenceControl conv = DEFAULT_CONV)
{
	double delta, pdf;
	for (usz i = 0; i < conv.max_iter; i++)
	{
		pdf = dist.pdf(x);
		if (pdf < 1e-300) break;

		delta = (dist.cdf(x) - p) / pdf;
		x -= delta;

		if (math::abs(delta) < conv.epsilon) return x;
	}
	return NOT_CONVERGED~;
}

<*
 Calculates the p-th quantile for a continuous distribution.
 @require p >= 0.0 && p <= 1.0
 @require low < high
*>
fn double find_quantile(ContinuousDistribution dist, double low, double high, double p)
{
	double mid = bisection_search(dist, low, high, p, RELAXED_CONV) ?? (low + high) * 0.5;
	return newton_raphson(dist, mid, p, DEFAULT_CONV) ?? mid;
}

<*
 Generate a chi-squared random sample.
 @param k : "Degrees of freedom"
 @require k > 0.0
*>
fn double chi_squared_sample(double k, Random rand)
{
	// Sum of k squared standard normals.
	NormalDist std_normal = normal(0.0, 1.0);
	double sum = 0.0;

	int k_int = (int)k;
	for (int i = 0; i < k_int; i++)
	{
		double z = std_normal.random(rand);
		sum += z * z;
	}

	// Handle fractional degrees of freedom.
	double frac = k - (double)k_int;
	if (frac > 0.0)
	{
		double z = std_normal.random(rand);
		sum += frac * z * z;
	}

	return sum;
}

<*
 Inverse of the error function (math::erf).
 Based on Golang's math.Erfinv.
*>
fn double inverse_erf(double x)
{
	if (x < -1 || x > 1)
	{
		return double.nan;
	}
	else if (x == 1.0)
	{
		return double.inf;
	}
	else if (x == -1.0)
	{
		return -double.inf;
	}

	const double LN2 = 6.931471805599453094172321214581e-1;

	const double A0 = 1.1975323115670912564578e0;
	const double A1 = 4.7072688112383978012285e1;
	const double A2 = 6.9706266534389598238465e2;
	const double A3 = 4.8548868893843886794648e3;
	const double A4 = 1.6235862515167575384252e4;
	const double A5 = 2.3782041382114385731252e4;
	const double A6 = 1.1819493347062294404278e4;
	const double A7 = 8.8709406962545514830200e2;

	const double B0 = 1.0000000000000000000e0;
	const double B1 = 4.2313330701600911252e1;
	const double B2 = 6.8718700749205790830e2;
	const double B3 = 5.3941960214247511077e3;
	const double B4 = 2.1213794301586595867e4;
	const double B5 = 3.9307895800092710610e4;
	const double B6 = 2.8729085735721942674e4;
	const double B7 = 5.2264952788528545610e3;

	const double C0 = 1.42343711074968357734e0;
	const double C1 = 4.63033784615654529590e0;
	const double C2 = 5.76949722146069140550e0;
	const double C3 = 3.64784832476320460504e0;
	const double C4 = 1.27045825245236838258e0;
	const double C5 = 2.41780725177450611770e-1;
	const double C6 = 2.27238449892691845833e-2;
	const double C7 = 7.74545014278341407640e-4;

	const double D0 = 1.4142135623730950488016887e0;
	const double D1 = 2.9036514445419946173133295e0;
	const double D2 = 2.3707661626024532365971225e0;
	const double D3 = 9.7547832001787427186894837e-1;
	const double D4 = 2.0945065210512749128288442e-1;
	const double D5 = 2.1494160384252876777097297e-2;
	const double D6 = 7.7441459065157709165577218e-4;
	const double D7 = 1.4859850019840355905497876e-9;

	const double E0 = 6.65790464350110377720e0;
	const double E1 = 5.46378491116411436990e0;
	const double E2 = 1.78482653991729133580e0;
	const double E3 = 2.96560571828504891230e-1;
	const double E4 = 2.65321895265761230930e-2;
	const double E5 = 1.24266094738807843860e-3;
	const double E6 = 2.71155556874348757815e-5;
	const double E7 = 2.01033439929228813265e-7;

	const double F0 = 1.414213562373095048801689e0;
	const double F1 = 8.482908416595164588112026e-1;
	const double F2 = 1.936480946950659106176712e-1;
	const double F3 = 2.103693768272068968719679e-2;
	const double F4 = 1.112800997078859844711555e-3;
	const double F5 = 2.611088405080593625138020e-5;
	const double F6 = 2.010321207683943062279931e-7;
	const double F7 = 2.891024605872965461538222e-15;

	double sign = 1.0;
	if (x < 0)
	{
		x = -x;
		sign = -1.0;
	}

	double ans;
	if (x <= 0.85)
	{
		double r = 0.180625 - 0.25 * x *x;
		double z1 = ((((((A7 * r + A6) * r + A5) * r + A4) * r + A3) * r + A2) * r + A1) * r + A0;
		double z2 = ((((((B7 * r + B6) * r + B5) * r + B4) * r + B3) * r + B2) * r + B1) * r + B0;
		ans = (x * z1) / z2;
	}
	else
	{
		double z1, z2;
		double r = math::sqrt(LN2 - math::ln(1.0 - x));
		if (r <= 5.0)
		{
			r -= 1.6;
			z1 = ((((((C7 * r + C6) * r + C5) * r + C4) * r + C3) * r + C2) * r + C1) * r + C0;
			z2 = ((((((D7 * r + D6) * r + D5) * r + D4) * r + D3) * r + D2) * r + D1) * r + D0;
		}
		else
		{
			r -= 5.0;
			z1 = ((((((E7 * r + E6) * r + E5) * r + E4) * r + E3) * r + E2) * r + E1) * r + E0;
			z2 = ((((((F7 * r + F6) * r + F5) * r + F4) * r + F3) * r + F2) * r + F1) * r + F0;
		}
		ans = z1 / z2;
	}
	return sign * ans;
}

<*
 Regularized Lower incomplete gamma function.
 Returns nan when not converged.
 @param s : "Shape parameter"
 @param x : "Upper limit of integration"
 @require s > 0.0  : "s must be positive."
 @require x >= 0.0 : "x must be non-negative."
*>
fn double lower_incomplete_gamma(double s, double x)
{
	if (x == 0.0) return 0.0;
	if (x == double.inf) return 1.0;

	// Use series expansion for x < s+1
	if (x < s + 1.0)
	{
	    return incomplete_gamma_series_expansion(s, x) ?? double.nan;
	}
	else
	{
		return 1.0 - incomplete_gamma_continued_fraction(s, x) ?? double.nan;
	}
}

<*
 Lower incomplete gamma series expansion.
 @return? NOT_CONVERGED
*>
fn double? incomplete_gamma_series_expansion(double s, double x, ConvergenceControl conv = DEFAULT_CONV)
{
	double lnpre = s * math::ln(x) - x - math::lgamma(s + 1.0);
	if (lnpre < -708.0) return 0.0;   // result underflows to zero

	double term = 1.0;
	double sum  = 1.0;
	double ap   = s;

	for (int n = 1; n <= conv.max_iter; n++)
	{
		ap   += 1.0;
		term *= x / ap;
		sum  += term;
		if (math::abs(term) < math::abs(sum) * conv.epsilon)
		{
		    return math::exp(lnpre) * sum;
		}
	}

 	// Non-convergence.
	return NOT_CONVERGED~;
}

<*
 Modified Lentz continued fraction.
 @return? NOT_CONVERGED
*>
fn double? incomplete_gamma_continued_fraction(double s, double x, ConvergenceControl conv = DEFAULT_CONV)
{
	double lnpre = s * math::ln(x) - x - math::lgamma(s);

	// Lentz initialisation: fpmin guards against division by zero.
	double fpmin = 1e-300;

	double b = x + 1.0 - s;
	double c = 1.0 / fpmin;
	double d = 1.0 / b;
	double h = d;

	for (int i = 1; i <= conv.max_iter; i++)
	{
		double an = (double)i * (s - (double)i);
		b += 2.0;

		d = an * d + b;
		if (math::abs(d) < fpmin) d = fpmin;

		c = b + an / c;
		if (math::abs(c) < fpmin) c = fpmin;

		d = 1.0 / d;
		double delta = d * c;
		h *= delta;

		if (math::abs(delta - 1.0) < conv.epsilon)
		{
			return math::exp(lnpre) * h;
		}
	}

 	// Non-convergence.
	return NOT_CONVERGED~;
}
