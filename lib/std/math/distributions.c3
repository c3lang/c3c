// Copyright (c) 2026 Koni Marti. All rights reserved.
// Use of this source code is governed by the MIT license.
<*
 This module provides a comprehensive set of continuous and discrete
 probability distributions with support for PDF, CDF, inverse CDF,
 random sampling, mean, and variance calculations.
*>
module std::math::distributions;
import std::math::random;

// Distribution interface defining common statistical operations
interface Distribution
{
	<* Calculate the mean (expected value) of the distribution *>
	fn double mean();

	<* Calculate the variance of the distribution *>
	fn double variance();
}

interface ContinuousDistribution : Distribution
{
	 <* Probability density function (PDF) *>
	fn double pdf(double x);

	<* Cumulative distribution function (CDF) *>
	fn double cdf(double x);

	<* Inverse cumulative distribution function (quantile function) *>
	fn double quantile(double p);

	<* Generate a random sample from the distribution *>
	fn double random(Random rand);
}

interface DiscreteDistribution : Distribution
{
	<* Probability mass function (PMF) *>
	fn double pmf(int k);

	<* Cumulative distribution function (CDF) *>
	fn double cdf(int k);

	<* Inverse cumulative distribution function *>
	fn int quantile(double p);

	<* Generate a random sample from the distribution *>
	fn int random(Random rand);
}

<*
 Uniform distribution over [a, b]
 *>
struct UniformDist (ContinuousDistribution)
{
	double a;
	double b;
}

<*
 @require b > a : "Upper bound must be greater than lower bound."
*>
fn UniformDist uniform(double a, double b)
{
	return (UniformDist){ a, b };
}

fn double UniformDist.mean(&self) @dynamic
{
	return (self.a + self.b) / 2.0;
}

fn double UniformDist.variance(&self) @dynamic
{
	double range = self.b - self.a;
	return range * range / 12.0;
}

fn double UniformDist.pdf(&self, double x) @dynamic
{
	if (x < self.a || x > self.b) return 0;
	return 1.0 / (self.b - self.a);
}

fn double UniformDist.cdf(&self, double x) @dynamic
{
	if (x < self.a) return 0.0;
	if (x > self.b) return 1.0;
	return (x - self.a) / (self.b - self.a);
}

<*
 @require p >= 0.0 && p <= 1.0 : "Probability must be between 0 and 1."
*>
fn double UniformDist.quantile(&self, double p) @dynamic
{
	return self.a + p * (self.b - self.a);
}

fn double UniformDist.random(&self, Random rand) @dynamic
{
	return self.a + random::next_double(rand) * (self.b - self.a);
}

<*
 Normal (Gaussian) distribution
 *>
struct NormalDist (ContinuousDistribution)
{
	double mu;
	double sigma;
}

<*
 @require sigma > 0.0 : "Standard deviation must be positive"
*>
fn NormalDist normal(double mu = 0.0, double sigma = 1.0)
{
	return (NormalDist){ mu, sigma };
}

fn double NormalDist.mean(&self) @dynamic
{
	return self.mu;
}

fn double NormalDist.variance(&self) @dynamic
{
	return self.sigma * self.sigma;
}

fn double NormalDist.pdf(&self, double x) @dynamic
{
	double z = (x - self.mu) / self.sigma;
	return math::exp(-0.5 * z * z) / (self.sigma * math::sqrt(2.0 * math::PI));
}

fn double NormalDist.cdf(&self, double x) @dynamic
{
	double z = (x - self.mu) / self.sigma;
	return math::clamp(0.5 * (1.0 + math::erf(z / math::SQRT2)), 0.0, 1.0);
}

<*
 @require p >= 0.0 && p <= 1.0 : "Probability must be between 0 and 1."
*>
fn double NormalDist.quantile(&self, double p) @dynamic
{
	double z = inverse_erf(2.0 * p - 1.0) * math::SQRT2;
	return self.mu + self.sigma * z;
}

fn double NormalDist.random(&self, Random rand) @dynamic
{
	// Box-Muller transform.
	double u1 = random::next_double(rand);
	double u2 = random::next_double(rand);
	double z = math::sqrt(-2.0 * math::ln(u1)) * math::cos(2.0 * math::PI * u2);
	return self.mu + self.sigma * z;
}

<*
 Exponential distribution
 *>
struct ExponentialDist (ContinuousDistribution)
{
	double lambda;
}

<*
 @require lambda > 0.0 : "Rate parameter must be positive."
*>
fn ExponentialDist exponential(double lambda = 1.0)
{
	return (ExponentialDist){ lambda };
}

<*
 @require self.lambda > 0.0 : "Rate parameter must be positive."
*>
fn double ExponentialDist.mean(&self) @dynamic
{
	return 1.0 / self.lambda;
}

<*
 @require self.lambda > 0.0 : "Rate parameter must be positive."
*>
fn double ExponentialDist.variance(&self) @dynamic
{
	return 1.0 / (self.lambda * self.lambda);
}

fn double ExponentialDist.pdf(&self, double x) @dynamic
{
	if (x < 0.0) return 0.0;
	return self.lambda * math::exp(-self.lambda * x);
}

fn double ExponentialDist.cdf(&self, double x) @dynamic
{
	if (x < 0.0) return 0.0;
	return math::clamp(1.0 - math::exp(-self.lambda * x), 0.0, 1.0);
}

<*
 @require p >= 0.0 && p <= 1.0 : "Probability must be between 0 and 1."
 @require self.lambda > 0.0 : "Rate parameter must be positive."
*>
fn double ExponentialDist.quantile(&self, double p) @dynamic
{
	return -math::ln(1.0 - p) / self.lambda;
}

<*
 @require self.lambda > 0.0 : "Rate parameter must be positive."
*>
fn double ExponentialDist.random(&self, Random rand) @dynamic
{
	return -math::ln(1.0 - random::next_double(rand)) / self.lambda;
}

<*
 Student's t-distribution
 *>
struct TDist (ContinuousDistribution)
{
	double df;
}

<*
 @require df > 0.0 : "Degrees of freedom must be positive."
*>
fn TDist t_distribution(double df)
{
	return (TDist){ df };
}

fn double TDist.mean(&self) @dynamic
{
	if (self.df <= 1.0) return double.nan;
	return 0.0;
}

fn double TDist.variance(&self) @dynamic
{
	if (self.df <= 1.0) return double.nan;
	if (self.df <= 2.0) return double.inf;
	return self.df / (self.df - 2.0);
}

fn double TDist.pdf(&self, double x) @dynamic
{
	double v = self.df;
	double coef = math::tgamma((v + 1.0) / 2.0) /
		(math::sqrt(v * math::PI) * math::tgamma(v / 2.0));
	return coef * math::pow(1.0 + x * x / v, -(v + 1.0) / 2.0);
}

fn double TDist.cdf(&self, double x) @dynamic
{
	double v = self.df;
	if (x == 0.0) return 0.5;

	double t = v / (v + x * x);
	double a = v / 2.0;
	double b = 0.5;

	// Using regularized incomplete beta function.
	double beta_cdf = incomplete_beta(t, a, b);

	double p = x >= 0.0 ? 1.0 - 0.5 * beta_cdf : 0.5 * beta_cdf;
	return math::clamp(p, 0.0, 1.0);
}

<*
 @require p >= 0.0 && p <= 1.0 : "Probability must be between 0 and 1."
*>
fn double TDist.quantile(&self, double p) @dynamic
{
	if (p == 0.5) return 0.0;
	double x = (p < 0.5) ? -1.0 : 1.0;
	return newton_raphson(self, x, p) ?? double.nan;
}

fn double TDist.random(&self, Random rand) @dynamic
{
	// Generate using relationship with normal and chi-squared
	NormalDist std_normal = normal(0.0, 1.0);
	double z = std_normal.random(rand);
	double v = chi_squared_sample(self.df, rand);
	return z / math::sqrt(v / self.df);
}

<*
 F-distribution
*>
struct FDist (ContinuousDistribution)
{
	double d1;
	double d2;
}

<*
 @require d1 > 0.0 && d2 > 0.0 : "Degrees of freedom must be positive."
*>
fn FDist f_distribution(double d1, double d2)
{
	return (FDist){ d1, d2 };
}

fn double FDist.mean(&self) @dynamic
{
	if (self.d2 <= 2.0) return double.nan;
	return self.d2 / (self.d2 - 2.0);
}

fn double FDist.variance(&self) @dynamic
{
	if (self.d2 <= 4.0) return double.nan;
	double d1 = self.d1;
	double d2 = self.d2;
	return 2.0 * d2 * d2 * (d1 + d2 - 2.0) /
		(d1 * (d2 - 2.0) * (d2 - 2.0) * (d2 - 4.0));
}

fn double FDist.pdf(&self, double x) @dynamic
{
	if (x < 0.0) return 0.0;

	double d1 = self.d1;
	double d2 = self.d2;

	double num = math::pow(d1 * x, d1) * math::pow(d2, d2);
	double denom = math::pow(d1 * x + d2, d1 + d2);
	double beta_term = x * beta_function(d1 / 2.0, d2 / 2.0);

	return math::sqrt(num / denom) / beta_term;
}


fn double FDist.cdf(&self, double x) @dynamic
{
	if (x <= 0.0) return 0.0;

	double d1 = self.d1;
	double d2 = self.d2;
	double t = d1 * x / (d1 * x + d2);

	double p = incomplete_beta(t, d1 / 2.0, d2 / 2.0);
	return math::clamp(p, 0.0, 1.0);
}

<*
 @require p >= 0.0 && p <= 1.0 : "Probability must be between 0 and 1."
*>
fn double FDist.quantile(&self, double p) @dynamic
{
	return find_quantile(self, 0.0, 1000.0, p);
}

fn double FDist.random(&self, Random rand) @dynamic
{
	// Generate using ratio of chi-squared variables.
	double u1 = chi_squared_sample(self.d1, rand);
	double u2 = chi_squared_sample(self.d2, rand);
	return (u1 / self.d1) / (u2 / self.d2);
}

<*
 Chi-squared distribution
 *>
struct ChiSquaredDist (ContinuousDistribution)
{
	double k;
}

<*
 @require k > 0.0 : "Degrees of freedom must be positive"
*>
fn ChiSquaredDist chi_squared(double k)
{
	return (ChiSquaredDist){ k };
}

fn double ChiSquaredDist.mean(&self) @dynamic
{
	return self.k;
}

fn double ChiSquaredDist.variance(&self) @dynamic
{
	return 2.0 * self.k;
}

fn double ChiSquaredDist.pdf(&self, double x) @dynamic
{
	if (x < 0.0) return 0.0;
	if (x == 0.0 && self.k < 2.0) return double.inf;
	if (x == 0.0) return 0.0;

	double k = self.k;
	return math::pow(x, k / 2.0 - 1.0) * math::exp(-x / 2.0) /
		   (math::pow(2.0, k / 2.0) * math::tgamma(k / 2.0));
}

fn double ChiSquaredDist.cdf(&self, double x) @dynamic
{
	if (x <= 0.0) return 0.0;
	double p = lower_incomplete_gamma(self.k / 2.0, x / 2.0);
	return math::clamp(p, 0.0, 1.0);
}

<*
 @require p >= 0.0 && p <= 1.0 : "Probability must be between 0 and 1."
*>
fn double ChiSquaredDist.quantile(&self, double p) @dynamic
{
	double low = 0.0;
	double high = self.k + 10.0 * math::sqrt(2.0 * self.k);
	return find_quantile(self, low, high, p);
}

fn double ChiSquaredDist.random(&self, Random rand) @dynamic
{
	return chi_squared_sample(self.k, rand);
}

<*
 Binomial distribution
 *>
struct BinomialDist (DiscreteDistribution)
{
	int n;
	double p;
}

<*
 @require n >= 0 : "Number of trials must be non-negative."
 @require p >= 0.0 && p <= 1.0 : "Probability must be between 0 and 1."
*>
fn BinomialDist binomial(int n, double p)
{
	return (BinomialDist){ n, p };
}

fn double BinomialDist.mean(&self) @dynamic
{
	return (double)self.n * self.p;
}

fn double BinomialDist.variance(&self) @dynamic
{
	return (double)self.n * self.p * (1.0 - self.p);
}

fn double BinomialDist.pmf(&self, int k) @dynamic
{
	if (k < 0 || k > self.n) return 0.0;
	return binomial_coefficient(self.n, k) *
		math::pow(self.p, (double)k) *
		math::pow(1.0 - self.p, (double)(self.n - k));
}

fn double BinomialDist.cdf(&self, int k) @dynamic
{
	if (k < 0) return 0.0;
	if (k >= self.n) return 1.0;

	double sum = 0.0;
	for (int i = 0; i <= k; i++)
	{
		sum += self.pmf(i);
	}
	return sum;
}

<*
 @require p >= 0.0 && p <= 1.0 : "Probability must be between 0 and 1."
*>
fn int BinomialDist.quantile(&self, double p) @dynamic
{
	double cumulative = 0.0;
	for (int k = 0; k <= self.n; k++)
	{
		cumulative += self.pmf(k);
		if (cumulative >= p) return k;
	}
	return self.n;
}

fn int BinomialDist.random(&self, Random rand) @dynamic
{
	// Generate using Bernoulli trials.
	int successes = 0;
	for (int i = 0; i < self.n; i++)
	{
		if (random::next_double(rand) < self.p)
		{
			successes++;
		}
	}
	return successes;
}

<*
 Poisson distribution
 *>
struct PoissonDist (DiscreteDistribution)
{
	double lambda;
}

<*
 @require lambda > 0.0 : "Rate parameter must be positive."
*>
fn PoissonDist poisson(double lambda)
{
	return (PoissonDist){ lambda };
}

fn double PoissonDist.mean(&self) @dynamic
{
	return self.lambda;
}

fn double PoissonDist.variance(&self) @dynamic
{
	return self.lambda;
}

fn double PoissonDist.pmf(&self, int k) @dynamic
{
	if (k < 0) return 0.0;
	return math::exp(-self.lambda + (double)k * math::ln(self.lambda) - ln_factorial(k));
}

fn double PoissonDist.cdf(&self, int k) @dynamic
{
	if (k < 0) return 0.0;

	double sum = 0.0;
	for (int i = 0; i <= k; i++)
	{
		sum += self.pmf(i);
	}
	return sum;
}

<*
 @require p >= 0.0 && p <= 1.0 : "Probability must be between 0 and 1."
*>
fn int PoissonDist.quantile(&self, double p) @dynamic
{
	double cumulative = 0.0;
	int k = 0;
	while (cumulative < p)
	{
		cumulative += self.pmf(k);
		if (cumulative >= p) return k;
		k++;
		if (k > 1_000_000) break; // Safety limit
	}
	return k;
}

fn int PoissonDist.random(&self, Random rand) @dynamic
{
	// Knuth's algorithm for small lambda.
	if (self.lambda < 30.0)
	{
		double l = math::exp(-self.lambda);
		int k = 0;
		double p = 1.0;
		do
		{
			k++;
			p *= random::next_double(rand);
		} while (p > l);
		return (k - 1);
	}
	else
	{
		// Use normal approximation for large lambda
		NormalDist approx = normal(self.lambda, math::sqrt(self.lambda));
		return (int)math::max(0.0, math::round(approx.random(rand)));
	}
}

