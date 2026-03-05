// Copyright (c) 2026 Koni Marti. All rights reserved.
// Use of this source code is governed by the MIT license.
module std::math::distributions_test;
import std::io, std::math, std::math::distributions @public;

const double TOLERANCE = 1e-10;
const double RELAXED_TOLERANCE = 1e-6;
const double VERY_RELAXED_TOLERANCE = 1e-3;

macro approx(double left, double right, double tol, String $msg = "")
{
	assert(math::is_approx(left, right, tol), "%s != %s (eps: %g): %s", left, right, tol, $msg);
}

fn void test_uniform_mean() @test
{
	UniformDist dist = distributions::uniform(0.0, 10.0);
	double mean = dist.mean();
	approx(mean, 5.0, TOLERANCE, "Uniform mean should be (a+b)/2");
}

fn void test_uniform_variance() @test
{
	UniformDist dist = distributions::uniform(0.0, 10.0);
	double variance = dist.variance();
	double expected = 100.0 / 12.0;
	approx(variance, expected, TOLERANCE, "Uniform variance should be (b-a)²/12");
}

fn void test_uniform_pdf() @test
{
	UniformDist dist = distributions::uniform(0.0, 10.0);

	// PDF should be constant in range
	approx(dist.pdf(5.0), 0.1, TOLERANCE, "PDF at midpoint");
	approx(dist.pdf(0.0), 0.1, TOLERANCE, "PDF at lower bound");
	approx(dist.pdf(10.0), 0.1, TOLERANCE, "PDF at upper bound");

	// PDF should be 0 outside range
	approx(dist.pdf(-1.0), 0.0, TOLERANCE, "PDF below range");
	approx(dist.pdf(11.0), 0.0, TOLERANCE, "PDF above range");
}

fn void test_uniform_cdf() @test
{
	UniformDist dist = distributions::uniform(0.0, 10.0);

	approx(dist.cdf(-1.0), 0.0, TOLERANCE, "CDF below range");
	approx(dist.cdf(0.0), 0.0, TOLERANCE, "CDF at lower bound");
	approx(dist.cdf(5.0), 0.5, TOLERANCE, "CDF at midpoint");
	approx(dist.cdf(7.5), 0.75, TOLERANCE, "CDF at 75%");
	approx(dist.cdf(10.0), 1.0, TOLERANCE, "CDF at upper bound");
	approx(dist.cdf(11.0), 1.0, TOLERANCE, "CDF above range");
}

fn void test_uniform_quantile() @test
{
	UniformDist dist = distributions::uniform(0.0, 10.0);

	approx(dist.quantile(0.0), 0.0, TOLERANCE);
	approx(dist.quantile(0.25), 2.5, TOLERANCE);
	approx(dist.quantile(0.5), 5.0, TOLERANCE);
	approx(dist.quantile(0.75), 7.5, TOLERANCE);
	approx(dist.quantile(1.0), 10.0, TOLERANCE);
}

fn void test_uniform_round_trip() @test
{
	UniformDist dist = distributions::uniform(0.0, 10.0);

	for (double x = 1.0; x <= 9.0; x += 2.0)
	{
		double p = dist.cdf(x);
		double x_recovered = dist.quantile(p);
		approx(x_recovered, x, TOLERANCE, "CDF/inverse_CDF round-trip");
	}
}

fn void test_uniform_random_samples() @test
{
	UniformDist dist = distributions::uniform(0.0, 10.0);

	DefaultRandom r;
	random::seed_entropy(&r);

	int n_samples = 10_000;
	for (int i = 0; i < n_samples; i++)
	{
		double sample = dist.random(&r);
		assert(sample >= 0.0 && sample <= 10.0, "Random sample in range");
	}
}

fn void test_normal_mean_variance() @test
{
	NormalDist dist = distributions::normal(10.0, 2.0);
	approx(dist.mean(), 10.0, TOLERANCE);
	approx(dist.variance(), 4.0, TOLERANCE);
}

fn void test_normal_pdf_at_mean() @test
{
	NormalDist std_normal = distributions::normal(0.0, 1.0);
	double pdf_0 = std_normal.pdf(0.0);
	double expected = 1.0 / math::sqrt(2.0 * math::PI);
	approx(pdf_0, expected, TOLERANCE, "PDF at mean");
}

fn void test_normal_pdf_symmetry() @test
{
	NormalDist std_normal = distributions::normal(0.0, 1.0);
	approx(std_normal.pdf(-1.0), std_normal.pdf(1.0), TOLERANCE, "PDF symmetry");
	approx(std_normal.pdf(-2.0), std_normal.pdf(2.0), TOLERANCE, "PDF symmetry at 2");
}

fn void test_normal_cdf_known_values() @test
{
	NormalDist std_normal = distributions::normal(0.0, 1.0);

	approx(std_normal.cdf(0.0),   0.500, TOLERANCE, "CDF at mean");
	approx(std_normal.cdf(-1.96), 0.025, VERY_RELAXED_TOLERANCE, "CDF at -1.96");
	approx(std_normal.cdf(1.96),  0.975, VERY_RELAXED_TOLERANCE, "CDF at 1.96");
	approx(std_normal.cdf(-2.58), 0.005, VERY_RELAXED_TOLERANCE, "CDF at -2.58");
	approx(std_normal.cdf(2.58),  0.995, VERY_RELAXED_TOLERANCE, "CDF at 2.58");
}

fn void test_normal_quantile() @test
{
	NormalDist std_normal = distributions::normal(0.0, 1.0);

	approx(std_normal.quantile(0.5), 0.0, TOLERANCE, "Median");

	double z_975 = std_normal.quantile(0.975);
	approx(z_975, 1.96, 0.02, "97.5th percentile");

	double z_025 = std_normal.quantile(0.025);
	approx(z_025, -1.96, 0.02, "2.5th percentile");
}

fn void test_normal_round_trip() @test
{
	NormalDist std_normal = distributions::normal(0.0, 1.0);

	for (double x = -3.0; x <= 3.0; x += 1.0)
	{
		double p = std_normal.cdf(x);
		double x_recovered = std_normal.quantile(p);
		approx(x_recovered, x, TOLERANCE, "Normal round-trip");
	}
}

fn void test_normal_custom_parameters() @test
{
	NormalDist custom = distributions::normal(100.0, 15.0);

	approx(custom.mean(), 100.0, TOLERANCE);
	approx(custom.variance(), 225.0, TOLERANCE);
	approx(custom.cdf(100.0), 0.5, TOLERANCE, "CDF at mean");
}

fn void test_exponential_mean_variance() @test
{
	ExponentialDist dist = distributions::exponential(2.0);
	approx(dist.mean(), 0.5, TOLERANCE);
	approx(dist.variance(), 0.25, TOLERANCE);
}

fn void test_exponential_pdf() @test
{
	ExponentialDist dist = distributions::exponential(2.0);

	approx(dist.pdf(0.0), 2.0, TOLERANCE, "PDF at 0");
	approx(dist.pdf(-1.0), 0.0, TOLERANCE, "PDF for negative x");

	double pdf_1 = dist.pdf(1.0);
	double expected = 2.0 * math::exp(-2.0);
	approx(pdf_1, expected, TOLERANCE, "PDF at 1");
}

fn void test_exponential_cdf() @test
{
	ExponentialDist dist = distributions::exponential(2.0);

	approx(dist.cdf(0.0), 0.0, TOLERANCE, "CDF at 0");
	approx(dist.cdf(-1.0), 0.0, TOLERANCE, "CDF for negative x");

	double cdf_mean = dist.cdf(dist.mean());
	approx(cdf_mean, 1.0 - math::exp(-1.0), TOLERANCE, "CDF at mean");
}

fn void test_exponential_quantile() @test
{
	ExponentialDist dist = distributions::exponential(2.0);

	approx(dist.quantile(0.0), 0.0, TOLERANCE);

	double median = dist.quantile(0.5);
	double expected_median = math::ln(2.0) / 2.0;
	approx(median, expected_median, TOLERANCE, "Median");
}

fn void test_exponential_round_trip() @test
{
	ExponentialDist dist = distributions::exponential(2.0);

	for (double p = 0.1; p <= 0.9; p += 0.1)
	{
		double x = dist.quantile(p);
		double p_recovered = dist.cdf(x);
		approx(p_recovered, p, TOLERANCE, "Exponential round-trip");
	}
}

fn void test_exponential_random_samples() @test
{
	ExponentialDist dist = distributions::exponential(2.0);

	DefaultRandom r;
	random::seed_entropy(&r);

	for (int i = 0; i < 100; i++)
	{
		double sample = dist.random(&r);
		assert(sample >= 0.0, "Random sample non-negative");
	}
}

fn void test_t_mean_variance() @test
{
	TDist dist = distributions::t_distribution(10.0);
	approx(dist.mean(), 0.0, TOLERANCE);
	approx(dist.variance(), 10.0/8.0, TOLERANCE);
}

fn void test_t_pdf_symmetry() @test
{
	TDist dist = distributions::t_distribution(10.0);
	approx(dist.pdf(-1.0), dist.pdf(1.0), TOLERANCE, "PDF symmetry");
	approx(dist.pdf(-2.0), dist.pdf(2.0), TOLERANCE, "PDF symmetry at 2");
}

fn void test_t_cdf_symmetry() @test
{
	TDist dist = distributions::t_distribution(10.0);

	approx(dist.cdf(0.0), 0.5, TOLERANCE, "CDF at 0");

	double cdf_1 = dist.cdf(1.0);
	double cdf_neg_1 = dist.cdf(-1.0);
	approx(cdf_1 + cdf_neg_1, 1.0, TOLERANCE, "CDF symmetry");
}

fn void test_t_quantile() @test
{
	TDist dist = distributions::t_distribution(10.0);

	approx(dist.quantile(0.5), 0.0, RELAXED_TOLERANCE, "Median is 0");

	double upper = dist.quantile(0.975);
	double lower = dist.quantile(0.025);
	approx(upper, -lower, RELAXED_TOLERANCE, "Inverse CDF symmetry");
}

fn void test_f_mean() @test
{
	FDist dist =distributions::f_distribution(5.0, 10.0);
	double expected_mean = 10.0 / 8.0;
	approx(dist.mean(), expected_mean, TOLERANCE);
}

fn void test_f_pdf() @test
{
	// FIXME: add tests for pdf
	FDist dist =distributions::f_distribution(5.0, 10.0);
	approx(dist.pdf(0.5), 0.687607, RELAXED_TOLERANCE, "PDF at x=0.5 for F(5,10) is wrong");
	approx(dist.pdf(10.0), 0.000478163, RELAXED_TOLERANCE, "PDF at x=10.0 for F(5,10) is wrong");
}

fn void test_f_cdf() @test
{
	FDist dist =distributions::f_distribution(5.0, 10.0);
	approx(dist.cdf(0.0), 0.0, TOLERANCE, "CDF at 0");
	approx(dist.cdf(-1.0), 0.0, TOLERANCE, "CDF for negative x");
}

fn void test_f_quantile() @test
{
	FDist dist =distributions::f_distribution(5.0, 10.0);
	double x_median = dist.quantile(0.5);
	assert(x_median > 0.0, "Median positive");

	double x_25 = dist.quantile(0.25);
	double x_75 = dist.quantile(0.75);
	assert(x_75 > x_25, "Inverse CDF monotonic");
}

fn void test_f_random_samples() @test
{
	FDist dist =distributions::f_distribution(5.0, 10.0);

 	DefaultRandom r;
	random::seed_entropy(&r);

	for (int i = 0; i < 100; i++)
	{
		double sample = dist.random(&r);
		assert(sample >= 0.0, "Random sample positive");
	}
}

fn void test_chi_squared_mean_variance() @test
{
	ChiSquaredDist dist = distributions::chi_squared(5.0);
	approx(dist.mean(), 5.0, TOLERANCE);
	approx(dist.variance(), 10.0, TOLERANCE);
}

fn void test_chi_squared_pdf() @test
{
	ChiSquaredDist dist = distributions::chi_squared(5.0);
	approx(dist.pdf(-1.0), 0.0, TOLERANCE, "PDF for negative x");
	approx(dist.pdf(1.145), 0.091910, RELAXED_TOLERANCE);
	approx(dist.pdf(5.000), 0.122042, RELAXED_TOLERANCE);
}

fn void test_chi_squared_cdf() @test
{
	ChiSquaredDist dist = distributions::chi_squared(1.0);
	approx(dist.cdf(1.0), 0.682689, RELAXED_TOLERANCE, "");

	dist.k = 5;
	approx(dist.cdf(5.0), 0.5841, VERY_RELAXED_TOLERANCE);
}

fn void test_chi_squared_quantile() @test
{
	ChiSquaredDist dist = distributions::chi_squared(5.0);
	approx(dist.quantile(0.95), 11.0705, 0.1, "95th percentile");
}

fn void test_chi_squared_round_trip() @test
{
	ChiSquaredDist dist = distributions::chi_squared(5.0);
	for (double p = 0.1; p <= 0.9; p += 0.2)
	{
		double x = dist.quantile(p);
		assert(x > 0.0, "Inverse CDF positive");
		double p_recovered = dist.cdf(x);
		approx(p_recovered, p, RELAXED_TOLERANCE, "Chi-squared round-trip");
	}
}

fn void test_chi_squared_random_samples() @test
{
	ChiSquaredDist dist = distributions::chi_squared(5.0);

 	DefaultRandom r;
	random::seed_entropy(&r);

	for (int i = 0; i < 100; i++)
	{
		double sample = dist.random(&r);
		assert(sample >= 0.0, "Random sample non-negative");
	}
}

fn void test_binomial_mean_variance() @test
{
	BinomialDist dist = distributions::binomial(10, 0.5);
	approx(dist.mean(), 5.0, TOLERANCE);
	approx(dist.variance(), 2.5, TOLERANCE);
}

fn void test_binomial_pmf_symmetry() @test
{
	BinomialDist dist = distributions::binomial(10, 0.5);
	for (int k = 0; k <= 5; k++)
	{
		double pmf_k = dist.pmf(k);
		double pmf_10_minus_k = dist.pmf(10 - k);
		approx(pmf_k, pmf_10_minus_k, TOLERANCE, "PMF symmetry");
	}
}

fn void test_binomial_pmf_sums_to_one() @test
{
	BinomialDist dist = distributions::binomial(10, 0.5);
	double sum = 0.0;
	for (int k = 0; k <= 10; k++)
	{
		sum += dist.pmf(k);
	}
	approx(sum, 1.0, TOLERANCE, "PMF sums to 1");
}

fn void test_binomial_cdf() @test
{
	BinomialDist dist = distributions::binomial(10, 0.5);
	approx(dist.cdf(-1), 0.0, TOLERANCE, "CDF below 0");
	approx(dist.cdf(10), 1.0, TOLERANCE, "CDF at n");
	approx(dist.cdf(5), 0.623, VERY_RELAXED_TOLERANCE, "CDF at mean");
}

fn void test_binomial_random_samples() @test
{
	BinomialDist dist = distributions::binomial(10, 0.5);

	DefaultRandom r;
	random::seed_entropy(&r);

	for (int i = 0; i < 100; i++)
	{
		double sample = dist.random(&r);
		assert(sample >= 0.0 && sample <= 10.0, "Random in range");
	}
}

fn void test_poisson_mean_variance() @test
{
	PoissonDist dist = distributions::poisson(3.0);
	approx(dist.mean(), 3.0, TOLERANCE);
	approx(dist.variance(), 3.0, TOLERANCE);
}

fn void test_poisson_pmf_sums_to_one() @test
{
	PoissonDist dist = distributions::poisson(3.0);
	double sum = 0.0;
	for (int k = 0; k <= 20; k++)
	{
		sum += dist.pmf(k);
	}
	assert(sum > 0.99, "PMF sums to ~1");
}

fn void test_poisson_pmf_at_zero() @test
{
	PoissonDist dist = distributions::poisson(3.0);
	double pmf_0 = dist.pmf(0);
	double expected = math::exp(-3.0);
	approx(pmf_0, expected, TOLERANCE, "PMF at 0");
}

fn void test_poisson_cdf() @test
{
	PoissonDist dist = distributions::poisson(3.0);
	approx(dist.cdf(-1), 0.0, TOLERANCE, "CDF for negative k");
}

fn void test_poisson_random_samples() @test
{
	PoissonDist dist = distributions::poisson(3.0);

	DefaultRandom r;
	random::seed_entropy(&r);

	for (int i = 0; i < 100; i++)
	{
		double sample = dist.random(&r);
		assert(sample >= 0.0, "Random non-negative");
	}
}

fn void test_poisson_large_lambda() @test
{
	PoissonDist large = distributions::poisson(50.0);
	approx(large.mean(), 50.0, TOLERANCE, "Large lambda mean");
}

fn void test_beta_function() @test
{
	assert(distributions::beta_function(2.5, 1.5) == distributions::beta_function(1.5, 2.5), "Beta function not symmetrical.");
	assert(math::is_approx(distributions::beta_function(3.0, 2.0), 0.08333333333, 1e-6), "Beta(3,2) is wrong");
	assert(math::is_approx(distributions::beta_function(4.0, 1.0), 1.0/4.0, 1e-6), "Beta(4,1) is wrong");
}

fn void test_lower_incomplete_gamma() @test
{
	approx(distributions::lower_incomplete_gamma(0.5,0.5), 0.682689, 1e-4, "");
}

