module std::math::random @test;
import std::math;

fn void test_regular_random()
{
	for (int i = 0; i < 100; i++) assert(rand(45) < 45 && rand(45) >= 0);
}

fn void test_next_bool()
{
	DefaultRandom rand;
	random::seed_entropy(&rand);
	for (int i = 0; i < 100; i++) random::next_bool(&rand);
}

fn void test_next()
{
	DefaultRandom rand;
	random::seed_entropy(&rand);
	for (int i = 0; i < 100; i++) assert(random::next(&rand, 100) < 100.0 && random::next(&rand, 100) >= 0);
}

fn void test_next_double()
{
	DefaultRandom rand;
	random::seed_entropy(&rand);
	for (int i = 0; i < 100; i++) assert(random::next_double(&rand) < 1.0 && random::next_double(&rand) >= 0);
}

fn void test_next_float()
{
	DefaultRandom rand;
	random::seed_entropy(&rand);
	for (int i = 0; i < 100; i++) assert(random::next_float(&rand) < 1.0 && random::next_float(&rand) >= 0);
}