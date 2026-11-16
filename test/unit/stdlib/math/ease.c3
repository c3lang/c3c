module std::math::ease_test;

import std::math;
import std::math::ease;
import std::math::easing;

alias UnaryEase = fn float(float);
const float EPSILON = 1e-5f;

macro check(float actual, float expected)
{
	assert(math::is_approx(actual, expected, EPSILON), "Expected %s, got %s", expected, actual);
}

fn void check_endpoints(UnaryEase ease_fn)
{
	check(ease_fn(0), 0);
	check(ease_fn(1), 1);
}

fn void test_endpoints() @test
{
	check_endpoints(&ease::linear);
	check_endpoints(&ease::smoothstep);
	check_endpoints(&ease::smootherstep);
	check_endpoints(&ease::sine_in);
	check_endpoints(&ease::sine_out);
	check_endpoints(&ease::sine_inout);
	check_endpoints(&ease::circ_in);
	check_endpoints(&ease::circ_out);
	check_endpoints(&ease::circ_inout);
	check_endpoints(&ease::cubic_in);
	check_endpoints(&ease::cubic_out);
	check_endpoints(&ease::cubic_inout);
	check_endpoints(&ease::quad_in);
	check_endpoints(&ease::quad_out);
	check_endpoints(&ease::quad_inout);
	check_endpoints(&ease::quart_in);
	check_endpoints(&ease::quart_out);
	check_endpoints(&ease::quart_inout);
	check_endpoints(&ease::quint_in);
	check_endpoints(&ease::quint_out);
	check_endpoints(&ease::quint_inout);
	check_endpoints(&ease::expo_in);
	check_endpoints(&ease::expo_out);
	check_endpoints(&ease::expo_inout);
	check_endpoints(&ease::bounce_in);
	check_endpoints(&ease::bounce_out);
	check_endpoints(&ease::bounce_inout);
	check_endpoints(&ease::elastic_in);
	check_endpoints(&ease::elastic_out);
	check_endpoints(&ease::elastic_inout);

	check(ease::back_in(0), 0);
	check(ease::back_in(1), 1);
	check(ease::back_out(0), 0);
	check(ease::back_out(1), 1);
	check(ease::back_inout(0), 0);
	check(ease::back_inout(1), 1);
}

fn void test_explicit_clamping() @test
{
	check(ease::@clamped(ease::linear, -1), 0);
	check(ease::@clamped(ease::quad_in, 0.5f), 0.25f);
	check(ease::@clamped(ease::linear, 2), 1);
}

fn void test_in_out_symmetry() @test
{
	float t = 0.37f;
	check(ease::smoothstep(t), 1 - ease::smoothstep(1 - t));
	check(ease::smootherstep(t), 1 - ease::smootherstep(1 - t));
	check(ease::sine_in(t), 1 - ease::sine_out(1 - t));
	check(ease::circ_in(t), 1 - ease::circ_out(1 - t));
	check(ease::cubic_in(t), 1 - ease::cubic_out(1 - t));
	check(ease::quad_in(t), 1 - ease::quad_out(1 - t));
	check(ease::quart_in(t), 1 - ease::quart_out(1 - t));
	check(ease::quint_in(t), 1 - ease::quint_out(1 - t));
	check(ease::expo_in(t), 1 - ease::expo_out(1 - t));
	check(ease::back_in(t), 1 - ease::back_out(1 - t));
	check(ease::bounce_in(t), 1 - ease::bounce_out(1 - t));
	check(ease::elastic_in(t), 1 - ease::elastic_out(1 - t));
}

fn void test_smoothstep() @test
{
	check(ease::smoothstep(0.25f), 0.15625f);
	check(ease::smoothstep(0.5f), 0.5f);
	check(ease::smoothstep(0.75f), 0.84375f);
	check(ease::smootherstep(0.25f), 0.103515625f);
	check(ease::smootherstep(0.5f), 0.5f);
	check(ease::smootherstep(0.75f), 0.896484375f);
}

fn void test_steps() @test
{
	check(ease::steps(0, 4, START), 0);
	check(ease::steps(0.24f, 4, START), 0.25f);
	check(ease::steps(0.25f, 4, START), 0.5f);
	check(ease::steps(0.24f, 4, END), 0);
	check(ease::steps(0.25f, 4, END), 0.25f);
	check(ease::steps(0.25f, 4, NONE), 1.0f / 3.0f);
	check(ease::steps(0.75f, 4, NONE), 1);
	check(ease::steps(0.25f, 4, BOTH), 0.4f);
	check(ease::steps(0.75f, 4, BOTH), 0.8f);
	check(ease::steps(1, 4, BOTH), 1);
}

fn void test_cubic_bezier() @test
{
	foreach (t : (float[5]){ 0, 0.1f, 0.37f, 0.75f, 1 })
	{
		check(ease::cubic_bezier(t, 0, 0, 1, 1), t);
	}
}

fn void test_legacy_wrappers() @test @allow_deprecated
{
	float t = 0.925f;
	float b = -3;
	float c = 7;
	float d = 2.5f;
	float u = t / d;

	check(easing::linear_none(t, b, c, d), b + c * ease::linear(u));
	check(easing::linear_in(t, b, c, d), b + c * ease::linear(u));
	check(easing::linear_out(t, b, c, d), b + c * ease::linear(u));
	check(easing::linear_inout(t, b, c, d), b + c * ease::linear(u));
	check(easing::sine_in(t, b, c, d), b + c * ease::sine_in(u));
	check(easing::sine_out(t, b, c, d), b + c * ease::sine_out(u));
	check(easing::sine_inout(t, b, c, d), b + c * ease::sine_inout(u));
	check(easing::circ_in(t, b, c, d), b + c * ease::circ_in(u));
	check(easing::circ_out(t, b, c, d), b + c * ease::circ_out(u));
	check(easing::circ_inout(t, b, c, d), b + c * ease::circ_inout(u));
	check(easing::cubic_in(t, b, c, d), b + c * ease::cubic_in(u));
	check(easing::cubic_out(t, b, c, d), b + c * ease::cubic_out(u));
	check(easing::cubic_inout(t, b, c, d), b + c * ease::cubic_inout(u));
	check(easing::quad_in(t, b, c, d), b + c * ease::quad_in(u));
	check(easing::quad_out(t, b, c, d), b + c * ease::quad_out(u));
	check(easing::quad_inout(t, b, c, d), b + c * ease::quad_inout(u));
	check(easing::expo_in(t, b, c, d), b + c * ease::expo_in(u));
	check(easing::expo_out(t, b, c, d), b + c * ease::expo_out(u));
	check(easing::expo_inout(t, b, c, d), b + c * ease::expo_inout(u));
	check(easing::back_in(t, b, c, d), b + c * ease::back_in(u));
	check(easing::back_out(t, b, c, d), b + c * ease::back_out(u));
	check(easing::back_inout(t, b, c, d), b + c * ease::back_inout(u));
	check(easing::bounce_in(t, b, c, d), b + c * ease::bounce_in(u));
	check(easing::bounce_out(t, b, c, d), b + c * ease::bounce_out(u));
	check(easing::bounce_inout(t, b, c, d), b + c * ease::bounce_inout(u));
	check(easing::elastic_in(t, b, c, d), b + c * ease::elastic_in(u));
	check(easing::elastic_out(t, b, c, d), b + c * ease::elastic_out(u));
	check(easing::elastic_inout(t, b, c, d), b + c * ease::elastic_inout(u));

	float s = 2.25f;
	check(easing::back_in(t, b, c, d, s), b + c * ease::back_in(u, s));
	check(easing::back_out(t, b, c, d, s), b + c * ease::back_out(u, s));
	check(easing::back_inout(t, b, c, d, s), b + c * ease::back_inout(u, s));
}
