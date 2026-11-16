module std::math::ease;

// Easing functions mapping `t` in [0, 1] to [0, 1] (`back_*` and `elastic_*` overshoot in between).
// The functions are only guaranteed to work in the [0, 1] interval. If clamping is needed,
// use `ease::@clamped(ease::quad_in, t)`.
// To ease between two values: `math::lerp(from, to, ease::quad_in(elapsed / duration))`.
//
// Easings are derived from Robert Penner's equations:
/*
 * TERMS OF USE - EASING EQUATIONS
 *
 * Open source under the BSD License.
 *
 * Copyright © 2001 Robert Penner
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this list of
 * conditions and the following disclaimer.
 *
 * Redistributions in binary form must reproduce the above copyright notice, this list
 * of conditions and the following disclaimer in the documentation and/or other materials
 * provided with the distribution.
 *
 * Neither the name of the author nor the names of contributors may be used to endorse
 * or promote products derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 *  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 *  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

<*
 Clamp a time between 0 and 1.

 @require values::@is_promotable_to_float(t) : `The input must be integer or floating type`
 @ensure return >= 0 && return <= 1
*>
macro float @clamped(#ease, t) => #ease(math::clamp((float)t, 0, 1));

fn float linear(float t) => t;

fn float smoothstep(float t) => sq(t) * (3 - 2 * t);
fn float smootherstep(float t) => cube(t) * (t * (6 * t - 15) + 10);

fn float sine_in(float t) => -math::cos(t * (float)math::HALF_PI) + 1;
fn float sine_out(float t) => math::sin(t * (float)math::HALF_PI);
fn float sine_inout(float t) => -0.5f * (math::cos(t * (float)math::PI) - 1);

fn float circ_in(float t) => 1 - math::sqrt(1 - sq(t));
fn float circ_out(float t) => math::sqrt(1 - sq(t - 1));
fn float circ_inout(float t) => t < 0.5f ? (1 - math::sqrt(1 - sq(2 * t))) / 2 : (math::sqrt(1 - sq(2 * t - 2)) + 1) / 2;

fn float cubic_in(float t) => cube(t);
fn float cubic_out(float t) => cube(t - 1) + 1;
fn float cubic_inout(float t) => t < 0.5f ? 4 * cube(t) : cube(2 * t - 2) / 2 + 1;

fn float quad_in(float t) => sq(t);
fn float quad_out(float t) => t * (2 - t);
fn float quad_inout(float t) => t < 0.5f ? 2 * sq(t) : 1 - sq(2 - 2 * t) / 2;

fn float quart_in(float t) => quart(t);
fn float quart_out(float t) => 1 - quart(t - 1);
fn float quart_inout(float t) => t < 0.5f ? 8 * quart(t) : 1 - quart(2 - 2 * t) / 2;

fn float quint_in(float t) => quint(t);
fn float quint_out(float t) => quint(t - 1) + 1;
fn float quint_inout(float t) => t < 0.5f ? 16 * quint(t) : quint(2 * t - 2) / 2 + 1;

fn float expo_in(float t) => t == 0 ? 0 : math::exp2(10 * t - 10);
fn float expo_out(float t) => t == 1 ? 1 : 1 - math::exp2(-10 * t);
fn float expo_inout(float t) => t == 0 ? 0 : t == 1 ? 1 : t < 0.5f ? 0.5f * math::exp2(20 * t - 10) : 0.5f * (2 - math::exp2(10 - 20 * t));

fn float back_in(float t, float s = 1.70158f) => sq(t) * ((s + 1) * t - s);
fn float back_out(float t, float s = 1.70158f) => sq(t - 1) * ((s + 1) * (t - 1) + s) + 1;
fn float back_inout(float t, float s = 1.70158f)
{
	if (t == 0) return 0;
	if (t == 1) return 1;
	s *= 1.525f;
	t *= 2;
	if (t < 1) return 0.5f * sq(t) * ((s + 1) * t - s);
	t -= 2;
	return 0.5f * (sq(t) * ((s + 1) * t + s) + 2);
}

fn float bounce_out(float t)
{
	switch
	{
		case t == 0:           return 0;
		case t == 1:           return 1;
		case t < 1 / 2.75f:    return 7.5625f * sq(t);
		case t < 2 / 2.75f:    return 7.5625f * sq(t - 1.5f / 2.75f) + 0.75f;
		case t < 2.5f / 2.75f: return 7.5625f * sq(t - 2.25f / 2.75f) + 0.9375f;
		default:               return 7.5625f * sq(t - 2.625f / 2.75f) + 0.984375f;
	}
}
fn float bounce_in(float t) => 1 - bounce_out(1 - t);
fn float bounce_inout(float t) => t < 0.5f ? 0.5f * (1 - bounce_out(1 - 2 * t)) : 0.5f * (1 + bounce_out(2 * t - 1));

fn float elastic_in(float t) => t == 0 ? 0 : -math::exp2(10 * t - 10) * math::sin((10 * t - 10.75f) * ELASTIC_C4);
fn float elastic_out(float t) => t == 1 ? 1 : math::exp2(-10 * t) * math::sin((10 * t - 0.75f) * ELASTIC_C4) + 1;

fn float elastic_inout(float t)
{
	if (t == 0) return 0;
	if (t == 1) return 1;
	float s = math::sin((20 * t - 11.125f) * ELASTIC_C5);
	return t < 0.5f ? -0.5f * math::exp2(20 * t - 10) * s : 0.5f * math::exp2(10 - 20 * t) * s + 1;
}

// Where a step function jumps, as in the CSS `jump-*` step positions.
enum JumpAt
{
	START, // The first jump is at the start, so the value is never 0 after t = 0.
	END,   // The last jump is at the end, so the value is not 1 until t = 1.
	NONE,  // No jump at the start or the end, the value is 0 until the first jump and 1 from the last.
	BOTH,  // Jumps at both the start and the end.
}

<*
 Divide the range into `count` equal steps, like CSS `steps(count, jump-*)`. Unlike CSS, `t <= 0` is always 0.

 @require count > 0 : "At least one step is required"
 @require at != NONE || count > 1 : "JumpAt.NONE needs at least two steps"
*>
fn float steps(float t, int count, JumpAt at = END)
{
	if (t <= 0) return 0;
	if (t >= 1) return 1;
	float step = math::floor(t * count) + (at == START || at == BOTH ? 1 : 0);
	float jumps = (float)(count + (at == BOTH ? 1 : at == NONE ? -1 : 0));
	return math::min(step, jumps) / jumps;
}

<*
 A cubic Bézier curve from (0, 0) to (1, 1) through the control points (x1, y1) and (x2, y2), like CSS
 `cubic-bezier(x1, y1, x2, y2)`. The input `t` is the x coordinate, and the result is the y coordinate,
 which `y1` and `y2` outside of [0, 1] make overshoot.

 @require x1 >= 0 && x1 <= 1 && x2 >= 0 && x2 <= 1 : "The x coordinates must be in [0, 1]"
*>
fn float cubic_bezier(float t, float x1, float y1, float x2, float y2)
{
	if (t == 0) return 0;
	if (t == 1) return 1;
	// The curve as polynomials in s: ((a * s + b) * s + c) * s
	float cx = 3 * x1;
	float bx = 3 * (x2 - x1) - cx;
	float ax = 1 - cx - bx;
	float cy = 3 * y1;
	float by = 3 * (y2 - y1) - cy;
	float ay = 1 - cy - by;
	// Find s where x(s) == t, with Newton's method starting at s = t, and bisection if it fails to converge.
	float s = t;
	for (int i = 0; i < 8; i++)
	{
		float x = ((ax * s + bx) * s + cx) * s - t;
		if (math::abs(x) < BEZIER_EPSILON) return ((ay * s + by) * s + cy) * s;
		float dx = (3 * ax * s + 2 * bx) * s + cx;
		if (math::abs(dx) < BEZIER_EPSILON) break;
		s -= x / dx;
	}
	float lo = 0;
	float hi = 1;
	s = t;
	for OUTER: (int i = 0; i < 32; i++)
	{
		float x = ((ax * s + bx) * s + cx) * s;
		switch
		{
			case math::abs(x - t) < BEZIER_EPSILON: break OUTER;
			case x < t:                             lo = s;
			default:                                hi = s;
		}
		s = (lo + hi) / 2;
	}
	return ((ay * s + by) * s + cy) * s;
}

module std::math::ease @private;

const float BEZIER_EPSILON = 1e-6f;
const float ELASTIC_C4 = (float)(2 * math::PI / 3);
const float ELASTIC_C5 = (float)(2 * math::PI / 4.5);

macro sq(x) => x * x;
macro cube(x) => x * x * x;
macro quart(x) => sq(sq(x));
macro quint(x) => sq(sq(x)) * x;
