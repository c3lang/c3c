module std::math::easing;

import std::math::ease;

// Compatibility wrappers for the original Penner-style (time, begin, change, duration) API.
fn float linear_none(float t, float b, float c, float d) @deprecated @inline => b + c * ease::linear(t / d);
fn float linear_in(float t, float b, float c, float d) @deprecated @inline => b + c * ease::linear(t / d);
fn float linear_out(float t, float b, float c, float d) @deprecated @inline => b + c * ease::linear(t / d);
fn float linear_inout(float t, float b, float c, float d) @deprecated @inline => b + c * ease::linear(t / d);

fn float sine_in(float t, float b, float c, float d) @deprecated @inline => b + c * ease::sine_in(t / d);
fn float sine_out(float t, float b, float c, float d) @deprecated @inline => b + c * ease::sine_out(t / d);
fn float sine_inout(float t, float b, float c, float d) @deprecated @inline => b + c * ease::sine_inout(t / d);

fn float circ_in(float t, float b, float c, float d) @deprecated @inline => b + c * ease::circ_in(t / d);
fn float circ_out(float t, float b, float c, float d) @deprecated @inline => b + c * ease::circ_out(t / d);
fn float circ_inout(float t, float b, float c, float d) @deprecated @inline => b + c * ease::circ_inout(t / d);

fn float cubic_in(float t, float b, float c, float d) @deprecated @inline => b + c * ease::cubic_in(t / d);
fn float cubic_out(float t, float b, float c, float d) @deprecated @inline => b + c * ease::cubic_out(t / d);
fn float cubic_inout(float t, float b, float c, float d) @deprecated @inline => b + c * ease::cubic_inout(t / d);

fn float quad_in(float t, float b, float c, float d) @deprecated @inline => b + c * ease::quad_in(t / d);
fn float quad_out(float t, float b, float c, float d) @deprecated @inline => b + c * ease::quad_out(t / d);
fn float quad_inout(float t, float b, float c, float d) @deprecated @inline => b + c * ease::quad_inout(t / d);

fn float expo_in(float t, float b, float c, float d) @deprecated @inline => b + c * ease::expo_in(t / d);
fn float expo_out(float t, float b, float c, float d) @deprecated @inline => b + c * ease::expo_out(t / d);
fn float expo_inout(float t, float b, float c, float d) @deprecated @inline => b + c * ease::expo_inout(t / d);

fn float back_in(float t, float b, float c, float d, float s = 1.70158f) @deprecated @inline => b + c * ease::back_in(t / d, s);
fn float back_out(float t, float b, float c, float d, float s = 1.70158f) @deprecated @inline => b + c * ease::back_out(t / d, s);
fn float back_inout(float t, float b, float c, float d, float s = 1.70158f) @deprecated @inline => b + c * ease::back_inout(t / d, s);

fn float bounce_in(float t, float b, float c, float d) @deprecated @inline => b + c * ease::bounce_in(t / d);
fn float bounce_out(float t, float b, float c, float d) @deprecated @inline => b + c * ease::bounce_out(t / d);
fn float bounce_inout(float t, float b, float c, float d) @deprecated @inline => b + c * ease::bounce_inout(t / d);

fn float elastic_in(float t, float b, float c, float d) @deprecated @inline => b + c * ease::elastic_in(t / d);
fn float elastic_out(float t, float b, float c, float d) @deprecated @inline => b + c * ease::elastic_out(t / d);
fn float elastic_inout(float t, float b, float c, float d) @deprecated @inline => b + c * ease::elastic_inout(t / d);
