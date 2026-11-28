module std::math::nolibc @feat(NO_LIBC, C3_MATH);

fn void _sincos_std(double x, double* sin, double* cos) @cname("sincos") @weak @nostrip @feat(!WIN32) => sincos(x, sin, cos);
fn void _sincosf_std(float x, float* sin, float* cos) @cname("sincosf") @weak @nostrip @feat(!WIN32) => sincosf(x, sin, cos);
