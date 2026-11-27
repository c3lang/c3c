module std::math::nolibc @feat(NO_LIBC, C3_MATH);

fn double ldexp(double x, int exp) @cname("ldexp") => math::ldexp(x, exp);
fn float ldexpf(float x, int exp) @cname("ldexpf") => math::ldexp(x, exp);
