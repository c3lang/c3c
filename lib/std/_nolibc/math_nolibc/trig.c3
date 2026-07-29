module std::math::nolibc @feat(NO_LIBC, C3_MATH);

fn double sincos_broken(double x) @cname("sincos") @weak @nostrip
{
	unreachable("'sinccos' not supported");
}


