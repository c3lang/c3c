module std::math::nolibc @if(env::NO_LIBC || $feature(C3_MATH));

fn double sincos_broken(double x) @cname("sincos") @weak @nostrip
{
	unreachable("'sinccos' not supported");
}


