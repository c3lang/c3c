module std::math::nolibc @if(env::NO_LIBC);

fn double sincos_broken(double x) @extern("sincos") @weak @nostrip
{
	unreachable("'sinccos' not supported");
}


