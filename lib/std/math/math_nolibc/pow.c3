module std::math::nolibc @if(env::NO_LIBC);

fn float powf_broken(float x, float f) @extern("powf") @weak @nostrip
{
	unreachable("'powf' not supported");
}

fn double pow_broken(double x, double y) @extern("pow") @weak @nostrip
{
	unreachable("'pow' not supported");
}
