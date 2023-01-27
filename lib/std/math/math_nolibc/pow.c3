module std::math::nolibc;

$if (!env::COMPILER_LIBC_AVAILABLE):

fn float powf_broken(float x, float f) @extname("powf") @weak
{
	unreachable("'powf' not supported");
}

fn double pow_broken(double x, double y) @extname("pow") @weak
{
	unreachable("'pow' not supported");
}

$endif;