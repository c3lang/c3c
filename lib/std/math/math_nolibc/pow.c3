module std::math::nolibc;

$if (!env::COMPILER_LIBC_AVAILABLE):

fn float powf_broken(float x) @extname("powf") @weak
{
	unreachable("'powf' not supported");
}

fn float pow_broken(float x) @extname("pow") @weak
{
	unreachable("'pow' not supported");
}

$endif;