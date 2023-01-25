module std::math::nolibc;

$if (!env::COMPILER_LIBC_AVAILABLE):

fn float exp2f_broken(float x) @extname("exp2f") @weak
{
	unreachable("'exp2f' not supported");
}

fn float exp2_broken(float x) @extname("exp2") @weak
{
	unreachable("'exp2' not supported");
}

$endif;