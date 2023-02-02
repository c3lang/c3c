module std::math::nolibc::trig;

$if (!env::COMPILER_LIBC_AVAILABLE):

fn double sincos_broken(double x) @extname("sincos") @weak
{
	unreachable("'sinccos' not supported");
}


$endif;
