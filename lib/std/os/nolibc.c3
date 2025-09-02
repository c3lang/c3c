module std::math::nolibc @if(env::NO_LIBC);

fn void __stack_chk_fail() @extern("__stack_chk_fail") @nostrip @noreturn @weak
{
	$$trap();
}