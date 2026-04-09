module std::nolibc @if(env::NO_LIBC);

fn void __stack_chk_fail() @cname("__stack_chk_fail") @nostrip @noreturn @weak
{
	$$trap();
}

fn usz strlen(char* s) @cname("strlen") @nostrip @weak
{
	char* p = s;
	while (*p) p++;
	return (usz)(p - s);
}