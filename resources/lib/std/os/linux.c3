module std::os::linux;
import std::env;

$if (env::OS_TYPE == OsType.LINUX):

extern fn int* __errno_location();

fn int errno() @inline
{
    return *__errno_location();
}

fn void errno_set(int errno)
{
    *(__errno_location()) = errno;
}

$endif;
