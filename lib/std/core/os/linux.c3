module std::core::os::linux;

$if (env::OS_TYPE == OsType.LINUX):

extern fn int* __errno_location();

fn int errno() @inline
{
    return *__errno_location();
}

fn void errno_set(int err)
{
    *(__errno_location()) = err;
}

$endif;
