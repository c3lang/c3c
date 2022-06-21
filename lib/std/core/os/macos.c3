module std::core::os::macos;
$if (env::OS_TYPE == OsType.MACOSX):

extern fn int* __error();

fn int errno() @inline
{
    return *__error();
}
fn void errno_set(int err)
{
    *(__error()) = err;
}
$endif;
