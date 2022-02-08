module std::os::windows;
import std::env;

$if (env::OS_TYPE == OsType.WIN32):

extern fn int getLastError() @stdcall @extname("GetLastError");
fn int errno() @inline
{
    return getLastError();
}
$endif;
