module libc::os;

// Linux
extern fn int* __errno_location() @if(LINUX_LIBC);
macro int errno() @if(LINUX_LIBC) => *__errno_location();
macro void errno_set(int err) @if(LINUX_LIBC) => *(__errno_location()) = err;

// Darwin
extern fn int* __error() @if(DARWIN_LIBC);
macro int errno() @if(DARWIN_LIBC) => *__error();
macro void errno_set(int err) @if(DARWIN_LIBC) => *(__error()) = err;

// Win32
macro int errno() @if(WIN32_LIBC)
{
	int holder;
	_get_errno(&holder);
	return holder;
}
macro void errno_set(int err) @if(WIN32_LIBC) => _set_errno(err);
extern fn void _get_errno(int* result) @if(WIN32_LIBC);
extern fn void _set_errno(int err) @if(WIN32_LIBC);

// Default
const ERRNO_DEFAULT @local = !LINUX_LIBC && !DARWIN_LIBC && !WIN32_LIBC;
tlocal int _errno_c3 @if(ERRNO_DEFAULT) = 0;
fn void errno_set(int err) @if(ERRNO_DEFAULT) => _errno_c3 = err;
fn int errno() @if(ERRNO_DEFAULT) => _errno_c3;