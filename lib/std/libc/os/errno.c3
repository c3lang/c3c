module libc::os @if(env::LIBC);
import std::core::env;

// Linux
extern fn int* __errno_location() @if(env::LINUX);
macro int errno() @if(env::LINUX) => *__errno_location();
macro void errno_set(int err) @if(env::LINUX) => *(__errno_location()) = err;

// Android 9
extern fn int* __errno() @if(env::ANDROID);
macro int errno() @if(env::ANDROID) => *__errno();
macro void errno_set(int err) @if(env::ANDROID) => *(__errno()) = err;

// OpenBSD and NetBSD
extern fn int* __errno() @if(env::OPENBSD || env::NETBSD);
macro int errno() @if(env::OPENBSD || env::NETBSD) => *__errno();
macro void errno_set(int err) @if(env::OPENBSD || env::NETBSD) => *(__errno()) = err;

// Darwin
extern fn int* __error() @if(env::DARWIN);
macro int errno() @if(env::DARWIN) => *__error();
macro void errno_set(int err) @if(env::DARWIN) => *(__error()) = err;

// Win32
macro int errno() @if(env::WIN32)
{
	int holder;
	_get_errno(&holder);
	return holder;
}
macro void errno_set(int err) @if(env::WIN32) => _set_errno(err);
extern fn void _get_errno(int* result) @if(env::WIN32);
extern fn void _set_errno(int err) @if(env::WIN32);

// Default
module libc::os @if(!env::LIBC || !env::HAS_NATIVE_ERRNO);
tlocal int _errno_c3 = 0;
fn void errno_set(int err) => _errno_c3 = err;
fn int errno() => _errno_c3;
