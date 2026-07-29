module libc::os;
import std::core::env;

// Linux
extern fn int* __errno_location() @feat(LINUX, EMSCRIPTEN);
macro int errno()                 @feat(LINUX, EMSCRIPTEN) => *__errno_location();
macro void errno_set(int err)     @feat(LINUX, EMSCRIPTEN) => *(__errno_location()) = err;

// Android 9
extern fn int* __errno()          @feat(ANDROID);
macro int errno()                 @feat(ANDROID) => *__errno();
macro void errno_set(int err)     @feat(ANDROID) => *(__errno()) = err;

// OpenBSD and NetBSD
extern fn int* __errno()          @feat(OPENBSD, NETBSD);
macro int errno()                 @feat(OPENBSD, NETBSD) => *__errno();
macro void errno_set(int err)     @feat(OPENBSD, NETBSD) => *(__errno()) = err;

// Darwin
extern fn int* __error()          @feat(DARWIN, FREEBSD);
macro int errno()                 @feat(DARWIN, FREEBSD) => *__error();
macro void errno_set(int err)     @feat(DARWIN, FREEBSD) => *(__error()) = err;

// Win32
macro int errno()                 @feat(WIN32)
{
	int holder;
	_get_errno(&holder);
	return holder;
}
extern fn void _get_errno(int* result) @feat(WIN32);
extern fn void _set_errno(int err)     @feat(WIN32);
macro void errno_set(int err)          @feat(WIN32) => _set_errno(err);

// Default
tlocal int _errno_c3 = 0;
macro void errno_set(int err) @weak => _errno_c3 = err;
macro int errno() @weak => _errno_c3;
