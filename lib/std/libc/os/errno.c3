module libc::os;


$switch

$case env::COMPILER_LIBC_AVAILABLE && env::OS_TYPE == LINUX:

extern fn int* __errno_location();
macro int errno() => *__errno_location();
macro void errno_set(int err) => *(__errno_location()) = err;

$case env::COMPILER_LIBC_AVAILABLE && env::OS_TYPE == MACOSX:

extern fn int* __error();
macro int errno() => *__error();
macro void errno_set(int err) => *(__error()) = err;

$case env::COMPILER_LIBC_AVAILABLE && env::OS_TYPE == WIN32:

macro int errno()
{
	int holder;
	_get_errno(&holder);
	return holder;
}

macro void errno_set(int err) => _set_errno(err);

extern fn void _get_errno(int* result);
extern fn void _set_errno(int err);

$default:

tlocal int _errno_c3 = 0;
fn void errno_set(int err) => _errno_c3 = err;
fn int errno() => _errno_c3;

$endswitch
