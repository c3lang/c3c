module libc::os;

$switch (env::OS_TYPE):

$case LINUX:

extern fn int* __errno_location();

macro int errno() => *__errno_location();
macro void errno_set(int err) => *(__errno_location()) = err;

$case MACOSX:

extern fn int* __error();
macro int errno() => *__error();
macro void errno_set(int err) => *(__error()) = err;

$case WIN32:

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

macro int errno() => 1;
fn void errno_set(int err) {}

$endswitch;
