module std::io::os;
import libc;

$switch

$case env::COMPILER_LIBC_AVAILABLE && env::os_is_win32():

macro String! getcwd(Allocator* using = mem::heap())
{
	const DEFAULT_BUFFER = 256;
	Char16[DEFAULT_BUFFER] buffer;
	Char16 *res = win32::_wgetcwd(&buffer, DEFAULT_BUFFER);
	bool free = false;
	defer if (free) libc::free(res);
	if (!res)
	{
	    if (libc::errno() != errno::ERANGE) return IoError.GENERAL_ERROR?;
		res = win32::_wgetcwd(null, 0);
		free = true;
	}
	Char16[] str16 = res[:win32::wcslen(res)];
	return string::from_utf16(str16, using);
}

$case env::COMPILER_LIBC_AVAILABLE && env::os_is_posix():

macro String! getcwd(Allocator* using = mem::heap())
{
	const usz DEFAULT_BUFFER = 256;
	char[DEFAULT_BUFFER] buffer;
	ZString res = posix::getcwd(&buffer, DEFAULT_BUFFER);
	bool free = false;
	if (!res)
	{
		// Improve error
		if (libc::errno() != errno::ERANGE) return IoError.GENERAL_ERROR?;
		res = posix::getcwd(null, 0);
		free = true;
	}
	defer if (free) libc::free((void*)res);
	return res.copy(using);
}

$default:

fn String! getcwd(Allocator* using = mem::heap())
{
	unreachable("'getcwd' not available");
}

$endswitch