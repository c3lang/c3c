module std::io::os;
import libc;


$switch
	$case !env::COMPILER_LIBC_AVAILABLE:

fn void! native_chdir(Path path)
{
	unreachable("'getcwd' not available");
}

	$case env::os_is_win32():

macro void! native_chdir(Path path)
{
	@pool()
	{
		 if (files::win32_SetCurrentDirectoryW(path.as_str().to_temp_utf16()!!)) return;
	};
	return IoError.GENERAL_ERROR!;
}

	$default:

extern fn int _chdir(ZString) @extern("chdir");
macro void! native_chdir(Path p)
{
	if (_chdir((ZString)p.as_str()))
	{
		switch (libc::errno())
		{
			case errno::EACCES: return IoError.NO_PERMISSION!;
			case errno::ENAMETOOLONG: return IoError.NAME_TOO_LONG!;
			case errno::ENOTDIR: return IoError.FILE_NOT_DIR!;
			case errno::ENOENT: return IoError.FILE_NOT_FOUND!;
			case errno::ELOOP: return IoError.SYMLINK_FAILED!;
			default: return IoError.GENERAL_ERROR!;
		}
	}
}

$endswitch