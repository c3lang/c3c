module std::io::os;
import libc;

macro void! native_chdir(Path path)
{
	$switch
		$case env::POSIX:
			if (posix::chdir(path.as_zstr()))
			{
				switch (libc::errno())
				{
					case errno::EACCES: return IoError.NO_PERMISSION?;
					case errno::ENAMETOOLONG: return IoError.NAME_TOO_LONG?;
					case errno::ENOTDIR: return IoError.FILE_NOT_DIR?;
					case errno::ENOENT: return IoError.FILE_NOT_FOUND?;
					case errno::ELOOP: return IoError.SYMLINK_FAILED?;
					default: return IoError.GENERAL_ERROR?;
				}
			}
		$case env::WIN32:
			@pool()
			{
				// TODO improve with better error handling.
				if (win32::setCurrentDirectoryW(path.as_str().to_temp_utf16()!!)) return;
			};
			return IoError.GENERAL_ERROR?;
		$default:
			unreachable("'getcwd' not available");
	$endswitch
}
