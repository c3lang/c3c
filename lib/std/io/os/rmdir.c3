module std::io::os;
import libc;
import std::io::path;
import std::os::win32;
import std::os::posix;

macro bool? native_rmdir(Path path)
{
	$switch:
		$case env::POSIX:
			if (!posix::rmdir(path.as_zstr())) return true;
			switch (libc::errno())
			{
				case errno::EBUSY: return io::BUSY?;
				case errno::EACCES:
				case errno::EPERM:
				case errno::EROFS:
				case errno::EFAULT: return io::NO_PERMISSION?;
				case errno::ENAMETOOLONG: return io::NAME_TOO_LONG?;
				case errno::ENOTDIR:
				case errno::ENOENT: return false;
				case errno::ENOTEMPTY: return io::DIR_NOT_EMPTY?;
				case errno::ELOOP: return io::SYMLINK_FAILED?;
				default: return io::GENERAL_ERROR?;
			}
		$case env::WIN32:
			@pool()
			{
				if (win32::removeDirectoryW(path.str_view().to_temp_utf16()!!)) return true;
				switch (win32::getLastError())
				{
					case win32::ERROR_ACCESS_DENIED:
						return io::NO_PERMISSION?;
					case win32::ERROR_CURRENT_DIRECTORY:
						return io::BUSY?;
					case win32::ERROR_DIR_NOT_EMPTY:
						return io::DIR_NOT_EMPTY?;
					case win32::ERROR_DIRECTORY:
					case win32::ERROR_PATH_NOT_FOUND:
						return false;
					default:
						return io::GENERAL_ERROR?;
				}
			};
		$default:
			return io::UNSUPPORTED_OPERATION?;
	$endswitch
}
