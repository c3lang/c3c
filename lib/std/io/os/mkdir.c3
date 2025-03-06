module std::io::os;
import libc;
import std::io::path;
import std::os::win32;
import std::os::posix;


macro bool? native_mkdir(Path path, MkdirPermissions permissions)
{
	$switch:
		$case env::POSIX:
			if (!posix::mkdir(path.as_zstr(), permissions == NORMAL ? 0o777 : 0o700)) return true;
			switch (libc::errno())
			{
				case errno::EACCES:
				case errno::EPERM:
				case errno::EROFS:
				case errno::EFAULT: return io::NO_PERMISSION?;
				case errno::ENAMETOOLONG: return io::NAME_TOO_LONG?;
				case errno::EDQUOT:
				case errno::ENOSPC: return io::OUT_OF_SPACE?;
				case errno::EISDIR:
				case errno::EEXIST: return false;
				case errno::ELOOP: return io::SYMLINK_FAILED?;
				case errno::ENOTDIR: return io::FILE_NOT_FOUND?;
				default: return io::GENERAL_ERROR?;
			}
		$case env::WIN32:
			@pool()
			{
				// TODO security attributes
				if (win32::createDirectoryW(path.str_view().to_temp_utf16()!!, null)) return true;
				switch (win32::getLastError())
				{
					case win32::ERROR_ACCESS_DENIED:
						return io::NO_PERMISSION?;
					case win32::ERROR_DISK_FULL:
						return io::OUT_OF_SPACE?;
					case win32::ERROR_ALREADY_EXISTS:
						return false;
					case win32::ERROR_PATH_NOT_FOUND:
						return io::FILE_NOT_FOUND?;
					default:
						return io::GENERAL_ERROR?;
				}
			};
		$default:
			return io::UNSUPPORTED_OPERATION?;
	$endswitch
}