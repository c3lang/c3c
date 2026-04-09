module std::io::os;
import libc;
import std::io::path;
import std::os::win32;
import std::os::posix;


macro bool? native_mkdir(Path path, MkdirPermissions permissions)
{
	@pool()
	{
	$switch:
		$case env::POSIX:
			if (!posix::mkdir(path.str_view().zstr_tcopy(), permissions == NORMAL ? 0o777 : 0o700)) return true;
			switch (libc::errno())
			{
				case EACCES:
				case EPERM:
				case EROFS:
				case EFAULT: return io::NO_PERMISSION~;
				case ENAMETOOLONG: return io::NAME_TOO_LONG~;
				case EDQUOT:
				case ENOSPC: return io::OUT_OF_SPACE~;
				case EISDIR:
				case EEXIST: return false;
				case ELOOP: return io::SYMLINK_FAILED~;
				case ENOTDIR: return io::FILE_NOT_FOUND~;
				case ENOENT: return io::PARENT_DIR_MISSING~;
				default:
					return io::GENERAL_ERROR~;
			}
		$case env::WIN32:
				// TODO security attributes
				if (win32::createDirectoryW(path.str_view().to_temp_utf16()!!, null)) return true;
				switch (win32::getLastError())
				{
					case win32::ERROR_ACCESS_DENIED:
						return io::NO_PERMISSION~;
					case win32::ERROR_DISK_FULL:
						return io::OUT_OF_SPACE~;
					case win32::ERROR_ALREADY_EXISTS:
						return false;
					case win32::ERROR_PATH_NOT_FOUND:
						return io::FILE_NOT_FOUND~;
					default:
						return io::GENERAL_ERROR~;
				}
		$default:
			return io::UNSUPPORTED_OPERATION~;
	$endswitch
	};
}