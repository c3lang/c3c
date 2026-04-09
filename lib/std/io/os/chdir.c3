module std::io::os;
import std::io::path, libc, std::os;

macro void? native_chdir(Path path)
{
	@pool()
	{
		$switch:
			$case env::POSIX:
				if (posix::chdir(path.str_view().zstr_tcopy()))
				{
					switch (libc::errno())
					{
						case EACCES:       return io::NO_PERMISSION~;
						case ENAMETOOLONG: return io::NAME_TOO_LONG~;
						case ENOTDIR:      return io::FILE_NOT_DIR~;
						case ENOENT:       return io::FILE_NOT_FOUND~;
						case ELOOP:        return io::SYMLINK_FAILED~;
						default:           return io::GENERAL_ERROR~;
					}
				}
			$case env::WIN32:
				// TODO improve with better error handling.
				if (win32::setCurrentDirectoryW(path.str_view().to_temp_utf16()!!)) return;
				return io::GENERAL_ERROR~;
			$default:
				return io::UNSUPPORTED_OPERATION~;
		$endswitch
	};
}
