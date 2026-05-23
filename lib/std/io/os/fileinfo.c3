module std::io::os;
import libc, std::os, std::io;

fn void? native_stat(Stat* stat, String path) @if(env::DARWIN || env::LINUX || env::ANDROID || env::BSD_FAMILY || env::EMSCRIPTEN) => @pool()
{
	$if env::DARWIN || env::LINUX || env::ANDROID || env::BSD_FAMILY || env::EMSCRIPTEN:
		int res = libc::stat(path.zstr_tcopy(), stat);
	$else
		unreachable("Stat unimplemented");
		int res = 0;
	$endif
	if (res != 0)
	{
		switch (libc::errno())
		{
			case EBADF:        return io::FILE_NOT_VALID~;
			case EFAULT:       unreachable("Invalid stat");
			case EIO:          return io::GENERAL_ERROR~;
			case EACCES:       return io::NO_PERMISSION~;
			case ELOOP:        return io::NO_PERMISSION~;
			case ENAMETOOLONG: return io::NAME_TOO_LONG~;
			case ENOENT:       return io::FILE_NOT_FOUND~;
			case ENOTDIR:      return io::FILE_NOT_DIR~;
			case EOVERFLOW:    return io::GENERAL_ERROR~;
			default:           return io::UNKNOWN_ERROR~;
		}
	}
}

fn long? native_file_size(String path)
{
	$switch:
		$case env::WIN32:
			Win32_FILE_ATTRIBUTE_DATA data;
			get_win32_file_attributes(path, &data)!;
			if (data.dwFileAttributes & win32::FILE_ATTRIBUTE_DIRECTORY) return io::FILE_IS_DIR~;
			Win32_LARGE_INTEGER size;
			size.lowPart = (Win32_DWORD)data.nFileSizeLow;
			size.highPart = (Win32_LONG)data.nFileSizeHigh;
			return (long)size.quadPart;
		$case env::DARWIN || env::LINUX || env::ANDROID || env::BSD_FAMILY  || env::EMSCRIPTEN:
			Stat stat;
			native_stat(&stat, path)!;
			if (libc_S_ISTYPE(stat.st_mode, libc::S_IFDIR)) return io::FILE_IS_DIR~;
			return stat.st_size;
		$default:
			File f = file::open(path, "r")!;
			defer (void)f.close();
			f.seek(0, FROM_END)!;
			return f.cursor();
	$endswitch

}

fn bool native_file_or_dir_exists(String path)
{
	$switch:
		$case env::DARWIN:
		$case env::FREEBSD:
		$case env::NETBSD:
		$case env::OPENBSD:
		$case env::LINUX:
		$case env::ANDROID:
		$case env::EMSCRIPTEN:
			Stat stat;
			return @ok(native_stat(&stat, path));
		$case env::WIN32:
			@pool()
			{
				return (bool)win32::pathFileExistsW(path.to_temp_utf16()) ?? false;
			};
		$case env::POSIX:
			@pool()
			{
				return posix::access(path.zstr_tcopy(), 0 /* F_OK */) != -1;
			};
		$default:
			unreachable("Not supported");
	$endswitch
}

fn bool native_is_file(String path)
{
	$switch:
		$case env::DARWIN:
		$case env::FREEBSD:
		$case env::NETBSD:
		$case env::OPENBSD:
		$case env::LINUX:
		$case env::ANDROID:
		$case env::EMSCRIPTEN:
			Stat stat;
			return @ok(native_stat(&stat, path)) && libc_S_ISTYPE(stat.st_mode, libc::S_IFREG);
		$default:
			File? f = file::open(path, "r");
			defer (void)f.close();
			return @ok(f);
	$endswitch
}

fn bool native_is_dir(String path)
{
	$if env::DARWIN || env::LINUX || env::ANDROID || env::BSD_FAMILY || env::EMSCRIPTEN:
		Stat stat;
		return @ok(native_stat(&stat, path)) && libc_S_ISTYPE(stat.st_mode, libc::S_IFDIR);
	$else
		return native_file_or_dir_exists(path) && !native_is_file(path);
	$endif
}

fn Time_t? native_get_modified_time(String path) => @pool()
{
	$switch:
		$case env::WIN32:
			Win32_FILE_ATTRIBUTE_DATA data;
			get_win32_file_attributes(path, &data)!;
			ulong ticks = ((ulong)data.ftLastWriteTime.dwHighDateTime << 32) | data.ftLastWriteTime.dwLowDateTime;
			return (Time_t)(ticks / 10_000_000UL - 11644473600UL);
		$case env::DARWIN:
			Stat stat;
			native_stat(&stat, path)!;
			return stat.st_mtimespec.s;
		$case env::LINUX:
		$case env::ANDROID:
		$case env::NETBSD:
		$case env::EMSCRIPTEN:
			Stat stat;
			native_stat(&stat, path)!;
			return (Time_t)stat.st_mtime;
		$case env::BSD_FAMILY:
			Stat stat;
			native_stat(&stat, path)!;
			return stat.st_mtime.s;
		$default:
			return io::UNSUPPORTED_OPERATION~;
	$endswitch
}


macro fault file_attr_errno() @local @if(env::WIN32)
{
	switch (win32::getLastError())
	{
		case win32::ERROR_FILE_NOT_FOUND:        return io::FILE_NOT_FOUND;
		case win32::ERROR_PATH_NOT_FOUND:        return io::PATH_COULD_NOT_BE_FOUND;
		case win32::ERROR_BAD_NETPATH:           return io::PATH_COULD_NOT_BE_FOUND;
		case win32::ERROR_ACCESS_DENIED:         return io::NO_PERMISSION;
		case win32::ERROR_SHARING_VIOLATION:     return io::BUSY;
		case win32::ERROR_LOCK_VIOLATION:        return io::BUSY;
		case win32::ERROR_TOO_MANY_OPEN_FILES:   return io::TOO_MANY_DESCRIPTORS;
		case win32::ERROR_FILENAME_EXCED_RANGE:  return io::NAME_TOO_LONG;
		case win32::ERROR_BUFFER_OVERFLOW:       return io::NAME_TOO_LONG;
		case win32::ERROR_INVALID_NAME:          return io::ILLEGAL_ARGUMENT;
		case win32::ERROR_BAD_PATHNAME:          return io::ILLEGAL_ARGUMENT;
		case win32::ERROR_DIRECTORY:             return io::FILE_NOT_DIR;
		case win32::ERROR_INVALID_HANDLE:        return io::FILE_NOT_VALID;
		case win32::ERROR_NOT_READY:             return io::BUSY;
		case win32::ERROR_WRITE_PROTECT:         return io::READ_ONLY;
		default:                                 return io::UNKNOWN_ERROR;
	}
}

fn void? get_win32_file_attributes(String path, Win32_FILE_ATTRIBUTE_DATA* data_ptr) @local @if(env::WIN32) => @pool()
{
	if (!win32::getFileAttributesExW(path.to_temp_wstring()!, Win32_GET_FILEEX_INFO_LEVELS.STANDARD, data_ptr))
	{
		return file_attr_errno()~;
	}
}
