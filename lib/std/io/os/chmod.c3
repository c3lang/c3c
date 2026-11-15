module std::io::os;
import libc, std::io::file;
import std::os::win32;
import std::os::posix;

fn void? native_chmod(String path, FilePermission mode) => @pool()
{
	$switch:
		$case env::POSIX:
			if (posix::chmod(path.zstr_tcopy(), (uint)mode)) return chmod_errno()~;
		$case env::WIN32:
			WString wpath = path.to_temp_wstring()!;
			Win32_DWORD attributes = win32::getFileAttributesW(wpath);
			if (attributes == win32::INVALID_FILE_ATTRIBUTES) return file_attr_errno()~;
			Win32_DWORD new_attributes = win32_attributes_for_mode(attributes, mode);
			if (new_attributes == attributes) return;
			if (!win32::setFileAttributesW(wpath, new_attributes)) return file_attr_errno()~;
		$default:
			return io::UNSUPPORTED_OPERATION~;
	$endswitch
}

fn void? native_fchmod(CFile file, FilePermission mode) @feat(LIBC)
{
	$switch:
		$case env::POSIX:
			if (posix::fchmod(libc::fileno(file), (uint)mode)) return chmod_errno()~;
		$case env::WIN32:
			Win32_HANDLE handle = (Win32_HANDLE)win32::_get_osfhandle(libc::fileno(file));
			if (handle == win32::INVALID_HANDLE_VALUE) return io::FILE_NOT_VALID~;
			Win32_FILE_BASIC_INFO info;
			if (!win32::getFileInformationByHandleEx(handle, FILE_BASIC_INFO, &info, Win32_FILE_BASIC_INFO::size)) return file_attr_errno()~;
			Win32_DWORD attributes = win32_attributes_for_mode(info.fileAttributes, mode);
			if (attributes == info.fileAttributes) return;
			// Zero times are left unchanged.
			info = { .fileAttributes = attributes };
			if (!win32::setFileInformationByHandle(handle, FILE_BASIC_INFO, &info, Win32_FILE_BASIC_INFO::size)) return file_attr_errno()~;
		$default:
			return io::UNSUPPORTED_OPERATION~;
	$endswitch
}

macro Win32_DWORD win32_attributes_for_mode(Win32_DWORD attributes, FilePermission mode) @local @feat(WIN32)
{
	attributes &= ~win32::FILE_ATTRIBUTE_NORMAL;
	if (mode & USER_WRITE)
	{
		attributes &= ~win32::FILE_ATTRIBUTE_READONLY;
	}
	else
	{
		attributes |= win32::FILE_ATTRIBUTE_READONLY;
	}
	return attributes ? attributes : win32::FILE_ATTRIBUTE_NORMAL;
}

macro fault chmod_errno() @local @feat(POSIX)
{
	switch (libc::errno())
	{
		case EACCES:
		case EPERM: return io::NO_PERMISSION;
		case EROFS: return io::READ_ONLY;
		case ENAMETOOLONG: return io::NAME_TOO_LONG;
		case ENOTDIR:
		case ENOENT: return io::FILE_NOT_FOUND;
		case ELOOP: return io::SYMLINK_FAILED;
		case EBADF: return io::FILE_NOT_VALID;
		case EINTR: return io::INTERRUPTED;
		default: return io::GENERAL_ERROR;
	}
}
