module std::io::os;
import libc;
import std::io::path;
import std::os::win32;
import std::os::posix;

const MAX_LINK_LEN = 64 * 1024;

fn void? native_symlink(Path target, Path link)
{
	@pool()
	{
	$switch:
		$case env::POSIX:
			if (!posix::symlink(target.zstr_tcopy(), link.zstr_tcopy())) return;
			switch (libc::errno())
			{
				case EEXIST: return io::ALREADY_EXISTS~;
				case EACCES:
				case EPERM:
				case EROFS:
				case EFAULT: return io::NO_PERMISSION~;
				case ENAMETOOLONG: return io::NAME_TOO_LONG~;
				case ENOTDIR:
				case ENOENT: return io::FILE_NOT_FOUND~;
				case EDQUOT:
				case ENOSPC: return io::OUT_OF_SPACE~;
				case ELOOP: return io::SYMLINK_FAILED~;
				default: return io::GENERAL_ERROR~;
			}
		$case env::WIN32:
			// Windows needs to know up front whether the target is a directory.
			// A relative target is relative to the directory containing the link.
			bool is_dir;
			if (target.is_absolute()!!)
			{
				is_dir = native_is_dir(target);
			}
			else
			{
				String dir = link.dirname();
				is_dir = native_is_dir(dir.len ? (String)path::tnew(dir).tappend(target)!! : target);
			}
			Win32_DWORD flags = is_dir ? win32::SYMBOLIC_LINK_FLAG_DIRECTORY : 0;
			Char16* link16 = link.to_temp_utf16()!.ptr;
			Char16* target16 = target.to_temp_utf16()!.ptr;
			if (win32::createSymbolicLinkW(link16, target16, flags | win32::SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE)) return;
			Win32_DWORD error = win32::getLastError();
			// Older Windows versions reject the unprivileged flag, retry without it.
			if (error == win32::ERROR_INVALID_PARAMETER)
			{
				if (win32::createSymbolicLinkW(link16, target16, flags)) return;
				error = win32::getLastError();
			}
			switch (error)
			{
				case win32::ERROR_ALREADY_EXISTS:
				case win32::ERROR_FILE_EXISTS:
					return io::ALREADY_EXISTS~;
				case win32::ERROR_ACCESS_DENIED:
				case win32::ERROR_PRIVILEGE_NOT_HELD:
					return io::NO_PERMISSION~;
				case win32::ERROR_FILE_NOT_FOUND:
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

fn Path? native_readlink(Allocator allocator, Path link)
{
	$switch:
		$case env::POSIX:
			@stack_mem(1024; smem)
			{
				ZString link_z = link.zstr_copy(smem);
				sz size = 256;
				char[] buffer = alloc::alloc_array_try(smem, char, size)!;
				while (true)
				{
					sz len = posix::readlink(link_z, buffer.ptr, (usz)size);
					if (len < 0)
					{
						switch (libc::errno())
						{
							case EINVAL: return io::FILE_NOT_VALID~;
							case EACCES:
							case EPERM: return io::NO_PERMISSION~;
							case ENAMETOOLONG: return io::NAME_TOO_LONG~;
							case ENOTDIR:
							case ENOENT: return io::FILE_NOT_FOUND~;
							case ELOOP: return io::SYMLINK_FAILED~;
							default: return io::GENERAL_ERROR~;
						}
					}
					// A full buffer means the target may have been truncated.
					if (len < size) return path::new(allocator, (String)buffer[:len]);
					if (size >= MAX_LINK_LEN) return io::NAME_TOO_LONG~;
					size *= 2;
					buffer = alloc::alloc_array_try(smem, char, size)!;
				}
			};
		$case env::WIN32:
			@pool()
			{
				Win32_HANDLE handle = win32::createFileW(link.to_temp_utf16()!.ptr, 0,
					win32::FILE_SHARE_READ | win32::FILE_SHARE_WRITE | win32::FILE_SHARE_DELETE, null,
					win32::OPEN_EXISTING, win32::FILE_FLAG_OPEN_REPARSE_POINT | win32::FILE_FLAG_BACKUP_SEMANTICS, null);
				if (handle == win32::INVALID_HANDLE_VALUE)
				{
					switch (win32::getLastError())
					{
						case win32::ERROR_ACCESS_DENIED: return io::NO_PERMISSION~;
						case win32::ERROR_FILE_NOT_FOUND:
						case win32::ERROR_PATH_NOT_FOUND: return io::FILE_NOT_FOUND~;
						default: return io::GENERAL_ERROR~;
					}
				}
				defer win32::closeHandle(handle);
				char[] buffer = alloc::alloc_array_try(tmem, char, win32::MAXIMUM_REPARSE_DATA_BUFFER_SIZE)!;
				Win32_DWORD returned;
				if (!win32::deviceIoControl(handle, win32::FSCTL_GET_REPARSE_POINT, null, 0,
					buffer.ptr, (Win32_DWORD)buffer.len, &returned, null))
				{
					switch (win32::getLastError())
					{
						case win32::ERROR_NOT_A_REPARSE_POINT: return io::FILE_NOT_VALID~;
						case win32::ERROR_ACCESS_DENIED: return io::NO_PERMISSION~;
						default: return io::GENERAL_ERROR~;
					}
				}
				Win32_REPARSE_SYMLINK_BUFFER* data = (Win32_REPARSE_SYMLINK_BUFFER*)buffer.ptr;
				if (data.reparseTag != win32::IO_REPARSE_TAG_SYMLINK) return io::FILE_NOT_VALID~;
				Char16* names = &data.pathBuffer[0];
				Char16[] name;
				if (data.printNameLength)
				{
					name = names[data.printNameOffset / 2 : data.printNameLength / 2];
				}
				else
				{
					name = names[data.substituteNameOffset / 2 : data.substituteNameLength / 2];
					// Strip the NT namespace prefix "\??\"
					if (name.len >= 4 && name[0] == '\\' && name[1] == '?' && name[2] == '?' && name[3] == '\\') name = name[4..];
				}
				return path::new(allocator, string::tfrom_utf16(name)!);
			};
		$default:
			return io::UNSUPPORTED_OPERATION~;
	$endswitch
}
