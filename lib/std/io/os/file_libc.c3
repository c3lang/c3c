module std::io::os @if(env::LIBC);
import libc;

<*
 @require mode.len > 0
 @require filename.len > 0
*>
fn void*? native_fopen(String filename, String mode) @inline => @pool()
{
	$if env::WIN32:
		void* file = libc::_wfopen(filename.to_temp_wstring(), mode.to_temp_wstring())!;
	$else
		void* file = libc::fopen(filename.zstr_tcopy(), mode.zstr_tcopy());
	$endif
		return file ?: file_open_errno()?;
}

fn void? native_remove(String filename) => @pool()
{
	$if env::WIN32:
		CInt result = libc::_wremove(filename.to_temp_wstring())!;
	$else
		CInt result = libc::remove(filename.zstr_tcopy());
	$endif
	if (result)
	{
		switch (libc::errno())
		{
			case errno::ENOENT:
				return io::FILE_NOT_FOUND?;
			case errno::EACCES:
			default:
				return io::FILE_CANNOT_DELETE?;
		}
	}
}

<*
 @require mode.len > 0
 @require filename.len > 0
*>
fn void*? native_freopen(void* file, String filename, String mode) @inline => @pool()
{
	$if env::WIN32:
		file = libc::_wfreopen(filename.to_temp_wstring(), mode.to_temp_wstring(), file)!;
	$else
		file = libc::freopen(filename.zstr_tcopy(), mode.zstr_tcopy(), file);
	$endif
		return file ?: file_open_errno()?;
}

fn void? native_fseek(void* file, isz offset, Seek seek_mode) @inline
{
	if (libc::fseek(file, (SeekIndex)offset, seek_mode.ordinal)) return file_seek_errno()?;
}


fn usz? native_ftell(CFile file) @inline
{
	long index = libc::ftell(file);
	return index >= 0 ? (usz)index : file_seek_errno()?;
}

fn usz? native_fwrite(CFile file, char[] buffer) @inline
{
	return libc::fwrite(buffer.ptr, 1, buffer.len, file);
}

fn void? native_fputc(CInt c, CFile stream) @inline
{
	if (libc::fputc(c, stream) == libc::EOF) return io::EOF?;
}

fn usz? native_fread(CFile file, char[] buffer) @inline
{
	return libc::fread(buffer.ptr, 1, buffer.len, file);
}

macro fault file_open_errno() @local
{
	switch (libc::errno())
	{
		case errno::EACCES: return io::NO_PERMISSION;
		case errno::EDQUOT: return io::OUT_OF_SPACE;
		case errno::EBADF: return io::FILE_NOT_VALID;
		case errno::EEXIST: return io::ALREADY_EXISTS;
		case errno::EINTR: return io::INTERRUPTED;
		case errno::EFAULT: return io::GENERAL_ERROR;
		case errno::EISDIR: return io::FILE_IS_DIR;
		case errno::ELOOP: return io::SYMLINK_FAILED;
		case errno::EMFILE: return io::TOO_MANY_DESCRIPTORS;
		case errno::ENAMETOOLONG: return io::NAME_TOO_LONG;
		case errno::ENFILE: return io::OUT_OF_SPACE;
		case errno::ENOTDIR: return io::FILE_NOT_DIR;
		case errno::ENOENT: return io::FILE_NOT_FOUND;
		case errno::ENOSPC: return io::OUT_OF_SPACE;
		case errno::ENXIO: return io::FILE_NOT_FOUND;
		case errno::EOVERFLOW: return io::OVERFLOW;
		case errno::EROFS: return io::READ_ONLY;
		case errno::EOPNOTSUPP: return io::UNSUPPORTED_OPERATION;
		case errno::EIO: return io::INCOMPLETE_WRITE;
		case errno::EWOULDBLOCK: return io::WOULD_BLOCK;
		default: return io::UNKNOWN_ERROR;
	}
}

macro fault file_seek_errno() @local
{
	switch (libc::errno())
	{
		case errno::ESPIPE: return io::FILE_IS_PIPE;
		case errno::EPIPE: return io::FILE_IS_PIPE;
		case errno::EOVERFLOW: return io::OVERFLOW;
		case errno::ENXIO: return io::FILE_NOT_FOUND;
		case errno::ENOSPC: return io::OUT_OF_SPACE;
		case errno::EIO: return io::INCOMPLETE_WRITE;
		case errno::EINVAL: return io::INVALID_POSITION;
		case errno::EINTR: return io::INTERRUPTED;
		case errno::EFBIG: return io::OUT_OF_SPACE;
		case errno::EBADF: return io::FILE_NOT_VALID;
		case errno::EAGAIN: return io::WOULD_BLOCK;
		default: return io::UNKNOWN_ERROR;
	}
}

