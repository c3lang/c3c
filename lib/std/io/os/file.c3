module std::io::os;
import libc;

typedef FopenFn = fn void*!(String, String);
typedef FreopenFn = fn void*!(void*, String, String);
typedef FcloseFn = fn void!(void*);
typedef FseekFn = fn void!(void*, isz, Seek);
typedef FtellFn = fn usz!(void*);
typedef FwriteFn = fn usz!(void*, char[] buffer);
typedef FreadFn = fn usz!(void*, char[] buffer);

$if (!$defined(native_fopen_fn))
FopenFn native_fopen_fn @weak;
$endif
$if (!$defined(native_fclose_fn))
FcloseFn native_fclose_fn @weak;
$endif
$if (!$defined(native_freopen_fn))
FreopenFn native_freopen_fn @weak;
$endif
$if (!$defined(native_fseek_fn))
FseekFn native_fseek_fn @weak;
$endif
$if (!$defined(native_ftell_fn))
FtellFn native_ftell_fn @weak;
$endif
$if (!$defined(native_fwrite_fn))
FwriteFn native_fwrite_fn @weak;
$endif
$if (!$defined(native_fread_fn))
FreadFn native_fread_fn @weak;
$endif


/**
 * @require mode.len > 0
 * @require filename.len > 0
 **/
fn void*! native_fopen(String filename, String mode) @inline
{
$if (!env::COMPILER_LIBC_AVAILABLE)
		if (native_fopen_fn) return native_fopen_fn(filename, mode);
		unreachable("Tried to call fopen without support.");
$else
	@pool()
	{
	$if (env::os_is_win32())
		void* file = (CFile)_wfopen(filename.to_temp_utf16(), filename.to_temp_utf16())?;
	$else
		void* file = libc::fopen(filename.zstr_tcopy(), mode.zstr_tcopy());
	$endif
		return file ?: file_open_errno()!;
	};
$endif
}

/**
 * @require mode.len > 0
 * @require filename.len > 0
 **/
fn void*! native_freopen(void* file, String filename, String mode) @inline
{
$if (!env::COMPILER_LIBC_AVAILABLE)
		if (native_freopen_fn) return native_freopen_fn(file, filename, mode);
		unreachable("Tried to call freopen without support.");
$else
	@pool()
	{
	$if (env::os_is_win32())
		file = _wfreopen(filename.to_temp_utf16(), mode.to_temp_utf16(), file)?;
	$else
		file = libc::freopen(filename.zstr_tcopy(), mode.zstr_tcopy(), file);
	$endif
		return file ?: file_open_errno()!;
	};
$endif
}

fn void! native_fseek(void* file, isz offset, Seek seek_mode) @inline
{
$if (!env::COMPILER_LIBC_AVAILABLE)
	if (native_fseek_fn) return native_fseek_fn(file, offset, seek_mode);
	unreachable("Tried to call fseek without support.");
$else
	$if (env::os_is_win32())
		bool success = _fseeki64(file, (long)offset, (int)seek_mode) == 0;
	$else
		bool success = libc::fseek(file, (SeekIndex)offset, (CInt)seek_mode) == 0;
	$endif
		if (!success) return file_seek_errno()!;
$endif
}

fn usz! native_ftell(CFile file) @inline
{
$if (!env::COMPILER_LIBC_AVAILABLE)
	if (native_ftell_fn) return native_ftell_fn(file);
	unreachable("Tried to call ftell without support.");
$else
	$if (env::os_is_win32())
		long index = _ftelli64(file);
		return index >= 0 ? index : file_seek_errno()!;
	$else
		SeekIndex index = libc::ftell(file);
		return index >= 0 ? index : file_seek_errno()!;
	$endif
$endif
}

fn usz! native_fwrite(CFile file, char[] buffer) @inline
{
$if (!env::COMPILER_LIBC_AVAILABLE)
	if (native_fwrite_fn) return native_fwrite_fn(file, buffer);
	unreachable("Tried to call fwrite without support.");
$else
    return libc::fwrite(buffer.ptr, 1, buffer.len, file);
$endif
}

fn usz! native_fread(CFile file, char[] buffer) @inline
{
$if (!env::COMPILER_LIBC_AVAILABLE)
	if (native_fread_fn) return native_fread_fn(file, buffer);
	unreachable("Tried to call fread without support.");
$else
    return libc::fread(buffer.ptr, 1, buffer.len, file);
$endif
}

macro anyerr file_open_errno() @local
{
	switch (libc::errno())
	{
	    case errno::EACCES: return IoError.NO_PERMISSION;
	    case errno::EDQUOT: return IoError.OUT_OF_DISK;
	    case errno::EBADF: return IoError.FILE_NOT_VALID;
	    case errno::EEXIST: return IoError.ALREADY_EXISTS;
	    case errno::EINTR: return IoError.INTERRUPTED;
	    case errno::EFAULT: return IoError.GENERAL_ERROR;
	    case errno::EISDIR: return IoError.FILE_IS_DIR;
	    case errno::ELOOP: return IoError.SYMLINK_FAILED;
	    case errno::EMFILE: return IoError.TOO_MANY_DESCRIPTORS;
	    case errno::ENAMETOOLONG: return IoError.NAME_TOO_LONG;
	    case errno::ENFILE: return IoError.OUT_OF_DISK;
	    case errno::ENOTDIR: return IoError.FILE_NOT_DIR;
		case errno::ENOENT: return IoError.FILE_NOT_FOUND;
	    case errno::ENOSPC: return IoError.OUT_OF_DISK;
		case errno::ENXIO: return IoError.FILE_NOT_FOUND;
		case errno::EOVERFLOW: return IoError.OVERFLOW;
		case errno::EROFS: return IoError.READ_ONLY;
		case errno::EOPNOTSUPP: return IoError.UNSUPPORTED_OPERATION;
	    case errno::EIO: return IoError.INCOMPLETE_WRITE;
	    case errno::EWOULDBLOCK: return IoError.WOULD_BLOCK;
	    default: return IoError.UNKNOWN_ERROR;
	}
}

macro anyerr file_seek_errno() @local
{
	switch (libc::errno())
	{
		case errno::ESPIPE: return IoError.FILE_IS_PIPE;
		case errno::EPIPE: return IoError.FILE_IS_PIPE;
		case errno::EOVERFLOW: return IoError.OVERFLOW;
	    case errno::ENXIO: return IoError.FILE_NOT_FOUND;
	    case errno::ENOSPC: return IoError.OUT_OF_DISK;
        case errno::EIO: return IoError.INCOMPLETE_WRITE;
		case errno::EINVAL: return IoError.INVALID_POSITION;
	    case errno::EINTR: return IoError.INTERRUPTED;
	    case errno::EFBIG: return IoError.OUT_OF_DISK;
	    case errno::EBADF: return IoError.FILE_NOT_VALID;
	    case errno::EAGAIN: return IoError.WOULD_BLOCK;
	    default: return IoError.UNKNOWN_ERROR;
	}
}

// Win functions
$if (env::os_is_win32())
extern fn void* _wfopen(Char16*, Char16*) @local;
extern fn void* _wfreopen(Char16*, Char16*, CFile) @local;
extern fn int _fseeki64(CFile, long, int) @local;
extern fn long _ftelli64(CFile) @local;
$endif

$if (env::os_is_posix())
extern fn CInt access(ZString path, CInt mode);
$endif
