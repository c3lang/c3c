module std::io::os @if(env::LIBC);
import libc;

<*
 @require mode.len > 0
 @require filename.len > 0
*>
fn void*? native_fopen(String filename, String mode) @inline => @stack_mem(256; Allocator smem)
{
	$if env::WIN32:
		void* file = libc::_wfopen(filename.to_wstring(smem), mode.to_wstring(smem))!;
	$else
		void* file = libc::fopen(filename.zstr_copy(smem), mode.zstr_copy(smem));
	$endif
	return file ?: file_open_errno()~;
}

fn void? native_remove(String filename) => @stack_mem(256; Allocator smem)
{
	$if env::WIN32:
		CInt result = libc::_wremove(filename.to_wstring(smem))!;
	$else
		CInt result = libc::remove(filename.zstr_copy(smem));
	$endif
	if (result)
	{
		switch (libc::errno())
		{
			case errno::ENOENT:
				return io::FILE_NOT_FOUND~;
			case errno::EACCES:
			default:
				return io::FILE_CANNOT_DELETE~;
		}
	}
}

<*
 @require mode.len > 0
 @require filename.len > 0
*>
fn void*? native_freopen(void* file, String filename, String mode) @inline  => @stack_mem(256; Allocator smem)
{
	$if env::WIN32:
		file = libc::_wfreopen(filename.to_wstring(smem), mode.to_wstring(smem), file)!;
	$else
		file = libc::freopen(filename.zstr_copy(smem), mode.zstr_copy(smem), file);
	$endif
		return file ?: file_open_errno()~;
}

fn void? native_fseek(void* file, long offset, SeekOrigin seek_mode) @inline
{
	if (libc::fseek(file, (SeekIndex)offset, seek_mode.ordinal)) return file_seek_errno()~;
}


fn long? native_ftell(CFile file) @inline
{
	long index = libc::ftell(file);
	return index >= 0 ? index : file_seek_errno()~;
}

fn usz? native_fwrite(CFile file, char[] buffer) @inline
{
	return libc::fwrite(buffer.ptr, 1, buffer.len, file);
}

fn void? native_fputc(CInt c, CFile stream) @inline
{
	if (libc::fputc(c, stream) == libc::EOF) return io::EOF~;
}

fn usz? native_fread(CFile file, char[] buffer) @inline
{
	return libc::fread(buffer.ptr, 1, buffer.len, file);
}

fn void? native_fflush(CFile file) @inline @maydiscard
{
	if (libc::fflush(file) != 0) return io::GENERAL_ERROR~;
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


struct Utimbuf @if(!env::WIN32)
{
	Time_t actime;
	Time_t modtime;
}
extern fn int utime(char* filename, void* times) @if(!env::WIN32);

struct FileTime @if(env::WIN32) {
	uint low;
	uint high;
}
extern fn void* create_file_w(WChar* lpFileName, uint dwDesiredAccess, uint dwShareMode, void* lpSecurityAttributes, uint dwCreationDisposition, uint dwFlagsAndAttributes, void* hTemplateFile) @cname("CreateFileW") @if(env::WIN32);
extern fn int set_file_time(void* hFile, void* lpCreationTime, void* lpLastAccessTime, void* lpLastWriteTime) @cname("SetFileTime") @if(env::WIN32);
extern fn int close_handle(void* hObject) @cname("CloseHandle") @if(env::WIN32);

fn void? native_set_modified_time(String filename, Time_t time) => @stack_mem(256; Allocator smem)
{
	$if env::WIN32:
		void* h = create_file_w(filename.to_wstring(smem)!, 0x40000000, 1 | 2 | 4, null, 3, 0x02000000, null);
		if (h == (void*)(uptr)-1) return io::GENERAL_ERROR~;
		defer (void)close_handle(h);

		ulong ticks = ((ulong)time + 11644473600UL) * 10_000_000UL;
		FileTime ft = { (uint)(ticks & 0xFFFFFFFF), (uint)(ticks >> 32) };

		if (!set_file_time(h, null, null, &ft)) return io::GENERAL_ERROR~;
	$else
		Utimbuf times = { time, time };
		if (utime(filename.zstr_copy(smem), &times)) return io::GENERAL_ERROR~;
	$endif
}
