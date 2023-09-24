module std::io;
import libc;

struct File
{
	inline Stream stream;
	CFile file;
}

module std::io::file;
import libc;

fn File! open(String filename, String mode)
{
	return from_handle(os::native_fopen(filename, mode));
}

fn File! open_path(Path path, String mode)
{
	return from_handle(os::native_fopen(path.str_view(), mode));
}

fn File from_handle(CFile file)
{
	return { .stream.fns = &FILESTREAM_INTERFACE, .file = file };
}

fn bool is_file(String path)
{
	return os::native_is_file(path);
}

fn usz! get_size(String path)
{
	return os::native_file_size(path);
}

fn void! delete(String filename) => os::native_remove(filename) @inline;


/**
 * @require self.file != null
 **/
fn void! File.reopen(&self, String filename, String mode)
{
	self.file = os::native_freopen(self.file, filename, mode)!;
}

/**
 * @require self.file != null
 **/
fn usz! File.seek(&self, isz offset, Seek seek_mode = Seek.SET)
{
	os::native_fseek(self.file, offset, seek_mode)!;
	return os::native_ftell(self.file);
}


/*
Implement later
/**
 * @require self.file == null
 **/
fn void! File.memopen(File* file, char[] data, String mode)
{
	@pool()
	{
		file.file = libc::memopen(data.ptr, data.len, mode.zstr_tcopy(), file.file);
		// TODO errors
	};
}
*/


/**
 * @require self.file != null
 */
fn void! File.write_byte(&self, char c)
{
	if (!libc::fputc(c, self.file)) return IoError.EOF?;
}

/**
 * @param [&inout] self
 */
fn void! File.close(&self) @inline
{
	if (self.file && libc::fclose(self.file))
	{
		switch (libc::errno())
		{
			case errno::ECONNRESET:
			case errno::EBADF: return IoError.FILE_NOT_VALID?;
			case errno::EINTR: return IoError.INTERRUPTED?;
			case errno::EDQUOT:
			case errno::EFAULT:
			case errno::EAGAIN:
			case errno::EFBIG:
			case errno::ENETDOWN:
			case errno::ENETUNREACH:
			case errno::ENOSPC:
			case errno::EIO: return IoError.INCOMPLETE_WRITE?;
			default: return IoError.UNKNOWN_ERROR?;
		}
	}
	self.file = null;
}

/**
 * @require self.file
 */
fn bool File.eof(&self) @inline
{
	return libc::feof(self.file) != 0;
}

/**
 * @param [in] buffer
 */
fn usz! File.read(&self, char[] buffer)
{
	return os::native_fread(self.file, buffer);
}

/**
 * @param [out] buffer
 * @require self.file `File must be initialized`
 */
fn usz! File.write(&self, char[] buffer)
{
	return os::native_fwrite(self.file, buffer);
}


fn char! File.read_byte(&self)
{
	int c = libc::fgetc(self.file);
	if (c == -1) return IoError.EOF?;
	return (char)c;
}

/**
 * @require self.file `File must be initialized`
 */
fn void! File.flush(&self)
{
	libc::fflush(self.file);
}

const StreamInterface FILESTREAM_INTERFACE = {
	.close_fn = (CloseStreamFn)&File.close,
	.seek_fn = (SeekStreamFn)&File.seek,
	.read_fn = (ReadStreamFn)&File.read,
	.read_byte_fn = (ReadByteStreamFn)&File.read_byte,
	.write_fn = (WriteStreamFn)&File.write,
	.write_byte_fn = (WriteByteStreamFn)&File.write_byte,
	.flush_fn = (FlushStreamFn)&File.flush
};