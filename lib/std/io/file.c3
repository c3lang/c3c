module std::io;
import libc;

struct File (InStream, OutStream)
{
	CFile file;
}

module std::io::file;
import libc, std::io::path, std::io::os, std::time;

fn File? open(String filename, String mode)
{
	return from_handle(os::native_fopen(filename, mode));
}

fn File? open_path(Path path, String mode)
{
	return from_handle(os::native_fopen((String)path, mode));
}

fn bool exists(String file) => @pool()
{
	return os::native_file_or_dir_exists(file);
}

fn File from_handle(CFile file)
{
	return { .file = file };
}

fn bool is_file(String path)
{
	return os::native_is_file(path);
}

fn bool is_dir(String path)
{
	return os::native_is_dir(path);
}

fn long? get_size(String path)
{
	return os::native_file_size(path);
}

fn void? set_modified_time(String path, Time_t time)
{
	return os::native_set_modified_time(path, time);
}

fn Time? last_modified(String filename)
{
	Time_t? seconds = os::native_get_modified_time(filename)!;
	return (Time)(seconds * (Time_t)time::SEC);
}

fn void? delete(String filename) @maydiscard
{
	return os::native_remove(filename) @inline;
}


<*
 @require self.file != null
*>
fn void? File.reopen(&self, String filename, String mode)
{
	self.file = os::native_freopen(self.file, filename, mode)!;
}

<*
 @require self.file != null
*>
fn void? File.seek(&self, long offset, SeekOrigin whence = FROM_START) @dynamic
{
	return os::native_fseek(self.file, offset, whence);
}

<*
 @require self.file != null
*>
fn long? File.cursor(&self) @dynamic
{
	return os::native_ftell(self.file);
}

/*
Implement later
<*
 @require self.file == null
*>
fn void? File.memopen(File* file, char[] data, String mode)
{
	@pool()
	{
		file.file = libc::memopen(data.ptr, data.len, mode.to_temp_zstr(), file.file);
		// TODO errors
	};
}
*/


<*
 @require self.file != null
*>
fn void? File.write_byte(&self, char c) @dynamic
{
	return os::native_fputc(c, self.file);
}

<*
 @param [&inout] self
*>
fn void? File.close(&self) @inline @dynamic @maydiscard
{
	defer self.file = null;
	if (self.file && libc::fclose(self.file))
	{
		switch (libc::errno())
		{
			case ECONNRESET:
			case EBADF: return io::FILE_NOT_VALID~;
			case EINTR: return io::INTERRUPTED~;
			case EDQUOT:
			case EFAULT:
			case EAGAIN:
			case EFBIG:
			case ENETDOWN:
			case ENETUNREACH:
			case ENOSPC:
			case EIO: return io::INCOMPLETE_WRITE~;
			default: return io::UNKNOWN_ERROR~;
		}
	}
}

fn long? File.size(&self) @dynamic
{
	long curr = self.cursor()!;
	defer (void)self.seek(curr);
	self.seek(0, FROM_END)!;
	return self.cursor()!;
}

<*
 @require self.file != null
*>
fn bool File.eof(&self) @inline
{
	return libc::feof(self.file) != 0;
}

<*
 @param [in] buffer
*>
fn sz? File.read(&self, char[] buffer) @dynamic
{
	return os::native_fread(self.file, buffer);
}

<*
 @param [out] buffer
 @require self.file != null : `File must be initialized`
*>
fn sz? File.write(&self, char[] buffer) @dynamic
{
	return os::native_fwrite(self.file, buffer);
}

fn Fd File.fd(self) @if(env::LIBC)
{
	return libc::fileno(self.file);
}

fn bool File.isatty(self) @if(env::LIBC)
{
	return libc::isatty(self.fd()) > 0;
}

fn char? File.read_byte(&self) @dynamic
{
	int c = libc::fgetc(self.file);
	if (c == -1) return io::EOF~;
	return (char)c;
}

<*
 Load up to buffer.len characters. Returns io::OVERFLOW if the file is longer
 than the buffer.

 @param filename : "The path to the file to read"
 @param [in] buffer : "The buffer to read to"
 @return? io::OVERFLOW, io::read_buffer_to_end!
*>
fn char[]? load_buffer(String filename, char[] buffer)
{
	File file = open(filename, "rb")!;
	defer (void)file.close();
	long len = file.size()!;
	if (len > buffer.len) return io::OVERFLOW~;
	return io::read_buffer_to_end(file, buffer[:len]);
}

<*
 Loads a file into memory
 @return? io::OUT_OF_SPACE, io::read_buffer_to_end!
*>
fn char[]? load(Allocator allocator, String filename)
{
	File file = open(filename, "rb")!;
	defer (void)file.close();
	long len = file.size()!;
	if (len > sz::max) return io::OUT_OF_SPACE~;
	char* data = alloc::malloc_try(allocator, (sz)len)!;
	defer catch alloc::free(allocator, data);
	return io::read_buffer_to_end(file, data[:len]);
}

fn char[]? load_path(Allocator allocator, Path path) => load(allocator, (String)path);

fn char[]? load_temp(String filename) => load(tmem, filename);

fn char[]? load_path_temp(Path path) => load_temp((String)path);

<*
 Save the data to a file. Not atomic.

 @return? io::write_all!
*>
fn void? save(String filename, char[] data)
{
	File file = open(filename, "wb")!;
	defer (void)file.close();
	io::write_all(&file, data)!;
}

<*
 @require self.file != null : `File must be initialized`
*>
fn void? File.flush(&self) @dynamic
{
	libc::fflush(self.file);
}
