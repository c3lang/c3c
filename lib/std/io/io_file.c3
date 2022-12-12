module std::io;
import libc;

fn void! File.open(File* file, char[] filename, char[] mode)
{
	@pool()
	{
	    char* filename_copy = tmalloc(filename.len + 1);
        char* mode_copy = tmalloc(mode.len + 1);

	    mem::copy(filename_copy, (char*)(filename), filename.len);
        mem::copy(mode_copy, (char*)(mode), mode.len);
        filename_copy[filename.len] = 0;
        mode_copy[filename.len] = 0;
        file.file = libc::fopen(filename_copy, mode_copy);
        if (!file.file) return IoError.FILE_NOT_FOUND!;
	};
}

/**
 * @require file.file != null
 **/
fn void! File.seek(File *file, long offset, Seek seekMode = Seek.SET)
{
    if (libc::fseek(file.file, (SeekIndex)(offset), (int)(seekMode)))
    {
        switch (libc::errno())
        {
            case errno::EBADF: return IoError.FILE_NOT_SEEKABLE!;
            case errno::EINVAL: return IoError.FILE_INVALID_POSITION!;
            case errno::EOVERFLOW: return IoError.FILE_OVERFLOW!;
            case errno::ESPIPE: return IoError.FILE_IS_PIPE!;
            default: return IoError.UNKNOWN_ERROR!;
        }
    }
}

/**
 * @require file && file.file != null
 */
fn void! File.putc(File *file, char c)
{
    if (!libc::fputc(c, file.file)) return IoError.FILE_EOF!;
}

/**
 * @require file != null
 */
fn void! File.close(File *file) @inline
{
    if (file.file && libc::fclose(file.file))
    {
        switch (libc::errno())
        {
            case errno::ECONNRESET:
            case errno::EBADF: return IoError.FILE_NOT_VALID!;
            case errno::EINTR: return IoError.INTERRUPTED!;
            case errno::EDQUOT:
            case errno::EFAULT:
            case errno::EAGAIN:
            case errno::EFBIG:
            case errno::ENETDOWN:
            case errno::ENETUNREACH:
            case errno::ENOSPC:
            case errno::EIO: return IoError.FILE_INCOMPLETE_WRITE!;
            default: return IoError.UNKNOWN_ERROR!;
        }
    }
    file.file = null;
}

/**
 * @require file && file.file
 */
fn bool File.eof(File* file) @inline
{
    return libc::feof(file.file) != 0;
}

/**
 * @require file && file.file
 */
fn usz File.read(File* file, void* buffer, usz items, usz element_size = 1)
{
    return libc::fread(buffer, element_size, items, file.file);
}

/**
 * @param [&in] file
 * @param [&out] buffer
 * @param items
 * @param element_size
 * @require file.file `File must be initialized`
 * @require element_size > 1
 */
fn usz File.write(File* file, void* buffer, usz items, usz element_size = 1)
{
    return libc::fwrite(buffer, element_size, items, file.file);
}

/**
 * @param [&in] file
 * @require file.file `File must be initialized`
 */
fn usz! File.println(File* file, char[] string)
{
    usz len = string.len;
    if (len != libc::fwrite(string.ptr, 1, len, file.file)) return IoError.UNKNOWN_ERROR!;
    if (!libc::putc('\n', file.file)) return IoError.UNKNOWN_ERROR!;
    return len + 1;
}

/**
 * @param [&in] file
 * @require file.file `File must be initialized`
 */
fn String File.getline(File* file, Allocator* allocator = mem::current_allocator())
{
	String s = string::new_with_capacity(120, allocator);
	while (!file.eof())
	{
		int c = libc::fgetc(file.file);
		if (c == -1) break;
		if (c == '\n') break;
		s.append_char((char)c);
	}
	return s;
}

/**
 * @param [&in] file
 * @require file.file `File must be initialized`
 * @return "a zero terminated char[] (the pointer may be safely cast into a ZString)"
 */
fn char[] File.tgetline(File* file)
{

	String s = file.getline(mem::temp_allocator());
	ZString z = s.zstr();
	return z[:s.len()];
}

fn char! File.getc(File* file)
{
	int c = libc::fgetc(file.file);
	if (c == -1) return IoError.FILE_EOF!;
	return (char)c;
}

/**
 * @param [&in] file
 * @require file.file `File must be initialized`
 */
fn void File.flush(File* file)
{
	libc::fflush(file.file);
}
