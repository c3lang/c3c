module std::io;
import libc;

fn void! File.open(File* file, String filename, String mode)
{
	@pool()
	{
		file.file = libc::fopen(filename.zstrtcopy(), mode.zstrtcopy());
        if (!file.file) return IoError.FILE_NOT_FOUND!;
	};
}

/**
 * @require file.file != null
 **/
fn void! File.reopen(File* file, String filename, String mode)
{
	@pool()
	{
		file.file = libc::freopen(filename.zstrtcopy(), mode.zstrtcopy(), file.file);
        if (!file.file) return IoError.FILE_NOT_FOUND!;
	};
}

/**
 * @require file.file != null
 **/
fn void! File.chmode(File* file, String filename, String mode)
{
	@pool()
	{
		file.file = libc::freopen(null, mode.zstrtcopy(), file.file);
        if (!file.file) return IoError.FILE_NOT_FOUND!;
	};
}
/*
Implement later
/**
 * @require file.file == null
 **/
fn void! File.memopen(File* file, char[] data, String mode)
{
	@pool()
	{
		file.file = libc::memopen(data.ptr, data.len, mode.zstrtcopy(), file.file);
		// TODO errors
	};
}
*/

/**
 * @require file.file != null
 **/
fn usz! File.seek(File *file, isz offset, Seek seekMode = Seek.SET)
{
    if (libc::fseek(file.file, (SeekIndex)(offset), (int)(seekMode)))
    {
        switch (libc::errno())
        {
            case errno::EBADF: return IoError.NOT_SEEKABLE!;
            case errno::EINVAL: return IoError.INVALID_POSITION!;
            case errno::EOVERFLOW: return IoError.FILE_OVERFLOW!;
            case errno::ESPIPE: return IoError.FILE_IS_PIPE!;
            default: return IoError.UNKNOWN_ERROR!;
        }
    }
    if (seekMode == Seek.SET) return offset;
    return libc::ftell(file.file);
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
            case errno::EIO: return IoError.INCOMPLETE_WRITE!;
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
 * @param [in] buffer
 */
fn usz File.read(File* file, char[] buffer)
{
    return libc::fread(buffer.ptr, 1, buffer.len, file.file);
}

/**
 * @param [&in] file
 * @param [&out] buffer
 * @require file.file `File must be initialized`
 */
fn usz File.write(File* file, char[] buffer)
{
    return libc::fwrite(buffer.ptr, 1, buffer.len, file.file);
}

/**
 * @param [&in] file
 * @require file.file `File must be initialized`
 */
fn usz! File.printn(File* file, String string)
{
	usz len = file.print(string)?;
	if (!libc::putc('\n', file.file)) return IoError.UNKNOWN_ERROR!;
    return len + 1;
}

/**
 * @param [&in] file
 * @require file.file `File must be initialized`
 */
fn usz! File.print(File* file, String string)
{
    usz len = string.len;
    if (len != libc::fwrite(string.ptr, 1, len, file.file)) return IoError.UNKNOWN_ERROR!;
    return len;
}

fn usz! File.println(File* file, String string) => file.printn(string);

/**
 * @param [&in] file
 * @require file.file `File must be initialized`
 */
fn VarString File.getline(File* file, Allocator* allocator = mem::current_allocator())
{
	VarString s = string::new_with_capacity(120, allocator);
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
 * @return "a zero terminated String (the pointer may be safely cast into a ZString)"
 */
fn String File.tgetline(File* file)
{
	return file.getline(mem::temp_allocator()).zstr().as_str();
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
