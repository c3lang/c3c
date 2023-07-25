module std::io::file;
import libc;

fn File! open(String filename, String mode)
{
	return { .file = os::native_fopen(filename, mode) };
}

fn void! delete(String filename) => os::native_remove(filename) @inline;

fn File! open_path(Path path, String mode)
{
	return { .file = os::native_fopen(path.as_str(), mode) };
}

fn File from_libc(CFile file)
{
	return { .file = file };
}

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
fn usz! File.seek(self, isz offset, Seek seek_mode = Seek.SET)
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
 * @require file && file.file != null
 */
fn void! File.putc(File *file, char c)
{
    if (!libc::fputc(c, file.file)) return IoError.EOF?;
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
fn usz! File.read(File* file, char[] buffer)
{
	return os::native_fread(file.file, buffer);
}

/**
 * @param [&in] file
 * @param [&out] buffer
 * @require file.file `File must be initialized`
 */
fn usz! File.write(File file, char[] buffer)
{
	return os::native_fwrite(file.file, buffer);
}

/**
 * @param [&in] file
 * @require file.file `File must be initialized`
 */
fn usz! File.printn(File file, String string = "")
{
	usz len = file.print(string)!;
	if (!libc::putc('\n', file.file)) return IoError.UNKNOWN_ERROR?;
    return len + 1;
}

/**
 * @param [&in] file
 * @require file.file `File must be initialized`
 */
fn usz! File.print(File file, String string)
{
    usz len = string.len;
    if (len != file.write((char[])string)!) return IoError.UNKNOWN_ERROR?;
    return len;
}

/**
 * @param [&in] file
 * @require file.file `File must be initialized`
 */
fn DString File.getline(File* file, Allocator* using = mem::heap())
{
	DString s = dstring::new_with_capacity(120, using);
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
	return file.getline(mem::temp()).zstr().as_str();
}

fn char! File.getc(File* file)
{
	int c = libc::fgetc(file.file);
	if (c == -1) return IoError.EOF?;
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
