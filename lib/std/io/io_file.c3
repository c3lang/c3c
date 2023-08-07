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
 * @require self.file != null
 */
fn void! File.putc(self, char c)
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
fn bool File.eof(self) @inline
{
	return libc::feof(self.file) != 0;
}

/**
 * @param [in] buffer
 */
fn usz! File.read(self, char[] buffer)
{
	return os::native_fread(self.file, buffer);
}

/**
 * @param ref "the object to read into."
 * @require ref.ptr != null
 * @ensure return == ref.type.sizeof
 */
fn usz! File.read_any(self, any ref)
{
	return self.read(ref.ptr[:ref.type.sizeof]);
}

/**
 * @param [out] buffer
 * @require self.file `File must be initialized`
 */
fn usz! File.write(self, char[] buffer)
{
	return os::native_fwrite(self.file, buffer);
}

/**
 * @param ref "the object to write."
 * @require ref.ptr != null
 * @ensure return == ref.type.sizeof
 */
fn usz! File.write_any(self, any ref)
{
	return self.write(ref.ptr[:ref.type.sizeof]);
}

/**
 * @require self.file `File must be initialized`
 */
fn usz! File.printn(self, String string = "")
{
	usz len = self.print(string)!;
	if (!libc::putc('\n', self.file)) return IoError.UNKNOWN_ERROR?;
	return len + 1;
}

/**
 * @require self.file `File must be initialized`
 */
fn usz! File.print(self, String string)
{
	usz len = string.len;
	if (len != self.write((char[])string)!) return IoError.UNKNOWN_ERROR?;
	return len;
}

/**
 * @require self.file `File must be initialized`
 */
fn DString File.getline(self, Allocator* using = mem::heap())
{
	DString s = dstring::new_with_capacity(120, using);
	while (!self.eof())
	{
		int c = libc::fgetc(self.file);
		if (c == -1) break;
		if (c == '\n') break;
		s.append_char((char)c);
	}
	return s;
}

/**
 * @require self.file `File must be initialized`
 * @return "a zero terminated String (the pointer may be safely cast into a ZString)"
 */
fn String File.tgetline(self)
{
	return self.getline(mem::temp()).zstr().as_str();
}

fn char! File.getc(self)
{
	int c = libc::fgetc(self.file);
	if (c == -1) return IoError.EOF?;
	return (char)c;
}

/**
 * @require self.file `File must be initialized`
 */
fn void File.flush(self)
{
	libc::fflush(self.file);
}
