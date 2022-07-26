// Copyright (c) 2021-2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::io;
import libc;

struct File
{
    CFile file;
}

fn int putchar(char c) @inline
{
    return libc::putchar(c);
}

/**
 * @param [&in] message
 * @return `number of bytes printed.`
 */
fn int print(char* message)
{
    char* pointer = message;
    while (*pointer != '\0')
    {
        if (!putchar(*pointer)) return 0;
        pointer++;
    }
    return 1;
}

/**
 * @param [&in] message
 * @return `number of bytes printed.`
 */
fn int println(char *message = "") @inline
{
    return libc::puts(message);
}

fn void! File.open(File* file, char[] filename, char[] mode)
{
    char* filename_copy = mem::talloc(filename.len + 1)!!;

    char* mode_copy = mem::talloc(mode.len + 1)!!;

    mem::copy(filename_copy, (char*)(filename), filename.len);
    mem::copy(mode_copy, (char*)(mode), mode.len);
    filename_copy[filename.len] = 0;
    mode_copy[filename.len] = 0;
    file.file = libc::fopen(filename_copy, mode_copy);
    if (!file.file) return IoError.FILE_NOT_FOUND!;
}

enum Seek
{
    SET,
    CURSOR,
    END
}

fault IoError
{
    FILE_NOT_FOUND,
    FILE_NOT_SEEKABLE,
    FILE_NOT_VALID,
    FILE_INVALID_POSITION,
    FILE_OVERFLOW,
    FILE_IS_PIPE,
    FILE_EOF,
    FILE_INCOMPLETE_WRITE,
    INTERRUPTED,
    UNKNOWN_ERROR,
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
fn usize File.read(File* file, void* buffer, usize items, usize element_size = 1)
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
fn usize File.write(File* file, void* buffer, usize items, usize element_size = 1)
{
    return libc::fwrite(buffer, element_size, items, file.file);
}

/**
 * @param [&in] file
 * @require file.file `File must be initialized`
 */
fn usize! File.println(File* file, char[] string)
{
    usize len = string.len;
    if (len != libc::fwrite(string.ptr, 1, len, file.file)) return IoError.UNKNOWN_ERROR!;
    if (!libc::putc('\n', file.file)) return IoError.UNKNOWN_ERROR!;
    return len + 1;
}

fn File stdout()
{
	return { libc::stdout() };
}

/*


error FileError
{
    ulong errno;
}

fn FileError errorFromErrno()
{
    return FileError { };
}




pubic fn void! File.clearerr(File *file) @inline
{
    clearerr(file->file);
}



fn void File.error(File *file) @inline
{
    int err = ferror
}
*/
