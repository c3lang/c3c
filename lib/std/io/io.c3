// Copyright (c) 2021-2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::io;
import libc;

struct File
{
    CFile file;
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
    FILE_NOT_DIR,
    NO_PERMISSION,
    NAME_TOO_LONG,
    INTERRUPTED,
    GENERAL_ERROR,
    UNKNOWN_ERROR,
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




fn File stdout()
{
	return { libc::stdout() };
}

fn File stderr()
{
	return { libc::stderr() };
}

fn File stdin()
{
	return { libc::stdin() };
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
