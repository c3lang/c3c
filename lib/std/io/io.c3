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
    FILE_NOT_VALID,
    INVALID_POSITION,
    OVERFLOW,
    FILE_IS_PIPE,
    FILE_EOF,
    INCOMPLETE_WRITE,
    BUSY,
    NO_PERMISSION,
    OUT_OF_SPACE,
    INVALID_PUSHBACK,
    EOF,
    CANNOT_READ_DIR,
    TOO_MANY_DESCRIPTORS,
    FILE_IS_DIR,
    READ_ONLY,
    FILE_NOT_DIR,
    SYMLINK_FAILED,
    ALREADY_EXISTS,
    NOT_SEEKABLE,
    NAME_TOO_LONG,
    WOULD_BLOCK,
    DIR_NOT_EMPTY,
    INTERRUPTED,
    GENERAL_ERROR,
    UNKNOWN_ERROR,
    UNSUPPORTED_OPERATION,
}

fn void putchar(char c) @inline
{
    libc::putchar(c);
}

macro void print(x)
{
	var $Type = $typeof(x);
	$switch ($Type)
	$case String:
		(void)stdout().print(x);
	$case ZString:
		(void)stdout().print(x.as_str());
	$case DString:
		(void)stdout().print(x.str());
	$default:
		$if (@convertible(x, String))
			(void)stdout().print((String)x);
		$else
			(void)stdout().printf("%s", x);
		$endif
	$endswitch
}

macro void printn(x = "")
{
	var $Type = $typeof(x);
	$switch ($Type)
	$case String:
		(void)stdout().printn(x);
	$case ZString:
		(void)stdout().printn(x.as_str());
	$case DString:
		(void)stdout().printn(x.str());
	$default:
		$if (@convertible(x, String))
			(void)stdout().printn((String)x);
		$else
			(void)stdout().printfn("%s", x);
		$endif
	$endswitch
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
