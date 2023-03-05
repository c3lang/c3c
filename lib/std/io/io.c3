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
    NO_PERMISSION,
    OUT_OF_DISK,
    INVALID_PUSHBACK,
    EOF,
    TOO_MANY_DESCRIPTORS,
    FILE_IS_DIR,
    READ_ONLY,
    FILE_NOT_DIR,
    SYMLINK_FAILED,
    ALREADY_EXISTS,
    NOT_SEEKABLE,
    NAME_TOO_LONG,
    WOULD_BLOCK,
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
	$switch ($Type):
	$case String:
		catch(stdout().print(x));
	$case ZString:
		catch(stdout().print(x.as_str()));
	$case DString:
		catch(stdout().print(x.str()));
	$case VarString:
		catch(stdout().print(x.str()));
	$default:
		$if (@convertible(x, String)):
			catch(stdout().print((String)x));
		$else:
			catch(stdout().printf("%s", x));
		$endif;
	$endswitch;
}

macro void printn(x = "")
{
	var $Type = $typeof(x);
	$switch ($Type):
	$case String:
		catch(stdout().printn(x));
	$case ZString:
		catch(stdout().printn(x.as_str()));
	$case DString:
		catch(stdout().printn(x.str()));
	$case VarString:
		catch(stdout().printn(x.str()));
	$default:
		$if (@convertible(x, String)):
			catch(stdout().printn((String)x));
		$else:
			catch(stdout().printfn("%s", x));
		$endif;
	$endswitch;
}

macro void println(x = "") => printn(x);

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
