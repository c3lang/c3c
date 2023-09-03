// Copyright (c) 2021-2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::io;
import libc;

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
	INCOMPLETE_WRITE,
	BUSY,
	NO_PERMISSION,
	OUT_OF_SPACE,
	INVALID_PUSHBACK,
	EOF,
	UNEXPECTED_EOF,
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
	FILE_CANNOT_DELETE,
}


macro void print(x)
{
	(void)print_gen(stdout(), x);
}

macro void printn(x = "")
{
	(void)printn_gen(stdout(), x);
}

fn void! out_putchar_fn(void* data @unused, char c) @private
{
	libc::putchar(c);
}


fn usz! printf(String format, args...) @maydiscard
{
	Formatter formatter;
	formatter.init(&out_putchar_fn);
	return formatter.vprintf(format, args);
}

fn usz! printfn(String format, args...) @maydiscard
{
	Formatter formatter;
	formatter.init(&out_putchar_fn);
	usz len = formatter.vprintf(format, args)!;
	putchar('\n');
	return len + 1;
}

fn char[]! bprintf(char[] buffer, String format, args...) @maydiscard
{
	Formatter formatter;
	BufferData data = { .buffer = buffer };
	formatter.init(&out_buffer_fn, &data);
	usz size = formatter.vprintf(format, args)!;
	return buffer[:data.written];
}

fn void! out_buffer_fn(void *data, char c) @private
{
	BufferData *buffer_data = data;
	if (buffer_data.written >= buffer_data.buffer.len) return PrintFault.BUFFER_EXCEEDED?;
	buffer_data.buffer[buffer_data.written++] = c;
}


struct BufferData @private
{
	char[] buffer;
	usz written;
}


module std::io @if (env::LIBC);
import libc;

fn void putchar(char c) @inline
{
	libc::putchar(c);
}

fn File* stdout()
{
	static File file;
	if (!file.fns) file = file::from_handle(libc::stdout());
	return &file;
}

fn File* stderr()
{
	static File file;
	if (!file.fns) file = file::from_handle(libc::stderr());
	return &file;
}

fn File* stdin()
{
	static File file;
	if (!file.fns) file = file::from_handle(libc::stdin());
	return &file;
}

module std::io @if(!env::LIBC);

File stdin_file;
File stdout_file;
File stderr_file;

fn void putchar(char c) @inline
{
	(void)stdout_file.putc(c);
}

fn File stdout()
{
	return stdout_file;
}

fn File stderr()
{
	return stderr_file;
}

fn File stdin()
{
	return stdin_file;
}
