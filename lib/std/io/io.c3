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
	ALREADY_EXISTS,
	BUSY,
	CANNOT_READ_DIR,
	DIR_NOT_EMPTY,
	EOF,
	FILE_CANNOT_DELETE,
	FILE_IS_DIR,
	FILE_IS_PIPE,
	FILE_NOT_DIR,
	FILE_NOT_FOUND,
	FILE_NOT_VALID,
	GENERAL_ERROR,
	ILLEGAL_ARGUMENT,
	INCOMPLETE_WRITE,
	INTERRUPTED,
	INVALID_POSITION,
	INVALID_PUSHBACK,
	NAME_TOO_LONG,
	NOT_SEEKABLE,
	NO_PERMISSION,
	OUT_OF_SPACE,
	OVERFLOW,
	READ_ONLY,
	SYMLINK_FAILED,
	TOO_MANY_DESCRIPTORS,
	UNEXPECTED_EOF,
	UNKNOWN_ERROR,
	UNSUPPORTED_OPERATION,
	WOULD_BLOCK,
}


/**
 * @param stream
 * @require @is_instream(stream)
 **/
macro String! readline(stream = io::stdin(), Allocator* allocator = allocator::heap())
{
	bool $is_stream = @typeid(stream) == InStream*.typeid;
	$if $is_stream:
		$typeof(&stream.read_byte) func = &stream.read_byte;
		char val = func((void*)stream)!;
	$else
		char val = stream.read_byte()!;
	$endif

    if (val == '\n') return "";
    @pool(allocator)
    {
        DString str = dstring::temp_with_capacity(256);
    	if (val != '\r') str.append(val);
    	while (1)
    	{
    	    $if $is_stream:
    	        char! c = func((void*)stream);
    	    $else
    	        char! c = stream.read_byte();
    	    $endif
    		if (catch err = c)
    		{
    			if (err == IoError.EOF) break;
    			return err?;
    		}
    		if (c == '\r') continue;
    		if (c == '\n') break;
    		str.append_char(c);
    	}
    	return str.copy_str(allocator);
    };
}

macro String! treadline(stream = io::stdin()) => readline(stream, allocator::temp()) @inline;

/**
 * @require @is_outstream(out) "The output must implement OutStream"
 */
macro usz! fprint(out, x)
{
	var $Type = $typeof(x);
	$switch ($Type)
	$case String:
		return out.write(x);
	$case ZString:
		return out.write(x.str_view());
	$case DString:
		return out.write(x.str_view());
	$default:
		$if $assignable(x, String):
			return out.write((String)x);
		$else
			return fprintf(out, "%s", x);
		$endif
	$endswitch
}

fn usz! fprintf(OutStream* out, String format, args...)
{
	Formatter formatter;
	formatter.init(&out_putstream_fn, &out);
	return formatter.vprintf(format, args);
}

fn usz! fprintfn(OutStream* out, String format, args...) @maydiscard
{
	Formatter formatter;
	formatter.init(&out_putstream_fn, &out);
	usz len = formatter.vprintf(format, args)!;
	out.write_byte('\n')!;
	if (&out.flush) out.flush()!;
	return len + 1;
}

/**
 * @require @is_outstream(out) "The output must implement OutStream"
 */
macro usz! fprintn(out, x = "")
{
	usz len = fprint(out, x)!;
	out.write_byte('\n')!;
	$switch
		$case @typeid(out) == OutStream*.typeid:
			if (&out.flush) out.flush()!;
		$case $defined(out.flush):
			out.flush()!;
	$endswitch
	return len + 1;
}

macro void print(x)
{
	(void)fprint(io::stdout(), x);
}

macro void printn(x = "")
{
	(void)fprintn(io::stdout(), x);
}

macro void eprint(x)
{
	(void)fprint(io::stderr(), x);
}

macro void eprintn(x)
{
	(void)fprintn(io::stderr(), x);
}


fn void! out_putstream_fn(void* data, char c) @private
{
	OutStream** stream = data;
	return (*stream).write_byte(c);
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
	io::stdout().flush()!;
	return len + 1;
}

fn usz! eprintf(String format, args...) @maydiscard
{
	Formatter formatter;
	OutStream* stream = stderr();
	formatter.init(&out_putstream_fn, &stream);
	return formatter.vprintf(format, args);
}


fn usz! eprintfn(String format, args...) @maydiscard
{
	Formatter formatter;
	OutStream* stream = stderr();
	formatter.init(&out_putstream_fn, &stream);
	usz len = formatter.vprintf(format, args)! + 1;
	stderr().write_byte('\n')!;
	stderr().flush()!;
	return len;
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
	if (!file.file) file = file::from_handle(libc::stdout());
	return &file;
}

fn File* stderr()
{
	static File file;
	if (!file.file) file = file::from_handle(libc::stderr());
	return &file;
}

fn File* stdin()
{
	static File file;
	if (!file.file) file = file::from_handle(libc::stdin());
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

fn File* stdout()
{
	return &stdout_file;
}

fn File* stderr()
{
	return &stderr_file;
}

fn File* stdin()
{
	return &stdin_file;
}
