// Copyright (c) 2021-2025 Christoffer Lerno. All rights reserved.
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

faultdef
	ALREADY_EXISTS,
	BUSY,
	CANNOT_READ_DIR,
	DIR_NOT_EMPTY,
	PARENT_DIR_MISSING,
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
	PATH_COULD_NOT_BE_FOUND,
	READ_ONLY,
	SYMLINK_FAILED,
	TOO_MANY_DESCRIPTORS,
	UNEXPECTED_EOF,
	UNKNOWN_ERROR,
	UNSUPPORTED_OPERATION,
	WOULD_BLOCK;


<*
 Read from a stream (default is stdin) to the next "\n"
 or to the end of the stream, whatever comes first.
 "\r" will be filtered from the String.

 @param stream : `The stream to read from.`
 @require @is_not_instream_if_ptr(stream) : "The value for 'stream' should have been passed as a pointer and not as a value, please add '&'."
 @require @is_instream(stream) : `Make sure that the stream is actually an InStream.`
 @param [inout] allocator : `the allocator to use.`
 @return `The string containing the data read.`
*>
macro String? readline(Allocator allocator, stream = io::stdin())
{
	if (allocator == tmem)
	{
		DString str = dstring::temp_with_capacity(256);
		readline_to_stream(&str, stream)!;
		return str.str_view();
	}
	@pool()
	{
		DString str = dstring::temp_with_capacity(256);
		readline_to_stream(&str, stream)!;
		return str.copy_str(allocator);
	};
}

<*
 Reads a string, see `readline`, except the it is allocated
 on the temporary allocator and does not need to be freed.

 @param stream : `The stream to read from.`
 @require @is_not_instream_if_ptr(stream) : "The value for 'stream' should have been passed as a pointer and not as a value, please add '&'."
 @require @is_instream(stream) : `The stream must implement InStream.`
 @return `The temporary string containing the data read.`
*>
macro String? treadline(stream = io::stdin())
{
	return readline(tmem, stream) @inline;
}

<*
 Reads a string, see `readline`, the data is passed to an outstream

 @param out_stream : `The stream to write to`
 @param in_stream : `The stream to read from.`
 @require @is_not_instream_if_ptr(in_stream) : "The value for 'in_stream' should have been passed as a pointer and not as a value, please add '&'."
 @require @is_not_outstream_if_ptr(out_stream) : "The value for 'out_stream' should have been passed as a pointer and not as a value, please add '&'."
 @require @is_instream(in_stream) : `The in_stream must implement InStream.`
 @require @is_outstream(out_stream) : `The out_stream must implement OutStream.`
 @return `The number of bytes written`
*>
macro usz? readline_to_stream(out_stream, in_stream = io::stdin())
{
	bool $is_stream = $typeof(in_stream) == InStream;
	$if $is_stream:
		var func = &in_stream.read_byte;
		char val = func((void*)in_stream)!;
	$else
		char val = in_stream.read_byte()!;
	$endif
	bool $is_out_stream = $typeof(out_stream) == OutStream;
	$if $is_out_stream:
		var out_func = &out_stream.write_byte;
	$endif

	if (val == '\n') return 0;
	usz len;
	if (val != '\r')
	{
		$if $is_out_stream:
			out_func((void*)out_stream.ptr, val)!;
		$else
			out_stream.write_byte(val)!;
		$endif
		len++;
	}
	while (1)
	{
		$if $is_stream:
			char? c = func((void*)in_stream);
		$else
			char? c = in_stream.read_byte();
		$endif
		if (catch err = c)
		{
			if (err == io::EOF) break;
			return err?;
		}
		if (c == '\r') continue;
		if (c == '\n') break;
		$if $is_out_stream:
			out_func((void*)out_stream.ptr, c)!;
		$else
			out_stream.write_byte(c)!;
		$endif
		len++;
	}
	return len;
}
<*
 Print a value to a stream.

 @param out : `the stream to print to`
 @param x : `the value to print`
 @require @is_outstream(out) : `The output must implement OutStream.`
 @return `the number of bytes printed.`
*>
macro usz? fprint(out, x)
{
	var $Type = $typeof(x);
	$switch $Type:
		$case String: return out.write(x);
		$case ZString: return out.write(x.str_view());
		$case DString: return out.write(x.str_view());
		$default:
			$if $defined(String a = x):
				return out.write((String)x);
			$else
				$if is_struct_with_default_print($Type):
					Formatter formatter;
                    formatter.init(&out_putstream_fn, &&(OutStream)out);
                    return struct_to_format(x, &formatter, false);
				$else
					return fprintf(out, "%s", x);
				$endif
			$endif
	$endswitch
}

<*
 Prints using a 'printf'-style formatting string.
 See `printf` for details on formatting.

 @param [inout] out : `The OutStream to print to`
 @param [in] format : `The printf-style format string`
 @return `the number of characters printed`
*>
fn usz? fprintf(OutStream out, String format, args...) @format(1)
{
	Formatter formatter;
	formatter.init(&out_putstream_fn, &out);
	return formatter.vprintf(format, args);
}

<*
 Prints using a 'printf'-style formatting string,
 appending '\n' at the end. See `printf`.

 @param [inout] out : `The OutStream to print to`
 @param [in] format : `The printf-style format string`
 @return `the number of characters printed`
*>
fn usz? fprintfn(OutStream out, String format, args...) @format(1) @maydiscard
{
	Formatter formatter;
	formatter.init(&out_putstream_fn, &out);
	usz len = formatter.vprintf(format, args)!;
	out.write_byte('\n')!;
	if (&out.flush) out.flush()!;
	return len + 1;
}

<*
 @require @is_outstream(out) : "The output must implement OutStream"
*>
macro usz? fprintn(out, x = "")
{
	usz len = fprint(out, x)!;
	out.write_byte('\n')!;
	$switch:
		$case $typeof(out) == OutStream:
			if (&out.flush) out.flush()!;
		$case $defined(out.flush):
			out.flush()!;
	$endswitch
	return len + 1;
}

<*
 Print any value to stdout.
*>
macro void print(x)
{
	(void)fprint(io::stdout(), x);
}

<*
 Print any value to stdout, appending an '\n’ after.

 @param x : "The value to print"
*>
macro void printn(x = "")
{
	(void)fprintn(io::stdout(), x);
}

<*
 Print any value to stderr.
*>
macro void eprint(x)
{
	(void)fprint(io::stderr(), x);
}

<*
 Print any value to stderr, appending an '\n’ after.

 @param x : "The value to print"
*>
macro void eprintn(x = "")
{
	(void)fprintn(io::stderr(), x);
}


fn void? out_putstream_fn(void* data, char c) @private
{
	OutStream* stream = data;
	return (*stream).write_byte(c);
}

macro usz putchar_buf_size() @const
{
	$switch env::MEMORY_ENV:
		$case NORMAL: return 32 * 1024;
		$case SMALL: return 1024;
		$case TINY: return 256;
		$case NONE: return 256;
	$endswitch
}

struct PutcharBuffer
{
	char[putchar_buf_size()] data;
	usz len;
	bool should_flush;
}

fn void? write_putchar_buffer(PutcharBuffer* buff, bool flush) @private
{
	File* stdout = io::stdout();
	libc::fwrite(&buff.data, 1, buff.len, stdout.file);
	buff.len = 0;
	if (flush) stdout.flush()!;
}

fn void? out_putchar_buffer_fn(void* data @unused, char c) @private
{
	$if env::TESTING:
		// HACK: this is used for the purpose of unit test output hijacking
		File* stdout = io::stdout();
		assert(stdout.file);
		libc::fputc(c, stdout.file);
	$else
		PutcharBuffer* buff = data;
		buff.data[buff.len++] = c;
		if (c == '\n') buff.should_flush = true;
		if (buff.len == buff.data.len) write_putchar_buffer(buff, false)!;
	$endif
}

<*
 Prints using a 'printf'-style formatting string.
 To print integer numbers, use "%d" or "%x"/"%X,
 the latter gives the hexadecimal representation.

 All types can be printed using "%s" which gives
 the default representation of the value.

 To create a custom output for a type, implement
 the Printable interface.

 @param [in] format : `The printf-style format string`
 @return `the number of characters printed`
*>
fn usz? printf(String format, args...) @format(0) @maydiscard
{
	Formatter formatter;
	PutcharBuffer buff;
	formatter.init(&out_putchar_buffer_fn, &buff);
	usz? len = formatter.vprintf(format, args);
	write_putchar_buffer(&buff, buff.should_flush)!;
	return len;
}

<*
 Prints using a 'printf'-style formatting string,
 appending '\n' at the end. See `printf`.

 @param [in] format : `The printf-style format string`
 @return `the number of characters printed`
*>
fn usz? printfn(String format, args...) @format(0) @maydiscard
{
	Formatter formatter;
	PutcharBuffer buff;
	formatter.init(&out_putchar_buffer_fn, &buff);
	usz? len = formatter.vprintf(format, args);
	formatter.out('\n')!;
	write_putchar_buffer(&buff, true)!;
	return len + 1;
}

<*
 Prints using a 'printf'-style formatting string
 to stderr.

 @param [in] format : `The printf-style format string`
 @return `the number of characters printed`
*>
fn usz? eprintf(String format, args...) @maydiscard
{
	Formatter formatter;
	OutStream stream = stderr();
	formatter.init(&out_putstream_fn, &stream);
	return formatter.vprintf(format, args);
}


<*
 Prints using a 'printf'-style formatting string,
 to stderr appending '\n' at the end. See `printf`.

 @param [in] format : `The printf-style format string`
 @return `the number of characters printed`
*>
fn usz? eprintfn(String format, args...) @maydiscard
{
	Formatter formatter;
	OutStream stream = stderr();
	formatter.init(&out_putstream_fn, &stream);
	usz? len = formatter.vprintf(format, args);
	stderr().write_byte('\n')!;
	stderr().flush()!;
	return len + 1;
}

<*
 Prints using a 'printf'-style formatting string,
 to a string buffer. See `printf`.

 @param [inout] buffer : `The buffer to print to`
 @param [in] format : `The printf-style format string`
 @return `a slice formed from the "buffer" with the resulting length.`
*>
fn char[]? bprintf(char[] buffer, String format, args...) @maydiscard
{
	Formatter formatter;
	BufferData data = { .buffer = buffer };
	formatter.init(&out_buffer_fn, &data);
	usz size = formatter.vprintf(format, args)!;
	return buffer[:data.written];
}

// Used to print to a buffer.
fn void? out_buffer_fn(void *data, char c) @private
{
	BufferData *buffer_data = data;
	if (buffer_data.written >= buffer_data.buffer.len) return BUFFER_EXCEEDED?;
	buffer_data.buffer[buffer_data.written++] = c;
}

// Used for buffer printing
struct BufferData @private
{
	char[] buffer;
	usz written;
}

// Only available with LIBC
module std::io @if (env::LIBC);
import libc;

<*
 Libc `putchar`, prints a single character to stdout.
*>
fn void putchar(char c) @inline
{
	libc::putchar(c);
}

<*
 Get standard out.

 @return `stdout as a File`
*>
fn File* stdout()
{
	static File file;
	if (!file.file) file = file::from_handle(libc::stdout());
	return &file;
}

<*
 Get standard err.

 @return `stderr as a File`
*>
fn File* stderr()
{
	static File file;
	if (!file.file) file = file::from_handle(libc::stderr());
	return &file;
}

<*
 Get standard in.

 @return `stdin as a File`
*>
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
	(void)stdout_file.write_byte(c);
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

