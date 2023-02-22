module std::io;

fn Stream File.as_stream(File* file)
{
	return { .fns = &filestream_interface, .data = file };
}

StreamInterface filestream_interface = {
	.close_fn = fn (s) => ((File*)s.data).close(),
	.seek_fn = fn (s, offset, seek) => ((File*)s.data).seek(offset, seek) @inline,
	.read_fn = fn (s, char[] bytes) => ((File*)s.data).read(bytes) @inline,
	.write_fn = fn (s, char[] bytes) => ((File*)s.data).write(bytes) @inline,
	.write_byte_fn = fn (s, char c) => ((File*)s.data).putc(c) @inline,
	.read_byte_fn = fn (s) => ((File*)s.data).getc() @inline,
	.flush_fn = fn (s) => ((File*)s.data).flush() @inline,
};


