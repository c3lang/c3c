module std::io;

fn Stream DString.as_stream(DString* dstring)
{
	return { .fns = &dstring_interface, .data = dstring };
}

StreamInterface dstring_interface = {
	.destroy_fn = fn (s) => ((DString*)s.data).free(),
	.len_fn = fn (s) => ((DString*)s.data).len(),
	.write_fn = fn (s, char[] bytes) { ((DString*)s.data).append_chars((String)bytes); return bytes.len; },
	.write_byte_fn = fn (s, char c) => ((DString*)s.data).append_char(c),
	.read_stream_fn = fn (s, reader) => ((DString*)s.data).read_from_stream(reader),
};