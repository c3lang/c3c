module std::io;

struct ReadBuffer
{
    Stream stream;
    char[] bytes;
    usz read_idx;
    usz write_idx;
}

/**
 * Buffer reads from a stream.
 * @param [inout] b
 * @require bytes.len > 0
 * @require b.bytes.len == 0 "Init may not run on already initialized data"
 **/
fn void ReadBuffer.init(ReadBuffer *b, Stream stream, char[] bytes)
{
    *b = { .stream = stream, .bytes = bytes };
}

fn Stream ReadBuffer.as_stream(ReadBuffer *b)
{
    return { .fns = &readbuffer_interface, .data = b };
}

StreamInterface readbuffer_interface = {
    .read_fn = fn(s, char[] bytes) => ((ReadBuffer*)s.data).read(bytes),
    .read_byte_fn = fn(s) => ((ReadBuffer*)s.data).read_byte(),
};

fn usz! ReadBuffer.read(ReadBuffer *b, char[] bytes)
{
    if (b.read_idx == b.write_idx)
    {
        if (b.read_idx == 0 && bytes.len >= b.bytes.len)
        {
            // Read directly into the input buffer.
            return b.stream.read(bytes)!;
        }
        b.refill()!;
    }
    usz n = min(b.write_idx - b.read_idx, bytes.len);
    bytes[:n] = b.bytes[b.read_idx:n];
    b.read_idx += n;
    return n;
}

fn char! ReadBuffer.read_byte(ReadBuffer *b)
{
    if (b.read_idx == b.write_idx) b.refill()!;
    if (b.read_idx == b.write_idx) return IoError.EOF?;
    char c = b.bytes[b.read_idx];
    b.read_idx++;
    return c;
}

fn void! ReadBuffer.refill(ReadBuffer* b) @local
{
    b.read_idx = 0;
    b.write_idx = b.stream.read(b.bytes)!;
}

struct WriteBuffer
{
    Stream stream;
    char[] bytes;
    usz index;
}

/**
 * Buffer writes to a stream. Call `flush` when done writing to the buffer.
 * @param [inout] b
 * @require bytes.len > 0 "Non-empty buffer required"
 * @require b.bytes.len == 0 "Init may not run on already initialized data"
 **/
fn void WriteBuffer.init(WriteBuffer *b, Stream stream, char[] bytes)
{
    *b = { .stream = stream, .bytes = bytes };
}

fn Stream WriteBuffer.as_stream(WriteBuffer *b)
{
    return { .fns = &writebuffer_interface, .data = b };
}

StreamInterface writebuffer_interface = {
    .flush_fn = fn(s) => ((WriteBuffer*)s.data).flush(),
    .write_fn = fn(s, char[] bytes) => ((WriteBuffer*)s.data).write(bytes),
    .write_byte_fn = fn(s, char c) => ((WriteBuffer*)s.data).write_byte(c),
};

fn String WriteBuffer.as_str(WriteBuffer* b)
{
    return (String)b.bytes[:b.index];
}

fn void! WriteBuffer.flush(WriteBuffer* b)
{
    b.write_pending()!;
    if (b.stream.supports_flush()) b.stream.flush()!;
}

fn usz! WriteBuffer.write(WriteBuffer* b, char[] bytes)
{
    usz n = b.bytes.len - b.index;
    if (bytes.len < n)
    {
        // Enough room in the buffer.
        b.bytes[b.index:bytes.len] = bytes[..];
        b.index += bytes.len;
        return bytes.len;
    }
    b.write_pending()!;
    if (bytes.len >= b.bytes.len)
    {
        // Write directly to the stream.
        return b.stream.write(bytes);
    }
    // Buffer the data.
    b.bytes[:bytes.len] = bytes[..];
    b.index = bytes.len;
    return bytes.len;
}

fn void! WriteBuffer.write_byte(WriteBuffer* b, char c)
{
    usz n = b.bytes.len - b.index;
    if (n == 0) b.write_pending()!;
    b.bytes[0] = c;
    b.index = 1;
}

fn void! WriteBuffer.write_pending(WriteBuffer* b) @local
{
    b.index -= b.stream.write(b.bytes[:b.index])!;
    if (b.index != 0) return IoError.INCOMPLETE_WRITE?;
}