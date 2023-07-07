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
 * @param [inout] self
 * @require bytes.len > 0
 * @require self.bytes.len == 0 "Init may not run on already initialized data"
 **/
fn void ReadBuffer.init(&self, Stream stream, char[] bytes)
{
    *self = { .stream = stream, .bytes = bytes };
}

fn Stream ReadBuffer.as_stream(&self)
{
    return { .fns = &readbuffer_interface, .data = self };
}

StreamInterface readbuffer_interface = {
    .read_fn = fn(s, char[] bytes) => ((ReadBuffer*)s.data).read(bytes),
    .read_byte_fn = fn(s) => ((ReadBuffer*)s.data).read_byte(),
};

fn String ReadBuffer.as_str(&self)
{
    return (String)self.bytes[self.read_idx:self.write_idx - self.read_idx];
}

fn usz! ReadBuffer.read(&self, char[] bytes)
{
    if (self.read_idx == self.write_idx)
    {
        if (self.read_idx == 0 && bytes.len >= self.bytes.len)
        {
            // Read directly into the input buffer.
            return self.stream.read(bytes)!;
        }
        self.refill()!;
    }
    usz n = min(self.write_idx - self.read_idx, bytes.len);
    bytes[:n] = self.bytes[self.read_idx:n];
    self.read_idx += n;
    return n;
}

fn char! ReadBuffer.read_byte(&self)
{
    if (self.read_idx == self.write_idx) self.refill()!;
    if (self.read_idx == self.write_idx) return IoError.EOF?;
    char c = self.bytes[self.read_idx];
    self.read_idx++;
    return c;
}

fn void! ReadBuffer.refill(&self) @local
{
    self.read_idx = 0;
    self.write_idx = self.stream.read(self.bytes)!;
}

struct WriteBuffer
{
    Stream stream;
    char[] bytes;
    usz index;
}

/**
 * Buffer writes to a stream. Call `flush` when done writing to the buffer.
 * @param [inout] self
 * @require bytes.len > 0 "Non-empty buffer required"
 * @require self.bytes.len == 0 "Init may not run on already initialized data"
 **/
fn void WriteBuffer.init(&self, Stream stream, char[] bytes)
{
    *self = { .stream = stream, .bytes = bytes };
}

fn Stream WriteBuffer.as_stream(&self)
{
    return { .fns = &writebuffer_interface, .data = self };
}

StreamInterface writebuffer_interface = {
    .flush_fn = fn(s) => ((WriteBuffer*)s.data).flush(),
    .write_fn = fn(s, char[] bytes) => ((WriteBuffer*)s.data).write(bytes),
    .write_byte_fn = fn(s, char c) => ((WriteBuffer*)s.data).write_byte(c),
};

fn String WriteBuffer.as_str(&self)
{
    return (String)self.bytes[:self.index];
}

fn void! WriteBuffer.flush(&self)
{
    self.write_pending()!;
    if (self.stream.supports_flush()) self.stream.flush()!;
}

fn usz! WriteBuffer.write(&self, char[] bytes)
{
    usz n = self.bytes.len - self.index;
    if (bytes.len < n)
    {
        // Enough room in the buffer.
        self.bytes[self.index:bytes.len] = bytes[..];
        self.index += bytes.len;
        return bytes.len;
    }
    self.write_pending()!;
    if (bytes.len >= self.bytes.len)
    {
        // Write directly to the stream.
        return self.stream.write(bytes);
    }
    // Buffer the data.
    self.bytes[:bytes.len] = bytes[..];
    self.index = bytes.len;
    return bytes.len;
}

fn void! WriteBuffer.write_byte(&self, char c)
{
    usz n = self.bytes.len - self.index;
    if (n == 0) self.write_pending()!;
    self.bytes[0] = c;
    self.index = 1;
}

fn void! WriteBuffer.write_pending(&self) @local
{
    self.index -= self.stream.write(self.bytes[:self.index])!;
    if (self.index != 0) return IoError.INCOMPLETE_WRITE?;
}