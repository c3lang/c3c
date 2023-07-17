module std::io::stream;

struct LimitReader
{
    Stream reader;
    usz limit;
}

fn void LimitReader.init(&self, Stream reader, usz limit)
{
    *self = { .reader = reader, .limit = limit };
}

fn usz! LimitReader.read(&self, char[] bytes)
{
    if (self.limit == 0) return IoError.EOF?;
    usz m = min(bytes.len, self.limit);
    usz n = self.reader.read(bytes[:m])!;
    self.limit -= n;
    return n;
}

fn Stream LimitReader.as_stream(&self)
{
    return { .fns = &limitreader_interface, .data = self };
}

fn usz LimitReader.available(&self)
{
    return self.limit;
}

StreamInterface limitreader_interface = {
    .read_fn = fn(s, char[] bytes) => ((LimitReader*)s.data).read(bytes),
    .available_fn = fn(s) => ((LimitReader*)s.data).available(),
};