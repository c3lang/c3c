module std::io;

struct ByteReader (InStream)
{
	char[] bytes;
	sz index;
}

fn long ByteReader.len(&self) @dynamic
{
	return self.bytes.len;
}

fn ByteReader* ByteReader.init(&self, char[] bytes)
{
	*self = { .bytes = bytes };
	return self;
}

fn sz? ByteReader.read(&self, char[] bytes) @dynamic
{
	if (self.index >= self.bytes.len) return io::EOF~;
	sz len = min(self.bytes.len - self.index, bytes.len);
	if (len == 0) return 0;
	mem::copy(bytes.ptr, &self.bytes[self.index], len);
	self.index += len;
	return len;
}

fn char? ByteReader.read_byte(&self) @dynamic
{
	if (self.index >= self.bytes.len) return io::EOF~;
	return self.bytes[self.index++];
}

fn void? ByteReader.pushback_byte(&self) @dynamic
{
	if (!self.index) return INVALID_PUSHBACK~;
	self.index--;
}

fn long? ByteReader.cursor(&self) @dynamic
{
	return self.index;
}

fn void? ByteReader.seek(&self, long offset, SeekOrigin whence = FROM_START) @dynamic
{
	long new_index;
	switch (whence)
	{
		case FROM_START:  new_index = offset;
		case FROM_CURSOR: new_index = self.index + offset;
		case FROM_END:    new_index = self.bytes.len + offset;
	}
	if (new_index < 0 || new_index > self.bytes.len) return INVALID_POSITION~;
	self.index = (sz)new_index;
}

fn long? ByteReader.write_to(&self, OutStream writer) @dynamic
{
	if (self.index >= self.bytes.len) return 0;
	sz written = writer.write(self.bytes[self.index..])!;
	self.index += written;
	assert(self.index <= self.bytes.len);
	return written;
}

fn long? ByteReader.available(&self) @inline @dynamic
{
	return max(0, self.bytes.len - self.index);
}
