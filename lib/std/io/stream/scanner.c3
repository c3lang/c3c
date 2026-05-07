module std::io;

struct Scanner (InStream)
{
	InStream wrapped_stream;
	char[] buf;
	sz pattern_idx;
	sz read_idx;
}

<*
 Scanner provides a way to read delimited data (with newlines as the default).
 The supplied buffer must be at least as large as the expected data length
 including its pattern.

 @param [&in] stream : "The stream to read data from."
 @require buffer.len > 0 : "Non-empty buffer required."
*>
fn void Scanner.init(&self, InStream stream, char[] buffer)
{
	*self = { .wrapped_stream = stream, .buf = buffer };
}


<*
 Return and clear any remaining unscanned data.
*>
fn char[] Scanner.flush(&self) @dynamic
{
	assert(self.read_idx >= self.pattern_idx);
	sz n = self.read_idx - self.pattern_idx;
	char[] buf = self.buf[self.pattern_idx:n];
	self.pattern_idx = 0;
	self.read_idx = 0;
	return buf;
}

fn void? Scanner.close(&self) @dynamic
{
	if (&self.wrapped_stream.close) return self.wrapped_stream.close();
}

<*
 Scan the stream for the next split character and return data up to the match.
 @require pattern.len > 0 : "Non-empty pattern required."
 @require self.buf.len > pattern.len : "Pattern too large."
*>
fn char[]? Scanner.scan(&self, String pattern = "\n")
{
	if (self.read_idx == 0)
	{
		// First read.
		self.read_idx = _scanner_refill(self, self.buf)!;
		self.pattern_idx = 0;
	}
	assert(self.read_idx >= self.pattern_idx);
	sz n = self.read_idx - self.pattern_idx;
	char[] buf = self.buf[self.pattern_idx:n];
	if (try i = _scanner_find(self, buf, pattern))
	{
		self.pattern_idx += i + pattern.len;
		return buf[:i];
	}
	if (self.pattern_idx == 0 || self.read_idx < self.buf.len)
	{
		// Split pattern not found with maximized search, abort.
		// Split pattern not found and already read as much as possible.
		return NOT_FOUND~;
	}
	// Split pattern not found: maximize the search and try one more time.
	self.buf[:n] = buf[..];
	self.pattern_idx = 0;

	buf = self.buf[n..];
	sz p = _scanner_refill(self, buf)!;
	self.read_idx = n + p;

	buf = buf[:p];
	sz i = _scanner_find(self, buf, pattern)!;
	self.pattern_idx = n + i + pattern.len;

	return self.buf[:n + i];
}

macro sz? _scanner_find(Scanner* self, buf, pattern) @private
{
	return ((String)buf).index_of(pattern);
}

macro sz? _scanner_refill(Scanner* self, buf) @private
{
	sz? n = self.wrapped_stream.read(buf);
	if (catch err = n)
	{
		if (err == io::EOF) return NOT_FOUND~;
		return err~;
	}
	return n;
}

fn sz? Scanner.read(&self, char[] bytes) @dynamic
{
	sz n;
	if (self.pattern_idx < self.read_idx)
	{
		n = min(bytes.len, self.read_idx - self.pattern_idx);
		bytes[:n] = self.buf[self.pattern_idx:n];
		self.pattern_idx += n;
		bytes = bytes[n..];
	}
	n += self.wrapped_stream.read(bytes)!;
	return n;
}

fn char? Scanner.read_byte(&self) @dynamic
{
	if (self.pattern_idx < self.read_idx)
	{
		return self.buf[self.pattern_idx++];
	}
	return self.wrapped_stream.read_byte();
}
