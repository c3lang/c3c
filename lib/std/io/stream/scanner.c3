module std::io;

struct Scanner
{
	Stream* stream;
	char[] buf;
	usz pattern_idx;
	usz read_idx;
}

/**
 * Scanner provides a way to read delimited data (with newlines as the default).
 * The supplied buffer must be at least as large as the expected data length
 * including its pattern.
 *
 * @param [&in] stream "The stream to read data from."
 * @require buffer.len > 0 "Non-empty buffer required."
 * @require stream.supports_read() "The stream must support read."
 *
 **/
fn void Scanner.init(&self, Stream* stream, char[] buffer)
{
	*self = { .stream = stream, .buf = buffer };
}

/**
 * Return and clear any remaining unscanned data.
 **/
fn char[] Scanner.flush(&self)
{
	assert(self.read_idx >= self.pattern_idx);
	usz n = self.read_idx - self.pattern_idx;
	char[] buf = self.buf[self.pattern_idx:n];
	self.pattern_idx = 0;
	self.read_idx = 0;
	return buf;
}

/**
 * Scan the stream for the next split character and return data up to the match.
 * @require pattern.len > 0 "Non-empty pattern required."
 * @require self.buf.len > pattern.len "Pattern too large."
 **/
fn char[]! Scanner.scan(&self, String pattern = "\n")
{
	if (self.read_idx == 0)
	{
		// First read.
		self.read_idx = self.refill(self.buf)!;
		self.pattern_idx = 0;
	}
	assert(self.read_idx >= self.pattern_idx);
	usz n = self.read_idx - self.pattern_idx;
	char[] buf = self.buf[self.pattern_idx:n];
	if (try i = self.find(buf, pattern))
	{
		self.pattern_idx += i + pattern.len;
		return buf[:i];
	}
	if (self.pattern_idx == 0 || self.read_idx < self.buf.len)
	{
		// Split pattern not found with maximized search, abort.
		// Split pattern not found and already read as much as possible.
		return SearchResult.MISSING?;
	}
	// Split pattern not found: maximize the search and try one more time.
	self.buf[:n] = buf[..];
	self.pattern_idx = 0;

	buf = self.buf[n..];
	usz p = self.refill(buf)!;
	self.read_idx = n + p;

	buf = buf[:p];
	usz i = self.find(buf, pattern)!;
	self.pattern_idx = n + i + pattern.len;

	return self.buf[:n + i];
}

macro usz! Scanner.find(&self, buf, pattern) @private
{
	return ((String)buf).index_of(pattern);
}

macro usz! Scanner.refill(&self, buf) @private
{
	usz! n = self.stream.read(buf);
	if (catch err = n)
	{
		case IoError.EOF:
			return SearchResult.MISSING?;
		default:
			return err?;
	}
	return n;
}