module std::io;

struct BitReader
{
	InStream reader;
	uint bits;
	uint len;
}

fn void BitReader.init(&self, InStream byte_reader)
{
	*self = { .reader = byte_reader };
}

fn void BitReader.clear(&self) @inline
{
	self.len = 0;
}

<*
 @require nbits <= 8
 @require self.len + nbits <= uint.sizeof * 8
*>
fn char? BitReader.read_bits(&self, uint nbits)
{
	uint bits = self.bits;
	if (self.len < nbits)
	{
		// New bits are pushed right.
		char c = self.reader.read_byte()!;
		bits <<= 8;
		bits |= c;
		self.bits = bits;
		self.len += 8;
	}
	self.len -= nbits;
	uint mask = (1 << nbits) - 1;
	return (char)((bits >> self.len) & mask);
}

struct BitWriter
{
	OutStream writer;
	uint bits;
	uint len;
}

// c3 doesn't allow to shift more than bit width of a variable,
// so use closest byte boundary of 24 instead of 32
const int WRITER_BITS = 24;

fn void BitWriter.init(&self, OutStream byte_writer)
{
	*self = { .writer = byte_writer };
}

fn void? BitWriter.flush(&self)
{
	if (self.len == 0) return;

	int padding = ($sizeof(self.bits) * 8 - self.len);
	uint bits = self.bits << padding;
	uint n = (self.len + 7) / 8;
	char[4] buffer;
	bitorder::write(bits, &buffer, UIntBE);
	io::write_all(self.writer, buffer[:n])!;
	self.len = 0;
}

<*
 @require nbits <= 32
*>
fn void? BitWriter.write_bits(&self, uint bits, uint nbits)
{
	if (nbits == 0) return;
	while (self.len + nbits > WRITER_BITS)
	{
		uint to_push = WRITER_BITS - self.len;
		uint bits_to_push = (bits >> (nbits - to_push)) & ((1 << to_push) - 1);

		self.bits <<= to_push;
		self.bits |= bits_to_push;
		self.len += to_push;
		nbits -= to_push;

		self.flush()!;
	}

	if (nbits == 0) return;

	self.bits <<= nbits;
	self.bits |= bits & ((1 << nbits) - 1);
	self.len += nbits;
}