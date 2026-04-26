module std::io;

struct BitReader
{
	InStream reader;
	uint bits;
	int len;
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
 @require nbits <= 8 && nbits > 0
 @require self.len + (sz)nbits <= uint::size * 8
*>
fn char? BitReader.read_bits(&self, int nbits)
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
	uint mask = (1u << nbits) - 1u;
	return (char)((bits >> self.len) & mask);
}

struct BitWriter
{
	OutStream writer;
	uint bits;
	int len;
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
	if (!self.len) return;

	int padding = (@sizeof(self.bits) * 8 - self.len);
	uint bits = self.bits << padding;
	sz n = ((sz)self.len + 7) / 8;
	char[4] buffer;
	bitorder::write(bits, &buffer, UIntBE);
	io::write_all(self.writer, buffer[:n])!;
	self.len = 0;
}

<*
 @require nbits <= 32 && nbits >= 0
*>
fn void? BitWriter.write_bits(&self, uint bits, int nbits)
{
	if (!nbits) return;
	while (self.len + nbits > WRITER_BITS)
	{
		int to_push = WRITER_BITS - self.len;
		uint bits_to_push = (bits >> (nbits - to_push)) & ((1u << to_push) - 1u);

		self.bits <<= to_push;
		self.bits |= bits_to_push;
		self.len += to_push;
		nbits -= to_push;

		self.flush()!;
	}

	if (!nbits) return;

	self.bits <<= nbits;
	self.bits |= bits & ((1u << nbits) - 1u);
	self.len += nbits;
}