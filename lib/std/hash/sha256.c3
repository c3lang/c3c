module std::hash::sha256;
import std::bits, std::hash::hmac;

const BLOCK_SIZE = 64;
const HASH_SIZE = 32;

alias HmacSha256 = Hmac{Sha256, HASH_SIZE, BLOCK_SIZE};
alias hmac = hmac::hash{Sha256, HASH_SIZE, BLOCK_SIZE};
alias pbkdf2 = hmac::pbkdf2{Sha256, HASH_SIZE, BLOCK_SIZE};

const uint[64] K @local = {
	0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
	0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
	0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
	0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
	0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
	0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
	0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
	0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
};

macro uint @ch(uint x, uint y, uint z) @local => (x & y) ^ (~x & z);
macro uint @maj(uint x, uint y, uint z) @local => (x & y) ^ (x & z) ^ (y & z);
macro uint @_sigma0(uint x) @local => x.rotr(2) ^ x.rotr(13) ^ x.rotr(22);
macro uint @_sigma1(uint x) @local => x.rotr(6) ^ x.rotr(11) ^ x.rotr(25);
macro uint @sigma0(uint x) @local => x.rotr(7) ^ x.rotr(18) ^ (x >> 3);
macro uint @sigma1(uint x) @local => x.rotr(17) ^ x.rotr(19) ^ (x >> 10);

struct Sha256
{
	uint[8] state @align(usz.sizeof);
	char[BLOCK_SIZE] buffer @align(ulong.sizeof);   // must align along bitcount sizeof - see `final`
	ulong bitcount;
}

<*
 Compute and return a hash value.

 @param [in] data : "The input data to hash."
*>
fn char[HASH_SIZE] hash(char[] data)
{
	Sha256 sha256 @noinit;
	sha256.init();
	sha256.update(data);
	return sha256.final();
}

fn void Sha256.init(&self) => *self = {
	.state = {
		0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
		0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
	},
};

<*
 @param [in] data
 @require data.len <= uint.max
*>
fn void Sha256.update(&self, char[] data)
{
	uint buffer_pos = (uint)(self.bitcount >> 3) % BLOCK_SIZE;
	self.bitcount += (ulong)data.len << 3; // always record ingested bits count immediately

	// Get the buffer position back to 0 if we're midway through consuming some data.
	if (buffer_pos > 0 && buffer_pos < BLOCK_SIZE)
	{
		usz len = min(BLOCK_SIZE - buffer_pos, data.len);
		self.buffer[buffer_pos:len] = data[:len];
		data = data[len..];
		if (buffer_pos + len == BLOCK_SIZE) self.transform();
	}

	// When the data pointer is aligned, we can disregard unaligned loading in the `transform` macro.
	//   We do this here from the outer call to reduce the expense of checking alignment on every single block.
	if (0 == (usz)data.ptr % usz.sizeof)
	{
		for (; data.len >= BLOCK_SIZE; data = data[BLOCK_SIZE..]) self.transform((uint*)data.ptr);
	}
	else
	{
		for (; data.len >= BLOCK_SIZE; data = data[BLOCK_SIZE..]) self.transform_unaligned((uint*)data.ptr);
	}

	// Leftover data just gets stored away for the next update or final.
	if (data.len)
	{
		self.buffer[..] = 0;
		self.buffer[:data.len] = data[..];
	}
}

fn char[HASH_SIZE] Sha256.final(&self)
{
	char[HASH_SIZE] hash @align(uint.sizeof);
	ulong i = (self.bitcount / 8) % BLOCK_SIZE;

	// Append 0x80 to the buffer
	self.buffer[i++] = 0x80;

	// Pad the buffer with zeros
	if (i > BLOCK_SIZE - 8)
	{
		self.buffer[i..] = 0x00;
		self.transform();
		i = 0;  // Reset buffer index after transformation
	}

	self.buffer[i..(BLOCK_SIZE - 8)] = 0x00;

	// Append the bitcount in big-endian format
	*(ulong*)(&self.buffer[BLOCK_SIZE - 8]) = env::BIG_ENDIAN ??? self.bitcount : bswap(self.bitcount);

	self.transform();

	// Convert state to the final hash
	foreach (x, s : self.state) *(uint*)(&hash[x * uint.sizeof]) = env::BIG_ENDIAN ??? s : bswap(s);
	
	return hash;
}

// These wrappers are necessary to significantly reduce code generation from macro expansions.
//   Note that transformations on `self.buffer` (when incoming == null) should always be aligned.
fn void Sha256.transform(&self, uint* incoming = null) @local @noinline => self.do_transform(incoming, true);
fn void Sha256.transform_unaligned(&self, uint* incoming = null) @local @noinline => self.do_transform(incoming, false);

macro Sha256.do_transform(&self, uint* incoming = null, bool $aligned = true) @local
{
	uint a, b, c, d, e, f, g, h, t1, t2 @noinit;
	uint[64] m @noinit;
	int i @noinit;

	if (!incoming) incoming = (uint*)&self.buffer;

	$if env::BIG_ENDIAN:
		@as_char_view(m)[:BLOCK_SIZE] = @as_char_view(incoming)[:BLOCK_SIZE];
	$else
		// Unrolling this seems to make the hash slower.
		for (i = 0; i < 16; ++i) m[i] = bswap($aligned ??? incoming[i] : @unaligned_load(incoming[i], 1));
	$endif

	for (i = 16; i < 64; i++) m[i] = @sigma1(m[i - 2]) + m[i - 7] + @sigma0(m[i - 15]) + m[i - 16];

	a = self.state[0];
	b = self.state[1];
	c = self.state[2];
	d = self.state[3];
	e = self.state[4];
	f = self.state[5];
	g = self.state[6];
	h = self.state[7];

	$for usz $i = 0; $i < 64; $i++:
		t1 = h + @_sigma1(e) + @ch(e, f, g) + K[$i] + m[$i];
		t2 = @_sigma0(a) + @maj(a, b, c);
		h = g;
		g = f;
		f = e;
		e = d + t1;
		d = c;
		c = b;
		b = a;
		a = t1 + t2;
	$endfor

	self.state[0] += a;
	self.state[1] += b;
	self.state[2] += c;
	self.state[3] += d;
	self.state[4] += e;
	self.state[5] += f;
	self.state[6] += g;
	self.state[7] += h;
}

