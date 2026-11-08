<*
 DEFLATE decompression (RFC 1951).

 The decoder is table driven: literal/length and distance codes are resolved with
 a two level lookup table whose entries already carry the base value and the number
 of extra bits, so that a match is decoded without any further table access.
*>
module std::compression::deflate;
import std::io, std::math, std::thread;

const sz INFLATE_HISTORY @private = 32768;
const sz INFLATE_CHUNK @private = 32768;
// 258 (longest match) + 16 (a match is copied in 16 byte steps and may overshoot)
const sz INFLATE_MARGIN @private = 274;
const sz INFLATE_WINDOW_LEN @private = INFLATE_HISTORY + INFLATE_CHUNK + INFLATE_MARGIN;
const sz INFLATE_OWN_INPUT @private = 1024;
const sz INFLATE_MIN_INPUT @private = 64;

const int LIT_TABLE_BITS @private = 11;
const int DIST_TABLE_BITS @private = 8;
const int CODELEN_TABLE_BITS @private = 7;
const int LIT_TABLE_LEN @private = 2560;
const int DIST_TABLE_LEN @private = 512;

// Decode table entry: [31:16] value | [15:12] flags | [11:8] code length (subtable bits for ENTRY_SUB) | [7:0] bits to consume.
// For lengths and distances the bits to consume are the code length plus the extra bits.
const uint ENTRY_LIT @private = 0x1000;
const uint ENTRY_EOB @private = 0x2000;
const uint ENTRY_SUB @private = 0x4000;
const uint ENTRY_BAD @private = 0x8000;

enum InflatePhase @private
{
	HEADER,
	STORED,
	CODES,
	DONE
}

enum InflateStatus @private
{
	BLOCK_END,
	NEED_INPUT,
	OUT_FULL,
	FINISHED
}

<*
 The decoding state machine, independent of where input comes from and where output goes.
 Input is read from `in_buf[in_pos:in_len]`, refilled from `stream` when there is one.
 Output is appended to `out[0:out_cap]`, everything before `out_pos` is the history.
*>
struct InflateState @private
{
	InStream stream;
	SeekFn seek_fn;
	char[] in_buf;
	sz in_pos;
	sz in_len;
	ulong bit_buf;
	int bit_cnt;
	int pad_bits;
	char* out;
	sz out_pos;
	sz out_cap;
	sz out_limit;
	uint* lit_tab;
	uint* dist_tab;
	sz stored_left;
	InflatePhase phase;
	bool final_block;
	bool eof;
	char[320] lens;
	uint[LIT_TABLE_LEN] dyn_lit;
	uint[DIST_TABLE_LEN] dyn_dist;
}

<*
 Stateful DEFLATE inflater.
 Implements InStream for pull-based decompression.
*>
struct Inflater (InStream)
{
	InflateState st;
	sz out_read;
	char[INFLATE_OWN_INPUT] own_in;
	char[INFLATE_WINDOW_LEN] window @align(8);
}

fn void Inflater.init(&self, InStream input, char[] bit_reader_buf = {})
{
	*self = {};
	inflate_init_tables();
	InflateState* st = &self.st;
	st.stream = input;
	st.seek_fn = &input.seek;
	st.in_buf = bit_reader_buf.len >= INFLATE_MIN_INPUT ? bit_reader_buf : self.own_in[..];
	st.set_output(&self.window[0], INFLATE_WINDOW_LEN);
	st.phase = HEADER;
}

fn sz? Inflater.read(&self, char[] buffer) @dynamic
{
	InflateState* st = &self.st;
	sz total = 0;
	while (total < buffer.len)
	{
		sz avail = st.out_pos - self.out_read;
		if (avail > 0)
		{
			sz n = math::min(avail, buffer.len - total);
			mem::copy(buffer.ptr + total, st.out + self.out_read, n);
			self.out_read += n;
			total += n;
			continue;
		}
		if (st.phase == DONE) break;
		if (st.out_pos > st.out_limit) self.slide();
		st.pump()!;
	}
	return total;
}

fn char? Inflater.read_byte(&self) @dynamic
{
	return io::read_byte_using_read(self);
}

fn void? Inflater.close(&self) @dynamic
{
	self.st.release();
}

<*
 Copy the input that was read from the underlying stream but not used by the DEFLATE data.
 @param dest : `Where to copy the unused bytes.`
 @return `The number of bytes copied.`
*>
fn sz Inflater.copy_unused_input(&self, char[] dest)
{
	InflateState* st = &self.st;
	st.bit_buf >>= (st.bit_cnt & 7);
	st.bit_cnt -= st.bit_cnt & 7;
	sz from_bits = math::min((sz)(st.bit_cnt / 8), dest.len);
	sz from_buf = math::min(st.in_len - st.in_pos, dest.len - from_bits);
	// `dest` may alias the input buffer: move the buffered input first, then the bytes held in the bit buffer.
	if (from_buf > 0)
	{
		mem::move(dest.ptr + from_bits, st.in_buf.ptr + st.in_pos, from_buf);
		st.in_pos += from_buf;
	}
	for (sz i = 0; i < from_bits; i++)
	{
		dest[i] = (char)st.bit_buf;
		st.bit_buf >>= 8;
		st.bit_cnt -= 8;
	}
	return from_bits + from_buf;
}

// Discard all but the last 32 KiB of history. Only valid when all output has been consumed.
fn void Inflater.slide(&self)
{
	InflateState* st = &self.st;
	sz keep = math::min(st.out_pos, INFLATE_HISTORY);
	mem::move(st.out, st.out + st.out_pos - keep, keep);
	st.out_pos = keep;
	self.out_read = keep;
}

<*
 Decompress a complete DEFLATE stream held in memory.
 @param allocator : `The allocator for the result.`
 @param input : `The compressed data.`
 @return `The decompressed data.`
*>
fn char[]? decompress(Allocator allocator, char[] input)
{
	if (!input.len) return alloc::new_array(allocator, char, 0);

	InflateState st;
	inflate_init_tables();
	st.in_buf = input;
	st.in_len = input.len;
	st.eof = true;
	st.phase = HEADER;

	sz cap = math::max(input.len * 3, (sz)4096);
	char[] buf = alloc::alloc_array(allocator, char, cap);
	defer catch alloc::free(allocator, buf.ptr);
	st.set_output(buf.ptr, cap);
	while (st.pump()! != FINISHED)
	{
		if (cap > sz::max / 2) return io::OVERFLOW~;
		cap *= 2;
		char* grown = alloc::realloc(allocator, buf.ptr, cap);
		buf = grown[:cap];
		st.set_output(buf.ptr, cap);
	}
	sz len = st.out_pos;
	if (!len)
	{
		alloc::free(allocator, buf.ptr);
		return alloc::new_array(allocator, char, 0);
	}
	char* shrunk = alloc::realloc(allocator, buf.ptr, len);
	return shrunk[:len];
}

<*
 Decompress stream using DEFLATE.
 Reads from `input`, writes to `output`.
 Uses blocking I/O and the temp allocator
*>
fn void? decompress_stream(InStream input, OutStream output) => @pool()
{
	Inflater* inflater = mem::tnew(Inflater);
	char[] bit_buf = mem::tnew(char[8192]);
	inflater.init(input, bit_buf);
	io::copy_to(inflater, output)!;
}

// Point the decoder at an output buffer. Everything before `out_pos` is kept as history.
fn void InflateState.set_output(&self, char* out, sz cap)
{
	self.out = out;
	self.out_cap = cap;
	self.out_limit = cap - INFLATE_MARGIN;
}

// Run the state machine until the output buffer needs attention (OUT_FULL) or the stream ended (FINISHED).
fn InflateStatus? InflateState.pump(&self)
{
	while (true)
	{
		switch (self.phase)
		{
			case HEADER:
				self.read_header()!;
			case STORED:
				self.copy_stored()!;
				if (self.stored_left > 0) return OUT_FULL;
				self.unpad()!;
				self.end_block();
			case CODES:
				InflateStatus status = self.run()!;
				switch (status)
				{
					case NEED_INPUT:
						self.read_more()!;
					case OUT_FULL:
						self.unpad()!;
						return OUT_FULL;
					default:
						self.unpad()!;
						self.end_block();
				}
			case DONE:
				return FINISHED;
		}
	}
}

fn void InflateState.end_block(&self)
{
	if (!self.final_block)
	{
		self.phase = HEADER;
		return;
	}
	self.phase = DONE;
	self.release();
}

// Give input that was read but not used back to the stream, when the stream can seek.
fn void InflateState.release(&self)
{
	if (!self.seek_fn) return;
	long seek_back = (long)(self.in_len - self.in_pos) + (long)(self.bit_cnt / 8);
	if (seek_back > 0 && @catch(self.seek_fn(self.stream, -seek_back, FROM_CURSOR))) return;
	self.in_len = 0;
	self.in_pos = 0;
	self.bit_cnt = 0;
	self.bit_buf = 0;
}

// Padding is added when the input ends inside the bit buffer. Using it means the input was truncated.
fn void? InflateState.unpad(&self)
{
	if (!self.pad_bits) return;
	if (self.pad_bits > self.bit_cnt) return CORRUPTED_DATA~;
	self.bit_cnt -= self.pad_bits;
	self.bit_buf &= (1UL << self.bit_cnt) - 1;
	self.pad_bits = 0;
}

// Read more input from the stream, keeping the unread tail of the buffer.
fn void? InflateState.read_more(&self)
{
	if (self.eof) return;
	sz rem = self.in_len - self.in_pos;
	if (self.in_pos > 0)
	{
		if (rem > 0) mem::move(self.in_buf.ptr, self.in_buf.ptr + self.in_pos, rem);
		self.in_pos = 0;
		self.in_len = rem;
	}
	sz? got = self.stream.read(self.in_buf[rem..]);
	if (catch err = got)
	{
		if (err != io::EOF) return err~;
		self.eof = true;
		return;
	}
	if (!got)
	{
		self.eof = true;
		return;
	}
	self.in_len = rem + got;
}

// Top up the bit buffer to more than 56 bits, blocking on input. Zero bits are added when the input ended.
fn void? InflateState.fill(&self)
{
	while (self.bit_cnt <= 56)
	{
		if (self.in_pos < self.in_len)
		{
			self.bit_buf |= (ulong)self.in_buf[self.in_pos++] << self.bit_cnt;
		}
		else if (!self.eof)
		{
			self.read_more()!;
			continue;
		}
		else
		{
			self.pad_bits += 8;
		}
		self.bit_cnt += 8;
	}
}

fn void? InflateState.need(&self, int bits) @inline
{
	if (self.bit_cnt < bits) self.fill()!;
}

fn uint InflateState.take(&self, int bits) @inline
{
	uint value = (uint)(self.bit_buf & ((1UL << bits) - 1));
	self.bit_buf >>= bits;
	self.bit_cnt -= bits;
	return value;
}

fn void? InflateState.read_header(&self)
{
	self.need(3)!;
	uint header = self.take(3);
	self.final_block = (header & 1) != 0;
	switch (header >> 1)
	{
		case 0:
			self.take(self.bit_cnt & 7);
			self.need(32)!;
			uint len = self.take(16);
			uint nlen = self.take(16);
			if ((len ^ 0xFFFF) != nlen) return CORRUPTED_DATA~;
			self.stored_left = (sz)len;
			self.phase = STORED;
		case 1:
			self.lit_tab = &fixed_lit_table[0];
			self.dist_tab = &fixed_dist_table[0];
			self.phase = CODES;
		case 2:
			self.read_dynamic()!;
			self.phase = CODES;
		default:
			return CORRUPTED_DATA~;
	}
	if (self.pad_bits > self.bit_cnt) return CORRUPTED_DATA~;
}

fn void? InflateState.read_dynamic(&self)
{
	self.need(14)!;
	int nlit = (int)self.take(5) + 257;
	int ndist = (int)self.take(5) + 1;
	int ncode = (int)self.take(4) + 4;
	if (nlit > 286 || ndist > 30) return CORRUPTED_DATA~;

	char[19] code_lens;
	for (int i = 0; i < ncode; i++)
	{
		self.need(3)!;
		code_lens[ORDER[i]] = (char)self.take(3);
	}
	uint[128] code_tab;
	if (!build_decode_table(&code_tab[0], 128, CODELEN_TABLE_BITS, code_lens[..], codelen_template[..])) return CORRUPTED_DATA~;

	int total = nlit + ndist;
	int i = 0;
	while (i < total)
	{
		self.need(14)!;
		uint entry = code_tab[self.bit_buf & 127];
		if (entry & ENTRY_BAD) return CORRUPTED_DATA~;
		self.take((int)(entry & 0xFF));
		int sym = (int)(entry >> 16);
		if (sym < 16)
		{
			self.lens[i++] = (char)sym;
			continue;
		}
		char value = 0;
		int repeat;
		switch (sym)
		{
			case 16:
				if (i == 0) return CORRUPTED_DATA~;
				value = self.lens[i - 1];
				repeat = 3 + (int)self.take(2);
			case 17:
				repeat = 3 + (int)self.take(3);
			default:
				repeat = 11 + (int)self.take(7);
		}
		if (i + repeat > total) return CORRUPTED_DATA~;
		for (int r = 0; r < repeat; r++) self.lens[i++] = value;
	}
	if (self.pad_bits > self.bit_cnt) return CORRUPTED_DATA~;
	if (self.lens[256] == 0) return CORRUPTED_DATA~;
	if (!build_decode_table(&self.dyn_lit[0], LIT_TABLE_LEN, LIT_TABLE_BITS, self.lens[:nlit], litlen_template[..])) return CORRUPTED_DATA~;
	if (!build_decode_table(&self.dyn_dist[0], DIST_TABLE_LEN, DIST_TABLE_BITS, self.lens[nlit:ndist], dist_template[..])) return CORRUPTED_DATA~;
	self.lit_tab = &self.dyn_lit[0];
	self.dist_tab = &self.dyn_dist[0];
}

// Copy as much of a stored block as fits in the output buffer.
fn void? InflateState.copy_stored(&self)
{
	while (self.stored_left > 0)
	{
		sz room = self.out_cap - self.out_pos;
		if (room <= 0) return;
		sz n = math::min(self.stored_left, room);
		// Whole bytes that are still in the bit buffer come first.
		while (n > 0 && self.bit_cnt >= 8)
		{
			self.out[self.out_pos++] = (char)self.bit_buf;
			self.bit_buf >>= 8;
			self.bit_cnt -= 8;
			self.stored_left--;
			n--;
		}
		if (self.pad_bits > self.bit_cnt) return CORRUPTED_DATA~;
		if (n == 0) continue;
		sz avail = self.in_len - self.in_pos;
		if (avail == 0)
		{
			if (self.eof) return CORRUPTED_DATA~;
			self.read_more()!;
			continue;
		}
		sz m = math::min(n, avail);
		mem::copy(self.out + self.out_pos, self.in_buf.ptr + self.in_pos, m);
		self.in_pos += m;
		self.out_pos += m;
		self.stored_left -= m;
	}
}

fn ulong load_le64(char* p) @private @inline
{
	ulong w = mem::load((ulong*)p, 1);
	$if env::BIG_ENDIAN:
		w = bswap(w);
	$endif
	return w;
}

<*
 The hot loop: decode literal/length and distance codes of one block.
 Returns BLOCK_END at the end-of-block code, OUT_FULL when the output is close to full, or
 NEED_INPUT when fewer than 8 bytes of input are buffered and the stream may have more.
*>
fn InflateStatus? InflateState.run(&self)
{
	ulong bb = self.bit_buf;
	int bc = self.bit_cnt;
	int pad = self.pad_bits;
	sz ip = self.in_pos;
	sz in_len = self.in_len;
	char* inp = self.in_buf.ptr;
	bool eof = self.eof;
	char* out = self.out;
	sz op = self.out_pos;
	sz limit = self.out_limit;
	uint* lt = self.lit_tab;
	uint* dt = self.dist_tab;
	defer
	{
		self.bit_buf = bb & ((1UL << bc) - 1);
		self.bit_cnt = bc;
		self.pad_bits = pad;
		self.in_pos = ip;
		self.out_pos = op;
	}

	while (true)
	{
		if (op > limit) return OUT_FULL;

		// Refill. The fast path always leaves 56..63 bits in the buffer.
		if (@likely(ip + 8 <= in_len))
		{
			bb |= load_le64(inp + ip) << bc;
			ip += (sz)((63 - bc) >> 3);
			bc |= 56;
		}
		else
		{
			if (pad > bc) return CORRUPTED_DATA~;
			if (!eof) return NEED_INPUT;
			while (bc <= 56)
			{
				if (ip < in_len)
				{
					bb |= (ulong)inp[ip++] << bc;
				}
				else
				{
					pad += 8;
				}
				bc += 8;
			}
		}

		uint e = lt[bb & (1UL << LIT_TABLE_BITS - 1)];
		if (e & ENTRY_SUB)
		{
			bb >>= (e & 0xFF);
			bc -= (int)(e & 0xFF);
			e = lt[(e >> 16) + (uint)(bb & ((1UL << ((e >> 8) & 0xF)) - 1))];
		}
		ulong saved = bb;
		int total = (int)(e & 0xFF);
		bb >>= total;
		bc -= (int)total;

		if (e & ENTRY_LIT)
		{
			out[op++] = (char)(e >> 16);
			// At least 41 bits are left: decode up to two more literals without refilling.
			e = lt[bb & (1UL << LIT_TABLE_BITS - 1)];
			if (e & ENTRY_LIT)
			{
				bb >>= (e & 0xFF);
				bc -= (int)(e & 0xFF);
				out[op++] = (char)(e >> 16);
				e = lt[bb & (1UL << LIT_TABLE_BITS - 1)];
				if (e & ENTRY_LIT)
				{
					bb >>= (e & 0xFF);
					bc -= (int)(e & 0xFF);
					out[op++] = (char)(e >> 16);
				}
			}
			continue;
		}
		if (e & ENTRY_EOB) return BLOCK_END;
		if (e & ENTRY_BAD) return CORRUPTED_DATA~;

		// Length: base value plus extra bits. The extra bits sit right behind the code in `saved`.
		int len = (int)(e >> 16 + (uint)((saved & (1UL << total - 1)) >> ((e >> 8) & 0xF)));

		// Distance. At most 20 bits were used so far, so there is no need to refill.
		uint d = dt[bb & (1UL << DIST_TABLE_BITS - 1)];
		if (d & ENTRY_SUB)
		{
			bb >>= (d & 0xFF);
			bc -= (int)(d & 0xFF);
			d = dt[(d >> 16) + (uint)(bb & ((1UL << ((d >> 8) & 0xF)) - 1))];
		}
		if (d & ENTRY_BAD) return CORRUPTED_DATA~;
		saved = bb;
		total = (int)(d & 0xFF);
		bb >>= total;
		bc -= (int)total;
		int dist = (int)(d >> 16 + (uint)((saved & (1UL << total - 1)) >> ((d >> 8) & 0xF)));
		if ((sz)dist > op) return CORRUPTED_DATA~;

		// Copy the match. Output room for 258 bytes plus 16 bytes of overshoot is guaranteed by `limit`.
		char* dst = out + op;
		char* src = dst - (sz)dist;
		op += (sz)len;
		if (dist >= 16)
		{
			char* end = out + op;
			do
			{
				mem::store((char[<16>]*)dst, mem::load((char[<16>]*)src, 1), 1);
				dst += 16;
				src += 16;
			}
			while (dst < end);
		}
		else if (dist >= 8)
		{
			char* end = out + op;
			do
			{
				mem::store((ulong*)dst, mem::load((ulong*)src, 1), 1);
				dst += 8;
				src += 8;
			}
			while (dst < end);
		}
		else if (dist == 1)
		{
			char c = src[0];
			dst[:len] = c;
		}
		else
		{
			for (int i = 0; i < len; i++) dst[i] = src[i];
		}
	}
}

// Build a two level decode table from code lengths. `tmpl` holds one entry (without code length) per symbol.
// Returns false for over-subscribed codes and for incomplete codes, except for the single one-bit code allowed by RFC 1951.
fn bool build_decode_table(uint* table, int table_len, int root_bits, char[] lens, uint[] tmpl) @private
{
	int[16] count;
	foreach (l : lens) count[l]++;
	count[0] = 0;

	int used = 0;
	int max_len = 0;
	int left = 1;
	for (int l = 1; l <= 15; l++)
	{
		used += count[l];
		left = (left << 1) - count[l];
		if (left < 0) return false;
		if (count[l]) max_len = l;
	}
	if (left > 0 && used > 0 && !(used == 1 && count[1] == 1)) return false;

	int root_size = 1 << root_bits;
	for (int i = 0; i < root_size; i++) table[i] = ENTRY_BAD;
	if (!used) return true;

	// Symbols sorted by code length, then symbol value: the canonical code order.
	int[16] offs;
	for (int l = 1; l < 15; l++) offs[l + 1] = offs[l] + count[l];
	int[288] sorted;
	foreach (int s, l : lens)
	{
		if (l) sorted[offs[l]++] = s;
	}

	int[16] remaining = count;
	uint code = 0;
	int idx = 0;
	int next_free = root_size;
	bool in_sub = false;
	uint sub_prefix = 0;
	int sub_base = 0;
	int sub_bits = 0;
	uint root_mask = (uint)root_size - 1;
	for (int l = 1; l <= max_len; l++)
	{
		for (int k = 0; k < count[l]; k++)
		{
			int sym = sorted[idx++];
			uint rev = (uint)reverse16((ushort)code) >> (16 - l);
			if (l <= root_bits)
			{
				uint entry = tmpl[sym] + (uint)l * 0x101;
				for (int j = (int)rev; j < root_size; j += 1 << l) table[j] = entry;
			}
			else
			{
				uint prefix = rev & root_mask;
				if (!in_sub || prefix != sub_prefix)
				{
					// Start a new subtable, sized to hold every code that shares this prefix.
					in_sub = true;
					sub_prefix = prefix;
					int curr = l - root_bits;
					int room = 1 << curr;
					while (curr + root_bits < max_len)
					{
						room -= remaining[curr + root_bits];
						if (room <= 0) break;
						curr++;
						room <<= 1;
					}
					if (next_free + (1 << curr) > table_len) return false;
					sub_bits = curr;
					sub_base = next_free;
					for (int j = 0; j < (1 << curr); j++) table[next_free + j] = ENTRY_BAD;
					table[prefix] = ENTRY_SUB | (uint)root_bits | ((uint)curr << 8) | ((uint)sub_base << 16);
					next_free += 1 << curr;
				}
				uint entry = tmpl[sym] + (uint)(l - root_bits) * 0x101;
				for (int j = (int)(rev >> root_bits); j < 1 << sub_bits; j += 1 << (l - root_bits)) table[sub_base + j] = entry;
			}
			code++;
			remaining[l]--;
		}
		code <<= 1;
	}
	return true;
}

uint[288] litlen_template @private;
uint[32] dist_template @private;
uint[19] codelen_template @private;
uint[LIT_TABLE_LEN] fixed_lit_table @private;
uint[DIST_TABLE_LEN] fixed_dist_table @private;

fn void inflate_init_tables() @private
{
	static OnceFlag once;
	once.call(fn void() {
		for (int s = 0; s < 256; s++) litlen_template[s] = ENTRY_LIT | ((uint)s << 16);
		litlen_template[256] = ENTRY_EOB;
		for (int s = 257; s < 286; s++)
		{
			litlen_template[s] = ((uint)LENGTH_BASE[s - 257] << 16) | (uint)LENGTH_EXTRA[s - 257];
		}
		litlen_template[286] = ENTRY_BAD;
		litlen_template[287] = ENTRY_BAD;
		for (int s = 0; s < 30; s++)
		{
			dist_template[s] = ((uint)DIST_BASE[s] << 16) | (uint)DIST_EXTRA[s];
		}
		dist_template[30] = ENTRY_BAD;
		dist_template[31] = ENTRY_BAD;
		for (int s = 0; s < 19; s++) codelen_template[s] = (uint)s << 16;

		char[288] lit_lens;
		for (int s = 0; s <= 143; s++) lit_lens[s] = 8;
		for (int s = 144; s <= 255; s++) lit_lens[s] = 9;
		for (int s = 256; s <= 279; s++) lit_lens[s] = 7;
		for (int s = 280; s <= 287; s++) lit_lens[s] = 8;
		build_decode_table(&fixed_lit_table[0], LIT_TABLE_LEN, LIT_TABLE_BITS, lit_lens[..], litlen_template[..]);
		char[32] dist_lens;
		dist_lens[..] = 5;
		build_decode_table(&fixed_dist_table[0], DIST_TABLE_LEN, DIST_TABLE_BITS, dist_lens[..], dist_template[..]);
	});
}
