<*
 DEFLATE compression implementation (RFC 1951).

 API:
 - fn char[]? decompress(Allocator allocator, char[] input)
 - fn void?   decompress_stream(InStream input, OutStream output)
 - fn void?   compress_stream(Allocator allocator, InStream input, OutStream output, int level = DEFAULT_LEVEL)
 - fn char[]? compress(Allocator allocator, char[] input, int level = DEFAULT_LEVEL)
   `level` runs from MIN_LEVEL (1, fastest) to MAX_LEVEL (9, smallest output).
 - struct Deflater: stateful compressor (use `init`, `write`, then `close`).
 - struct Inflater: InStream for pull-based decompression (use `init` then `read`).
*>
module std::compression::deflate;
import std::io, std::math, std::bits, std::sort, std::collections::list;
import std::thread;

const int MIN_MATCH = 3;
const int MAX_MATCH = 258;
const uint WINDOW_SIZE = 32768;
const uint HASH_SIZE = 32768;
const uint HASH_MASK = HASH_SIZE - 1;
const int MIN_LEVEL = 1;       // fastest
const int MAX_LEVEL = 9;       // best compression
const int DEFAULT_LEVEL = 4;

// Per level search effort. `lazy` is the longest match for which the next position is still searched for
// a longer one; 0 selects greedy matching.
const ushort[10] LEVEL_CHAIN @private = { 0, 2, 4, 8, 16, 16, 32, 64, 128, 512 };
const ushort[10] LEVEL_GOOD @private = { 0, 8, 8, 16, 32, 8, 8, 16, 32, 64 };
const ushort[10] LEVEL_NICE @private = { 0, 16, 32, 64, 128, 32, 128, 128, 258, 258 };
const ushort[10] LEVEL_LAZY @private = { 0, 0, 0, 0, 0, 16, 16, 32, 64, 258 };
const int TOO_FAR @private = 4096;      // a match of length 3 further away than this costs more than three literals

excuse
{
	CORRUPTED_DATA,
	IO_ERROR,
	INVALID_STATE
}


fn void? compress_stream(Allocator allocator, InStream input, OutStream output, int level = DEFAULT_LEVEL)
{
	Deflater* deflater = alloc::new(allocator, Deflater);
	deflater.init(allocator, output, level);
	defer {
		(void)deflater.close();
		alloc::free(allocator, deflater);
	}

	char[] buf = alloc::alloc_array(allocator, char, 32768);
	defer alloc::free(allocator, buf);

	while (sz n = io::read_buffer(input, buf)!)
	{
		deflater.write(buf[:n])!;
	}
}

struct Deflater (OutStream)
{
	OutStream output;
	Allocator allocator;

	uint[] head;
	uint[] prev;
	char[] window;
	uint window_fill;

	Token[] tokens;
	sz token_count;
	sz max_tokens;
	uint[286] lit_freqs;
	uint[30] dist_freqs;

	BitWriter bwriter;

	uint hash;
	uint strstart;
	uint match_pos;
	bool hash_ready;
	int max_chain;
	int good_match;
	int nice_match;
	int max_lazy;
	bool pending;      // lazy matching: the position before `match_pos` is waiting for a decision
	int prev_len;      // the match found at that position (0 if none)
	int prev_dist;
	char[] source;      // the complete input when it is known (in-memory compress), used for stored blocks
	sz block_origin;    // offset in `source` where the current block starts
	sz block_bytes;     // input bytes covered by the tokens of the current block
	sz total_in;
	sz total_out;
	bool closed;
}

<*
 @param allocator : `The allocator for the compressor state.`
 @param output : `Where compressed data is written.`
 @param level : `MIN_LEVEL (fastest) to MAX_LEVEL (smallest output).`
 @require level >= MIN_LEVEL && level <= MAX_LEVEL
*>
fn void Deflater.init(&self, Allocator allocator, OutStream output = null, int level = DEFAULT_LEVEL)
{
	*self = {};
	self.max_chain = LEVEL_CHAIN[level];
	self.good_match = LEVEL_GOOD[level];
	self.nice_match = LEVEL_NICE[level];
	self.max_lazy = LEVEL_LAZY[level];
	deflate_init_tables();
	self.allocator = allocator;
	self.output = output;
	self.head = alloc::alloc_array(allocator, uint, HASH_SIZE);
	self.head[..] = 0xFFFFFFFF;
	self.prev = alloc::alloc_array(allocator, uint, WINDOW_SIZE);
	self.window = alloc::alloc_array(allocator, char, WINDOW_SIZE * 2 + MAX_MATCH + 1);
	self.window_fill = WINDOW_SIZE;
	self.strstart = WINDOW_SIZE;
	self.match_pos = WINDOW_SIZE;
	self.max_tokens = WINDOW_SIZE * 4;
	self.tokens = alloc::alloc_array(allocator, Token, self.max_tokens + 1);
	self.bwriter.init(allocator, WINDOW_SIZE);
}

fn void Deflater.free(&self)
{
	if (self.head.ptr) alloc::free(self.allocator, self.head);
	if (self.prev.ptr) alloc::free(self.allocator, self.prev);
	if (self.window.ptr) alloc::free(self.allocator, self.window);
	if (self.tokens.ptr) alloc::free(self.allocator, self.tokens);
	if (self.bwriter.data.ptr) alloc::free(self.allocator, self.bwriter.data.ptr);
}

fn sz? Deflater.write(&self, char[] data) @dynamic
{
	if (self.closed) return IO_ERROR~;

	sz remaining = data.len;
	sz offset = 0;
	while (remaining > 0)
	{
		sz space = (WINDOW_SIZE * 2) - (sz)self.window_fill;
		assert(space >= 0);
		if (space == 0)
		{
			self.scan_matches(false)!;
			mem::copy(self.window, self.window[WINDOW_SIZE..], WINDOW_SIZE);
			self.window_fill -= WINDOW_SIZE;
			self.match_pos -= WINDOW_SIZE;
			space = WINDOW_SIZE;
		}

		sz to_copy = math::min(remaining, space);
		mem::copy(self.window[self.window_fill..], data[offset..], to_copy);
		self.window_fill += (uint)to_copy;
		remaining -= to_copy;
		offset += to_copy;

		self.scan_matches(false)!;
	}
	self.total_in += data.len;
	return data.len;
}

fn void? Deflater.write_byte(&self, char c) @dynamic
{
	char[1] b = { c };
	self.write(b[..])!;
}

fn void? Deflater.close(&self) @dynamic
{
	if (self.closed) return;
	self.scan_matches(true)!;
	self.emit_block(true)!;
	self.closed = true;
	self.free();
}

fn void? Deflater.scan_matches(&self, bool final)
{
	char[] input = self.window;
	uint wfill = self.window_fill;

	if (!self.hash_ready && wfill > WINDOW_SIZE + 1)
	{
		self.hash = ((uint)input[WINDOW_SIZE] << 5) ^ (uint)input[WINDOW_SIZE + 1];
		self.hash_ready = true;
	}

	// Copy hot fields to locals for register allocation
	uint[] head = self.head;
	uint[] prev = self.prev;
	uint[] lfreqs = self.lit_freqs[..];
	uint[] dfreqs = self.dist_freqs[..];
	Token[] tokens = self.tokens;
	sz tcount = self.token_count;
	sz max_t = self.max_tokens;
	sz bbytes = self.block_bytes;

	int max_chain = self.max_chain;
	int good_match = self.good_match;
	int nice_match = self.nice_match;
	int max_lazy = self.max_lazy;
	bool pending = self.pending;
	int prev_len = self.prev_len;
	int prev_dist = self.prev_dist;

	uint hash = self.hash;
	uint strstart = self.strstart;
	long pos = self.match_pos;
	uint win_offset = strstart - (uint)pos;

	while (pos < wfill)
	{
		int best_len = 0;
		int best_dist = 0;

		if (pos + 2 < wfill)
		{
			hash = ((hash << 5) ^ (uint)input[pos + 2]) & HASH_MASK;
			uint match_head = head[hash];
			head[hash] = strstart;
			prev[strstart & 0x7FFF] = match_head;

			// With lazy matching the previous position already has a match: only a longer one is interesting,
			// and a good enough one is not worth looking past.
			if (!pending || prev_len < max_lazy)
			{
				int floor_len = prev_len >= MIN_MATCH ? prev_len : 0;
				int chain = max_chain;
				if (floor_len >= good_match) chain >>= 2;
				best_len = floor_len;
				int chain_len = 0;
				uint curr = match_head;

				while (curr != 0xFFFFFFFF && strstart - curr < WINDOW_SIZE && chain_len < chain)
				{
					chain_len++;
					sz curr_rel = (sz)(curr - win_offset);

					if (pos + best_len < wfill && input[curr_rel + best_len] == input[pos + best_len])
					{
						if (best_len < 3 || mem::load((ushort*)&input[pos + best_len - 1], 1) == mem::load((ushort*)&input[curr_rel + best_len - 1], 1))
						{
							int match_l = 0;
							while (match_l + 8 <= MAX_MATCH && pos + match_l + 8 <= wfill)
							{
								if (mem::load((ulong*)&input[curr_rel + match_l], 1) == mem::load((ulong*)&input[pos + match_l], 1))
								{
									match_l += 8;
								}
								else { break; }
							}

							while (match_l < MAX_MATCH &&
								   pos + match_l < wfill &&
								   input[curr_rel + match_l] == input[pos + match_l])
							{
								match_l++;
							}

							if (match_l >= MIN_MATCH && match_l > best_len)
							{
								best_len = match_l;
								best_dist = (int)(pos - curr_rel);
								if (best_len >= nice_match) break;
							}
						}
					}
					curr = prev[curr & 0x7FFF];
					if (best_len >= good_match) chain_len++;
				}
				if (best_len == floor_len) best_len = 0;
			}
		}
		else if (!final)
		{
			break;
		}

		if (best_len == MIN_MATCH && best_dist > TOO_FAR) best_len = 0;

		// Decide what to emit now. A match starts at `pos`, or at `pos - 1` when the pending match wins.
		int m_len = 0;
		int m_dist = 0;
		long lit_at = -1;
		long next_pos = pos + 1;
		if (max_lazy == 0)
		{
			if (best_len >= MIN_MATCH)
			{
				m_len = best_len;
				m_dist = best_dist;
				next_pos = pos + best_len;
			}
			else
			{
				lit_at = pos;
			}
		}
		else if (pending && prev_len >= MIN_MATCH && best_len == 0)
		{
			m_len = prev_len;
			m_dist = prev_dist;
			next_pos = pos - 1 + prev_len;
			pending = false;
			prev_len = 0;
		}
		else
		{
			if (pending) lit_at = pos - 1;
			pending = true;
			prev_len = best_len;
			prev_dist = best_dist;
		}

		if (lit_at >= 0)
		{
			lfreqs[(uint)input[lit_at]]++;
			tokens[tcount++] = { (ushort)input[lit_at], 0 };
			bbytes++;
		}
		if (m_len > 0)
		{
			lfreqs[257 + len_code_table[m_len]]++;
			dfreqs[dist_code_of((uint)m_dist)]++;
			tokens[tcount++] = { (ushort)m_len, (ushort)m_dist };
			bbytes += m_len;
		}

		// Add the positions that were skipped over to the hash chains.
		uint ss = strstart;
		for (long q = pos + 1; q < next_pos; q++)
		{
			ss++;
			if (q + 2 < wfill)
			{
				hash = ((hash << 5) ^ (uint)input[q + 2]) & HASH_MASK;
				uint m_h = head[hash];
				head[hash] = ss;
				prev[ss & 0x7FFF] = m_h;
			}
		}
		strstart += (uint)(next_pos - pos);
		pos = next_pos;

		if (tcount >= max_t)
		{
			self.token_count = tcount;
			self.block_bytes = bbytes;
			self.emit_block(false)!;
			tcount = 0;
			bbytes = 0;
		}
	}

	// At the very end only a literal can still be pending: nothing is left to extend a match.
	if (final && pending)
	{
		if (prev_len >= MIN_MATCH)
		{
			lfreqs[257 + len_code_table[prev_len]]++;
			dfreqs[dist_code_of((uint)prev_dist)]++;
			tokens[tcount++] = { (ushort)prev_len, (ushort)prev_dist };
			bbytes += prev_len;
		}
		else
		{
			lfreqs[(uint)input[pos - 1]]++;
			tokens[tcount++] = { (ushort)input[pos - 1], 0 };
			bbytes++;
		}
		pending = false;
		prev_len = 0;
	}

	// Write back hot fields
	self.token_count = tcount;
	self.block_bytes = bbytes;
	self.pending = pending;
	self.prev_len = prev_len;
	self.prev_dist = prev_dist;
	self.hash = hash;
	self.strstart = strstart;
	self.match_pos = (uint)pos;
}

fn void? Deflater.emit_block(&self, bool final) => @pool()
{
	if (self.token_count == 0 && !final) return;

	self.lit_freqs[256] = 1;
	self.tokens[self.token_count++] = { 256, 0 };

	// Build dynamic Huffman trees from frequency tables
	Huffman lit_huff, dist_huff;
	lit_huff.build_from_freqs(self.lit_freqs[..], 15);

	uint d_sum = 0;
	foreach (f : self.dist_freqs) d_sum += f;
	if (d_sum == 0) self.dist_freqs[0] = 1;
	dist_huff.build_from_freqs(self.dist_freqs[..], 15);

	sz saved_len = self.bwriter.len;
	ulong saved_buffer = self.bwriter.buffer;
	int saved_nbits = self.bwriter.nbits;

	self.bwriter.write_bits(final ? 1 : 0, 1);
	self.bwriter.write_bits(2, 2); // BTYPE=2 (Dynamic Huffman)

	char[] lit_lens = lit_huff.get_lengths(285);
	char[] dist_lens = dist_huff.get_lengths(29);

	sz hlit_cnt = 286;
	while (hlit_cnt > 257 && lit_lens[hlit_cnt - 1] == 0) hlit_cnt--;
	sz hdist_cnt = 30;
	while (hdist_cnt > 1 && dist_lens[hdist_cnt - 1] == 0) hdist_cnt--;

	self.bwriter.write_bits((uint)hlit_cnt - 257u, 5);
	self.bwriter.write_bits((uint)hdist_cnt - 1u, 5);

	// RLE-encode the combined code lengths
	List{Token} tree_tokens; tree_tokens.tinit();
	uint[] tree_freqs = mem::temp_array(uint, 19);
	char[] all_lens = mem::temp_array(char, hlit_cnt + hdist_cnt);
	all_lens[:hlit_cnt] = lit_lens[:hlit_cnt];
	all_lens[hlit_cnt:hdist_cnt] = dist_lens[:hdist_cnt];

	_rle_encode_tree_lengths(all_lens, &tree_tokens, tree_freqs);

	// Build the code length Huffman tree
	Huffman tree_huff;
	tree_huff.build_from_freqs(tree_freqs[..], 7);
	char[] tree_lens = tree_huff.get_lengths(18);
	sz hclen_cnt = 19;
	while (hclen_cnt > 4 && tree_lens[ORDER[hclen_cnt - 1]] == 0) hclen_cnt--;
	self.bwriter.write_bits((uint)hclen_cnt - 4u, 4);
	for (sz i = 0; i < hclen_cnt; i++) self.bwriter.write_bits(tree_lens[ORDER[i]], 3);

	// Write the RLE-encoded code lengths
	Code[19] tree_codes;
	gen_canonical_codes(&tree_codes, tree_lens);
	foreach (t : tree_tokens)
	{
		self.bwriter.write_huffman(tree_codes[t.val].code, tree_codes[t.val].len);
		switch (t.val)
		{
			case 16: self.bwriter.write_bits(t.dist, 2);
			case 17: self.bwriter.write_bits(t.dist, 3);
			case 18: self.bwriter.write_bits(t.dist, 7);
		}
	}

	// Generate canonical codes for lit/dist trees
	Code[286] lit_codes;
	gen_canonical_codes(&lit_codes, lit_lens[:hlit_cnt]);
	Code[30] dist_codes;
	gen_canonical_codes(&dist_codes, dist_lens[:hdist_cnt]);

	// Write the actual compressed data tokens
	_write_compressed_tokens(self, &lit_codes, &dist_codes);

	// Incompressible data: when stored blocks are smaller, replace the block just written.
	if (self.source.len > 0 && self.block_bytes > 0)
	{
		sz dynamic_bits = (self.bwriter.len - saved_len) * 8 + self.bwriter.nbits - saved_nbits;
		sz stored_blocks = (self.block_bytes + 65534) / 65535;
		sz stored_bits = (self.block_bytes + stored_blocks * 5) * 8;
		if (stored_bits < dynamic_bits)
		{
			self.bwriter.len = saved_len;
			self.bwriter.buffer = saved_buffer;
			self.bwriter.nbits = saved_nbits;
			self.bwriter.write_stored(self.source[self.block_origin:self.block_bytes], final);
		}
	}
	self.block_origin += self.block_bytes;
	self.block_bytes = 0;

	if (final)
	{
		char[] compressed = self.bwriter.finish();
		if (self.output) self.output.write(compressed)!;
		self.total_out += compressed.len;
	}
	else
	{
		if (self.output && self.bwriter.len > 0)
		{
			self.output.write(self.bwriter.data[:self.bwriter.len])!;
			self.total_out += self.bwriter.len;
			self.bwriter.len = 0;
		}
	}

	// Reset for next block
	tree_tokens.free();
	self.lit_freqs = {};
	self.dist_freqs = {};
	self.token_count = 0;
}

<*
 Compress data using the DEFLATE algorithm, this uses the temp allocator

 @param input : `The data to compress.`
 @param allocator : `The allocator to use.`
 @param level : `MIN_LEVEL (fastest) to MAX_LEVEL (smallest output), DEFAULT_LEVEL if omitted.`
 @return `The compressed data.`
 @require level >= MIN_LEVEL && level <= MAX_LEVEL
*>
fn char[]? compress(Allocator allocator, char[] input, int level = DEFAULT_LEVEL) => @pool()
{
	if (input.len == 0)
	{
		BitWriter writer;
		writer.init(allocator, 8);
		writer.write_bits(1, 1);
		writer.write_bits(1, 2);
		writer.write_huffman(0, 7);
		return writer.finish();
	}

	if (input.len < 128)
	{
		ensure_fixed_codes_built();
		BitWriter writer;
		writer.init(allocator, input.len + 16);
		writer.write_bits(1, 1); // final
		writer.write_bits(1, 2); // BTYPE=1
		foreach (c : input)
		{
			Code code = fixed_lit_codes_global[(uint)c];
			writer.write_huffman(code.code, code.len);
		}
		writer.write_huffman(fixed_lit_codes_global[256].code, fixed_lit_codes_global[256].len);
		return writer.finish();
	}

	Deflater* deflater = mem::tnew(Deflater);
	deflater.init(allocator, null, level);
	defer deflater.free();
	deflater.source = input;

	deflater.write(input)!;
	deflater.scan_matches(true)!;
	deflater.emit_block(true)!;

	char[] finished = deflater.bwriter.finish();
	char[] result = alloc::alloc_array(allocator, char, finished.len);
	result[..] = finished[..];
	return result;
}

/*-----------------------------------------------------------------------------*/

Code[288] fixed_lit_codes_global @private;
Code[32] fixed_dist_codes_global @private;
bool fixed_built_global @private;


fn void ensure_fixed_codes_built() @private
{
	if (fixed_lit_codes_global[0].len != 0) return;
	gen_fixed_codes(&fixed_lit_codes_global);
	for (uint i = 0; i < 32; i++) { fixed_dist_codes_global[i] = { (ushort)i, 5, (ushort)bits::reverse(i << 27) }; }
}


struct BitWriter @private
{
	char[] data;
	sz len;
	ulong buffer;
	int nbits;
	Allocator allocator;
}

<*
 @require initial_cap > 0
*>
fn void BitWriter.init(&self, Allocator allocator, sz initial_cap)
{
	self.allocator = allocator;
	self.data = alloc::alloc_array(allocator, char, initial_cap);
	self.len = 0;
	self.buffer = 0;
	self.nbits = 0;
}

// Bits are collected in a 64 bit buffer that is flushed 32 bits at a time. At most 31 bits are pending between calls.
fn void BitWriter.write_bits(&self, uint value, int count) @inline
{
	self.buffer |= (ulong)(value & ((1u << count) - 1u)) << self.nbits;
	self.nbits += count;
	if (self.nbits >= 32) self.flush32();
}

fn void BitWriter.flush32(&self)
{
	if (self.len + 4 > self.data.len) self.grow(4);
	uint w = (uint)self.buffer;
	char* dst = &self.data[self.len];
	dst[0] = (char)w;
	dst[1] = (char)(w >> 8);
	dst[2] = (char)(w >> 16);
	dst[3] = (char)(w >> 24);
	self.len += 4;
	self.buffer >>= 32;
	self.nbits -= 32;
}

// Make room for at least `extra` more bytes.
fn void BitWriter.grow(&self, sz extra)
{
	sz new_cap = self.data.len;
	if (new_cap < 1024) new_cap = 1024;
	while (new_cap < self.len + extra)
	{
		if (new_cap > sz::max / 2)
		{
			new_cap = self.len + extra;
			break;
		}
		new_cap *= 2;
	}
	self.data = alloc::realloc_array(self.allocator, self.data.ptr, char, new_cap);
}

// Pad with zero bits to the next byte boundary and flush every pending whole byte.
fn void BitWriter.align(&self)
{
	if (self.nbits & 7) self.write_bits(0, 8 - (self.nbits & 7));
	while (self.nbits > 0)
	{
		if (self.len >= self.data.len) self.grow(1);
		self.data[self.len++] = (char)self.buffer;
		self.buffer >>= 8;
		self.nbits -= 8;
	}
}

// Write `raw` as stored blocks (BTYPE=0, at most 65535 bytes each).
fn void BitWriter.write_stored(&self, char[] raw, bool final)
{
	sz pos = 0;
	do
	{
		sz n = math::min(raw.len - pos, (sz)65535);
		bool last = final && pos + n == raw.len;
		self.write_bits(last ? 1 : 0, 1);
		self.write_bits(0, 2);
		self.align();
		if (self.len + n + 4 > self.data.len) self.grow(n + 4);
		self.data[self.len++] = (char)n;
		self.data[self.len++] = (char)(n >> 8);
		self.data[self.len++] = (char)~n;
		self.data[self.len++] = (char)(~n >> 8);
		mem::copy(&self.data[self.len], &raw[pos], n);
		self.len += n;
		pos += n;
	}
	while (pos < raw.len);
}

fn void BitWriter.write_huffman(&self, uint code, int len)
{
	uint rev = bits::reverse(code << (32 - len));
	self.write_bits(rev, len);
}

fn char[] BitWriter.finish(&self)
{
	while (self.nbits > 0)
	{
		if (self.len >= self.data.len) self.grow(1);
		self.data[self.len++] = (char)self.buffer;
		self.buffer >>= 8;
		self.nbits = self.nbits > 8 ? self.nbits - 8 : 0;
	}
	return self.data[:self.len];
}

struct Huffman @private
{
	ushort[16] counts;
	ushort[288] symbols;
	uint[2048] table; // 11-bit lookup table: (len << 16) | symbol
}

fn int reverse16(ushort x) @private @inline
{
	uint res = x;
	res = ((res & 0xAAAA) >> 1) | ((res & 0x5555) << 1);
	res = ((res & 0xCCCC) >> 2) | ((res & 0x3333) << 2);
	res = ((res & 0xF0F0) >> 4) | ((res & 0x0F0F) << 4);
	res = ((res & 0xFF00) >> 8) | ((res & 0x00FF) << 8);
	return (ushort)res;
}

fn void Huffman.build(&self, char[] lengths)
{
	ushort[16] offsets;
	self.counts = {};
	foreach (len : lengths) { if (len > 0 && len < 16) self.counts[len]++; }
	ushort offset = 0;
	for (int i = 1; i < 16; i++) { offsets[i] = offset; offset += self.counts[i]; }
	foreach (int i, len : lengths) { if (len > 0 && len < 16) { ushort sym_idx = offsets[len]++; if (sym_idx < 288) self.symbols[sym_idx] = (ushort)i; } }

	// Build 11-bit lookup table
	self.table = {};
	uint code = 0;
	uint sym_idx = 0;
	for (int len = 1; len <= 11; len++)
	{
		int count = self.counts[len];
		for (int i = 0; i < count; i++)
		{
			ushort symbol = self.symbols[sym_idx++];
			int rev_code = (int)reverse16((ushort)code) >> (16 - len);
			uint entry = ((uint)len << 16) | symbol;
			int num_entries = 1 << (11 - len);
			for (int j = 0; j < num_entries; j++)
			{
				self.table[rev_code | (j << len)] = entry;
			}
			code++;
		}
		code <<= 1;
	}
}

struct Token @private
{
	ushort val;
	ushort dist;
}

struct IndexMap @private
{
	sz index;
	uint freq;
}
fn bool IndexMap.less(&self, IndexMap other) => self.freq < other.freq;

fn void _pkg_merge_backtrack(ulong[] is_merged, uint[] hist, sz max_elements, sz num_relevant, ulong mask) @local
{
	mask >>= 1;
	hist[..] = 0;
	sz num_analyze = num_relevant;
	while (mask != 0)
	{
		sz num_merged_loop = 0;
		if (max_elements >= 2) { hist[0]++; hist[1]++; }
		sz symbol = 2;
		for (sz i = symbol; i < num_analyze; i++)
		{
			if (i < max_elements && (is_merged[i] & mask) == 0)
			{
				if (symbol < max_elements) hist[symbol]++;
				symbol++;
			}
			else { num_merged_loop++; }
		}
		num_analyze = 2 * num_merged_loop;
		mask >>= 1;
	}
	for (sz i = 0; i < num_analyze; i++) hist[i]++;
}

fn sz _pkg_merge_round(uint[] hist, sz n, uint[] current, uint[] previous, sz num_previous, ulong[] is_merged, ulong mask, sz max_elements) @local
{
	num_previous &= (sz)~1;
	current[0] = hist[0];
	current[1] = hist[1];
	uint sum = current[0] + current[1];
	sz num_current = 2; sz num_hist = 2; sz num_merged = 0;

	while (true)
	{
		if (num_hist < n && hist[num_hist] <= sum)
		{
			if (num_current < max_elements)
			{
				current[num_current++] = hist[num_hist++];
				continue;
			} else { break; }
		}
		if (num_current < max_elements)
		{
			is_merged[num_current] |= mask;
			current[num_current] = sum;
			num_current++;
			num_merged++;
		}
		else
		{
			break;
		}

		if (num_merged * 2 >= num_previous) break;
		sum = previous[num_merged * 2] + previous[num_merged * 2 + 1];
	}
	while (num_hist < n && num_current < max_elements)
	{
		current[num_current++] = hist[num_hist++];
	}
	return num_current;
}

fn char[] pkg_merge(Allocator allocator, uint[] freqs, int max_bits) @private => @pool()
{
	IndexMap[] map = mem::temp_array(IndexMap, freqs.len);
	sz n = 0;
	foreach (i, freq : freqs) { if (freq != 0) map[n++] = {i, freq}; }
	if (n == 0) return {};
	IndexMap[] sorted_map = map[:n];
	sort::quicksort(sorted_map);
	if (n == 1)
	{
		char[] blen = alloc::new_array(allocator, char, freqs.len);
		blen[sorted_map[0].index] = 1;
		return blen;
	}
	sz max_elements = 2 * n;
	uint[] hist = mem::temp_array(uint, max_elements);
	foreach (i, m : sorted_map) hist[i] = m.freq;
	uint[] current = mem::temp_array(uint, max_elements);
	uint[] previous = mem::temp_array(uint, max_elements);
	ulong[] is_merged = mem::temp_array(ulong, max_elements);
	previous[:n] = hist[:n];
	sz num_previous = n;
	is_merged[:max_elements] = 0;
	sz num_relevant = 2 * n - 2;
	ulong mask = 1;

	for (int bits = max_bits - 1; bits > 0; bits--)
	{
		sz num_current = _pkg_merge_round(hist, n, current, previous, num_previous, is_merged, mask, max_elements);
		mask <<= 1;
		if (num_previous >= num_relevant)
		{
			bool keep_going = false;
			for (sz i = num_relevant; i > 1; i--)
			{
				if (previous[i - 1] != current[i - 1])
				{
					keep_going = true;
					break;
				}
			}
			if (!keep_going) break;
		}
		@swap(previous, current); num_previous = num_current;
	}

	_pkg_merge_backtrack(is_merged, hist, max_elements, num_relevant, mask);
	char[] blen = alloc::new_array(allocator, char, freqs.len);
	foreach (i, m : sorted_map) blen[m.index] = (char)hist[i];
	return blen;
}

fn char[] Huffman.get_lengths(&self, sz max_sym)
{
	char[] blen = alloc::new_array(tmem, char, max_sym + 1);
	ushort index = 0;
	for (int len = 1; len < 16; len++)
	{
		int count = self.counts[len];
		for (int i = 0; i < count; i++)
		{
			blen[self.symbols[index++]] = (char)len;
		}
	}
	return blen;
}

fn void Huffman.build_from_freqs(&self, uint[] freqs, int max_bits) => @pool()
{
	char[] blen = pkg_merge(tmem, freqs, max_bits);
	self.build(blen);
}

fn void gen_canonical_codes(Code* codes, char[] blen) @private
{
	ushort[16] bl_count; foreach (len : blen) { if (len > 0) bl_count[len]++; }
	ushort[16] next_code; ushort code = 0; bl_count[0] = 0;
	for (int bits = 1; bits <= 15; bits++) { code = (code + bl_count[bits - 1]) << 1; next_code[bits] = code; }
	foreach (uint n, len : blen)
	{
		if (len == 0)
		{
			codes[n].code = 0;
			codes[n].len = 0;
			codes[n].rcode = 0;
			continue;
		}
		uint c = next_code[len];
		codes[n].code = (ushort)c;
		codes[n].len = (ushort)len;
		// Deflate sends Huffman codes starting with the most significant bit, the bit writer is LSB first.
		codes[n].rcode = (ushort)bits::reverse(c << (32 - len));
		next_code[len]++;
	}
}

struct Code @private
{
	ushort code;
	ushort len;
	ushort rcode; // `code` bit-reversed, ready to be written LSB first
}

fn void gen_fixed_codes(Code* codes) @private
{
	char[288] lens;
	for (uint i = 0; i <= 143; i++) lens[i] = 8;
	for (uint i = 144; i <= 255; i++) lens[i] = 9;
	for (uint i = 256; i <= 279; i++) lens[i] = 7;
	for (uint i = 280; i <= 287; i++) lens[i] = 8;
	ushort[16] bl_count;
	for (uint i = 0; i < 288; i++) { if (lens[i] > 0) bl_count[lens[i]]++; }
	ushort[16] next_code; ushort code = 0; bl_count[0] = 0;
	for (int bits = 1; bits <= 15; bits++) { code = (code + bl_count[bits - 1]) << 1; next_code[bits] = code; }
	for (uint n = 0; n < 288; n++)
	{
		uint len = lens[n];
		if (len == 0)
		{
			codes[n].code = 0;
			codes[n].len = 0;
			codes[n].rcode = 0;
			continue;
		}
		uint c = next_code[len];
		codes[n].code = (ushort)c;
		codes[n].len = (ushort)len;
		codes[n].rcode = (ushort)bits::reverse(c << (32 - len));
		next_code[len]++;
	}
}

const ushort[31] LENGTH_BASE @private = { 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0 };
const char[31] LENGTH_EXTRA @private = { 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, 0, 0 };
const ushort[32] DIST_BASE @private = { 1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577, 0, 0 };
const char[32] DIST_EXTRA @private = { 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 0, 0 };
const uint[19] ORDER @private = { 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 };

// Length (3..258) to length code index (0..28), and distance (1..32768) to distance code.
char[259] len_code_table @private;
char[256] dist_code_small @private;
char[256] dist_code_large @private;

fn void deflate_init_tables() @private
{
	static OnceFlag once;
	once.call(fn void() {
		for (int c = 0; c < 29; c++)
		{
			int first = LENGTH_BASE[c];
			int last = c == 28 ? 258 : first + (1 << LENGTH_EXTRA[c]) - 1;
			for (int l = first; l <= last; l++) len_code_table[l] = (char)c;
		}
		// code 28 (258) must win over the last range of code 27, which does not include 258.
		for (int c = 0; c < 30; c++)
		{
			int first = DIST_BASE[c];
			int last = first + (1 << DIST_EXTRA[c]) - 1;
			for (int d = first; d <= last; d++)
			{
				if (d <= 256)
				{
					dist_code_small[d - 1] = (char)c;
				}
				else
				{
					dist_code_large[(d - 1) >> 7] = (char)c;
				}
			}
		}
	});
}

fn uint dist_code_of(uint dist) @private @inline
{
	return dist <= 256 ? dist_code_small[dist - 1] : dist_code_large[(dist - 1) >> 7];
}

fn void _write_compressed_tokens(Deflater* self, Code* lit_codes, Code* dist_codes) @local
{
	BitWriter* bw = &self.bwriter;
	foreach (t : self.tokens[:self.token_count])
	{
		if (bw.len + 16 > bw.data.len) bw.grow(16);
		if (t.dist == 0)
		{
			Code c = lit_codes[t.val];
			bw.write_bits(c.rcode, c.len);
			continue;
		}
		uint li = len_code_table[t.val];
		Code lc = lit_codes[257 + li];
		uint leb = LENGTH_EXTRA[li];
		bw.write_bits((uint)lc.rcode | (((uint)t.val - LENGTH_BASE[li]) << lc.len), (int)(lc.len + leb));
		uint di = dist_code_of(t.dist);
		Code dc = dist_codes[di];
		uint deb = DIST_EXTRA[di];
		bw.write_bits((uint)dc.rcode | (((uint)t.dist - DIST_BASE[di]) << dc.len), (int)(dc.len + deb));
	}
}

fn void _rle_encode_tree_lengths(char[] all_lens, List{Token}* tree_tokens, uint[] tree_freqs) @local
{
	for (long i = 0; i < all_lens.len;)
	{
		char len = all_lens[i];
		long count = 1;
		while (i + count < all_lens.len && all_lens[i + count] == len) count++;

		if (len == 0)
		{
			while (count >= 11)
			{
				uint c = (uint)math::min(count, (long)138);
				tree_tokens.push({ 18, (ushort)(c - 11u) });
				tree_freqs[18]++;
				i += c; count -= c;
			}
			while (count >= 3)
			{
				uint c = (uint)math::min(count, (long)10);
				tree_tokens.push({ 17, (ushort)(c - 3u) });
				tree_freqs[17]++;
				i += c; count -= c;
			}
		}
		else if (count >= 4)
		{
			tree_tokens.push({ (ushort)len, 0 });
			tree_freqs[(uint)len]++;
			i++; count--;
			while (count >= 3)
			{
				uint c = (uint)math::min(count, (long)6);
				tree_tokens.push({ 16, (ushort)(c - 3u) });
				tree_freqs[16]++;
				i += c; count -= c;
			}
		}
		while (count--)
		{
			tree_tokens.push({ (ushort)len, 0 });
			tree_freqs[(uint)len]++;
			i++;
		}
	}
}