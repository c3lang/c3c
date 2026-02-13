<*
 DEFLATE compression implementation (RFC 1951).

 API:
 - fn char[]? decompress(char[] input, Allocator allocator = mem)
 - fn void?   decompress_stream(InStream input, OutStream output, Allocator allocator = mem)
 - fn char[]? compress(char[] input, Allocator allocator = mem)
 - struct Inflater: InStream for pull-based decompression (use `init` then `read`).
*>
module std::compression::deflate;
import std::core::mem, std::io, std::math, std::bits, std::sort, std::collections::list;

faultdef
	CORRUPTED_DATA;

<*
 Stateful DEFLATE inflater.
 Implements InStream for pull-based decompression.
*>
struct Inflater (InStream)
{
	StreamBitReader reader;
	InflaterState state;
	uint hlit, hdist, hclen;
	uint dyn_i;
	uint dyn_num_lengths;
	uint stored_len;
	uint match_len;
	uint match_dist;
	bool final;
	bool done;

	char[65536] window;
	ulong pos;
	ulong read_pos;

	Huffman* lit_ptr;
	Huffman* dist_ptr;

	char[19] code_lengths;
	char[320] lit_dist_lengths;

	Huffman dyn_lit;
	Huffman dyn_dist;
	Huffman dyn_code_huff;
}

fn void Inflater.init(&self, InStream input, char[] bit_reader_buf = {})
{
	mem::set(self, 0, Inflater.sizeof);
	self.reader = { .stream = input, .buffer = bit_reader_buf };
	self.state = InflaterState.START_BLOCK;
	ensure_fixed_built();
}

fn usz? Inflater.read(&self, char[] buffer) @dynamic
{
	if (self.done && self.pos == self.read_pos) return 0;

	usz total_out = 0;
	while (total_out < buffer.len)
	{
		if (self.read_pos < self.pos)
		{
			usz to_copy = (usz)math::min((ulong)(buffer.len - total_out), self.pos - self.read_pos);

			usz start_idx = (usz)(self.read_pos & 0xFFFF);
			usz len = (usz)math::min((ulong)to_copy, (ulong)(65536 - start_idx));

			mem::copy(buffer.ptr + total_out, &self.window[start_idx], len);
			total_out += len;
			self.read_pos += len;

			if (len < to_copy)
			{
				mem::copy(buffer.ptr + total_out, &self.window[0], to_copy - len);
				total_out += to_copy - len;
				self.read_pos += (ulong)(to_copy - len);
			}
			continue;
		}

		if (self.done) break;

		if (self.pos - self.read_pos >= 32768) break;

		self.step()!;
	}

	return total_out;
}

fn char? Inflater.read_byte(&self) @dynamic
{
	char[1] b;
	if (try n = self.read(b[..]))
	{
		if (n == 0) return io::EOF~;
		return b[0];
	}
	return io::EOF~;
}

fn char[]? decompress(char[] input, Allocator allocator = mem)
{
	if (input.len == 0) return allocator::new_array(allocator, char, 0);

	io::ByteReader mem_stream;
	mem_stream.init(input);

	Inflater* inflater = allocator::new(allocator, Inflater);
	defer allocator::free(allocator, inflater);
	char[4096] tmp_bit_buf;
	inflater.init(&mem_stream, tmp_bit_buf[..]);

	usz out_cap = input.len * 2;
	if (out_cap < 1024) out_cap = 1024;

	usz out_len = 0;
	char* out = allocator::malloc(allocator, out_cap);
	defer allocator::free(allocator, out);

	while (true)
	{
		if (out_len + 8192 > out_cap)
		{
			usz next_cap = out_cap * 2;
			if (next_cap < out_len + 8192) next_cap = out_len + 8192;
			out = allocator::realloc_array(allocator, out, char, next_cap);
			out_cap = next_cap;
		}

		char[] target = out[out_len : 8192];
		usz n = inflater.read(target)!;
		if (n == 0) break;
		out_len += n;
	}

	if (out_len == 0) return allocator::new_array(allocator, char, 0);

	char[] result = allocator::malloc(allocator, out_len)[:out_len];
	mem::copy(result.ptr, out, out_len);
	return result;
}

<*
 Decompress stream using DEFLATE.
 Reads from `input`, writes to `output`.
 Uses blocking I/O.
*>
fn void? decompress_stream(InStream input, OutStream output, Allocator allocator = mem)
{
	Inflater* inflater = allocator::new(allocator, Inflater);
	char* bit_buf = allocator::malloc(allocator, 8192);
	inflater.init(input, bit_buf[:8192]);
	defer {
		allocator::free(allocator, bit_buf);
		allocator::free(allocator, inflater);
	}

	char[16384] out_buf;
	while (true)
	{
		usz n = inflater.read(out_buf[..])!;
		if (n == 0) break;
		output.write(out_buf[:n])!;
	}
}

<*
 Compress data using the DEFLATE algorithm.
 @param input : `The data to compress.`
 @param allocator : `The allocator to use.`
 @return `The compressed data.`
*>
fn char[]? compress(char[] input, Allocator allocator = mem) => @pool()
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

	const uint MIN_MATCH = 3;
	const uint MAX_MATCH = 258;
	const uint WINDOW_SIZE = 32768;
	const uint HASH_SIZE = 32768;
	const uint HASH_MASK = HASH_SIZE - 1;
	const uint MAX_CHAIN = 16; //Fast = 16, Good = 32, Best = 128

	uint[] head = allocator::alloc_array(tmem, uint, HASH_SIZE);
	mem::set(head.ptr, 0xFF, HASH_SIZE * uint.sizeof);

	uint[] prev = allocator::alloc_array(tmem, uint, WINDOW_SIZE);

	uint[] lit_freqs = mem::temp_array(uint, 286);
	uint[] dist_freqs = mem::temp_array(uint, 30);
	List{Token} tokens;
	tokens.tinit();

	uint hash = 0;
	if (input.len >= 2)
	{
		hash = ((uint)input[0] << 5) ^ (uint)input[1];
	}

	usz pos = 0;
	while (pos < input.len)
	{
		uint best_len = 0;
		uint best_dist = 0;

		if (pos + 2 < input.len)
		{
			hash = ((hash << 5) ^ (uint)input[pos + 2]) & HASH_MASK;
			uint match_head = head[hash];
			head[hash] = (uint)pos;
			prev[pos & 0x7FFF] = match_head;

			uint chain_len = 0;
			uint curr = match_head;
			while (curr != 0xFFFFFFFF && pos - curr < WINDOW_SIZE && chain_len < MAX_CHAIN)
			{
				if (pos + best_len < input.len && input[curr + best_len] == input[pos + best_len])
				{
					uint match_len = 0;
					while (match_len < MAX_MATCH &&
						   pos + match_len < input.len &&
						   input[curr + match_len] == input[pos + match_len])
					{
						match_len++;
					}

					if (match_len >= MIN_MATCH && match_len > best_len)
					{
						best_len = match_len;
						best_dist = (uint)(pos - (usz)curr);
						if (best_len == MAX_MATCH) break;
					}
				}
				curr = prev[curr & 0x7FFF];
				chain_len++;
			}
		}

		if (best_len >= MIN_MATCH)
		{
			uint len_code;
			switch (best_len)
			{
				case 3..10:   len_code = 257 + best_len - 3;
				case 11..18:  len_code = 265 + (best_len - 11) / 2;
				case 19..34:  len_code = 269 + (best_len - 19) / 4;
				case 35..66:  len_code = 273 + (best_len - 35) / 8;
				case 67..130: len_code = 277 + (best_len - 67) / 16;
				case 131..257:len_code = 281 + (best_len - 131) / 32;
				default:      len_code = 285;
			}
			lit_freqs[len_code]++;

			uint dist_code;
			switch (best_dist)
			{
				case 1..4:     dist_code = best_dist - 1;
				case 5..8:     dist_code = 4 + (best_dist - 5) / 2;
				case 9..16:    dist_code = 6 + (best_dist - 9) / 4;
				case 17..32:   dist_code = 8 + (best_dist - 17) / 8;
				case 33..64:   dist_code = 10 + (best_dist - 33) / 16;
				case 65..128:  dist_code = 12 + (best_dist - 65) / 32;
				case 129..256: dist_code = 14 + (best_dist - 129) / 64;
				case 257..512: dist_code = 16 + (best_dist - 257) / 128;
				case 513..1024: dist_code = 18 + (best_dist - 513) / 256;
				case 1025..2048: dist_code = 20 + (best_dist - 1025) / 512;
				case 2049..4096: dist_code = 22 + (best_dist - 2049) / 1024;
				case 4097..8192: dist_code = 24 + (best_dist - 4097) / 2048;
				case 8193..16384: dist_code = 26 + (best_dist - 8193) / 4096;
				default:          dist_code = 28 + (best_dist - 16385) / 8192;
			}
			dist_freqs[dist_code]++;
			tokens.push({ (ushort)best_len, (ushort)best_dist });

			for (uint k = 1; k < best_len; k++)
			{
				pos++;
				if (pos + 2 < input.len)
				{
					hash = ((hash << 5) ^ (uint)input[pos + 2]) & HASH_MASK;
					prev[pos & 0x7FFF] = head[hash];
					head[hash] = (uint)pos;
				}
			}
			pos++;
		}
		else
		{
			lit_freqs[(uint)input[pos]]++;
			tokens.push({ (ushort)input[pos], 0 });
			pos++;
		}
	}
	lit_freqs[256] = 1;
	tokens.push({ 256, 0 });

	Huffman lit_huff, dist_huff;
	lit_huff.build_from_freqs(lit_freqs, 15);

	uint total_dist = 0;
	foreach (f : dist_freqs) total_dist += f;
	if (total_dist == 0) dist_freqs[0] = 1;
	dist_huff.build_from_freqs(dist_freqs, 15);

	BitWriter writer;
	writer.init(allocator, math::max((usz)input.len / 2, (usz)1024));
	writer.write_bits(1, 1);
	writer.write_bits(2, 2);

	char[] lit_lens = lit_huff.get_lengths(285);
	char[] dist_lens = dist_huff.get_lengths(29);

	usz hlit_count = 286;
	while (hlit_count > 257 && lit_lens[hlit_count - 1] == 0) hlit_count--;
	usz hdist_count = 30;
	while (hdist_count > 1 && dist_lens[hdist_count - 1] == 0) hdist_count--;

	writer.write_bits((uint)hlit_count - 257, 5);
	writer.write_bits((uint)hdist_count - 1, 5);

	List{Token} tree_tokens; tree_tokens.tinit();
	uint[] tree_freqs = mem::temp_array(uint, 19);

	char[] all_lens = mem::temp_array(char, hlit_count + hdist_count);
	mem::copy(all_lens.ptr, lit_lens.ptr, hlit_count);
	mem::copy(all_lens.ptr + hlit_count, dist_lens.ptr, hdist_count);

	for (usz i = 0; i < all_lens.len;)
	{
		char len = all_lens[i];
		usz count = 1;
		while (i + count < all_lens.len && all_lens[i + count] == len) count++;

		if (len == 0)
		{
			while (count >= 11)
			{
				uint c = (uint)math::min(count, (usz)138);
				tree_tokens.push({ 18, (ushort)(c - 11) });
				tree_freqs[18]++;
				i += c; count -= c;
			}
			while (count >= 3)
			{
				uint c = (uint)math::min(count, (usz)10);
				tree_tokens.push({ 17, (ushort)(c - 3) });
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
				uint c = (uint)math::min(count, (usz)6);
				tree_tokens.push({ 16, (ushort)(c - 3) });
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

	Huffman tree_huff;
	tree_huff.build_from_freqs(tree_freqs, 7);
	char[] tree_lens = tree_huff.get_lengths(18);

	usz hclen_count = 19;
	while (hclen_count > 4 && tree_lens[ORDER[hclen_count - 1]] == 0) hclen_count--;
	writer.write_bits((uint)hclen_count - 4, 4);

	for (usz i = 0; i < hclen_count; i++)
	{
		writer.write_bits(tree_lens[ORDER[i]], 3);
	}

	Code[19] tree_codes;
	gen_canonical_codes(&tree_codes, tree_lens);
	foreach (t : tree_tokens)
	{
		writer.write_huffman(tree_codes[t.val].code, tree_codes[t.val].len);
		if (t.val == 16) { writer.write_bits(t.dist, 2); }
		else if (t.val == 17) { writer.write_bits(t.dist, 3); }
		else if (t.val == 18) { writer.write_bits(t.dist, 7); }
	}

	Code[286] lit_codes;
	gen_canonical_codes(&lit_codes, lit_lens[0:hlit_count]);
	Code[30] dist_codes;
	gen_canonical_codes(&dist_codes, dist_lens[0:hdist_count]);

	foreach (t : tokens)
	{
		if (t.dist == 0)
		{
			writer.write_huffman(lit_codes[t.val].code, lit_codes[t.val].len);
		}
		else
		{
			uint best_len = t.val;
			uint best_dist = t.dist;

			uint len_code;
			uint len_extra_bits = 0;
			uint len_extra = 0;
			switch (best_len)
			{
				case 3..10:   len_code = 257 + best_len - 3;
				case 11..18:  len_code = 265 + (best_len - 11) / 2; len_extra_bits = 1; len_extra = (best_len - 11) % 2;
				case 19..34:  len_code = 269 + (best_len - 19) / 4; len_extra_bits = 2; len_extra = (best_len - 19) % 4;
				case 35..66:  len_code = 273 + (best_len - 35) / 8; len_extra_bits = 3; len_extra = (best_len - 35) % 8;
				case 67..130: len_code = 277 + (best_len - 67) / 16; len_extra_bits = 4; len_extra = (best_len - 67) % 16;
				case 131..257:len_code = 281 + (best_len - 131) / 32; len_extra_bits = 5; len_extra = (best_len - 131) % 32;
				default:      len_code = 285;
			}
			writer.write_huffman(lit_codes[len_code].code, lit_codes[len_code].len);
			if (len_extra_bits > 0) { writer.write_bits(len_extra, len_extra_bits); }

			uint dist_code;
			uint dist_extra_bits = 0;
			uint dist_extra = 0;
			switch (best_dist)
			{
				case 1..4:     dist_code = best_dist - 1;
				case 5..8:     dist_code = 4 + (best_dist - 5) / 2; dist_extra_bits = 1; dist_extra = (best_dist - 5) % 2;
				case 9..16:    dist_code = 6 + (best_dist - 9) / 4; dist_extra_bits = 2; dist_extra = (best_dist - 9) % 4;
				case 17..32:   dist_code = 8 + (best_dist - 17) / 8; dist_extra_bits = 3; dist_extra = (best_dist - 17) % 8;
				case 33..64:   dist_code = 10 + (best_dist - 33) / 16; dist_extra_bits = 4; dist_extra = (best_dist - 33) % 16;
				case 65..128:  dist_code = 12 + (best_dist - 65) / 32; dist_extra_bits = 5; dist_extra = (best_dist - 65) % 32;
				case 129..256: dist_code = 14 + (best_dist - 129) / 64; dist_extra_bits = 6; dist_extra = (best_dist - 129) % 64;
				case 257..512: dist_code = 16 + (best_dist - 257) / 128; dist_extra_bits = 7; dist_extra = (best_dist - 257) % 128;
				case 513..1024: dist_code = 18 + (best_dist - 513) / 256; dist_extra_bits = 8; dist_extra = (best_dist - 513) % 256;
				case 1025..2048: dist_code = 20 + (best_dist - 1025) / 512; dist_extra_bits = 9; dist_extra = (best_dist - 1025) % 512;
				case 2049..4096: dist_code = 22 + (best_dist - 2049) / 1024; dist_extra_bits = 10; dist_extra = (best_dist - 2049) % 1024;
				case 4097..8192: dist_code = 24 + (best_dist - 4097) / 2048; dist_extra_bits = 11; dist_extra = (best_dist - 4097) % 2048;
				case 8193..16384: dist_code = 26 + (best_dist - 8193) / 4096; dist_extra_bits = 12; dist_extra = (best_dist - 8193) % 4096;
				default:          dist_code = 28 + (best_dist - 16385) / 8192; dist_extra_bits = 13; dist_extra = (best_dist - 16385) % 8192;
			}
			writer.write_huffman(dist_codes[dist_code].code, dist_codes[dist_code].len);
			if (dist_extra_bits > 0) { writer.write_bits(dist_extra, dist_extra_bits); }
		}
	}

	return writer.finish();
}

/*-----------------------------------------------------------------------------*/

Huffman lit_fixed_global @private;
Huffman dist_fixed_global @private;
bool fixed_built_global @private;

fn void ensure_fixed_built() @private
{
	if (fixed_built_global) return;
	build_fixed_huffman(&lit_fixed_global, &dist_fixed_global);
	fixed_built_global = true;
}

struct StreamBitReader @private
{
	InStream stream;
	char[] buffer;
	usz buf_pos;
	usz buf_len;
	ulong bit_buf;
	uint nbits;
}

enum InflaterState @private
{
	START_BLOCK,
	READ_STORED_LEN,
	COPY_STORED,
	READ_DYN_COUNTS,
	READ_DYN_CODELENS,
	READ_DYN_TREES,
	DECODE_SYMBOL,
	READ_DIST_SYM,
	COPY_MATCH,
	DONE
}

fn bool StreamBitReader.refill(&self) @private @inline
{
	if (self.buf_pos >= self.buf_len)
	{
		if (try n = self.stream.read(self.buffer))
		{
			if (n == 0) return false;
			self.buf_len = n;
			self.buf_pos = 0;
		}
		else
		{
			return false;
		}
	}
	return true;
}

fn uint? StreamBitReader.read_bits(&self, uint count) @private @inline
{
	if (count == 0) return 0;
	if (self.nbits < count)
	{
		while (self.nbits <= 56)
		{
			if (self.buf_pos >= self.buf_len)
			{
				if (!self.refill()) break;
			}
			self.bit_buf |= (ulong)self.buffer[self.buf_pos++] << self.nbits;
			self.nbits += 8;
		}
	}
	if (self.nbits < count) return CORRUPTED_DATA~;
	uint value = (uint)(self.bit_buf & ((1UL << count) - 1));
	self.bit_buf >>= count;
	self.nbits -= count;
	return value;
}

fn void StreamBitReader.align(&self) @private @inline
{
	uint skip = self.nbits % 8;
	self.bit_buf >>= skip;
	self.nbits -= skip;
}

struct BitWriter @private
{
	char[] data;
	usz len;
	ulong buffer;
	uint nbits;
	Allocator allocator;
}

fn void BitWriter.init(&self, Allocator allocator, usz initial_cap) @private
{
	self.allocator = allocator;
	self.data = allocator::malloc(allocator, initial_cap)[:initial_cap];
	self.len = 0;
	self.buffer = 0;
	self.nbits = 0;
}

fn void BitWriter.write_bits(&self, uint value, uint count) @private
{
	value &= (1 << count) - 1;
	self.buffer |= (ulong)value << self.nbits;
	self.nbits += count;
	while (self.nbits >= 8)
	{
		if (self.len >= self.data.len)
		{
			usz new_cap = self.data.len * 2;
			self.data = allocator::realloc_array(self.allocator, self.data.ptr, char, new_cap);
		}
		self.data[self.len++] = (char)(self.buffer & 0xFF);
		self.buffer >>= 8;
		self.nbits -= 8;
	}
}

fn void BitWriter.write_huffman(&self, uint code, uint len) @private
{
	uint rev = bits::reverse(code << (32 - len));
	self.write_bits(rev, len);
}

fn char[] BitWriter.finish(&self) @private
{
	if (self.nbits > 0)
	{
		if (self.len >= self.data.len)
		{
			usz new_cap = self.len + 1;
			self.data = allocator::realloc_array(self.allocator, self.data.ptr, char, new_cap);
		}
		self.data[self.len++] = (char)(self.buffer & 0xFF);
		self.buffer = 0;
		self.nbits = 0;
	}
	return self.data[:self.len];
}

struct Huffman @private
{
	ushort[16] counts;
	ushort[288] symbols;
}

fn void Huffman.build(&self, char[] lengths) @private
{
	ushort[16] offsets;
	mem::set(&self.counts, 0, ushort.sizeof * 16);
	foreach (len : lengths)
	{
		if (len > 0 && len < 16) self.counts[len]++;
	}

	ushort offset = 0;
	for (uint i = 1; i < 16; i++)
	{
		offsets[i] = offset;
		offset += self.counts[i];
	}

	foreach (uint i, len : lengths)
	{
		if (len > 0 && len < 16)
		{
			ushort sym_idx = offsets[len]++;
			if (sym_idx < 288)
			{
				self.symbols[sym_idx] = (ushort)i;
			}
		}
	}
}

struct Token @private
{
	ushort val;
	ushort dist;
}

struct IndexMap @private
{
	usz index;
	uint freq;
}
fn bool IndexMap.less(&self, IndexMap other) => self.freq < other.freq;

fn char[] pkg_merge(Allocator allocator, uint[] freqs, uint max_bits) @private => @pool()
{
	List{IndexMap} map;
	map.tinit();

	usz n = 0;
	foreach (i, freq : freqs)
	{
		if (freq == 0) continue;
		map.push({i, freq});
		n++;
	}

	if (n == 0) return {};

	sort::quicksort(map.array_view());

	if (n == 1)
	{
		char[] blen = allocator::alloc_array(allocator, char, freqs.len);
		mem::set(blen.ptr, 0, freqs.len);
		blen[map[0].index] = 1;
		return blen;
	}

	uint[] hist = mem::temp_array(uint, n);
	foreach (i, m : map) hist[i] = m.freq;

	usz max_elements = 2 * n;
	uint[] current = mem::temp_array(uint, max_elements);
	uint[] previous = mem::temp_array(uint, max_elements);
	ulong[] is_merged = mem::temp_array(ulong, max_elements);

	previous[:n] = hist[:n];
	usz num_previous = n;
	is_merged[:max_elements] = 0;
	usz num_relevant = 2 * n - 2;

	ulong mask = 1;
	for (uint bits = max_bits - 1; bits > 0; bits--)
	{
		num_previous &= (usz)~1;
		current[0] = hist[0];
		current[1] = hist[1];
		uint sum = current[0] + current[1];

		usz num_current = 2;
		usz num_hist = 2;
		usz num_merged = 0;

		while (true)
		{
			if (num_hist < n && hist[num_hist] <= sum)
			{
				current[num_current++] = hist[num_hist++];
				continue;
			}

			is_merged[num_current] |= mask;
			current[num_current] = sum;
			num_current++;

			num_merged++;
			if (num_merged * 2 >= num_previous) break;

			sum = previous[num_merged * 2] + previous[num_merged * 2 + 1];
		}

		while (num_hist < n) current[num_current++] = hist[num_hist++];
		mask <<= 1;

		if (num_previous >= num_relevant)
		{
			bool keep_going = false;
			for (isz i = (isz)num_relevant - 1; i > 0; i--)
			{
				if (previous[i] != current[i])
				{
					keep_going = true;
					break;
				}
			}
			if (!keep_going) break;
		}

		@swap(previous, current);
		num_previous = num_current;
	}

	mask >>= 1;
	hist[..] = 0;

	usz num_analyze = num_relevant;
	while (mask != 0)
	{
		usz num_merged_loop = 0;
		hist[0]++;
		hist[1]++;

		usz symbol = 2;
		for (usz i = symbol; i < num_analyze; i++)
		{
			if ((is_merged[i] & mask) == 0)
			{
				hist[symbol]++;
				symbol++;
			}
			else
			{
				num_merged_loop++;
			}
		}
		num_analyze = 2 * num_merged_loop;
		mask >>= 1;
	}

	for (usz i = 0; i < num_analyze; i++) hist[i]++;

	char[] blen = allocator::alloc_array(allocator, char, freqs.len);
	mem::set(blen.ptr, 0, freqs.len);
	foreach (i, m : map)
	{
		blen[m.index] = (char)hist[i];
	}

	return blen;
}

fn char[] Huffman.get_lengths(&self, usz max_sym) @private
{
	char[] blen = allocator::alloc_array(tmem, char, max_sym + 1);
	mem::set(blen.ptr, 0, max_sym + 1);

	ushort index = 0;
	for (uint len = 1; len < 16; len++)
	{
		uint count = self.counts[len];
		for (uint i = 0; i < count; i++)
		{
			blen[self.symbols[index++]] = (char)len;
		}
	}
	return blen;
}

fn void Huffman.build_from_freqs(&self, uint[] freqs, uint max_bits) @private => @pool()
{
	char[] blen = pkg_merge(tmem, freqs, max_bits);
	self.build(blen);
}

fn void gen_canonical_codes(Code* codes, char[] blen) @private
{
	ushort[16] bl_count;
	mem::set(&bl_count, 0, ushort.sizeof * 16);
	foreach (len : blen)
	{
		if (len > 0) bl_count[len]++;
	}

	ushort[16] next_code;
	ushort code = 0;
	bl_count[0] = 0;
	for (uint bits = 1; bits <= 15; bits++)
	{
		code = (code + bl_count[bits - 1]) << 1;
		next_code[bits] = code;
	}

	foreach (uint n, len : blen)
	{
		if (len != 0)
		{
			uint c = next_code[len];
			codes[n].code = (ushort)c;
			codes[n].len = (ushort)len;
			next_code[len]++;
		}
		else
		{
			codes[n].code = 0;
			codes[n].len = 0;
		}
	}
}

fn void build_fixed_huffman(Huffman* lit, Huffman* dist) @private
{
	char[288] lit_lens;
	for (uint i = 0; i <= 143; i++) lit_lens[i] = 8;
	for (uint i = 144; i <= 255; i++) lit_lens[i] = 9;
	for (uint i = 256; i <= 279; i++) lit_lens[i] = 7;
	for (uint i = 280; i <= 287; i++) lit_lens[i] = 8;
	lit.build(&lit_lens);

	char[32] dist_lens;
	mem::set(&dist_lens, 5, 32);
	dist.build(&dist_lens);
}

struct Code @private
{
	ushort code;
	ushort len;
}

fn void gen_fixed_codes(Code* codes) @private
{
	char[288] lens;
	for (uint i = 0; i <= 143; i++) lens[i] = 8;
	for (uint i = 144; i <= 255; i++) lens[i] = 9;
	for (uint i = 256; i <= 279; i++) lens[i] = 7;
	for (uint i = 280; i <= 287; i++) lens[i] = 8;

	ushort[16] bl_count;
	mem::set(&bl_count, 0, ushort.sizeof * 16);
	for (uint i = 0; i < 288; i++)
	{
		if (lens[i] > 0) bl_count[lens[i]]++;
	}

	ushort[16] next_code;
	ushort code = 0;
	bl_count[0] = 0;
	for (uint bits = 1; bits <= 15; bits++)
	{
		code = (code + bl_count[bits - 1]) << 1;
		next_code[bits] = code;
	}

	for (uint n = 0; n < 288; n++)
	{
		uint len = lens[n];
		if (len != 0)
		{
			uint c = next_code[len];
			codes[n].code = (ushort)c;
			codes[n].len = (ushort)len;
			next_code[len]++;
		}
		else
		{
			codes[n].code = 0;
			codes[n].len = 0;
		}
	}
}

fn ushort? Huffman.decode_stream(&self, StreamBitReader* reader) @private @inline
{
	uint code = 0;
	uint first = 0;
	uint index = 0;
	for (uint len = 1; len < 16; len++)
	{
		code |= reader.read_bits(1)!;
		uint count = self.counts[len];
		if (code < first + count)
		{
			return self.symbols[index + (code - first)];
		}
		index += count;
		first += count;
		first <<= 1;
		code <<= 1;
	}
	return CORRUPTED_DATA~;
}

fn void? Inflater.step(&self) @private
{
	switch (self.state)
	{
		case START_BLOCK:
			self.final = self.reader.read_bits(1)! != 0;
			uint type = self.reader.read_bits(2)!;
			if (type == 0) {
				self.state = InflaterState.READ_STORED_LEN;
			} else if (type == 1) {
				self.lit_ptr = &lit_fixed_global;
				self.dist_ptr = &dist_fixed_global;
				self.state = InflaterState.DECODE_SYMBOL;
			} else if (type == 2) {
				self.state = InflaterState.READ_DYN_COUNTS;
			} else {
				return CORRUPTED_DATA~;
			}

		case READ_STORED_LEN:
			self.reader.align();
			self.stored_len = self.reader.read_bits(16)!;
			uint nlen = self.reader.read_bits(16)!;
			if (self.stored_len != (~nlen & 0xFFFF)) return CORRUPTED_DATA~;
			if (self.stored_len == 0) {
				self.state = self.final ? InflaterState.DONE : InflaterState.START_BLOCK;
			} else {
				self.state = InflaterState.COPY_STORED;
			}

		case COPY_STORED:
			char c = (char)self.reader.read_bits(8)!;
			self.write_byte(c);
			self.stored_len--;
			if (self.stored_len == 0) {
				self.state = self.final ? InflaterState.DONE : InflaterState.START_BLOCK;
			}

		case READ_DYN_COUNTS:
			self.hlit = self.reader.read_bits(5)! + 257;
			self.hdist = self.reader.read_bits(5)! + 1;
			self.hclen = self.reader.read_bits(4)! + 4;
			self.dyn_i = 0;
			mem::set(&self.code_lengths, 0, 19);
			self.state = InflaterState.READ_DYN_CODELENS;

		case READ_DYN_CODELENS:
			self.code_lengths[ORDER[self.dyn_i]] = (char)self.reader.read_bits(3)!;
			self.dyn_i++;
			if (self.dyn_i >= self.hclen) {
				self.dyn_code_huff.build(&self.code_lengths);
				self.dyn_i = 0;
				self.dyn_num_lengths = self.hlit + self.hdist;
				if (self.dyn_num_lengths > 320) return CORRUPTED_DATA~;
				mem::set(&self.lit_dist_lengths, 0, self.lit_dist_lengths.len);
				self.state = InflaterState.READ_DYN_TREES;
			}

		case READ_DYN_TREES:
			ushort sym = self.dyn_code_huff.decode_stream(&self.reader)!;
			if (sym < 16) {
				if (self.dyn_i >= 320) return CORRUPTED_DATA~;
				self.lit_dist_lengths[self.dyn_i++] = (char)sym;
			} else if (sym == 16) {
				if (self.dyn_i == 0) return CORRUPTED_DATA~;
				uint repeat_count = self.reader.read_bits(2)! + 3;
				char prev = self.lit_dist_lengths[self.dyn_i - 1];
				if (self.dyn_i + repeat_count > self.dyn_num_lengths || self.dyn_i + repeat_count > 320) return CORRUPTED_DATA~;
				for (uint k = 0; k < repeat_count; k++) self.lit_dist_lengths[self.dyn_i++] = prev;
			} else if (sym == 17) {
				uint zero_len = self.reader.read_bits(3)! + 3;
				if (self.dyn_i + zero_len > self.dyn_num_lengths || self.dyn_i + zero_len > 320) return CORRUPTED_DATA~;
				for (uint k = 0; k < zero_len; k++) self.lit_dist_lengths[self.dyn_i++] = 0;
			} else if (sym == 18) {
				uint zero_len = self.reader.read_bits(7)! + 11;
				if (self.dyn_i + zero_len > self.dyn_num_lengths || self.dyn_i + zero_len > 320) return CORRUPTED_DATA~;
				for (uint k = 0; k < zero_len; k++) self.lit_dist_lengths[self.dyn_i++] = 0;
			}
			if (self.dyn_i >= self.dyn_num_lengths) {
				uint hlit = self.hlit;
				uint hdist = self.hdist;
				uint total = hlit + hdist;
				if (hlit > 286 || hdist > 32 || total > 320 || hlit > total) {
					return CORRUPTED_DATA~;
				}
				self.dyn_lit.build(self.lit_dist_lengths[0:hlit]);
				self.dyn_dist.build(self.lit_dist_lengths[hlit:hdist]);
				self.lit_ptr = &self.dyn_lit;
				self.dist_ptr = &self.dyn_dist;
				self.state = InflaterState.DECODE_SYMBOL;
			}

		case DECODE_SYMBOL:
			ushort symbol = self.lit_ptr.decode_stream(&self.reader)!;
			if (symbol < 256) {
				self.write_byte((char)symbol);
			} else if (symbol == 256) {
				self.state = self.final ? InflaterState.DONE : InflaterState.START_BLOCK;
			} else if (symbol <= 285) {
				uint len_idx = symbol - 257;
				self.match_len = LENGTH_BASE[len_idx] + self.reader.read_bits(LENGTH_EXTRA[len_idx])!;
				self.state = InflaterState.READ_DIST_SYM;
			} else {
				return CORRUPTED_DATA~;
			}

		case READ_DIST_SYM:
			ushort dist_sym = self.dist_ptr.decode_stream(&self.reader)!;
			self.match_dist = DIST_BASE[dist_sym] + self.reader.read_bits(DIST_EXTRA[dist_sym])!;
			self.state = InflaterState.COPY_MATCH;

		case COPY_MATCH:
			if (self.match_dist > self.pos) return CORRUPTED_DATA~;
			char c = self.window[(usz)((self.pos - self.match_dist) & 0xFFFF)];
			self.write_byte(c);
			self.match_len--;
			if (self.match_len == 0) {
				self.state = InflaterState.DECODE_SYMBOL;
			}

		case DONE:
			self.done = true;
	}
	return;
}

fn void Inflater.write_byte(&self, char c) @private @inline
{
	self.window[(usz)(self.pos & 0xFFFF)] = c;
	self.pos++;
}

const ushort[31] LENGTH_BASE @private = {
	3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
	35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0
};

const char[31] LENGTH_EXTRA @private = {
	0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
	3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, 0, 0
};

const ushort[32] DIST_BASE @private = {
	1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
	257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
	8193, 12289, 16385, 24577, 0, 0
};

const char[32] DIST_EXTRA @private = {
	0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
	7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 0, 0
};

const uint[19] ORDER @private = { 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 };
