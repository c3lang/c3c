<*
 DEFLATE compression implementation (RFC 1951).

 API:
 - fn char[]? decompress(Allocator allocator, char[] input)
 - fn void?   decompress_stream(InStream input, OutStream output)
 - fn char[]? compress(Allocator allocator, char[] input, )
 - struct Inflater: InStream for pull-based decompression (use `init` then `read`).
*>
module std::compression::deflate;
import std::io, std::math, std::bits, std::sort, std::collections::list;
import std::thread;

const uint MIN_MATCH = 3;
const uint MAX_MATCH = 258;
const uint WINDOW_SIZE = 32768;
const uint HASH_SIZE = 32768;
const uint HASH_MASK = HASH_SIZE - 1;
const uint MAX_CHAIN = 16;
const uint GOOD_MATCH = 32;
const uint NICE_MATCH = 128;

faultdef
	CORRUPTED_DATA,
	IO_ERROR,
	INVALID_STATE;

<*
 Stateful DEFLATE inflater.
 Implements InStream for pull-based decompression.
*>
struct Inflater (InStream)
{
	StreamBitReader reader;
	char[65536 + 8] window @align(8);
	usz pos;
	bool final;
	uint stored_len;
	Huffman* lit_ptr;
	Huffman* dist_ptr;
	Huffman dyn_lit;
	Huffman dyn_dist;
	Huffman dyn_code_huff;
	uint hlit, hdist, hclen;
	uint dyn_i;
	uint dyn_num_lengths;
	char[320] lit_dist_lengths;
	char[19] code_lengths;
	uint match_len;
	uint match_dist;
	bool done;
	InflaterState state;
}

fn void Inflater.init(&self, InStream input, char[] bit_reader_buf = {})
{
	*self = {};
	self.reader.init(input, bit_reader_buf);
	self.state = START_BLOCK;
	static OnceFlag run_once;
    run_once.call(fn void() {
    	build_fixed_huffman(&lit_fixed_global, &dist_fixed_global);
    });
}

fn usz? Inflater.read(&self, char[] buffer) @dynamic
{
	if (self.done) return 0;

	usz total_out = 0;
	while (total_out < buffer.len)
	{
		switch (self.state)
		{
			case COPY_MATCH:
				if (self.match_len > 0)
				{
					uint m_len = self.match_len;
					uint m_dist = self.match_dist;
					if (m_dist > self.pos) return CORRUPTED_DATA~;

					usz to_copy = math::min((usz)m_len, buffer.len - total_out);
					usz s_pos = self.pos;
					usz orig_to_copy = to_copy;

					if (m_dist >= 16 && to_copy >= 16)
					{
						while (to_copy >= 16)
						{
							usz src_idx = (s_pos - m_dist) & 0xFFFF;
							usz dst_idx = s_pos & 0xFFFF;
							if (src_idx > 0xFFF0 || dst_idx > 0xFFF0) break;

							char[<16>] val = *(char[<16>]*)(&self.window[src_idx]);
							*(char[<16>]*)(&self.window[dst_idx]) = val;
							*(char[<16>]*)(&buffer[total_out]) = val;

							s_pos += 16;
							total_out += 16;
							to_copy -= 16;
						}
					}

					if (m_dist >= 8 && to_copy >= 8)
					{
						while (to_copy >= 8)
						{
							usz src_idx = (s_pos - m_dist) & 0xFFFF;
							usz dst_idx = s_pos & 0xFFFF;
							if (src_idx > 0xFFF8 || dst_idx > 0xFFF8) break;

							ulong val;
							mem::copy(&val, &self.window[src_idx], 8);
							mem::copy(&self.window[dst_idx], &val, 8);
							mem::copy(buffer.ptr + total_out, &val, 8);

							s_pos += 8;
							total_out += 8;
							to_copy -= 8;
						}
					}

					for (usz i = 0; i < to_copy; i++)
					{
						char c = self.window[(s_pos - m_dist) & 0xFFFF];
						buffer[total_out++] = c;
						self.window[s_pos & 0xFFFF] = c;
						s_pos++;
					}

					self.pos = s_pos;
					self.match_len -= (uint)orig_to_copy;
					if (self.match_len == 0) { self.state = DECODE_SYMBOL; }
					else { return total_out; } // Buffer full
				}
				else { self.state = DECODE_SYMBOL; }

			case DECODE_SYMBOL:
				while (total_out < buffer.len)
				{
					ushort symbol = self.lit_ptr.decode_stream(&self.reader)!;
					if (symbol < 256)
					{
						char c = (char)symbol;
						buffer[total_out++] = c;
						self.window[self.pos & 0xFFFF] = c;
						self.pos++;
					}
					else if (symbol == 256)
					{
						self.state = self.final ? DONE : START_BLOCK;
						break;
					}
					else if (symbol <= 285)
					{
						uint len_idx = symbol - 257;
						self.match_len = LENGTH_BASE[len_idx] + self.reader.read_bits(LENGTH_EXTRA[len_idx])!;
						ushort dist_sym = self.dist_ptr.decode_stream(&self.reader)!;
						self.match_dist = DIST_BASE[dist_sym] + self.reader.read_bits(DIST_EXTRA[dist_sym])!;
						self.state = COPY_MATCH;
						break; // Handle match in next iteration
					}
					else { return CORRUPTED_DATA~; }
				}

			default:
				if (self.done) break;
				usz start = self.pos;
				self.step()!;
				usz added = self.pos - start;
				if (added > 0)
				{
					buffer[total_out++] = self.window[start & 0xFFFF];
				}
		}
		if (self.done) break;
	}

	return total_out;
}

fn char? Inflater.read_byte(&self) @dynamic
{
	char[1] b;
	if (try n = self.read(&b) && n > 0)
	{
		return b[0];
	}
	return io::EOF~;
}

fn void? Inflater.close(&self) @dynamic
{
	self.reader.close();
}

fn char[]? decompress(Allocator allocator = mem, char[] input) => @pool()
{
	if (!input.len) return allocator::new_array(allocator, char, 0);

	ByteReader mem_stream;
	mem_stream.init(input);

	Inflater* inflater = allocator::new(tmem, Inflater);
	char[4096] tmp_bit_buf;
	inflater.init(&mem_stream, &tmp_bit_buf);

	usz out_cap = input.len * 2;
	if (out_cap < 1024) out_cap = 1024;

	ByteWriter writer;
	writer.tinit();
	writer.ensure_capacity(out_cap)!;

	usz out_len = io::copy_to(inflater, &writer)!;
	if (out_len == 0) return allocator::new_array(allocator, char, 0);

	char[] result = allocator::alloc_array(allocator, char, out_len);
	result[..] = writer.array_view()[:out_len];
	return result;
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

fn void? compress_stream(Allocator allocator = mem, InStream input, OutStream output)
{
	Deflater deflater;
	deflater.init(allocator, output);
	defer (void)deflater.close();

	char[32768] buf;
	while (true)
	{
		usz? n = input.read(buf[..]);
		if (catch err = n)
		{
			if (err == io::EOF) break;
			return err~;
		}
		if (n == 0) break;
		deflater.write(buf[:n])!;
	}
}

struct Deflater (OutStream)
{
	OutStream output;
	Allocator allocator;

	uint* head;
	uint* prev;
	char* window;
	uint window_fill;

	Token* tokens;
	usz token_count;
	usz max_tokens;
	uint[286] lit_freqs;
	uint[30] dist_freqs;

	BitWriter bwriter;

	uint hash;
	uint strstart;
	uint match_pos;
	bool hash_ready;
	usz total_in;
	usz total_out;
	bool closed;
}

fn void Deflater.init(&self, Allocator allocator = mem, OutStream output = null)
{
	*self = {};
	self.allocator = allocator;
	self.output = output;
	self.head = allocator::alloc_array(allocator, uint, HASH_SIZE);
	self.head[..HASH_SIZE] = 0xFFFFFFFF;
	self.prev = allocator::alloc_array(allocator, uint, WINDOW_SIZE);
	self.window = allocator::alloc_array(allocator, char, WINDOW_SIZE * 2 + MAX_MATCH + 1);
	self.window_fill = WINDOW_SIZE;
	self.strstart = WINDOW_SIZE;
	self.match_pos = WINDOW_SIZE;
	self.max_tokens = WINDOW_SIZE * 4;
	self.tokens = allocator::alloc_array(allocator, Token, self.max_tokens + 1);
	self.bwriter.init(allocator, WINDOW_SIZE);
}

fn void Deflater.free(&self)
{
	allocator::free(self.allocator, self.head);
	allocator::free(self.allocator, self.prev);
	allocator::free(self.allocator, self.window);
	allocator::free(self.allocator, self.tokens);
	if (self.bwriter.data.ptr) allocator::free(self.allocator, self.bwriter.data.ptr);
}

fn usz? Deflater.write(&self, char[] data) @dynamic
{
	if (self.closed) return IO_ERROR~;

	usz remaining = data.len;
	usz offset = 0;
	while (remaining > 0)
	{
		usz space = (WINDOW_SIZE * 2) - (usz)self.window_fill;
		if (space == 0)
		{
			self.scan_matches(false)!;
			mem::copy(self.window, self.window + WINDOW_SIZE, WINDOW_SIZE);
			self.window_fill -= WINDOW_SIZE;
			self.match_pos -= WINDOW_SIZE;
			space = WINDOW_SIZE;
		}

		usz to_copy = math::min(remaining, space);
		mem::copy(self.window + self.window_fill, data.ptr + offset, to_copy);
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

fn void? Deflater.scan_matches(&self, bool final) @private
{
	char* input = self.window;
	uint wfill = self.window_fill;

	if (!self.hash_ready && wfill > WINDOW_SIZE + 1)
	{
		self.hash = ((uint)input[WINDOW_SIZE] << 5) ^ (uint)input[WINDOW_SIZE + 1];
		self.hash_ready = true;
	}

	// Copy hot fields to locals for register allocation
	uint* head = self.head;
	uint* prev = self.prev;
	uint* lfreqs = &self.lit_freqs[0];
	uint* dfreqs = &self.dist_freqs[0];
	Token* tokens = self.tokens;
	usz tcount = self.token_count;
	usz max_t = self.max_tokens;

	uint hash = self.hash;
	uint strstart = self.strstart;
	usz pos = self.match_pos;
	uint win_offset = strstart - (uint)pos;

	while (pos < wfill)
	{
		uint best_len = 0;
		uint best_dist = 0;

		if (pos + 2 < wfill)
		{
			hash = ((hash << 5) ^ (uint)input[pos + 2]) & HASH_MASK;
			uint match_head = head[hash];
			head[hash] = strstart;
			prev[strstart & 0x7FFF] = match_head;

			uint chain_len = 0;
			uint curr = match_head;

			while (curr != 0xFFFFFFFF && strstart - curr < WINDOW_SIZE && chain_len < MAX_CHAIN)
			{
				chain_len++;
				usz curr_rel = (usz)(curr - win_offset);

				if (pos + best_len < wfill && input[curr_rel + best_len] == input[pos + best_len])
				{
					if (best_len < 3 || mem::load((ushort*)&input[pos + best_len - 1], 1) == mem::load((ushort*)&input[curr_rel + best_len - 1], 1))
					{
						uint match_l = 0;
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
							best_dist = (uint)(pos - curr_rel);
							if (best_len >= NICE_MATCH) break;
						}
					}
				}
				curr = prev[curr & 0x7FFF];
				if (best_len >= GOOD_MATCH) chain_len++;
			}
		}
		else if (!final)
		{
			break;
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
			lfreqs[len_code]++;

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
			dfreqs[dist_code]++;
			tokens[tcount++] = { (ushort)best_len, (ushort)best_dist };

			uint limit = best_len - 1;
			for (uint k = 0; k < limit; k++)
			{
				pos++;
				strstart++;
				if (pos + 2 < wfill)
				{
					hash = ((hash << 5) ^ (uint)input[pos + 2]) & HASH_MASK;
					uint m_h = head[hash];
					head[hash] = strstart;
					prev[strstart & 0x7FFF] = m_h;
				}
			}
			pos++;
			strstart++;
		}
		else
		{
			lfreqs[(uint)input[pos]]++;
			tokens[tcount++] = { (ushort)input[pos], 0 };
			pos++;
			strstart++;
		}

		if (tcount >= max_t)
		{
			self.token_count = tcount;
			self.emit_block(false)!;
			tcount = 0;
		}
	}

	// Write back hot fields
	self.token_count = tcount;
	self.hash = hash;
	self.strstart = strstart;
	self.match_pos = (uint)pos;
}

fn void? Deflater.emit_block(&self, bool final) @private => @pool()
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

	self.bwriter.write_bits(final ? 1 : 0, 1);
	self.bwriter.write_bits(2, 2); // BTYPE=2 (Dynamic Huffman)

	char[] lit_lens = lit_huff.get_lengths(285);
	char[] dist_lens = dist_huff.get_lengths(29);

	usz hlit_cnt = 286;
	while (hlit_cnt > 257 && lit_lens[hlit_cnt - 1] == 0) hlit_cnt--;
	usz hdist_cnt = 30;
	while (hdist_cnt > 1 && dist_lens[hdist_cnt - 1] == 0) hdist_cnt--;

	self.bwriter.write_bits((uint)hlit_cnt - 257, 5);
	self.bwriter.write_bits((uint)hdist_cnt - 1, 5);

	// RLE-encode the combined code lengths
	List{Token} tree_tokens; tree_tokens.tinit();
	uint[] tree_freqs = mem::temp_array(uint, 19);
	char[] all_lens = mem::temp_array(char, hlit_cnt + hdist_cnt);
	all_lens[:hlit_cnt] = lit_lens[:hlit_cnt];
	all_lens[hlit_cnt:hdist_cnt] = dist_lens[:hdist_cnt];

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

	// Build the code length Huffman tree
	Huffman tree_huff;
	tree_huff.build_from_freqs(tree_freqs[..], 7);
	char[] tree_lens = tree_huff.get_lengths(18);
	usz hclen_cnt = 19;
	while (hclen_cnt > 4 && tree_lens[ORDER[hclen_cnt - 1]] == 0) hclen_cnt--;
	self.bwriter.write_bits((uint)hclen_cnt - 4, 4);
	for (usz i = 0; i < hclen_cnt; i++) self.bwriter.write_bits(tree_lens[ORDER[i]], 3);

	// Write the RLE-encoded code lengths
	Code[19] tree_codes;
	gen_canonical_codes(&tree_codes, tree_lens);
	foreach (t : tree_tokens)
	{
		self.bwriter.write_huffman(tree_codes[t.val].code, tree_codes[t.val].len);
		if (t.val == 16) { self.bwriter.write_bits(t.dist, 2); }
		else if (t.val == 17) { self.bwriter.write_bits(t.dist, 3); }
		else if (t.val == 18) { self.bwriter.write_bits(t.dist, 7); }
	}

	// Generate canonical codes for lit/dist trees
	Code[286] lit_codes;
	gen_canonical_codes(&lit_codes, lit_lens[:hlit_cnt]);
	Code[30] dist_codes;
	gen_canonical_codes(&dist_codes, dist_lens[:hdist_cnt]);

	// Write the actual compressed data tokens
	foreach (t : self.tokens[:self.token_count])
	{
		if (t.dist == 0)
		{
			self.bwriter.write_huffman(lit_codes[t.val].code, lit_codes[t.val].len);
		}
		else
		{
			uint b_len = t.val; uint b_dist = t.dist;
			uint len_code; uint len_extra_bits = 0; uint len_extra = 0;
			switch (b_len)
			{
				case 3..10:   len_code = 257 + b_len - 3;
				case 11..18:  len_code = 265 + (b_len - 11) / 2; len_extra_bits = 1; len_extra = (b_len - 11) % 2;
				case 19..34:  len_code = 269 + (b_len - 19) / 4; len_extra_bits = 2; len_extra = (b_len - 19) % 4;
				case 35..66:  len_code = 273 + (b_len - 35) / 8; len_extra_bits = 3; len_extra = (b_len - 35) % 8;
				case 67..130: len_code = 277 + (b_len - 67) / 16; len_extra_bits = 4; len_extra = (b_len - 67) % 16;
				case 131..257:len_code = 281 + (b_len - 131) / 32; len_extra_bits = 5; len_extra = (b_len - 131) % 32;
				default:      len_code = 285;
			}
			self.bwriter.write_huffman(lit_codes[len_code].code, lit_codes[len_code].len);
			if (len_extra_bits > 0) self.bwriter.write_bits(len_extra, len_extra_bits);

			uint dist_c; uint dist_extra_b = 0; uint dist_ex = 0;
			switch (b_dist)
			{
				case 1..4:     dist_c = b_dist - 1;
				case 5..8:     dist_c = 4 + (b_dist - 5) / 2; dist_extra_b = 1; dist_ex = (b_dist - 5) % 2;
				case 9..16:    dist_c = 6 + (b_dist - 9) / 4; dist_extra_b = 2; dist_ex = (b_dist - 9) % 4;
				case 17..32:   dist_c = 8 + (b_dist - 17) / 8; dist_extra_b = 3; dist_ex = (b_dist - 17) % 8;
				case 33..64:   dist_c = 10 + (b_dist - 33) / 16; dist_extra_b = 4; dist_ex = (b_dist - 33) % 16;
				case 65..128:  dist_c = 12 + (b_dist - 65) / 32; dist_extra_b = 5; dist_ex = (b_dist - 65) % 32;
				case 129..256: dist_c = 14 + (b_dist - 129) / 64; dist_extra_b = 6; dist_ex = (b_dist - 129) % 64;
				case 257..512: dist_c = 16 + (b_dist - 257) / 128; dist_extra_b = 7; dist_ex = (b_dist - 257) % 128;
				case 513..1024: dist_c = 18 + (b_dist - 513) / 256; dist_extra_b = 8; dist_ex = (b_dist - 513) % 256;
				case 1025..2048: dist_c = 20 + (b_dist - 1025) / 512; dist_extra_b = 9; dist_ex = (b_dist - 1025) % 512;
				case 2049..4096: dist_c = 22 + (b_dist - 2049) / 1024; dist_extra_b = 10; dist_ex = (b_dist - 2049) % 1024;
				case 4097..8192: dist_c = 24 + (b_dist - 4097) / 2048; dist_extra_b = 11; dist_ex = (b_dist - 4097) % 2048;
				case 8193..16384: dist_c = 26 + (b_dist - 8193) / 4096; dist_extra_b = 12; dist_ex = (b_dist - 8193) % 4096;
				default:          dist_c = 28 + (b_dist - 16385) / 8192; dist_extra_b = 13; dist_ex = (b_dist - 16385) % 8192;
			}
			self.bwriter.write_huffman(dist_codes[dist_c].code, dist_codes[dist_c].len);
			if (dist_extra_b > 0) self.bwriter.write_bits(dist_ex, dist_extra_b);
		}
	}

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
 @return `The compressed data.`
*>
fn char[]? compress(Allocator allocator = mem, char[] input) => @pool()
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
	deflater.init(allocator);
	defer deflater.free();

	deflater.write(input)!;
	deflater.scan_matches(true)!;
	deflater.emit_block(true)!;

	char[] finished = deflater.bwriter.finish();
	char[] result = allocator::alloc_array(allocator, char, finished.len);
	result[..] = finished[..];
	return result;
}

/*-----------------------------------------------------------------------------*/

Huffman lit_fixed_global @private;
Huffman dist_fixed_global @private;
Code[288] fixed_lit_codes_global @private;
Code[32] fixed_dist_codes_global @private;
bool fixed_built_global @private;


fn void ensure_fixed_codes_built() @private
{
	if (fixed_lit_codes_global[0].len != 0) return;
	gen_fixed_codes(&fixed_lit_codes_global);
	for (uint i = 0; i < 32; i++) { fixed_dist_codes_global[i] = { (ushort)i, 5 }; }
}

struct StreamBitReader @private
{
	InStream stream;
	char[] buffer;
	usz buf_pos;
	usz buf_len;
	ulong bit_buf;
	uint nbits;
	SetCursorFn set_cursor_fn;
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

fn void StreamBitReader.init(&self, InStream reader, char[] buffer)
{
	*self = { .stream = reader, .buffer = buffer, .set_cursor_fn = &reader.set_cursor };
}

fn void StreamBitReader.close(&self)
{
	if (!self.set_cursor_fn) return;
	long seek_back = (long)self.buf_len - (long)self.buf_pos + (long)(self.nbits / 8);
	if (seek_back > 0)
	{
		(void)self.set_cursor_fn(self.stream, -seek_back, FROM_CURSOR);
	}
}

fn void? StreamBitReader.refill(&self) @inline
{
	if (self.buf_pos >= self.buf_len)
	{
		usz n = self.stream.read(self.buffer)!;
		if (!n) return io::EOF~;
		self.buf_len = n;
		self.buf_pos = 0;
	}
}

fn uint? StreamBitReader.peek_bits(&self, uint count) @private @inline
{
	if (count == 0) return 0;
	if (self.nbits < count)
	{
		while (self.nbits <= 56)
		{
			if (self.buf_pos >= self.buf_len)
			{
				if (@catch(self.refill())) break;
			}
			self.bit_buf |= (ulong)(self.buffer[self.buf_pos++]) << self.nbits;
			self.nbits += 8;
		}
	}
	if (self.nbits == 0 && count > 0) return CORRUPTED_DATA~;
	return (uint)(self.bit_buf & ((1UL << count) - 1));
}

fn void StreamBitReader.consume_bits(&self, uint count) @private @inline
{
	self.bit_buf >>= count;
	self.nbits -= count;
}

fn uint? StreamBitReader.read_bits(&self, uint count) @private @inline
{
	uint val = self.peek_bits(count)!;
	self.consume_bits(count);
	return val;
}

fn void StreamBitReader.align(&self) @inline
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

fn void BitWriter.init(&self, Allocator allocator, usz initial_cap)
{
	self.allocator = allocator;
	self.data = allocator::alloc_array(allocator, char, initial_cap);
	self.len = 0;
	self.buffer = 0;
	self.nbits = 0;
}

fn void BitWriter.write_bits(&self, uint value, uint count)
{
	self.buffer |= (ulong)(value & ((1 << count) - 1)) << self.nbits;
	self.nbits += count;
	while (self.nbits >= 8)
	{
		if (self.len >= self.data.len)
		{
			usz new_cap = self.data.len;
			if (new_cap < 1024) new_cap = 1024;
			while (new_cap <= self.len)
			{
				if (new_cap > usz.max / 2) { new_cap = self.len + 1; break; }
				new_cap *= 2;
			}
			self.data = allocator::realloc_array(self.allocator, self.data.ptr, char, new_cap);
		}
		self.data[self.len++] = (char)(self.buffer & 0xFF);
		self.buffer >>= 8;
		self.nbits -= 8;
	}
}

fn void BitWriter.write_huffman(&self, uint code, uint len)
{
	uint rev = bits::reverse(code << (32 - len));
	self.write_bits(rev, len);
}

fn char[] BitWriter.finish(&self)
{
	if (self.nbits > 0)
	{
		if (self.len >= self.data.len)
		{
			usz n_cap = self.len + 1;
			self.data = allocator::realloc_array(self.allocator, self.data.ptr, char, n_cap);
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
	uint[2048] table; // 11-bit lookup table: (len << 16) | symbol
}

fn ushort reverse16(ushort x) @private @inline
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
	for (uint i = 1; i < 16; i++) { offsets[i] = offset; offset += self.counts[i]; }
	foreach (uint i, len : lengths) { if (len > 0 && len < 16) { ushort sym_idx = offsets[len]++; if (sym_idx < 288) self.symbols[sym_idx] = (ushort)i; } }

	// Build 11-bit lookup table
	self.table = {};
	uint code = 0;
	uint sym_idx = 0;
	for (uint len = 1; len <= 11; len++)
	{
		uint count = self.counts[len];
		for (uint i = 0; i < count; i++)
		{
			ushort symbol = self.symbols[sym_idx++];
			uint rev_code = reverse16((ushort)code) >> (16 - len);
			uint entry = (len << 16) | (uint)symbol;
			uint num_entries = 1 << (11 - len);
			for (uint j = 0; j < num_entries; j++)
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
	usz index;
	uint freq;
}
fn bool IndexMap.less(&self, IndexMap other) => self.freq < other.freq;

fn char[] pkg_merge(Allocator allocator, uint[] freqs, uint max_bits) @private => @pool()
{
	List {IndexMap} map; map.tinit();
	usz n = 0;
	foreach (i, freq : freqs) { if (freq == 0) continue; map.push({i, freq}); n++; }
	if (n == 0) return {};
	sort::quicksort(map.array_view());
	if (n == 1) { char[] blen = allocator::new_array(allocator, char, freqs.len); blen[map[0].index] = 1; return blen; }
	usz max_elements = 2 * n;
	uint[] hist = mem::temp_array(uint, max_elements);
	foreach (i, m : map) hist[i] = m.freq;
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
		current[0] = hist[0]; current[1] = hist[1];
		uint sum = current[0] + current[1];
		usz num_current = 2; usz num_hist = 2; usz num_merged = 0;
		while (true)
		{
			if (num_hist < n && hist[num_hist] <= sum) { current[num_current++] = hist[num_hist++]; continue; }
			is_merged[num_current] |= mask; current[num_current] = sum; num_current++; num_merged++;
			if (num_merged * 2 >= num_previous) break;
			sum = previous[num_merged * 2] + previous[num_merged * 2 + 1];
		}
		while (num_hist < n) current[num_current++] = hist[num_hist++];
		mask <<= 1;
		if (num_previous >= num_relevant)
		{
			bool keep_going = false;
			for (usz i = num_relevant; i > 1; i--) { if (previous[i - 1] != current[i - 1]) { keep_going = true; break; } }
			if (!keep_going) break;
		}
		@swap(previous, current); num_previous = num_current;
	}
	mask >>= 1; hist[..] = 0; usz num_analyze = num_relevant;
	while (mask != 0)
	{
		usz num_merged_loop = 0; hist[0]++; hist[1]++;
		usz symbol = 2;
		for (usz i = symbol; i < num_analyze; i++) { if ((is_merged[i] & mask) == 0) { hist[symbol]++; symbol++; } else { num_merged_loop++; } }
		num_analyze = 2 * num_merged_loop; mask >>= 1;
	}
	for (usz i = 0; i < num_analyze; i++) hist[i]++;
	char[] blen = allocator::new_array(allocator, char, freqs.len);
	foreach (i, m : map) blen[m.index] = (char)hist[i];
	return blen;
}

fn char[] Huffman.get_lengths(&self, usz max_sym)
{
	char[] blen = allocator::new_array(tmem, char, max_sym + 1);
	ushort index = 0;
	for (uint len = 1; len < 16; len++) { uint count = self.counts[len]; for (uint i = 0; i < count; i++) blen[self.symbols[index++]] = (char)len; }
	return blen;
}

fn void Huffman.build_from_freqs(&self, uint[] freqs, uint max_bits) => @pool()
{
	char[] blen = pkg_merge(tmem, freqs, max_bits);
	self.build(blen);
}

fn void gen_canonical_codes(Code* codes, char[] blen) @private
{
	ushort[16] bl_count; foreach (len : blen) { if (len > 0) bl_count[len]++; }
	ushort[16] next_code; ushort code = 0; bl_count[0] = 0;
	for (uint bits = 1; bits <= 15; bits++) { code = (code + bl_count[bits - 1]) << 1; next_code[bits] = code; }
	foreach (uint n, len : blen) { if (len != 0) { uint c = next_code[len]; codes[n].code = (ushort)c; codes[n].len = (ushort)len; next_code[len]++; } else { codes[n].code = 0; codes[n].len = 0; } }
}

fn void build_fixed_huffman(Huffman* lit, Huffman* dist) @private
{
	char[288] lit_lens;
	for (uint i = 0; i <= 143; i++) lit_lens[i] = 8;
	for (uint i = 144; i <= 255; i++) lit_lens[i] = 9;
	for (uint i = 256; i <= 279; i++) lit_lens[i] = 7;
	for (uint i = 280; i <= 287; i++) lit_lens[i] = 8;
	lit.build(&lit_lens);
	char[32] dist_lens; dist_lens[..] = 5; dist.build(&dist_lens);
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
	for (uint i = 0; i < 288; i++) { if (lens[i] > 0) bl_count[lens[i]]++; }
	ushort[16] next_code; ushort code = 0; bl_count[0] = 0;
	for (uint bits = 1; bits <= 15; bits++) { code = (code + bl_count[bits - 1]) << 1; next_code[bits] = code; }
	for (uint n = 0; n < 288; n++) { uint len = lens[n]; if (len != 0) { uint c = next_code[len]; codes[n].code = (ushort)c; codes[n].len = (ushort)len; next_code[len]++; } else { codes[n].code = 0; codes[n].len = 0; } }
}

fn ushort? Huffman.decode_stream(&self, StreamBitReader* reader) @inline
{
	uint peek = reader.peek_bits(11)!;
	uint entry = self.table[peek];
	if (entry != 0)
	{
		uint len = entry >> 16;
		if (len <= reader.nbits)
		{
			reader.consume_bits(len);
			return (ushort)(entry & 0xFFFF);
		}
	}

	// Standard walk fallback
	uint code = 0; uint first = 0; uint index = 0;
	for (uint len = 1; len < 16; len++)
	{
		uint b = reader.read_bits(1)!;
		code |= b;
		uint count = self.counts[len];
		if (code < first + count) { return self.symbols[index + (code - first)]; }
		index += count; first += count; first <<= 1; code <<= 1;
	}
	return CORRUPTED_DATA~;
}

fn void? inflater_step(Inflater* self) @private
{
	switch (self.state)
	{
		case START_BLOCK:
			self.final = self.reader.read_bits(1)! != 0;
			switch (self.reader.read_bits(2)!)
			{
				case 0:
					self.state = READ_STORED_LEN;
				case 1:
					self.lit_ptr = &lit_fixed_global;
					self.dist_ptr = &dist_fixed_global;
					self.state = DECODE_SYMBOL;
				case 2:
					self.state = READ_DYN_COUNTS;
				default:
					return CORRUPTED_DATA~;
			}
		case READ_STORED_LEN:
			self.reader.align();
			self.stored_len = self.reader.read_bits(16)!;
			uint nlen = self.reader.read_bits(16)!;
			if (self.stored_len != (~nlen & 0xFFFF)) return CORRUPTED_DATA~;
			if (self.stored_len == 0)
			{
				self.state = self.final ? DONE : START_BLOCK;
				break;
			}
			self.state = COPY_STORED;

		case COPY_STORED:
			char c = (char)self.reader.read_bits(8)!;
			inflater_write_byte(self, c);
			self.stored_len--;
			if (self.stored_len == 0)
			{
				self.state = self.final ? DONE : START_BLOCK;
			}
		case READ_DYN_COUNTS:
			self.hlit = self.reader.read_bits(5)! + 257;
			self.hdist = self.reader.read_bits(5)! + 1;
			self.hclen = self.reader.read_bits(4)! + 4;
			self.dyn_i = 0;
			self.code_lengths = {};
			self.state = READ_DYN_CODELENS;

		case READ_DYN_CODELENS:
			self.code_lengths[ORDER[self.dyn_i]] = (char)self.reader.read_bits(3)!;
			self.dyn_i++;
			if (self.dyn_i >= self.hclen)
			{
				self.dyn_code_huff.build(&self.code_lengths);
				self.dyn_i = 0;
				self.dyn_num_lengths = self.hlit + self.hdist;
				if (self.dyn_num_lengths > 320) return CORRUPTED_DATA~;
				self.lit_dist_lengths[..] = 0;
				self.state = READ_DYN_TREES;
			}

		case READ_DYN_TREES:
			ushort sym = self.dyn_code_huff.decode_stream(&self.reader)!;
			switch (sym)
			{
				case 0..15:
					if (self.dyn_i >= 320) return CORRUPTED_DATA~;
					self.lit_dist_lengths[self.dyn_i++] = (char)sym;
				case 16:
					if (self.dyn_i == 0) return CORRUPTED_DATA~;
					uint repeat_count = self.reader.read_bits(2)! + 3;
					char prev = self.lit_dist_lengths[self.dyn_i - 1];
					if (self.dyn_i + repeat_count > self.dyn_num_lengths || self.dyn_i + repeat_count > 320) return CORRUPTED_DATA~;
					for (uint k = 0; k < repeat_count; k++) self.lit_dist_lengths[self.dyn_i++] = prev;
				case 17:
					uint zero_len = self.reader.read_bits(3)! + 3;
					if (self.dyn_i + zero_len > self.dyn_num_lengths || self.dyn_i + zero_len > 320) return CORRUPTED_DATA~;
					for (uint k = 0; k < zero_len; k++) self.lit_dist_lengths[self.dyn_i++] = 0;
				case 18:
					uint zero_len = self.reader.read_bits(7)! + 11;
					if (self.dyn_i + zero_len > self.dyn_num_lengths || self.dyn_i + zero_len > 320) return CORRUPTED_DATA~;
					for (uint k = 0; k < zero_len; k++) self.lit_dist_lengths[self.dyn_i++] = 0;
			}
			if (self.dyn_i >= self.dyn_num_lengths)
			{
				uint hlit = self.hlit;
				uint hdist = self.hdist;
				uint total = hlit + hdist;
				if (hlit > 286 || hdist > 32 || total > 320 || hlit > total)
				{
					return CORRUPTED_DATA~;
				}
				self.dyn_lit.build(self.lit_dist_lengths[0:hlit]);
				self.dyn_dist.build(self.lit_dist_lengths[hlit:hdist]);
				self.lit_ptr = &self.dyn_lit;
				self.dist_ptr = &self.dyn_dist;
				self.state = DECODE_SYMBOL;
			}

		case DECODE_SYMBOL:
			ushort symbol = self.lit_ptr.decode_stream(&self.reader)!;
			switch
			{
				case symbol < 256:
					inflater_write_byte(self, (char)symbol);
				case symbol == 256:
					self.state = self.final ? DONE : START_BLOCK;
				case symbol <= 285:
					uint len_idx = symbol - 257;
					self.match_len = LENGTH_BASE[len_idx] + self.reader.read_bits(LENGTH_EXTRA[len_idx])!;
					self.state = READ_DIST_SYM;
				default:
					return CORRUPTED_DATA~;
			}

		case READ_DIST_SYM:
			ushort dist_sym = self.dist_ptr.decode_stream(&self.reader)!;
			self.match_dist = DIST_BASE[dist_sym] + self.reader.read_bits(DIST_EXTRA[dist_sym])!;
			self.state = COPY_MATCH;

		case COPY_MATCH:
			if (self.match_dist > self.pos) return CORRUPTED_DATA~;
			char c = self.window[(usz)((self.pos - self.match_dist) & 0xFFFF)];
			inflater_write_byte(self, c);
			self.match_len--;
			if (self.match_len == 0)
			{
				self.state = DECODE_SYMBOL;
			}

		case DONE:
			self.done = true;
			self.reader.close();
	}
	return;
}

fn void Inflater.write_byte(&self, char c) @private @inline { self.window[(usz)(self.pos & 0xFFFF)] = c; self.pos++; }

const ushort[31] LENGTH_BASE @private = { 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0 };
const char[31] LENGTH_EXTRA @private = { 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, 0, 0 };
const ushort[32] DIST_BASE @private = { 1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577, 0, 0 };
const char[32] DIST_EXTRA @private = { 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 0, 0 };
const uint[19] ORDER @private = { 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 };
