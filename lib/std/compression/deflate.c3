<*
 DEFLATE compression module implementation as defined in RFC 1951.
*>
module std::compression::deflate;
import std::core::mem, std::io, std::math;

faultdef
	CORRUPTED_DATA;

struct BitReader @private
{
	char[] data;
	usz bit_pos;
}

fn uint BitReader.read_bits(&self, uint count) @private
{
	uint value = 0;
	for (uint i = 0; i < count; i++)
	{
		if (self.bit_pos >= self.data.len * 8) break;
		uint byte_idx = (uint)(self.bit_pos / 8);
		uint bit_idx = (uint)(self.bit_pos % 8);
		if (self.data[byte_idx] & (1 << bit_idx))
		{
			value |= (1 << i);
		}
		self.bit_pos++;
	}
	return value;
}

fn void BitReader.align(&self) @private
{
	self.bit_pos = (self.bit_pos + (usz)7) & ~(usz)7;
}

struct BitWriter @private
{
	char[] data;
	usz bit_pos;
	Allocator allocator;
}

fn void BitWriter.init(&self, Allocator allocator, usz initial_cap) @private
{
	self.allocator = allocator;
	self.data = allocator::malloc(allocator, initial_cap)[:initial_cap];
	self.bit_pos = 0;
	mem::set(self.data.ptr, 0, initial_cap);
}

fn void BitWriter.write_bits(&self, uint value, uint count) @private
{
	for (uint i = 0; i < count; i++)
	{
		usz byte_idx = self.bit_pos / 8;
		uint bit_idx = (uint)(self.bit_pos % 8);

		while (byte_idx >= self.data.len)
		{
			usz new_cap = self.data.len * 2;
			self.data = allocator::realloc_array(self.allocator, self.data.ptr, char, new_cap);
			mem::set(self.data.ptr + self.data.len / 2, 0, self.data.len / 2);
		}

		if (value & (1 << i))
		{
			self.data[byte_idx] |= (char)(1 << bit_idx);
		}
		self.bit_pos++;
	}
}

fn void BitWriter.write_huffman(&self, uint code, uint len) @private
{
	uint rev = 0;
	for (uint i = 0; i < len; i++)
	{
		if (code & (1 << i))
		{
			rev |= (1 << (len - 1 - i));
		}
	}
	self.write_bits(rev, len);
}

fn char[] BitWriter.finish(&self) @private
{
	usz final_len = (self.bit_pos + 7) / 8;
	return self.data[:final_len];
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
		if (len > 0) self.counts[len]++;
	}

	ushort offset = 0;
	for (uint i = 1; i < 16; i++)
	{
		offsets[i] = offset;
		offset += self.counts[i];
	}

	foreach (uint i, len : lengths)
	{
		if (len > 0)
		{
			self.symbols[offsets[len]++] = (ushort)i;
		}
	}
}

fn ushort? Huffman.decode(&self, BitReader* reader) @private
{
	uint code = 0;
	uint first = 0;
	uint index = 0;
	for (uint len = 1; len < 16; len++)
	{
		code |= reader.read_bits(1);
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
			codes[n].code = next_code[len];
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

<*
 Decompress data using the DEFLATE algorithm.
 @param input : `The compressed data to inflate.`
 @param allocator : `The allocator to use.`
 @return `The decompressed data.`
*>
fn char[]? inflate(char[] input, Allocator allocator = mem)
{
	BitReader reader = { .data = input, .bit_pos = 0 };

	usz out_cap = input.len * 2;
	if (out_cap < 1024) out_cap = 1024;
	char[] out = allocator::malloc(allocator, out_cap)[:out_cap];
	defer (catch err) allocator::free(allocator, out.ptr);

	usz out_len = 0;

	Huffman fixed_lit;
	Huffman fixed_dist;
	build_fixed_huffman(&fixed_lit, &fixed_dist);

	bool final = false;
	while (!final)
	{
		final = reader.read_bits(1) != 0;
		uint type = reader.read_bits(2);

		Huffman* lit;
		Huffman* dist;
		Huffman dyn_lit;
		Huffman dyn_dist;

		switch (type)
		{
			case 0: // No compression
				reader.align();
				uint len = (uint)reader.read_bits(16);
				uint nlen = (uint)reader.read_bits(16);
				if (len != (~nlen & 0xFFFF)) return CORRUPTED_DATA~;

				while (out_len + len > out.len)
				{
					out_cap *= 2;
					out = allocator::realloc_array(allocator, out.ptr, char, out_cap);
				}

				usz byte_pos = reader.bit_pos / 8;
				if (byte_pos + len > input.len) return CORRUPTED_DATA~;
				mem::copy(out.ptr + out_len, input.ptr + byte_pos, len);
				out_len += len;
				reader.bit_pos += (usz)len * 8;

			case 1: // Fixed Huffman
				lit = &fixed_lit;
				dist = &fixed_dist;
				nextcase 4;

			case 2: // Dynamic Huffman
				uint hlit = reader.read_bits(5) + 257;
				uint hdist = reader.read_bits(5) + 1;
				uint hclen = reader.read_bits(4) + 4;

				char[19] code_lengths;
				mem::set(&code_lengths, 0, 19);

				for (uint i = 0; i < hclen; i++)
				{
					code_lengths[ORDER[i]] = (char)reader.read_bits(3);
				}

				Huffman code_huff;
				code_huff.build(&code_lengths);

				char[288 + 32] lit_dist_lengths;
				mem::set(&lit_dist_lengths, 0, 288 + 32);
				uint num_lengths = hlit + hdist;
				uint i = 0;

				while (i < num_lengths)
				{
					ushort sym = code_huff.decode(&reader)!;
					switch
					{
						case sym < 16:
							lit_dist_lengths[i++] = (char)sym;
						case sym == 16:
							if (i == 0) return CORRUPTED_DATA~;
							char prev = lit_dist_lengths[i-1];
							uint copy_len = reader.read_bits(2) + 3;
							if (i + copy_len > num_lengths) return CORRUPTED_DATA~;
							for (uint k = 0; k < copy_len; k++) lit_dist_lengths[i++] = prev;
						case sym == 17:
							uint zero_len = reader.read_bits(3) + 3;
							if (i + zero_len > num_lengths) return CORRUPTED_DATA~;
							for (uint k = 0; k < zero_len; k++) lit_dist_lengths[i++] = 0;
						case sym == 18:
							uint zero_len = reader.read_bits(7) + 11;
							if (i + zero_len > num_lengths) return CORRUPTED_DATA~;
							for (uint k = 0; k < zero_len; k++) lit_dist_lengths[i++] = 0;
						default:
							return CORRUPTED_DATA~;
					}
				}

				dyn_lit.build(lit_dist_lengths[0:hlit]);
				dyn_dist.build(lit_dist_lengths[hlit:hlit + hdist]);
				lit = &dyn_lit;
				dist = &dyn_dist;
				nextcase;

			case 4: // Decode Block
				while BLOCK_LOOP: (true)
				{
					ushort symbol = lit.decode(&reader)!;
					switch
					{
						case symbol < 256:
							if (out_len >= out.len)
							{
								out_cap *= 2;
								out = allocator::realloc_array(allocator, out.ptr, char, out_cap);
							}
							out[out_len++] = (char)symbol;

						case symbol == 256:
							break BLOCK_LOOP;

						case symbol <= 285:
							uint len_idx = symbol - 257;
							uint length = LENGTH_BASE[len_idx] + reader.read_bits(LENGTH_EXTRA[len_idx]);

							ushort dist_sym = dist.decode(&reader)!;
							uint distance = DIST_BASE[dist_sym] + reader.read_bits(DIST_EXTRA[dist_sym]);

							if (distance > out_len) return CORRUPTED_DATA~;

							while (out_len + length > out.len)
							{
								out_cap *= 2;
								out = allocator::realloc_array(allocator, out.ptr, char, out_cap);
							}

							for (uint k = 0; k < length; k++)
							{
								out[out_len] = out[out_len - distance];
								out_len++;
							}

						default:
							return CORRUPTED_DATA~;
					}
				}

			default:
				return CORRUPTED_DATA~;
		}
	}

	if (out_len == 0)
	{
		allocator::free(allocator, out.ptr);
		return allocator::new_array(allocator, char, 0);
	}

	char[] result = allocator::malloc(allocator, out_len)[:out_len];
	mem::copy(result.ptr, out.ptr, out_len);
	allocator::free(allocator, out.ptr);
	return result;
}

<*
 Compress data using the DEFLATE algorithm.
 @param input : `The data to compress.`
 @param allocator : `The allocator to use.`
 @return `The compressed data.`
*>
fn char[]? compress(char[] input, Allocator allocator = mem)
{
	BitWriter writer;
	writer.init(allocator, (usz)math::max((long)input.len / 2, (long)1024));

	writer.write_bits(1, 1);
	writer.write_bits(1, 2);

	Code[288] codes;
	gen_fixed_codes(&codes);

	const uint MIN_MATCH = 3;
	const uint MAX_MATCH = 258;
	const uint WINDOW_SIZE = 32768;

	usz pos = 0;
	while (pos < input.len)
	{
		uint best_len = 0;
		uint best_dist = 0;

		usz search_start = pos > WINDOW_SIZE ? pos - WINDOW_SIZE : 0;

		for (usz i = search_start; i < pos; i++)
		{
			uint match_len = 0;
			while (match_len < MAX_MATCH &&
				   pos + match_len < input.len &&
				   input[i + match_len] == input[pos + match_len])
			{
				match_len++;
			}

			if (match_len >= MIN_MATCH && match_len > best_len)
			{
				best_len = match_len;
				best_dist = (uint)(pos - i);
			}
		}

		if (best_len >= MIN_MATCH)
		{
			uint len_code = 257;
			uint len_extra_bits = 0;
			uint len_extra = 0;

			switch (best_len)
			{
				case 3..10:
					len_code = 257 + best_len - 3;
				case 11..18:
					len_code = 265 + (best_len - 11) / 2;
					len_extra_bits = 1;
					len_extra = (best_len - 11) % 2;
				case 19..34:
					len_code = 269 + (best_len - 19) / 4;
					len_extra_bits = 2;
					len_extra = (best_len - 19) % 4;
				case 35..66:
					len_code = 273 + (best_len - 35) / 8;
					len_extra_bits = 3;
					len_extra = (best_len - 35) % 8;
				case 67..130:
					len_code = 277 + (best_len - 67) / 16;
					len_extra_bits = 4;
					len_extra = (best_len - 67) % 16;
				case 131..257:
					len_code = 281 + (best_len - 131) / 32;
					len_extra_bits = 5;
					len_extra = (best_len - 131) % 32;
				default:
					len_code = 285;
			}

			Code lc = codes[len_code];
			writer.write_huffman((uint)lc.code, (uint)lc.len);
			if (len_extra_bits > 0)
			{
				writer.write_bits(len_extra, len_extra_bits);
			}

			uint dist_code = 0;
			uint dist_extra_bits = 0;
			uint dist_extra = 0;

			switch (best_dist)
			{
				case 1..4:
					dist_code = best_dist - 1;
				case 5..8:
					dist_code = 4 + (best_dist - 5) / 2;
					dist_extra_bits = 1;
					dist_extra = (best_dist - 5) % 2;
				case 9..16:
					dist_code = 6 + (best_dist - 9) / 4;
					dist_extra_bits = 2;
					dist_extra = (best_dist - 9) % 4;
				case 17..32:
					dist_code = 8 + (best_dist - 17) / 8;
					dist_extra_bits = 3;
					dist_extra = (best_dist - 17) % 8;
				case 33..64:
					dist_code = 10 + (best_dist - 33) / 16;
					dist_extra_bits = 4;
					dist_extra = (best_dist - 33) % 16;
				case 65..128:
					dist_code = 12 + (best_dist - 65) / 32;
					dist_extra_bits = 5;
					dist_extra = (best_dist - 65) % 32;
				case 129..256:
					dist_code = 14 + (best_dist - 129) / 64;
					dist_extra_bits = 6;
					dist_extra = (best_dist - 129) % 64;
				case 257..512:
					dist_code = 16 + (best_dist - 257) / 128;
					dist_extra_bits = 7;
					dist_extra = (best_dist - 257) % 128;
				case 513..1024:
					dist_code = 18 + (best_dist - 513) / 256;
					dist_extra_bits = 8;
					dist_extra = (best_dist - 513) % 256;
				case 1025..2048:
					dist_code = 20 + (best_dist - 1025) / 512;
					dist_extra_bits = 9;
					dist_extra = (best_dist - 1025) % 512;
				case 2049..4096:
					dist_code = 22 + (best_dist - 2049) / 1024;
					dist_extra_bits = 10;
					dist_extra = (best_dist - 2049) % 1024;
				case 4097..8192:
					dist_code = 24 + (best_dist - 4097) / 2048;
					dist_extra_bits = 11;
					dist_extra = (best_dist - 4097) % 2048;
				case 8193..16384:
					dist_code = 26 + (best_dist - 8193) / 4096;
					dist_extra_bits = 12;
					dist_extra = (best_dist - 8193) % 4096;
				default:
					dist_code = 28 + (best_dist - 16385) / 8192;
					dist_extra_bits = 13;
					dist_extra = (best_dist - 16385) % 8192;
			}

			writer.write_huffman(dist_code, 5);
			if (dist_extra_bits > 0)
			{
				writer.write_bits(dist_extra, dist_extra_bits);
			}

			pos += best_len;
		}
		else
		{
			Code c = codes[(uint)input[pos]];
			writer.write_huffman((uint)c.code, (uint)c.len);
			pos++;
		}
	}

	Code eob = codes[256];
	writer.write_huffman((uint)eob.code, (uint)eob.len);

	return writer.finish();
}
