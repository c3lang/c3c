/*
 * LZX decompression implementation.
 * Adapted from Wine/gcab.
 */
#include "lib.h"
#include "lzx_decomp.h"
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <stddef.h>

#define DECR_OK           1
#define DECR_DATAFORMAT  -2
#define DECR_ILLEGALDATA -1
#define DECR_NOMEMORY    -3

#define LZX_BLOCKTYPE_VERBATIM     1
#define LZX_BLOCKTYPE_ALIGNED      2
#define LZX_BLOCKTYPE_UNCOMPRESSED 3

#define CAB_ULONG_BITS 32

/* LZX stuff */
#define LZX_MIN_MATCH                2
#define LZX_MAX_MATCH                257
#define LZX_NUM_CHARS                256
#define LZX_PRETREE_MAXSYMBOLS       20
#define LZX_ALIGNED_MAXSYMBOLS       8
#define LZX_NUM_SECONDARY_LENGTHS    249
#define LZX_MAINTREE_MAXSYMBOLS      (LZX_NUM_CHARS + 50*8)
#define LZX_LENGTH_MAXSYMBOLS        (LZX_NUM_SECONDARY_LENGTHS+1)

#define LZX_PRETREE_TABLEBITS   6
#define LZX_MAINTREE_TABLEBITS  12
#define LZX_LENGTH_TABLEBITS    12
#define LZX_ALIGNED_TABLEBITS   7

#define LZX_LENTABLE_SAFETY 64

#define LZX_DECLARE_TABLE(tbl) \
	uint16_t tbl##_table[(1 << LZX_##tbl##_TABLEBITS) + (LZX_##tbl##_MAXSYMBOLS << 1)]; \
	uint8_t tbl##_len[LZX_##tbl##_MAXSYMBOLS + LZX_LENTABLE_SAFETY]

struct LZXstate
{
	uint8_t *window;
	uint32_t window_size;
	uint32_t actual_size;
	uint32_t window_posn;
	uint32_t R0, R1, R2;
	uint16_t main_elements;
	int header_read;
	uint16_t block_type;
	uint32_t block_length;
	uint32_t block_remaining;
	uint32_t frames_read;
	int32_t intel_filesize;
	int32_t intel_curpos;
	int intel_started;

	LZX_DECLARE_TABLE(PRETREE);
	LZX_DECLARE_TABLE(MAINTREE);
	LZX_DECLARE_TABLE(LENGTH);
	LZX_DECLARE_TABLE(ALIGNED);
};

struct lzx_bits
{
	uint32_t bb;
	int bl;
	uint8_t *ip;
	uint8_t *limit;
};

struct lzx_decomp_state
{
	uint8_t *inbuf;
	uint8_t *outbuf;
	uint32_t lzx_position_base[51];
	uint8_t extra_bits[51];
	struct LZXstate lzx;
};

#define ENSURE_BITS(n)                                                                    \
	while (bitsleft < (n))                                                                \
	{                                                                                     \
		if (inpos + 2 > in_limit) break;                                                  \
		bitbuf |= ((uint32_t)((inpos[1] << 8) | inpos[0])) << (CAB_ULONG_BITS - 16 - bitsleft); \
		bitsleft += 16;                                                                   \
		inpos += 2;                                                                       \
	}

#define PEEK_BITS(n) (bitbuf >> (CAB_ULONG_BITS - (n)))
#define REMOVE_BITS(n) ((bitbuf <<= (n)), (bitsleft -= (n)))

#define READ_BITS(v, n)                                     \
	do                                                      \
	{                                                       \
		if (n)                                              \
		{                                                   \
			ENSURE_BITS(n);                                 \
			(v) = PEEK_BITS(n);                             \
			REMOVE_BITS(n);                                 \
		}                                                   \
		else                                                \
		{                                                   \
			(v) = 0;                                        \
		}                                                   \
	} while (0)

#define INIT_BITSTREAM      \
	do                      \
	{                       \
		bitbuf = 0;         \
		bitsleft = 0;       \
	} while (0)

static int make_decode_table(uint32_t nsyms, uint32_t nbits, const uint8_t *length, uint16_t *table)
{
	uint16_t sym;
	uint32_t leaf;
	uint8_t bit_num = 1;
	uint32_t fill;
	uint32_t pos = 0;
	uint32_t table_mask = 1 << nbits;
	uint32_t bit_mask = table_mask >> 1;
	uint32_t next_symbol = bit_mask;

	while (bit_num <= nbits)
	{
		for (sym = 0; sym < nsyms; sym++)
		{
			if (length[sym] == bit_num)
			{
				leaf = pos;
				if ((pos += bit_mask) > table_mask) return 1;
				fill = bit_mask;
				while (fill-- > 0)
					table[leaf++] = sym;
			}
		}
		bit_mask >>= 1;
		bit_num++;
	}

	if (pos != table_mask)
	{
		for (sym = pos; sym < table_mask; sym++)
			table[sym] = 0;
		pos <<= 16;
		table_mask <<= 16;
		bit_mask = 1 << 15;

		while (bit_num <= 16)
		{
			for (sym = 0; sym < nsyms; sym++)
			{
				if (length[sym] == bit_num)
				{
					leaf = pos >> 16;
					for (fill = 0; fill < bit_num - nbits; fill++)
					{
						if (table[leaf] == 0)
						{
							table[(next_symbol << 1)] = 0;
							table[(next_symbol << 1) + 1] = 0;
							table[leaf] = (uint16_t)next_symbol++;
						}
						leaf = table[leaf] << 1;
						if ((pos >> (15 - fill)) & 1) leaf++;
					}
					table[leaf] = sym;
					if ((pos += bit_mask) > table_mask) return 1;
				}
			}
			bit_mask >>= 1;
			bit_num++;
		}
	}
	if (pos == table_mask) return 0;
	for (sym = 0; sym < nsyms; sym++)
		if (length[sym]) return 1;
	return 0;
}

#define TABLEBITS(tbl)   (LZX_##tbl##_TABLEBITS)
#define MAXSYMBOLS(tbl)  (LZX_##tbl##_MAXSYMBOLS)
#define SYMTABLE(tbl)    (state->lzx.tbl##_table)
#define LENTABLE(tbl)    (state->lzx.tbl##_len)

#define BUILD_TABLE(tbl)                                                              \
	if (make_decode_table(                                                            \
			(uint32_t)MAXSYMBOLS(tbl), (uint32_t)TABLEBITS(tbl), LENTABLE(tbl), SYMTABLE(tbl))) \
	{                                                                                 \
		return DECR_ILLEGALDATA;                                                      \
	}

#define READ_HUFFSYM(tbl, var)                                                     \
	do                                                                             \
	{                                                                              \
		ENSURE_BITS(16);                                                           \
		hufftbl = SYMTABLE(tbl);                                                   \
		if ((i = hufftbl[PEEK_BITS(TABLEBITS(tbl))]) >= MAXSYMBOLS(tbl))           \
		{                                                                          \
			j = 1 << (CAB_ULONG_BITS - TABLEBITS(tbl));                            \
			do                                                                     \
			{                                                                      \
				j >>= 1;                                                           \
				i <<= 1;                                                           \
				i |= (bitbuf & j) ? 1 : 0;                                         \
				if (!j)                                                            \
				{                                                                  \
					return DECR_ILLEGALDATA;                                       \
				}                                                                  \
			} while ((i = hufftbl[i]) >= MAXSYMBOLS(tbl));                         \
		}                                                                          \
		(var) = i;                                                                 \
		j = LENTABLE(tbl)[i];                                                      \
		REMOVE_BITS(j);                                                            \
	} while (0)

#define READ_LENGTHS(tbl, first, last, fn)           \
	do                                               \
	{                                                \
		lb.bb = bitbuf;                              \
		lb.bl = bitsleft;                            \
		lb.ip = inpos;                               \
		if (fn(LENTABLE(tbl), (first), (last), &lb, state)) \
		{                                            \
			return DECR_ILLEGALDATA;                 \
		}                                            \
		bitbuf = lb.bb;                              \
		bitsleft = lb.bl;                            \
		inpos = lb.ip;                               \
	} while (0)

static int lzx_read_lens(uint8_t *lens, uint32_t first, uint32_t last, struct lzx_bits *lb, lzx_decomp_state *state)
{
	uint32_t x, y;
	int z;
	uint32_t bitbuf = lb->bb;
	int bitsleft = lb->bl;
	uint8_t *inpos = lb->ip;
	uint8_t *in_limit = lb->limit;
	uint16_t *hufftbl;
	uint32_t i, j;

	for (x = 0; x < 20; x++)
	{
		READ_BITS(y, 4);
		state->lzx.PRETREE_len[x] = (uint8_t)y;
	}
	BUILD_TABLE(PRETREE);

	for (x = first; x < last;)
	{
		READ_HUFFSYM(PRETREE, z);
		if (z == 17)
		{
			READ_BITS(y, 4);
			y += 4;
			while (y--)
				lens[x++] = 0;
		}
		else if (z == 18)
		{
			READ_BITS(y, 5);
			y += 20;
			while (y--)
				lens[x++] = 0;
		}
		else if (z == 19)
		{
			READ_BITS(y, 1);
			y += 4;
			int temp;
			READ_HUFFSYM(PRETREE, temp);
			z = (int)lens[x] - temp;
			if (z < 0) z += 17;
			while (y--)
				lens[x++] = (uint8_t)z;
		}
		else
		{
			z = (int)lens[x] - z;
			if (z < 0) z += 17;
			lens[x++] = (uint8_t)z;
		}
	}
	lb->bb = bitbuf;
	lb->bl = bitsleft;
	lb->ip = inpos;
	return 0;
}

lzx_decomp_state *lzx_decomp_create(int window)
{
	static const uint8_t bits[] =
	    {0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
	     7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14,
	     15, 15, 16, 16, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,
	     17, 17, 17};
	static const uint32_t base[] =
	    {0, 1, 2, 3, 4, 6, 8, 12,
	     16, 24, 32, 48, 64, 96, 128, 192,
	     256, 384, 512, 768, 1024, 1536, 2048, 3072,
	     4096, 6144, 8192, 12288, 16384, 24576, 32768, 49152,
	     65536, 98304, 131072, 196608, 262144, 393216, 524288, 655360,
	     786432, 917504, 1048576, 1179648, 1310720, 1441792, 1572864, 1703936,
	     1835008, 1966080, 2097152};

	if (window < 15 || window > 21) return NULL;

	lzx_decomp_state *state = ccalloc(1, sizeof(lzx_decomp_state));
	if (!state) return NULL;

	uint32_t wndsize = 1U << window;
	state->lzx.window = cmalloc(wndsize);
	if (!state->lzx.window)
	{
		free(state);
		return NULL;
	}
	state->lzx.actual_size = wndsize;
	state->lzx.window_size = wndsize;
	state->outbuf = cmalloc(32768);
	if (!state->outbuf)
	{
		free(state->lzx.window);
		free(state);
		return NULL;
	}

	memcpy(state->lzx_position_base, base, sizeof(base));
	memcpy(state->extra_bits, bits, sizeof(bits));

	int posn_slots = (window == 21) ? 50 : (window == 20) ? 42 : (window == 19) ? 38 : (window == 18) ? 34 : (window == 17) ? 30 : (window == 16) ? 26 : 22;

	state->lzx.R0 = state->lzx.R1 = state->lzx.R2 = 1;
	state->lzx.main_elements = (uint16_t)(LZX_NUM_CHARS + (posn_slots << 3));
	state->lzx.header_read = 0;
	state->lzx.frames_read = 0;
	state->lzx.intel_started = 0;
	state->lzx.window_posn = 0;

	return state;
}

int lzx_decomp_run(lzx_decomp_state *state, uint8_t *in, int inlen, uint8_t *out, int outlen)
{
	uint8_t *inpos = in;
	uint8_t *in_limit = in + inlen;
	uint8_t *outpos = state->outbuf;
	uint8_t *endpos = outpos + outlen;
	uint32_t bitbuf;
	int bitsleft;
	uint16_t *hufftbl;
	uint32_t i, j;
	int z;

	INIT_BITSTREAM;

	if (!state->lzx.header_read)
	{
		uint32_t intel;
		READ_BITS(intel, 1);
		if (intel) READ_BITS(state->lzx.intel_filesize, 31);
		state->lzx.header_read = 1;
	}

	while (outpos < endpos)
	{
		if (state->lzx.block_remaining == 0)
		{
			if (state->lzx.block_type == LZX_BLOCKTYPE_ALIGNED)
			{
				state->lzx.intel_curpos += (int32_t)state->lzx.block_length;
			}
			READ_BITS(state->lzx.block_type, 3);
			READ_BITS(i, 24);
			state->lzx.block_remaining = state->lzx.block_length = i;

			switch (state->lzx.block_type)
			{
				case LZX_BLOCKTYPE_ALIGNED:
					for (i = 0; i < 8; i++)
					{
						READ_BITS(j, 3);
						state->lzx.ALIGNED_len[i] = (uint8_t)j;
					}
					BUILD_TABLE(ALIGNED);
					/* fall through */
				case LZX_BLOCKTYPE_VERBATIM:
				{
					struct lzx_bits lb;
					lb.limit = in_limit;
					READ_LENGTHS(MAINTREE, 0, 256, lzx_read_lens);
					READ_LENGTHS(MAINTREE, 256, state->lzx.main_elements, lzx_read_lens);
					BUILD_TABLE(MAINTREE);
					READ_LENGTHS(LENGTH, 0, LZX_NUM_SECONDARY_LENGTHS, lzx_read_lens);
					BUILD_TABLE(LENGTH);
				}
				break;
				case LZX_BLOCKTYPE_UNCOMPRESSED:
					state->lzx.block_type = LZX_BLOCKTYPE_VERBATIM;
					bitsleft = 0;
					bitbuf = 0;
					for (i = 0; i < 3; i++)
					{
						READ_BITS(j, 8);
						((uint8_t *)&state->lzx.R0)[i] = (uint8_t)j;
					}
					for (i = 0; i < 3; i++)
					{
						READ_BITS(j, 8);
						((uint8_t *)&state->lzx.R1)[i] = (uint8_t)j;
					}
					for (i = 0; i < 3; i++)
					{
						READ_BITS(j, 8);
						((uint8_t *)&state->lzx.R2)[i] = (uint8_t)j;
					}
					break;
				default:
					return DECR_ILLEGALDATA;
			}
		}

		z = (int)state->lzx.block_remaining;
		while (z > 0 && outpos < endpos)
		{
			READ_HUFFSYM(MAINTREE, i);
			if (i < LZX_NUM_CHARS)
			{
				state->lzx.window[state->lzx.window_posn++] = (uint8_t)i;
				*outpos++ = (uint8_t)i;
				z--;
			}
			else
			{
				uint32_t match_len, match_offset, window_posn;
				uint8_t extra;

				i -= LZX_NUM_CHARS;
				match_len = i & 7;
				if (match_len == 7)
				{
					int temp;
					READ_HUFFSYM(LENGTH, temp);
					match_len += (uint32_t)temp;
				}
				match_len += LZX_MIN_MATCH;

				match_offset = i >> 3;
				if (match_offset > 2)
				{
					extra = state->extra_bits[match_offset];
					if (state->lzx.block_type == LZX_BLOCKTYPE_ALIGNED && extra >= 3)
					{
						READ_BITS(j, extra - 3);
						match_offset = state->lzx_position_base[match_offset] + (j << 3);
						int temp;
						READ_HUFFSYM(ALIGNED, temp);
						match_offset += (uint32_t)temp;
					}
					else
					{
						READ_BITS(j, extra);
						match_offset = state->lzx_position_base[match_offset] + j;
					}
					state->lzx.R2 = state->lzx.R1;
					state->lzx.R1 = state->lzx.R0;
					state->lzx.R0 = match_offset;
				}
				else if (match_offset == 0)
					match_offset = state->lzx.R0;
				else if (match_offset == 1)
				{
					match_offset = state->lzx.R1;
					state->lzx.R1 = state->lzx.R0;
					state->lzx.R0 = match_offset;
				}
				else
				{
					match_offset = state->lzx.R2;
					state->lzx.R2 = state->lzx.R0;
					state->lzx.R0 = match_offset;
				}

				window_posn = state->lzx.window_posn;
				if (match_len > (uint32_t)z) match_len = (uint32_t)z;
				if (match_len > (uint32_t)(endpos - outpos)) match_len = (uint32_t)(endpos - outpos);

				z -= (int)match_len;
				state->lzx.window_posn += match_len;

				while (match_len--)
				{
					i = (window_posn - match_offset) & (state->lzx.window_size - 1);
					state->lzx.window[window_posn++ & (state->lzx.window_size - 1)] = state->lzx.window[i];
					*outpos++ = state->lzx.window[i];
				}
			}
		}
		state->lzx.block_remaining = (uint32_t)z;
	}

	state->lzx.frames_read++;
	if (state->lzx.frames_read == 32768) state->lzx.intel_started = 1;

	if (state->lzx.intel_started && state->lzx.intel_filesize)
	{
		if (outlen > 10)
		{
			uint8_t *p = state->outbuf;
			uint8_t *e = p + outlen - 10;
			int32_t curpos = state->lzx.intel_curpos;
			int32_t filesize = state->lzx.intel_filesize;

			while (p < e)
			{
				if (*p++ == 0xE8)
				{
					int32_t abs_off = (int32_t)((p[3] << 24) | (p[2] << 16) | (p[1] << 8) | p[0]);
					if (abs_off >= -curpos && abs_off < filesize)
					{
						int32_t rel_off = (abs_off >= 0) ? abs_off - curpos : abs_off + filesize;
						p[0] = (uint8_t)rel_off;
						p[1] = (uint8_t)(rel_off >> 8);
						p[2] = (uint8_t)(rel_off >> 16);
						p[3] = (uint8_t)(rel_off >> 24);
					}
					p += 4;
					curpos += 5;
				}
				else
					curpos++;
			}
		}
	}
	state->lzx.intel_curpos += (int32_t)outlen;
	memcpy(out, state->outbuf, outlen);
	return DECR_OK;
}

void lzx_decomp_free(lzx_decomp_state *state)
{
	if (!state) return;
	if (state->lzx.window) free(state->lzx.window);
	if (state->outbuf) free(state->outbuf);
	free(state);
}
