/* SPDX-License-Identifier: 0BSD */

#include <stdbool.h>
#include <string.h>

#include "xz.h"
#include "xz_config.h"

#ifndef XZ_CONFIG_H
#define XZ_CONFIG_H
#if defined(_MSC_VER) && _MSC_VER < 1900 && !defined(inline)
#	define inline __inline
#endif
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#define kmalloc(size, flags) malloc(size)
#define kfree(ptr) free(ptr)
#define vmalloc(size) malloc(size)
#define vfree(ptr) free(ptr)
#define memeq(a, b, size) (memcmp(a, b, size) == 0)
#define memzero(buf, size) memset(buf, 0, size)
#ifndef min
#	define min(x, y) ((x) < (y) ? (x) : (y))
#endif
#define min_t(type, x, y) min(x, y)
#ifndef fallthrough
#	if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 202311
#		define fallthrough [[fallthrough]]
#	elif (defined(__GNUC__) && __GNUC__ >= 7) \
			|| (defined(__clang_major__) && __clang_major__ >= 10)
#		define fallthrough __attribute__((__fallthrough__))
#	else
#		define fallthrough do {} while (0)
#	endif
#endif
#ifndef __always_inline
#	ifdef __GNUC__
#		define __always_inline \
			inline __attribute__((__always_inline__))
#	else
#		define __always_inline inline
#	endif
#endif
#ifndef get_unaligned_le32
static inline uint32_t get_unaligned_le32(const uint8_t *buf)
{
	return (uint32_t)buf[0]
			| ((uint32_t)buf[1] << 8)
			| ((uint32_t)buf[2] << 16)
			| ((uint32_t)buf[3] << 24);
}
#endif
#ifndef get_unaligned_be32
static inline uint32_t get_unaligned_be32(const uint8_t *buf)
{
	return (uint32_t)((uint32_t)buf[0] << 24)
			| ((uint32_t)buf[1] << 16)
			| ((uint32_t)buf[2] << 8)
			| (uint32_t)buf[3];
}
#endif
#ifndef put_unaligned_le32
static inline void put_unaligned_le32(uint32_t val, uint8_t *buf)
{
	buf[0] = (uint8_t)val;
	buf[1] = (uint8_t)(val >> 8);
	buf[2] = (uint8_t)(val >> 16);
	buf[3] = (uint8_t)(val >> 24);
}
#endif
#ifndef put_unaligned_be32
static inline void put_unaligned_be32(uint32_t val, uint8_t *buf)
{
	buf[0] = (uint8_t)(val >> 24);
	buf[1] = (uint8_t)(val >> 16);
	buf[2] = (uint8_t)(val >> 8);
	buf[3] = (uint8_t)val;
}
#endif
#ifndef get_le32
#	define get_le32 get_unaligned_le32
#endif
#ifndef get_be32
#	define get_be32 get_unaligned_be32
#endif
#endif
#ifndef XZ_PRIVATE_H
#define XZ_PRIVATE_H
#ifdef __KERNEL__
#	include <linux/xz.h>
#	include <linux/kernel.h>
#	include <linux/unaligned.h>
#	ifndef XZ_PREBOOT
#		include <linux/slab.h>
#		include <linux/vmalloc.h>
#		include <linux/string.h>
#		ifdef CONFIG_XZ_DEC_X86
#			define XZ_DEC_X86
#		endif
#		ifdef CONFIG_XZ_DEC_POWERPC
#			define XZ_DEC_POWERPC
#		endif
#		ifdef CONFIG_XZ_DEC_IA64
#			define XZ_DEC_IA64
#		endif
#		ifdef CONFIG_XZ_DEC_ARM
#			define XZ_DEC_ARM
#		endif
#		ifdef CONFIG_XZ_DEC_ARMTHUMB
#			define XZ_DEC_ARMTHUMB
#		endif
#		ifdef CONFIG_XZ_DEC_SPARC
#			define XZ_DEC_SPARC
#		endif
#		ifdef CONFIG_XZ_DEC_ARM64
#			define XZ_DEC_ARM64
#		endif
#		ifdef CONFIG_XZ_DEC_RISCV
#			define XZ_DEC_RISCV
#		endif
#		ifdef CONFIG_XZ_DEC_MICROLZMA
#			define XZ_DEC_MICROLZMA
#		endif
#		define memeq(a, b, size) (memcmp(a, b, size) == 0)
#		define memzero(buf, size) memset(buf, 0, size)
#	endif
#	define get_le32(p) le32_to_cpup((const uint32_t *)(p))
#else
#endif
#if !defined(XZ_DEC_SINGLE) && !defined(XZ_DEC_PREALLOC) \
		&& !defined(XZ_DEC_DYNALLOC)
#	define XZ_DEC_SINGLE
#	define XZ_DEC_PREALLOC
#	define XZ_DEC_DYNALLOC
#endif
#ifdef XZ_DEC_SINGLE
#	define DEC_IS_SINGLE(mode) ((mode) == XZ_SINGLE)
#else
#	define DEC_IS_SINGLE(mode) (false)
#endif
#ifdef XZ_DEC_PREALLOC
#	define DEC_IS_PREALLOC(mode) ((mode) == XZ_PREALLOC)
#else
#	define DEC_IS_PREALLOC(mode) (false)
#endif
#ifdef XZ_DEC_DYNALLOC
#	define DEC_IS_DYNALLOC(mode) ((mode) == XZ_DYNALLOC)
#else
#	define DEC_IS_DYNALLOC(mode) (false)
#endif
#if !defined(XZ_DEC_SINGLE)
#	define DEC_IS_MULTI(mode) (true)
#elif defined(XZ_DEC_PREALLOC) || defined(XZ_DEC_DYNALLOC)
#	define DEC_IS_MULTI(mode) ((mode) != XZ_SINGLE)
#else
#	define DEC_IS_MULTI(mode) (false)
#endif
#ifndef XZ_DEC_BCJ
#	if defined(XZ_DEC_X86) || defined(XZ_DEC_POWERPC) \
			|| defined(XZ_DEC_IA64) \
			|| defined(XZ_DEC_ARM) || defined(XZ_DEC_ARMTHUMB) \
			|| defined(XZ_DEC_SPARC) || defined(XZ_DEC_ARM64) \
			|| defined(XZ_DEC_RISCV)
#		define XZ_DEC_BCJ
#	endif
#endif
struct xz_sha256 {
	uint8_t data[64];
	uint32_t state[8];
	uint64_t size;
};
XZ_EXTERN void xz_sha256_reset(struct xz_sha256 *s);
XZ_EXTERN void xz_sha256_update(const uint8_t *buf, size_t size,
				struct xz_sha256 *s);
XZ_EXTERN bool xz_sha256_validate(const uint8_t *buf, struct xz_sha256 *s);
XZ_EXTERN struct xz_dec_lzma2 *xz_dec_lzma2_create(enum xz_mode mode,
						   uint32_t dict_max);
XZ_EXTERN enum xz_ret xz_dec_lzma2_reset(struct xz_dec_lzma2 *s,
					 uint8_t props);
XZ_EXTERN enum xz_ret xz_dec_lzma2_run(struct xz_dec_lzma2 *s,
				       struct xz_buf *b);
XZ_EXTERN void xz_dec_lzma2_end(struct xz_dec_lzma2 *s);
#ifdef XZ_DEC_BCJ
XZ_EXTERN struct xz_dec_bcj *xz_dec_bcj_create(bool single_call);
XZ_EXTERN enum xz_ret xz_dec_bcj_reset(struct xz_dec_bcj *s, uint8_t id);
XZ_EXTERN enum xz_ret xz_dec_bcj_run(struct xz_dec_bcj *s,
				     struct xz_dec_lzma2 *lzma2,
				     struct xz_buf *b);
#define xz_dec_bcj_end(s) kfree(s)
#endif
#endif
#ifndef XZ_STREAM_H
#define XZ_STREAM_H
#if defined(__KERNEL__) && !XZ_INTERNAL_CRC32
#	include <linux/crc32.h>
#	undef crc32
#	define xz_crc32(buf, size, crc) \
		(~crc32_le(~(uint32_t)(crc), buf, size))
#endif
#define STREAM_HEADER_SIZE 12
#define HEADER_MAGIC "\3757zXZ"
#define HEADER_MAGIC_SIZE 6
#define FOOTER_MAGIC "YZ"
#define FOOTER_MAGIC_SIZE 2
typedef uint64_t vli_type;
#define VLI_MAX ((vli_type)-1 / 2)
#define VLI_UNKNOWN ((vli_type)-1)
#define VLI_BYTES_MAX (sizeof(vli_type) * 8 / 7)
enum xz_check {
	XZ_CHECK_NONE = 0,
	XZ_CHECK_CRC32 = 1,
	XZ_CHECK_CRC64 = 4,
	XZ_CHECK_SHA256 = 10
};
#define XZ_CHECK_MAX 15
#endif
#ifndef XZ_STREAM_H
#define XZ_STREAM_H
#if defined(__KERNEL__) && !XZ_INTERNAL_CRC32
#	include <linux/crc32.h>
#	undef crc32
#	define xz_crc32(buf, size, crc) \
		(~crc32_le(~(uint32_t)(crc), buf, size))
#endif
#define STREAM_HEADER_SIZE 12
#define HEADER_MAGIC "\3757zXZ"
#define HEADER_MAGIC_SIZE 6
#define FOOTER_MAGIC "YZ"
#define FOOTER_MAGIC_SIZE 2
typedef uint64_t vli_type;
#define VLI_MAX ((vli_type)-1 / 2)
#define VLI_UNKNOWN ((vli_type)-1)
#define VLI_BYTES_MAX (sizeof(vli_type) * 8 / 7)
enum xz_check {
	XZ_CHECK_NONE = 0,
	XZ_CHECK_CRC32 = 1,
	XZ_CHECK_CRC64 = 4,
	XZ_CHECK_SHA256 = 10
};
#define XZ_CHECK_MAX 15
#endif
#ifndef XZ_LZMA2_H
#define XZ_LZMA2_H
#define RC_SHIFT_BITS 8
#define RC_TOP_BITS 24
#define RC_TOP_VALUE (1 << RC_TOP_BITS)
#define RC_BIT_MODEL_TOTAL_BITS 11
#define RC_BIT_MODEL_TOTAL (1 << RC_BIT_MODEL_TOTAL_BITS)
#define RC_MOVE_BITS 5
#define POS_STATES_MAX (1 << 4)
enum lzma_state {
	STATE_LIT_LIT,
	STATE_MATCH_LIT_LIT,
	STATE_REP_LIT_LIT,
	STATE_SHORTREP_LIT_LIT,
	STATE_MATCH_LIT,
	STATE_REP_LIT,
	STATE_SHORTREP_LIT,
	STATE_LIT_MATCH,
	STATE_LIT_LONGREP,
	STATE_LIT_SHORTREP,
	STATE_NONLIT_MATCH,
	STATE_NONLIT_REP
};
#define STATES 12
#define LIT_STATES 7
static inline void lzma_state_literal(enum lzma_state *state)
{
	if (*state <= STATE_SHORTREP_LIT_LIT)
		*state = STATE_LIT_LIT;
	else if (*state <= STATE_LIT_SHORTREP)
		*state -= 3;
	else
		*state -= 6;
}
static inline void lzma_state_match(enum lzma_state *state)
{
	*state = *state < LIT_STATES ? STATE_LIT_MATCH : STATE_NONLIT_MATCH;
}
static inline void lzma_state_long_rep(enum lzma_state *state)
{
	*state = *state < LIT_STATES ? STATE_LIT_LONGREP : STATE_NONLIT_REP;
}
static inline void lzma_state_short_rep(enum lzma_state *state)
{
	*state = *state < LIT_STATES ? STATE_LIT_SHORTREP : STATE_NONLIT_REP;
}
static inline bool lzma_state_is_literal(enum lzma_state state)
{
	return state < LIT_STATES;
}
#define LITERAL_CODER_SIZE 0x300
#define LITERAL_CODERS_MAX (1 << 4)
#define MATCH_LEN_MIN 2
#define LEN_LOW_BITS 3
#define LEN_LOW_SYMBOLS (1 << LEN_LOW_BITS)
#define LEN_MID_BITS 3
#define LEN_MID_SYMBOLS (1 << LEN_MID_BITS)
#define LEN_HIGH_BITS 8
#define LEN_HIGH_SYMBOLS (1 << LEN_HIGH_BITS)
#define LEN_SYMBOLS (LEN_LOW_SYMBOLS + LEN_MID_SYMBOLS + LEN_HIGH_SYMBOLS)
#define MATCH_LEN_MAX (MATCH_LEN_MIN + LEN_SYMBOLS - 1)
#define DIST_STATES 4
static inline uint32_t lzma_get_dist_state(uint32_t len)
{
	return len < DIST_STATES + MATCH_LEN_MIN
			? len - MATCH_LEN_MIN : DIST_STATES - 1;
}
#define DIST_SLOT_BITS 6
#define DIST_SLOTS (1 << DIST_SLOT_BITS)
#define DIST_MODEL_START 4
#define DIST_MODEL_END 14
#define FULL_DISTANCES_BITS (DIST_MODEL_END / 2)
#define FULL_DISTANCES (1 << FULL_DISTANCES_BITS)
#define ALIGN_BITS 4
#define ALIGN_SIZE (1 << ALIGN_BITS)
#define ALIGN_MASK (ALIGN_SIZE - 1)
#define PROBS_TOTAL (1846 + LITERAL_CODERS_MAX * LITERAL_CODER_SIZE)
#define REPS 4
#endif
#ifndef STATIC_RW_DATA
#	define STATIC_RW_DATA static
#endif
STATIC_RW_DATA uint32_t xz_crc32_table[256];
XZ_EXTERN void xz_crc32_init(void)
{
	const uint32_t poly = 0xEDB88320;
	uint32_t i;
	uint32_t j;
	uint32_t r;
	for (i = 0; i < 256; ++i) {
		r = i;
		for (j = 0; j < 8; ++j)
			r = (r >> 1) ^ (poly & ~((r & 1) - 1));
		xz_crc32_table[i] = r;
	}
	return;
}
XZ_EXTERN uint32_t xz_crc32(const uint8_t *buf, size_t size, uint32_t crc)
{
	crc = ~crc;
	while (size != 0) {
		crc = xz_crc32_table[*buf++ ^ (crc & 0xFF)] ^ (crc >> 8);
		--size;
	}
	return ~crc;
}
#ifndef STATIC_RW_DATA
#	define STATIC_RW_DATA static
#endif
STATIC_RW_DATA uint64_t xz_crc64_table[256];
XZ_EXTERN void xz_crc64_init(void)
{
	const uint64_t poly = 0xC96C5795D7870F42ULL;
	uint32_t i;
	uint32_t j;
	uint64_t r;
	for (i = 0; i < 256; ++i) {
		r = i;
		for (j = 0; j < 8; ++j)
			r = (r >> 1) ^ (poly & ~((r & 1) - 1));
		xz_crc64_table[i] = r;
	}
	return;
}
XZ_EXTERN uint64_t xz_crc64(const uint8_t *buf, size_t size, uint64_t crc)
{
	crc = ~crc;
	while (size != 0) {
		crc = xz_crc64_table[*buf++ ^ (crc & 0xFF)] ^ (crc >> 8);
		--size;
	}
	return ~crc;
}
static inline uint32_t
rotr_32(uint32_t num, unsigned amount)
{
	return (num >> amount) | (num << (32 - amount));
}
#define blk0(i) (W[i] = get_be32(&data[4 * i]))
#define blk2(i) (W[i & 15] += s1(W[(i - 2) & 15]) + W[(i - 7) & 15] \
		+ s0(W[(i - 15) & 15]))
#define Ch(x, y, z) (z ^ (x & (y ^ z)))
#define Maj(x, y, z) ((x & (y ^ z)) + (y & z))
#define a(i) T[(0 - i) & 7]
#define b(i) T[(1 - i) & 7]
#define c(i) T[(2 - i) & 7]
#define d(i) T[(3 - i) & 7]
#define e(i) T[(4 - i) & 7]
#define f(i) T[(5 - i) & 7]
#define g(i) T[(6 - i) & 7]
#define h(i) T[(7 - i) & 7]
#define R(i, j, blk) \
	h(i) += S1(e(i)) + Ch(e(i), f(i), g(i)) + SHA256_K[i + j] + blk; \
	d(i) += h(i); \
	h(i) += S0(a(i)) + Maj(a(i), b(i), c(i))
#define R0(i) R(i, 0, blk0(i))
#define R2(i) R(i, j, blk2(i))
#define S0(x) rotr_32(x ^ rotr_32(x ^ rotr_32(x, 9), 11), 2)
#define S1(x) rotr_32(x ^ rotr_32(x ^ rotr_32(x, 14), 5), 6)
#define s0(x) (rotr_32(x ^ rotr_32(x, 11), 7) ^ (x >> 3))
#define s1(x) (rotr_32(x ^ rotr_32(x, 2), 17) ^ (x >> 10))
static const uint32_t SHA256_K[64] = {
	0x428A2F98, 0x71374491, 0xB5C0FBCF, 0xE9B5DBA5,
	0x3956C25B, 0x59F111F1, 0x923F82A4, 0xAB1C5ED5,
	0xD807AA98, 0x12835B01, 0x243185BE, 0x550C7DC3,
	0x72BE5D74, 0x80DEB1FE, 0x9BDC06A7, 0xC19BF174,
	0xE49B69C1, 0xEFBE4786, 0x0FC19DC6, 0x240CA1CC,
	0x2DE92C6F, 0x4A7484AA, 0x5CB0A9DC, 0x76F988DA,
	0x983E5152, 0xA831C66D, 0xB00327C8, 0xBF597FC7,
	0xC6E00BF3, 0xD5A79147, 0x06CA6351, 0x14292967,
	0x27B70A85, 0x2E1B2138, 0x4D2C6DFC, 0x53380D13,
	0x650A7354, 0x766A0ABB, 0x81C2C92E, 0x92722C85,
	0xA2BFE8A1, 0xA81A664B, 0xC24B8B70, 0xC76C51A3,
	0xD192E819, 0xD6990624, 0xF40E3585, 0x106AA070,
	0x19A4C116, 0x1E376C08, 0x2748774C, 0x34B0BCB5,
	0x391C0CB3, 0x4ED8AA4A, 0x5B9CCA4F, 0x682E6FF3,
	0x748F82EE, 0x78A5636F, 0x84C87814, 0x8CC70208,
	0x90BEFFFA, 0xA4506CEB, 0xBEF9A3F7, 0xC67178F2
};
static void
transform(uint32_t state[8], const uint8_t data[64])
{
	uint32_t W[16];
	uint32_t T[8];
	unsigned int j;
	memcpy(T, state, sizeof(T));
	R0( 0); R0( 1); R0( 2); R0( 3);
	R0( 4); R0( 5); R0( 6); R0( 7);
	R0( 8); R0( 9); R0(10); R0(11);
	R0(12); R0(13); R0(14); R0(15);
	for (j = 16; j < 64; j += 16) {
		R2( 0); R2( 1); R2( 2); R2( 3);
		R2( 4); R2( 5); R2( 6); R2( 7);
		R2( 8); R2( 9); R2(10); R2(11);
		R2(12); R2(13); R2(14); R2(15);
	}
	state[0] += a(0);
	state[1] += b(0);
	state[2] += c(0);
	state[3] += d(0);
	state[4] += e(0);
	state[5] += f(0);
	state[6] += g(0);
	state[7] += h(0);
}
XZ_EXTERN void xz_sha256_reset(struct xz_sha256 *s)
{
	static const uint32_t initial_state[8] = {
		0x6A09E667, 0xBB67AE85, 0x3C6EF372, 0xA54FF53A,
		0x510E527F, 0x9B05688C, 0x1F83D9AB, 0x5BE0CD19
	};
	memcpy(s->state, initial_state, sizeof(initial_state));
	s->size = 0;
}
XZ_EXTERN void xz_sha256_update(const uint8_t *buf, size_t size,
				struct xz_sha256 *s)
{
	size_t copy_start;
	size_t copy_size;
	while (size > 0) {
		copy_start = s->size & 0x3F;
		copy_size = 64 - copy_start;
		if (copy_size > size)
			copy_size = size;
		memcpy(s->data + copy_start, buf, copy_size);
		buf += copy_size;
		size -= copy_size;
		s->size += copy_size;
		if ((s->size & 0x3F) == 0)
			transform(s->state, s->data);
	}
}
XZ_EXTERN bool xz_sha256_validate(const uint8_t *buf, struct xz_sha256 *s)
{
	size_t i = s->size & 0x3F;
	s->data[i++] = 0x80;
	while (i != 64 - 8) {
		if (i == 64) {
			transform(s->state, s->data);
			i = 0;
		}
		s->data[i++] = 0x00;
	}
	s->size *= 8;
	for (i = 0; i < 8; ++i)
		s->data[64 - 8 + i] = (uint8_t)(s->size >> ((7 - i) * 8));
	transform(s->state, s->data);
	for (i = 0; i < 8; ++i)
		if (get_unaligned_be32(buf + 4 * i) != s->state[i])
			return false;
	return true;
}
#ifdef XZ_USE_CRC64
#	define IS_CRC64(check_type) ((check_type) == XZ_CHECK_CRC64)
#else
#	define IS_CRC64(check_type) false
#endif
#ifdef XZ_USE_SHA256
#	define IS_SHA256(check_type) ((check_type) == XZ_CHECK_SHA256)
#else
#	define IS_SHA256(check_type) false
#endif
struct xz_dec_hash {
	vli_type unpadded;
	vli_type uncompressed;
	uint32_t crc32;
};
struct xz_dec {
	enum {
		SEQ_STREAM_HEADER,
		SEQ_BLOCK_START,
		SEQ_BLOCK_HEADER,
		SEQ_BLOCK_UNCOMPRESS,
		SEQ_BLOCK_PADDING,
		SEQ_BLOCK_CHECK,
		SEQ_INDEX,
		SEQ_INDEX_PADDING,
		SEQ_INDEX_CRC32,
		SEQ_STREAM_FOOTER,
		SEQ_STREAM_PADDING
	} sequence;
	uint32_t pos;
	vli_type vli;
	size_t in_start;
	size_t out_start;
#ifdef XZ_USE_CRC64
	uint64_t crc;
#else
	uint32_t crc;
#endif
	enum xz_check check_type;
	enum xz_mode mode;
	bool allow_buf_error;
	struct {
		vli_type compressed;
		vli_type uncompressed;
		uint32_t size;
	} block_header;
	struct {
		vli_type compressed;
		vli_type uncompressed;
		vli_type count;
		struct xz_dec_hash hash;
	} block;
	struct {
		enum {
			SEQ_INDEX_COUNT,
			SEQ_INDEX_UNPADDED,
			SEQ_INDEX_UNCOMPRESSED
		} sequence;
		vli_type size;
		vli_type count;
		struct xz_dec_hash hash;
	} index;
	struct {
		size_t pos;
		size_t size;
		uint8_t buf[1024];
	} temp;
	struct xz_dec_lzma2 *lzma2;
#ifdef XZ_DEC_BCJ
	struct xz_dec_bcj *bcj;
	bool bcj_active;
#endif
#ifdef XZ_USE_SHA256
	struct xz_sha256 sha256;
#endif
};
#if defined(XZ_DEC_ANY_CHECK) || defined(XZ_USE_SHA256)
static const uint8_t check_sizes[16] = {
	0,
	4, 4, 4,
	8, 8, 8,
	16, 16, 16,
	32, 32, 32,
	64, 64, 64
};
#endif
static bool fill_temp(struct xz_dec *s, struct xz_buf *b)
{
	size_t copy_size = min_t(size_t,
			b->in_size - b->in_pos, s->temp.size - s->temp.pos);
	memcpy(s->temp.buf + s->temp.pos, b->in + b->in_pos, copy_size);
	b->in_pos += copy_size;
	s->temp.pos += copy_size;
	if (s->temp.pos == s->temp.size) {
		s->temp.pos = 0;
		return true;
	}
	return false;
}
static enum xz_ret dec_vli(struct xz_dec *s, const uint8_t *in,
			   size_t *in_pos, size_t in_size)
{
	uint8_t byte;
	if (s->pos == 0)
		s->vli = 0;
	while (*in_pos < in_size) {
		byte = in[*in_pos];
		++*in_pos;
		s->vli |= (vli_type)(byte & 0x7F) << s->pos;
		if ((byte & 0x80) == 0) {
			if (byte == 0 && s->pos != 0)
				return XZ_DATA_ERROR;
			s->pos = 0;
			return XZ_STREAM_END;
		}
		s->pos += 7;
		if (s->pos == 7 * VLI_BYTES_MAX)
			return XZ_DATA_ERROR;
	}
	return XZ_OK;
}
static enum xz_ret dec_block(struct xz_dec *s, struct xz_buf *b)
{
	enum xz_ret ret;
	s->in_start = b->in_pos;
	s->out_start = b->out_pos;
#ifdef XZ_DEC_BCJ
	if (s->bcj_active)
		ret = xz_dec_bcj_run(s->bcj, s->lzma2, b);
	else
#endif
		ret = xz_dec_lzma2_run(s->lzma2, b);
	s->block.compressed += b->in_pos - s->in_start;
	s->block.uncompressed += b->out_pos - s->out_start;
	if (s->block.compressed > s->block_header.compressed
			|| s->block.uncompressed
				> s->block_header.uncompressed)
		return XZ_DATA_ERROR;
	if (s->check_type == XZ_CHECK_CRC32)
		s->crc = xz_crc32(b->out + s->out_start,
				b->out_pos - s->out_start, s->crc);
#ifdef XZ_USE_CRC64
	else if (s->check_type == XZ_CHECK_CRC64)
		s->crc = xz_crc64(b->out + s->out_start,
				b->out_pos - s->out_start, s->crc);
#endif
#ifdef XZ_USE_SHA256
	else if (s->check_type == XZ_CHECK_SHA256)
		xz_sha256_update(b->out + s->out_start,
				b->out_pos - s->out_start, &s->sha256);
#endif
	if (ret == XZ_STREAM_END) {
		if (s->block_header.compressed != VLI_UNKNOWN
				&& s->block_header.compressed
					!= s->block.compressed)
			return XZ_DATA_ERROR;
		if (s->block_header.uncompressed != VLI_UNKNOWN
				&& s->block_header.uncompressed
					!= s->block.uncompressed)
			return XZ_DATA_ERROR;
		s->block.hash.unpadded += s->block_header.size
				+ s->block.compressed;
#if defined(XZ_DEC_ANY_CHECK) || defined(XZ_USE_SHA256)
		s->block.hash.unpadded += check_sizes[s->check_type];
#else
		if (s->check_type == XZ_CHECK_CRC32)
			s->block.hash.unpadded += 4;
		else if (IS_CRC64(s->check_type))
			s->block.hash.unpadded += 8;
#endif
		s->block.hash.uncompressed += s->block.uncompressed;
		s->block.hash.crc32 = xz_crc32(
				(const uint8_t *)&s->block.hash,
				sizeof(s->block.hash), s->block.hash.crc32);
		++s->block.count;
	}
	return ret;
}
static void index_update(struct xz_dec *s, const struct xz_buf *b)
{
	size_t in_used = b->in_pos - s->in_start;
	s->index.size += in_used;
	s->crc = xz_crc32(b->in + s->in_start, in_used, s->crc);
}
static enum xz_ret dec_index(struct xz_dec *s, struct xz_buf *b)
{
	enum xz_ret ret;
	do {
		ret = dec_vli(s, b->in, &b->in_pos, b->in_size);
		if (ret != XZ_STREAM_END) {
			index_update(s, b);
			return ret;
		}
		switch (s->index.sequence) {
		case SEQ_INDEX_COUNT:
			s->index.count = s->vli;
			if (s->index.count != s->block.count)
				return XZ_DATA_ERROR;
			s->index.sequence = SEQ_INDEX_UNPADDED;
			break;
		case SEQ_INDEX_UNPADDED:
			s->index.hash.unpadded += s->vli;
			s->index.sequence = SEQ_INDEX_UNCOMPRESSED;
			break;
		case SEQ_INDEX_UNCOMPRESSED:
			s->index.hash.uncompressed += s->vli;
			s->index.hash.crc32 = xz_crc32(
					(const uint8_t *)&s->index.hash,
					sizeof(s->index.hash),
					s->index.hash.crc32);
			--s->index.count;
			s->index.sequence = SEQ_INDEX_UNPADDED;
			break;
		}
	} while (s->index.count > 0);
	return XZ_STREAM_END;
}
static enum xz_ret crc_validate(struct xz_dec *s, struct xz_buf *b,
				uint32_t bits)
{
	do {
		if (b->in_pos == b->in_size)
			return XZ_OK;
		if (((s->crc >> s->pos) & 0xFF) != b->in[b->in_pos++])
			return XZ_DATA_ERROR;
		s->pos += 8;
	} while (s->pos < bits);
	s->crc = 0;
	s->pos = 0;
	return XZ_STREAM_END;
}
#ifdef XZ_DEC_ANY_CHECK
static bool check_skip(struct xz_dec *s, struct xz_buf *b)
{
	while (s->pos < check_sizes[s->check_type]) {
		if (b->in_pos == b->in_size)
			return false;
		++b->in_pos;
		++s->pos;
	}
	s->pos = 0;
	return true;
}
#endif
static enum xz_ret dec_stream_header(struct xz_dec *s)
{
	if (!memeq(s->temp.buf, HEADER_MAGIC, HEADER_MAGIC_SIZE))
		return XZ_FORMAT_ERROR;
	if (xz_crc32(s->temp.buf + HEADER_MAGIC_SIZE, 2, 0)
			!= get_le32(s->temp.buf + HEADER_MAGIC_SIZE + 2))
		return XZ_DATA_ERROR;
	if (s->temp.buf[HEADER_MAGIC_SIZE] != 0)
		return XZ_OPTIONS_ERROR;
	if (s->temp.buf[HEADER_MAGIC_SIZE + 1] > XZ_CHECK_MAX)
		return XZ_OPTIONS_ERROR;
	s->check_type = s->temp.buf[HEADER_MAGIC_SIZE + 1];
	if (s->check_type > XZ_CHECK_CRC32 && !IS_CRC64(s->check_type)
			&& !IS_SHA256(s->check_type)) {
#ifdef XZ_DEC_ANY_CHECK
		return XZ_UNSUPPORTED_CHECK;
#else
		return XZ_OPTIONS_ERROR;
#endif
	}
	return XZ_OK;
}
static enum xz_ret dec_stream_footer(struct xz_dec *s)
{
	if (!memeq(s->temp.buf + 10, FOOTER_MAGIC, FOOTER_MAGIC_SIZE))
		return XZ_DATA_ERROR;
	if (xz_crc32(s->temp.buf + 4, 6, 0) != get_le32(s->temp.buf))
		return XZ_DATA_ERROR;
	if ((s->index.size >> 2) != get_le32(s->temp.buf + 4))
		return XZ_DATA_ERROR;
	if (s->temp.buf[8] != 0 || s->temp.buf[9] != s->check_type)
		return XZ_DATA_ERROR;
	return XZ_STREAM_END;
}
static enum xz_ret dec_block_header(struct xz_dec *s)
{
	enum xz_ret ret;
	s->temp.size -= 4;
	if (xz_crc32(s->temp.buf, s->temp.size, 0)
			!= get_le32(s->temp.buf + s->temp.size))
		return XZ_DATA_ERROR;
	s->temp.pos = 2;
#ifdef XZ_DEC_BCJ
	if (s->temp.buf[1] & 0x3E)
#else
	if (s->temp.buf[1] & 0x3F)
#endif
		return XZ_OPTIONS_ERROR;
	if (s->temp.buf[1] & 0x40) {
		if (dec_vli(s, s->temp.buf, &s->temp.pos, s->temp.size)
					!= XZ_STREAM_END)
			return XZ_DATA_ERROR;
		s->block_header.compressed = s->vli;
	} else {
		s->block_header.compressed = VLI_UNKNOWN;
	}
	if (s->temp.buf[1] & 0x80) {
		if (dec_vli(s, s->temp.buf, &s->temp.pos, s->temp.size)
				!= XZ_STREAM_END)
			return XZ_DATA_ERROR;
		s->block_header.uncompressed = s->vli;
	} else {
		s->block_header.uncompressed = VLI_UNKNOWN;
	}
#ifdef XZ_DEC_BCJ
	s->bcj_active = s->temp.buf[1] & 0x01;
	if (s->bcj_active) {
		if (s->temp.size - s->temp.pos < 2)
			return XZ_OPTIONS_ERROR;
		ret = xz_dec_bcj_reset(s->bcj, s->temp.buf[s->temp.pos++]);
		if (ret != XZ_OK)
			return ret;
		if (s->temp.buf[s->temp.pos++] != 0x00)
			return XZ_OPTIONS_ERROR;
	}
#endif
	if (s->temp.size - s->temp.pos < 2)
		return XZ_DATA_ERROR;
	if (s->temp.buf[s->temp.pos++] != 0x21)
		return XZ_OPTIONS_ERROR;
	if (s->temp.buf[s->temp.pos++] != 0x01)
		return XZ_OPTIONS_ERROR;
	if (s->temp.size - s->temp.pos < 1)
		return XZ_DATA_ERROR;
	ret = xz_dec_lzma2_reset(s->lzma2, s->temp.buf[s->temp.pos++]);
	if (ret != XZ_OK)
		return ret;
	while (s->temp.pos < s->temp.size)
		if (s->temp.buf[s->temp.pos++] != 0x00)
			return XZ_OPTIONS_ERROR;
	s->temp.pos = 0;
	s->block.compressed = 0;
	s->block.uncompressed = 0;
	return XZ_OK;
}
static enum xz_ret dec_main(struct xz_dec *s, struct xz_buf *b)
{
	enum xz_ret ret;
	s->in_start = b->in_pos;
	while (true) {
		switch (s->sequence) {
		case SEQ_STREAM_HEADER:
			if (!fill_temp(s, b))
				return XZ_OK;
			s->sequence = SEQ_BLOCK_START;
			ret = dec_stream_header(s);
			if (ret != XZ_OK)
				return ret;
			fallthrough;
		case SEQ_BLOCK_START:
			if (b->in_pos == b->in_size)
				return XZ_OK;
			if (b->in[b->in_pos] == 0) {
				s->in_start = b->in_pos++;
				s->sequence = SEQ_INDEX;
				break;
			}
			s->block_header.size
				= ((uint32_t)b->in[b->in_pos] + 1) * 4;
			s->temp.size = s->block_header.size;
			s->temp.pos = 0;
			s->sequence = SEQ_BLOCK_HEADER;
			fallthrough;
		case SEQ_BLOCK_HEADER:
			if (!fill_temp(s, b))
				return XZ_OK;
			ret = dec_block_header(s);
			if (ret != XZ_OK)
				return ret;
#ifdef XZ_USE_SHA256
			if (s->check_type == XZ_CHECK_SHA256)
				xz_sha256_reset(&s->sha256);
#endif
			s->sequence = SEQ_BLOCK_UNCOMPRESS;
			fallthrough;
		case SEQ_BLOCK_UNCOMPRESS:
			ret = dec_block(s, b);
			if (ret != XZ_STREAM_END)
				return ret;
			s->sequence = SEQ_BLOCK_PADDING;
			fallthrough;
		case SEQ_BLOCK_PADDING:
			while (s->block.compressed & 3) {
				if (b->in_pos == b->in_size)
					return XZ_OK;
				if (b->in[b->in_pos++] != 0)
					return XZ_DATA_ERROR;
				++s->block.compressed;
			}
			s->sequence = SEQ_BLOCK_CHECK;
			fallthrough;
		case SEQ_BLOCK_CHECK:
			if (s->check_type == XZ_CHECK_CRC32) {
				ret = crc_validate(s, b, 32);
				if (ret != XZ_STREAM_END)
					return ret;
			}
			else if (IS_CRC64(s->check_type)) {
				ret = crc_validate(s, b, 64);
				if (ret != XZ_STREAM_END)
					return ret;
			}
#ifdef XZ_USE_SHA256
			else if (s->check_type == XZ_CHECK_SHA256) {
				s->temp.size = 32;
				if (!fill_temp(s, b))
					return XZ_OK;
				if (!xz_sha256_validate(s->temp.buf,
							&s->sha256))
					return XZ_DATA_ERROR;
				s->pos = 0;
			}
#endif
#ifdef XZ_DEC_ANY_CHECK
			else if (!check_skip(s, b)) {
				return XZ_OK;
			}
#endif
			s->sequence = SEQ_BLOCK_START;
			break;
		case SEQ_INDEX:
			ret = dec_index(s, b);
			if (ret != XZ_STREAM_END)
				return ret;
			s->sequence = SEQ_INDEX_PADDING;
			fallthrough;
		case SEQ_INDEX_PADDING:
			while ((s->index.size + (b->in_pos - s->in_start))
					& 3) {
				if (b->in_pos == b->in_size) {
					index_update(s, b);
					return XZ_OK;
				}
				if (b->in[b->in_pos++] != 0)
					return XZ_DATA_ERROR;
			}
			index_update(s, b);
			if (!memeq(&s->block.hash, &s->index.hash,
					sizeof(s->block.hash)))
				return XZ_DATA_ERROR;
			s->sequence = SEQ_INDEX_CRC32;
			fallthrough;
		case SEQ_INDEX_CRC32:
			ret = crc_validate(s, b, 32);
			if (ret != XZ_STREAM_END)
				return ret;
			s->temp.size = STREAM_HEADER_SIZE;
			s->sequence = SEQ_STREAM_FOOTER;
			fallthrough;
		case SEQ_STREAM_FOOTER:
			if (!fill_temp(s, b))
				return XZ_OK;
			return dec_stream_footer(s);
		case SEQ_STREAM_PADDING:
			break;
		}
	}
}
XZ_EXTERN enum xz_ret xz_dec_run(struct xz_dec *s, struct xz_buf *b)
{
	size_t in_start;
	size_t out_start;
	enum xz_ret ret;
	if (DEC_IS_SINGLE(s->mode))
		xz_dec_reset(s);
	in_start = b->in_pos;
	out_start = b->out_pos;
	ret = dec_main(s, b);
	if (DEC_IS_SINGLE(s->mode)) {
		if (ret == XZ_OK)
			ret = b->in_pos == b->in_size
					? XZ_DATA_ERROR : XZ_BUF_ERROR;
		if (ret != XZ_STREAM_END) {
			b->in_pos = in_start;
			b->out_pos = out_start;
		}
	} else if (ret == XZ_OK && in_start == b->in_pos
			&& out_start == b->out_pos) {
		if (s->allow_buf_error)
			ret = XZ_BUF_ERROR;
		s->allow_buf_error = true;
	} else {
		s->allow_buf_error = false;
	}
	return ret;
}
#ifdef XZ_DEC_CONCATENATED
XZ_EXTERN enum xz_ret xz_dec_catrun(struct xz_dec *s, struct xz_buf *b,
				    int finish)
{
	enum xz_ret ret;
	if (DEC_IS_SINGLE(s->mode)) {
		xz_dec_reset(s);
		finish = true;
	}
	while (true) {
		if (s->sequence == SEQ_STREAM_PADDING) {
			while (true) {
				if (b->in_pos == b->in_size) {
					if (!finish)
						return XZ_OK;
					if (s->pos != 0)
						return XZ_DATA_ERROR;
					return XZ_STREAM_END;
				}
				if (b->in[b->in_pos] != 0x00) {
					if (s->pos != 0)
						return XZ_DATA_ERROR;
					break;
				}
				++b->in_pos;
				s->pos = (s->pos + 1) & 3;
			}
			if (DEC_IS_MULTI(s->mode))
				xz_dec_reset(s);
		}
		ret = xz_dec_run(s, b);
		if (ret != XZ_STREAM_END)
			break;
		s->sequence = SEQ_STREAM_PADDING;
	}
	return ret;
}
#endif
XZ_EXTERN struct xz_dec *xz_dec_init(enum xz_mode mode, uint32_t dict_max)
{
	struct xz_dec *s = kmalloc(sizeof(*s), GFP_KERNEL);
	if (s == NULL)
		return NULL;
	s->mode = mode;
#ifdef XZ_DEC_BCJ
	s->bcj = xz_dec_bcj_create(DEC_IS_SINGLE(mode));
	if (s->bcj == NULL)
		goto error_bcj;
#endif
	s->lzma2 = xz_dec_lzma2_create(mode, dict_max);
	if (s->lzma2 == NULL)
		goto error_lzma2;
	xz_dec_reset(s);
	return s;
error_lzma2:
#ifdef XZ_DEC_BCJ
	xz_dec_bcj_end(s->bcj);
error_bcj:
#endif
	kfree(s);
	return NULL;
}
XZ_EXTERN void xz_dec_reset(struct xz_dec *s)
{
	s->sequence = SEQ_STREAM_HEADER;
	s->allow_buf_error = false;
	s->pos = 0;
	s->crc = 0;
	memzero(&s->block, sizeof(s->block));
	memzero(&s->index, sizeof(s->index));
	s->temp.pos = 0;
	s->temp.size = STREAM_HEADER_SIZE;
}
XZ_EXTERN void xz_dec_end(struct xz_dec *s)
{
	if (s != NULL) {
		xz_dec_lzma2_end(s->lzma2);
#ifdef XZ_DEC_BCJ
		xz_dec_bcj_end(s->bcj);
#endif
		kfree(s);
	}
}
#define RC_INIT_BYTES 5
#define LZMA_IN_REQUIRED 21
struct dictionary {
	uint8_t *buf;
	size_t start;
	size_t pos;
	size_t full;
	size_t limit;
	size_t end;
	uint32_t size;
	uint32_t size_max;
	uint32_t allocated;
	enum xz_mode mode;
};
struct rc_dec {
	uint32_t range;
	uint32_t code;
	uint32_t init_bytes_left;
	const uint8_t *in;
	size_t in_pos;
	size_t in_limit;
};
struct lzma_len_dec {
	uint16_t choice;
	uint16_t choice2;
	uint16_t low[POS_STATES_MAX][LEN_LOW_SYMBOLS];
	uint16_t mid[POS_STATES_MAX][LEN_MID_SYMBOLS];
	uint16_t high[LEN_HIGH_SYMBOLS];
};
struct lzma_dec {
	uint32_t rep0;
	uint32_t rep1;
	uint32_t rep2;
	uint32_t rep3;
	enum lzma_state state;
	uint32_t len;
	uint32_t lc;
	uint32_t literal_pos_mask; 
	uint32_t pos_mask;         
	uint16_t is_match[STATES][POS_STATES_MAX];
	uint16_t is_rep[STATES];
	uint16_t is_rep0[STATES];
	uint16_t is_rep1[STATES];
	uint16_t is_rep2[STATES];
	uint16_t is_rep0_long[STATES][POS_STATES_MAX];
	uint16_t dist_slot[DIST_STATES][DIST_SLOTS];
	uint16_t dist_special[FULL_DISTANCES - DIST_MODEL_END];
	uint16_t dist_align[ALIGN_SIZE];
	struct lzma_len_dec match_len_dec;
	struct lzma_len_dec rep_len_dec;
	uint16_t literal[LITERAL_CODERS_MAX][LITERAL_CODER_SIZE];
};
struct lzma2_dec {
	enum lzma2_seq {
		SEQ_CONTROL,
		SEQ_UNCOMPRESSED_1,
		SEQ_UNCOMPRESSED_2,
		SEQ_COMPRESSED_0,
		SEQ_COMPRESSED_1,
		SEQ_PROPERTIES,
		SEQ_LZMA_PREPARE,
		SEQ_LZMA_RUN,
		SEQ_COPY
	} sequence;
	enum lzma2_seq next_sequence;
	uint32_t uncompressed;
	uint32_t compressed;
	bool need_dict_reset;
	bool need_props;
#ifdef XZ_DEC_MICROLZMA
	bool pedantic_microlzma;
#endif
};
struct xz_dec_lzma2 {
	struct rc_dec rc;
	struct dictionary dict;
	struct lzma2_dec lzma2;
	struct lzma_dec lzma;
	struct {
		uint32_t size;
		uint8_t buf[3 * LZMA_IN_REQUIRED];
	} temp;
};
static void dict_reset(struct dictionary *dict, struct xz_buf *b)
{
	if (DEC_IS_SINGLE(dict->mode)) {
		dict->buf = b->out + b->out_pos;
		dict->end = b->out_size - b->out_pos;
	}
	dict->start = 0;
	dict->pos = 0;
	dict->limit = 0;
	dict->full = 0;
}
static void dict_limit(struct dictionary *dict, size_t out_max)
{
	if (dict->end - dict->pos <= out_max)
		dict->limit = dict->end;
	else
		dict->limit = dict->pos + out_max;
}
static inline bool dict_has_space(const struct dictionary *dict)
{
	return dict->pos < dict->limit;
}
static inline uint32_t dict_get(const struct dictionary *dict, uint32_t dist)
{
	size_t offset = dict->pos - dist - 1;
	if (dist >= dict->pos)
		offset += dict->end;
	return dict->full > 0 ? dict->buf[offset] : 0;
}
static inline void dict_put(struct dictionary *dict, uint8_t byte)
{
	dict->buf[dict->pos++] = byte;
	if (dict->full < dict->pos)
		dict->full = dict->pos;
}
static bool dict_repeat(struct dictionary *dict, uint32_t *len, uint32_t dist)
{
	size_t back;
	uint32_t left;
	if (dist >= dict->full || dist >= dict->size)
		return false;
	left = min_t(size_t, dict->limit - dict->pos, *len);
	*len -= left;
	back = dict->pos - dist - 1;
	if (dist >= dict->pos)
		back += dict->end;
	do {
		dict->buf[dict->pos++] = dict->buf[back++];
		if (back == dict->end)
			back = 0;
	} while (--left > 0);
	if (dict->full < dict->pos)
		dict->full = dict->pos;
	return true;
}
static void dict_uncompressed(struct dictionary *dict, struct xz_buf *b,
			      uint32_t *left)
{
	size_t copy_size;
	while (*left > 0 && b->in_pos < b->in_size
			&& b->out_pos < b->out_size) {
		copy_size = min(b->in_size - b->in_pos,
				b->out_size - b->out_pos);
		if (copy_size > dict->end - dict->pos)
			copy_size = dict->end - dict->pos;
		if (copy_size > *left)
			copy_size = *left;
		*left -= copy_size;
		memmove(dict->buf + dict->pos, b->in + b->in_pos, copy_size);
		dict->pos += copy_size;
		if (dict->full < dict->pos)
			dict->full = dict->pos;
		if (DEC_IS_MULTI(dict->mode)) {
			if (dict->pos == dict->end)
				dict->pos = 0;
			memmove(b->out + b->out_pos, b->in + b->in_pos,
					copy_size);
		}
		dict->start = dict->pos;
		b->out_pos += copy_size;
		b->in_pos += copy_size;
	}
}
#ifdef XZ_DEC_MICROLZMA
#	define DICT_FLUSH_SUPPORTS_SKIPPING true
#else
#	define DICT_FLUSH_SUPPORTS_SKIPPING false
#endif
static uint32_t dict_flush(struct dictionary *dict, struct xz_buf *b)
{
	size_t copy_size = dict->pos - dict->start;
	if (DEC_IS_MULTI(dict->mode)) {
		if (dict->pos == dict->end)
			dict->pos = 0;
		if (!DICT_FLUSH_SUPPORTS_SKIPPING || b->out != NULL)
			memcpy(b->out + b->out_pos, dict->buf + dict->start,
					copy_size);
	}
	dict->start = dict->pos;
	b->out_pos += copy_size;
	return copy_size;
}
static void rc_reset(struct rc_dec *rc)
{
	rc->range = (uint32_t)-1;
	rc->code = 0;
	rc->init_bytes_left = RC_INIT_BYTES;
}
static bool rc_read_init(struct rc_dec *rc, struct xz_buf *b)
{
	while (rc->init_bytes_left > 0) {
		if (b->in_pos == b->in_size)
			return false;
		rc->code = (rc->code << 8) + b->in[b->in_pos++];
		--rc->init_bytes_left;
	}
	return true;
}
static inline bool rc_limit_exceeded(const struct rc_dec *rc)
{
	return rc->in_pos > rc->in_limit;
}
static inline bool rc_is_finished(const struct rc_dec *rc)
{
	return rc->code == 0;
}
static __always_inline void rc_normalize(struct rc_dec *rc)
{
	if (rc->range < RC_TOP_VALUE) {
		rc->range <<= RC_SHIFT_BITS;
		rc->code = (rc->code << RC_SHIFT_BITS) + rc->in[rc->in_pos++];
	}
}
static __always_inline int rc_bit(struct rc_dec *rc, uint16_t *prob)
{
	uint32_t bound;
	int bit;
	rc_normalize(rc);
	bound = (rc->range >> RC_BIT_MODEL_TOTAL_BITS) * *prob;
	if (rc->code < bound) {
		rc->range = bound;
		*prob += (RC_BIT_MODEL_TOTAL - *prob) >> RC_MOVE_BITS;
		bit = 0;
	} else {
		rc->range -= bound;
		rc->code -= bound;
		*prob -= *prob >> RC_MOVE_BITS;
		bit = 1;
	}
	return bit;
}
static __always_inline uint32_t rc_bittree(struct rc_dec *rc,
					   uint16_t *probs, uint32_t limit)
{
	uint32_t symbol = 1;
	do {
		if (rc_bit(rc, &probs[symbol]))
			symbol = (symbol << 1) + 1;
		else
			symbol <<= 1;
	} while (symbol < limit);
	return symbol;
}
static __always_inline void rc_bittree_reverse(struct rc_dec *rc,
					       uint16_t *probs,
					       uint32_t *dest, uint32_t limit)
{
	uint32_t symbol = 1;
	uint32_t i = 0;
	do {
		if (rc_bit(rc, &probs[symbol])) {
			symbol = (symbol << 1) + 1;
			*dest += 1 << i;
		} else {
			symbol <<= 1;
		}
	} while (++i < limit);
}
static inline void rc_direct(struct rc_dec *rc, uint32_t *dest, uint32_t limit)
{
	uint32_t mask;
	do {
		rc_normalize(rc);
		rc->range >>= 1;
		rc->code -= rc->range;
		mask = (uint32_t)0 - (rc->code >> 31);
		rc->code += rc->range & mask;
		*dest = (*dest << 1) + (mask + 1);
	} while (--limit > 0);
}
static uint16_t *lzma_literal_probs(struct xz_dec_lzma2 *s)
{
	uint32_t prev_byte = dict_get(&s->dict, 0);
	uint32_t low = prev_byte >> (8 - s->lzma.lc);
	uint32_t high = (s->dict.pos & s->lzma.literal_pos_mask) << s->lzma.lc;
	return s->lzma.literal[low + high];
}
static void lzma_literal(struct xz_dec_lzma2 *s)
{
	uint16_t *probs;
	uint32_t symbol;
	uint32_t match_byte;
	uint32_t match_bit;
	uint32_t offset;
	uint32_t i;
	probs = lzma_literal_probs(s);
	if (lzma_state_is_literal(s->lzma.state)) {
		symbol = rc_bittree(&s->rc, probs, 0x100);
	} else {
		symbol = 1;
		match_byte = dict_get(&s->dict, s->lzma.rep0) << 1;
		offset = 0x100;
		do {
			match_bit = match_byte & offset;
			match_byte <<= 1;
			i = offset + match_bit + symbol;
			if (rc_bit(&s->rc, &probs[i])) {
				symbol = (symbol << 1) + 1;
				offset &= match_bit;
			} else {
				symbol <<= 1;
				offset &= ~match_bit;
			}
		} while (symbol < 0x100);
	}
	dict_put(&s->dict, (uint8_t)symbol);
	lzma_state_literal(&s->lzma.state);
}
static void lzma_len(struct xz_dec_lzma2 *s, struct lzma_len_dec *l,
		     uint32_t pos_state)
{
	uint16_t *probs;
	uint32_t limit;
	if (!rc_bit(&s->rc, &l->choice)) {
		probs = l->low[pos_state];
		limit = LEN_LOW_SYMBOLS;
		s->lzma.len = MATCH_LEN_MIN;
	} else {
		if (!rc_bit(&s->rc, &l->choice2)) {
			probs = l->mid[pos_state];
			limit = LEN_MID_SYMBOLS;
			s->lzma.len = MATCH_LEN_MIN + LEN_LOW_SYMBOLS;
		} else {
			probs = l->high;
			limit = LEN_HIGH_SYMBOLS;
			s->lzma.len = MATCH_LEN_MIN + LEN_LOW_SYMBOLS
					+ LEN_MID_SYMBOLS;
		}
	}
	s->lzma.len += rc_bittree(&s->rc, probs, limit) - limit;
}
static void lzma_match(struct xz_dec_lzma2 *s, uint32_t pos_state)
{
	uint16_t *probs;
	uint32_t dist_slot;
	uint32_t limit;
	lzma_state_match(&s->lzma.state);
	s->lzma.rep3 = s->lzma.rep2;
	s->lzma.rep2 = s->lzma.rep1;
	s->lzma.rep1 = s->lzma.rep0;
	lzma_len(s, &s->lzma.match_len_dec, pos_state);
	probs = s->lzma.dist_slot[lzma_get_dist_state(s->lzma.len)];
	dist_slot = rc_bittree(&s->rc, probs, DIST_SLOTS) - DIST_SLOTS;
	if (dist_slot < DIST_MODEL_START) {
		s->lzma.rep0 = dist_slot;
	} else {
		limit = (dist_slot >> 1) - 1;
		s->lzma.rep0 = 2 + (dist_slot & 1);
		if (dist_slot < DIST_MODEL_END) {
			s->lzma.rep0 <<= limit;
			probs = s->lzma.dist_special + s->lzma.rep0
					- dist_slot - 1;
			rc_bittree_reverse(&s->rc, probs,
					&s->lzma.rep0, limit);
		} else {
			rc_direct(&s->rc, &s->lzma.rep0, limit - ALIGN_BITS);
			s->lzma.rep0 <<= ALIGN_BITS;
			rc_bittree_reverse(&s->rc, s->lzma.dist_align,
					&s->lzma.rep0, ALIGN_BITS);
		}
	}
}
static void lzma_rep_match(struct xz_dec_lzma2 *s, uint32_t pos_state)
{
	uint32_t tmp;
	if (!rc_bit(&s->rc, &s->lzma.is_rep0[s->lzma.state])) {
		if (!rc_bit(&s->rc, &s->lzma.is_rep0_long[
				s->lzma.state][pos_state])) {
			lzma_state_short_rep(&s->lzma.state);
			s->lzma.len = 1;
			return;
		}
	} else {
		if (!rc_bit(&s->rc, &s->lzma.is_rep1[s->lzma.state])) {
			tmp = s->lzma.rep1;
		} else {
			if (!rc_bit(&s->rc, &s->lzma.is_rep2[s->lzma.state])) {
				tmp = s->lzma.rep2;
			} else {
				tmp = s->lzma.rep3;
				s->lzma.rep3 = s->lzma.rep2;
			}
			s->lzma.rep2 = s->lzma.rep1;
		}
		s->lzma.rep1 = s->lzma.rep0;
		s->lzma.rep0 = tmp;
	}
	lzma_state_long_rep(&s->lzma.state);
	lzma_len(s, &s->lzma.rep_len_dec, pos_state);
}
static bool lzma_main(struct xz_dec_lzma2 *s)
{
	uint32_t pos_state;
	if (dict_has_space(&s->dict) && s->lzma.len > 0)
		dict_repeat(&s->dict, &s->lzma.len, s->lzma.rep0);
	while (dict_has_space(&s->dict) && !rc_limit_exceeded(&s->rc)) {
		pos_state = s->dict.pos & s->lzma.pos_mask;
		if (!rc_bit(&s->rc, &s->lzma.is_match[
				s->lzma.state][pos_state])) {
			lzma_literal(s);
		} else {
			if (rc_bit(&s->rc, &s->lzma.is_rep[s->lzma.state]))
				lzma_rep_match(s, pos_state);
			else
				lzma_match(s, pos_state);
			if (!dict_repeat(&s->dict, &s->lzma.len, s->lzma.rep0))
				return false;
		}
	}
	rc_normalize(&s->rc);
	return true;
}
static void lzma_reset(struct xz_dec_lzma2 *s)
{
	uint16_t *probs;
	size_t i;
	s->lzma.state = STATE_LIT_LIT;
	s->lzma.rep0 = 0;
	s->lzma.rep1 = 0;
	s->lzma.rep2 = 0;
	s->lzma.rep3 = 0;
	s->lzma.len = 0;
	probs = s->lzma.is_match[0];
	for (i = 0; i < PROBS_TOTAL; ++i)
		probs[i] = RC_BIT_MODEL_TOTAL / 2;
	rc_reset(&s->rc);
}
static bool lzma_props(struct xz_dec_lzma2 *s, uint8_t props)
{
	if (props > (4 * 5 + 4) * 9 + 8)
		return false;
	s->lzma.pos_mask = 0;
	while (props >= 9 * 5) {
		props -= 9 * 5;
		++s->lzma.pos_mask;
	}
	s->lzma.pos_mask = (1 << s->lzma.pos_mask) - 1;
	s->lzma.literal_pos_mask = 0;
	while (props >= 9) {
		props -= 9;
		++s->lzma.literal_pos_mask;
	}
	s->lzma.lc = props;
	if (s->lzma.lc + s->lzma.literal_pos_mask > 4)
		return false;
	s->lzma.literal_pos_mask = (1 << s->lzma.literal_pos_mask) - 1;
	lzma_reset(s);
	return true;
}
static bool lzma2_lzma(struct xz_dec_lzma2 *s, struct xz_buf *b)
{
	size_t in_avail;
	uint32_t tmp;
	in_avail = b->in_size - b->in_pos;
	if (s->temp.size > 0 || s->lzma2.compressed == 0) {
		tmp = 2 * LZMA_IN_REQUIRED - s->temp.size;
		if (tmp > s->lzma2.compressed - s->temp.size)
			tmp = s->lzma2.compressed - s->temp.size;
		if (tmp > in_avail)
			tmp = in_avail;
		memcpy(s->temp.buf + s->temp.size, b->in + b->in_pos, tmp);
		if (s->temp.size + tmp == s->lzma2.compressed) {
			memzero(s->temp.buf + s->temp.size + tmp,
					sizeof(s->temp.buf)
						- s->temp.size - tmp);
			s->rc.in_limit = s->temp.size + tmp;
		} else if (s->temp.size + tmp < LZMA_IN_REQUIRED) {
			s->temp.size += tmp;
			b->in_pos += tmp;
			return true;
		} else {
			s->rc.in_limit = s->temp.size + tmp - LZMA_IN_REQUIRED;
		}
		s->rc.in = s->temp.buf;
		s->rc.in_pos = 0;
		if (!lzma_main(s) || s->rc.in_pos > s->temp.size + tmp)
			return false;
		s->lzma2.compressed -= s->rc.in_pos;
		if (s->rc.in_pos < s->temp.size) {
			s->temp.size -= s->rc.in_pos;
			memmove(s->temp.buf, s->temp.buf + s->rc.in_pos,
					s->temp.size);
			return true;
		}
		b->in_pos += s->rc.in_pos - s->temp.size;
		s->temp.size = 0;
	}
	in_avail = b->in_size - b->in_pos;
	if (in_avail >= LZMA_IN_REQUIRED) {
		s->rc.in = b->in;
		s->rc.in_pos = b->in_pos;
		if (in_avail >= s->lzma2.compressed + LZMA_IN_REQUIRED)
			s->rc.in_limit = b->in_pos + s->lzma2.compressed;
		else
			s->rc.in_limit = b->in_size - LZMA_IN_REQUIRED;
		if (!lzma_main(s))
			return false;
		in_avail = s->rc.in_pos - b->in_pos;
		if (in_avail > s->lzma2.compressed)
			return false;
		s->lzma2.compressed -= in_avail;
		b->in_pos = s->rc.in_pos;
	}
	in_avail = b->in_size - b->in_pos;
	if (in_avail < LZMA_IN_REQUIRED) {
		if (in_avail > s->lzma2.compressed)
			in_avail = s->lzma2.compressed;
		memcpy(s->temp.buf, b->in + b->in_pos, in_avail);
		s->temp.size = in_avail;
		b->in_pos += in_avail;
	}
	return true;
}
XZ_EXTERN enum xz_ret xz_dec_lzma2_run(struct xz_dec_lzma2 *s,
				       struct xz_buf *b)
{
	uint32_t tmp;
	while (b->in_pos < b->in_size || s->lzma2.sequence == SEQ_LZMA_RUN) {
		switch (s->lzma2.sequence) {
		case SEQ_CONTROL:
			tmp = b->in[b->in_pos++];
			if (tmp == 0x00)
				return XZ_STREAM_END;
			if (tmp >= 0xE0 || tmp == 0x01) {
				s->lzma2.need_props = true;
				s->lzma2.need_dict_reset = false;
				dict_reset(&s->dict, b);
			} else if (s->lzma2.need_dict_reset) {
				return XZ_DATA_ERROR;
			}
			if (tmp >= 0x80) {
				s->lzma2.uncompressed = (tmp & 0x1F) << 16;
				s->lzma2.sequence = SEQ_UNCOMPRESSED_1;
				if (tmp >= 0xC0) {
					s->lzma2.need_props = false;
					s->lzma2.next_sequence
							= SEQ_PROPERTIES;
				} else if (s->lzma2.need_props) {
					return XZ_DATA_ERROR;
				} else {
					s->lzma2.next_sequence
							= SEQ_LZMA_PREPARE;
					if (tmp >= 0xA0)
						lzma_reset(s);
				}
			} else {
				if (tmp > 0x02)
					return XZ_DATA_ERROR;
				s->lzma2.sequence = SEQ_COMPRESSED_0;
				s->lzma2.next_sequence = SEQ_COPY;
			}
			break;
		case SEQ_UNCOMPRESSED_1:
			s->lzma2.uncompressed
					+= (uint32_t)b->in[b->in_pos++] << 8;
			s->lzma2.sequence = SEQ_UNCOMPRESSED_2;
			break;
		case SEQ_UNCOMPRESSED_2:
			s->lzma2.uncompressed
					+= (uint32_t)b->in[b->in_pos++] + 1;
			s->lzma2.sequence = SEQ_COMPRESSED_0;
			break;
		case SEQ_COMPRESSED_0:
			s->lzma2.compressed
					= (uint32_t)b->in[b->in_pos++] << 8;
			s->lzma2.sequence = SEQ_COMPRESSED_1;
			break;
		case SEQ_COMPRESSED_1:
			s->lzma2.compressed
					+= (uint32_t)b->in[b->in_pos++] + 1;
			s->lzma2.sequence = s->lzma2.next_sequence;
			break;
		case SEQ_PROPERTIES:
			if (!lzma_props(s, b->in[b->in_pos++]))
				return XZ_DATA_ERROR;
			s->lzma2.sequence = SEQ_LZMA_PREPARE;
			fallthrough;
		case SEQ_LZMA_PREPARE:
			if (s->lzma2.compressed < RC_INIT_BYTES)
				return XZ_DATA_ERROR;
			if (!rc_read_init(&s->rc, b))
				return XZ_OK;
			s->lzma2.compressed -= RC_INIT_BYTES;
			s->lzma2.sequence = SEQ_LZMA_RUN;
			fallthrough;
		case SEQ_LZMA_RUN:
			dict_limit(&s->dict, min_t(size_t,
					b->out_size - b->out_pos,
					s->lzma2.uncompressed));
			if (!lzma2_lzma(s, b))
				return XZ_DATA_ERROR;
			s->lzma2.uncompressed -= dict_flush(&s->dict, b);
			if (s->lzma2.uncompressed == 0) {
				if (s->lzma2.compressed > 0 || s->lzma.len > 0
						|| !rc_is_finished(&s->rc))
					return XZ_DATA_ERROR;
				rc_reset(&s->rc);
				s->lzma2.sequence = SEQ_CONTROL;
			} else if (b->out_pos == b->out_size
					|| (b->in_pos == b->in_size
						&& s->temp.size
						< s->lzma2.compressed)) {
				return XZ_OK;
			}
			break;
		case SEQ_COPY:
			dict_uncompressed(&s->dict, b, &s->lzma2.compressed);
			if (s->lzma2.compressed > 0)
				return XZ_OK;
			s->lzma2.sequence = SEQ_CONTROL;
			break;
		}
	}
	return XZ_OK;
}
XZ_EXTERN struct xz_dec_lzma2 *xz_dec_lzma2_create(enum xz_mode mode,
						   uint32_t dict_max)
{
	struct xz_dec_lzma2 *s = kmalloc(sizeof(*s), GFP_KERNEL);
	if (s == NULL)
		return NULL;
	s->dict.mode = mode;
	s->dict.size_max = dict_max;
	if (DEC_IS_PREALLOC(mode)) {
		s->dict.buf = vmalloc(dict_max);
		if (s->dict.buf == NULL) {
			kfree(s);
			return NULL;
		}
	} else if (DEC_IS_DYNALLOC(mode)) {
		s->dict.buf = NULL;
		s->dict.allocated = 0;
	}
	return s;
}
XZ_EXTERN enum xz_ret xz_dec_lzma2_reset(struct xz_dec_lzma2 *s, uint8_t props)
{
	if (props > 39)
		return XZ_OPTIONS_ERROR;
	s->dict.size = 2 + (props & 1);
	s->dict.size <<= (props >> 1) + 11;
	if (DEC_IS_MULTI(s->dict.mode)) {
		if (s->dict.size > s->dict.size_max)
			return XZ_MEMLIMIT_ERROR;
		s->dict.end = s->dict.size;
		if (DEC_IS_DYNALLOC(s->dict.mode)) {
			if (s->dict.allocated < s->dict.size) {
				s->dict.allocated = s->dict.size;
				vfree(s->dict.buf);
				s->dict.buf = vmalloc(s->dict.size);
				if (s->dict.buf == NULL) {
					s->dict.allocated = 0;
					return XZ_MEM_ERROR;
				}
			}
		}
	}
	s->lzma2.sequence = SEQ_CONTROL;
	s->lzma2.need_dict_reset = true;
	s->temp.size = 0;
	return XZ_OK;
}
XZ_EXTERN void xz_dec_lzma2_end(struct xz_dec_lzma2 *s)
{
	if (DEC_IS_MULTI(s->dict.mode))
		vfree(s->dict.buf);
	kfree(s);
}
#ifdef XZ_DEC_MICROLZMA
struct xz_dec_microlzma {
	struct xz_dec_lzma2 s;
};
XZ_EXTERN enum xz_ret xz_dec_microlzma_run(struct xz_dec_microlzma *s_ptr,
					   struct xz_buf *b)
{
	struct xz_dec_lzma2 *s = &s_ptr->s;
	if (s->lzma2.sequence != SEQ_LZMA_RUN) {
		if (s->lzma2.sequence == SEQ_PROPERTIES) {
			if (b->in_pos >= b->in_size)
				return XZ_OK;
			if (!lzma_props(s, ~b->in[b->in_pos]))
				return XZ_DATA_ERROR;
			s->lzma2.sequence = SEQ_LZMA_PREPARE;
		}
		if (s->lzma2.compressed < RC_INIT_BYTES
				|| s->lzma2.compressed > (3U << 30))
			return XZ_DATA_ERROR;
		if (!rc_read_init(&s->rc, b))
			return XZ_OK;
		s->lzma2.compressed -= RC_INIT_BYTES;
		s->lzma2.sequence = SEQ_LZMA_RUN;
		dict_reset(&s->dict, b);
	}
	if (DEC_IS_SINGLE(s->dict.mode))
		s->dict.end = b->out_size - b->out_pos;
	while (true) {
		dict_limit(&s->dict, min_t(size_t, b->out_size - b->out_pos,
					   s->lzma2.uncompressed));
		if (!lzma2_lzma(s, b))
			return XZ_DATA_ERROR;
		s->lzma2.uncompressed -= dict_flush(&s->dict, b);
		if (s->lzma2.uncompressed == 0) {
			if (s->lzma2.pedantic_microlzma) {
				if (s->lzma2.compressed > 0 || s->lzma.len > 0
						|| !rc_is_finished(&s->rc))
					return XZ_DATA_ERROR;
			}
			return XZ_STREAM_END;
		}
		if (b->out_pos == b->out_size)
			return XZ_OK;
		if (b->in_pos == b->in_size
				&& s->temp.size < s->lzma2.compressed)
			return XZ_OK;
	}
}
XZ_EXTERN struct xz_dec_microlzma *xz_dec_microlzma_alloc(enum xz_mode mode,
							  uint32_t dict_size)
{
	struct xz_dec_microlzma *s;
	if (dict_size < 4096 || dict_size > (3U << 30))
		return NULL;
	s = kmalloc(sizeof(*s), GFP_KERNEL);
	if (s == NULL)
		return NULL;
	s->s.dict.mode = mode;
	s->s.dict.size = dict_size;
	if (DEC_IS_MULTI(mode)) {
		s->s.dict.end = dict_size;
		s->s.dict.buf = vmalloc(dict_size);
		if (s->s.dict.buf == NULL) {
			kfree(s);
			return NULL;
		}
	}
	return s;
}
XZ_EXTERN void xz_dec_microlzma_reset(struct xz_dec_microlzma *s,
				      uint32_t comp_size,
				      uint32_t uncomp_size,
				      int uncomp_size_is_exact)
{
	s->s.lzma2.compressed = comp_size;
	s->s.lzma2.uncompressed = uncomp_size;
	s->s.lzma2.pedantic_microlzma = uncomp_size_is_exact;
	s->s.lzma2.sequence = SEQ_PROPERTIES;
	s->s.temp.size = 0;
}
XZ_EXTERN void xz_dec_microlzma_end(struct xz_dec_microlzma *s)
{
	if (DEC_IS_MULTI(s->s.dict.mode))
		vfree(s->s.dict.buf);
	kfree(s);
}
#endif
#ifdef XZ_DEC_BCJ
struct xz_dec_bcj {
	enum {
		BCJ_X86 = 4,        
		BCJ_POWERPC = 5,    
		BCJ_IA64 = 6,       
		BCJ_ARM = 7,        
		BCJ_ARMTHUMB = 8,   
		BCJ_SPARC = 9,      
		BCJ_ARM64 = 10,     
		BCJ_RISCV = 11      
	} type;
	enum xz_ret ret;
	bool single_call;
	uint32_t pos;
	uint32_t x86_prev_mask;
	uint8_t *out;
	size_t out_pos;
	size_t out_size;
	struct {
		size_t filtered;
		size_t size;
		uint8_t buf[16];
	} temp;
};
#ifdef XZ_DEC_X86
static inline int bcj_x86_test_msbyte(uint8_t b)
{
	return b == 0x00 || b == 0xFF;
}
static size_t bcj_x86(struct xz_dec_bcj *s, uint8_t *buf, size_t size)
{
	static const bool mask_to_allowed_status[8]
		= { true, true, true, false, true, false, false, false };
	static const uint8_t mask_to_bit_num[8] = { 0, 1, 2, 2, 3, 3, 3, 3 };
	size_t i;
	size_t prev_pos = (size_t)-1;
	uint32_t prev_mask = s->x86_prev_mask;
	uint32_t src;
	uint32_t dest;
	uint32_t j;
	uint8_t b;
	if (size <= 4)
		return 0;
	size -= 4;
	for (i = 0; i < size; ++i) {
		if ((buf[i] & 0xFE) != 0xE8)
			continue;
		prev_pos = i - prev_pos;
		if (prev_pos > 3) {
			prev_mask = 0;
		} else {
			prev_mask = (prev_mask << (prev_pos - 1)) & 7;
			if (prev_mask != 0) {
				b = buf[i + 4 - mask_to_bit_num[prev_mask]];
				if (!mask_to_allowed_status[prev_mask]
						|| bcj_x86_test_msbyte(b)) {
					prev_pos = i;
					prev_mask = (prev_mask << 1) | 1;
					continue;
				}
			}
		}
		prev_pos = i;
		if (bcj_x86_test_msbyte(buf[i + 4])) {
			src = get_unaligned_le32(buf + i + 1);
			while (true) {
				dest = src - (s->pos + (uint32_t)i + 5);
				if (prev_mask == 0)
					break;
				j = mask_to_bit_num[prev_mask] * 8;
				b = (uint8_t)(dest >> (24 - j));
				if (!bcj_x86_test_msbyte(b))
					break;
				src = dest ^ (((uint32_t)1 << (32 - j)) - 1);
			}
			dest &= 0x01FFFFFF;
			dest |= (uint32_t)0 - (dest & 0x01000000);
			put_unaligned_le32(dest, buf + i + 1);
			i += 4;
		} else {
			prev_mask = (prev_mask << 1) | 1;
		}
	}
	prev_pos = i - prev_pos;
	s->x86_prev_mask = prev_pos > 3 ? 0 : prev_mask << (prev_pos - 1);
	return i;
}
#endif
#ifdef XZ_DEC_POWERPC
static size_t bcj_powerpc(struct xz_dec_bcj *s, uint8_t *buf, size_t size)
{
	size_t i;
	uint32_t instr;
	size &= ~(size_t)3;
	for (i = 0; i < size; i += 4) {
		instr = get_unaligned_be32(buf + i);
		if ((instr & 0xFC000003) == 0x48000001) {
			instr &= 0x03FFFFFC;
			instr -= s->pos + (uint32_t)i;
			instr &= 0x03FFFFFC;
			instr |= 0x48000001;
			put_unaligned_be32(instr, buf + i);
		}
	}
	return i;
}
#endif
#ifdef XZ_DEC_IA64
static size_t bcj_ia64(struct xz_dec_bcj *s, uint8_t *buf, size_t size)
{
	static const uint8_t branch_table[32] = {
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		4, 4, 6, 6, 0, 0, 7, 7,
		4, 4, 0, 0, 4, 4, 0, 0
	};
	size_t i;
	size_t j;
	uint32_t slot;
	uint32_t bit_pos;
	uint32_t byte_pos;
	uint32_t bit_res;
	uint32_t addr;
	uint32_t mask;
	uint64_t instr;
	uint64_t norm;
	size &= ~(size_t)15;
	for (i = 0; i < size; i += 16) {
		mask = branch_table[buf[i] & 0x1F];
		for (slot = 0, bit_pos = 5; slot < 3; ++slot, bit_pos += 41) {
			if (((mask >> slot) & 1) == 0)
				continue;
			byte_pos = bit_pos >> 3;
			bit_res = bit_pos & 7;
			instr = 0;
			for (j = 0; j < 6; ++j)
				instr |= (uint64_t)(buf[i + j + byte_pos])
						<< (8 * j);
			norm = instr >> bit_res;
			if (((norm >> 37) & 0x0F) == 0x05
					&& ((norm >> 9) & 0x07) == 0) {
				addr = (norm >> 13) & 0x0FFFFF;
				addr |= ((uint32_t)(norm >> 36) & 1) << 20;
				addr <<= 4;
				addr -= s->pos + (uint32_t)i;
				addr >>= 4;
				norm &= ~((uint64_t)0x8FFFFF << 13);
				norm |= (uint64_t)(addr & 0x0FFFFF) << 13;
				norm |= (uint64_t)(addr & 0x100000)
						<< (36 - 20);
				instr &= (1 << bit_res) - 1;
				instr |= norm << bit_res;
				for (j = 0; j < 6; j++)
					buf[i + j + byte_pos]
						= (uint8_t)(instr >> (8 * j));
			}
		}
	}
	return i;
}
#endif
#ifdef XZ_DEC_ARM
static size_t bcj_arm(struct xz_dec_bcj *s, uint8_t *buf, size_t size)
{
	size_t i;
	uint32_t addr;
	size &= ~(size_t)3;
	for (i = 0; i < size; i += 4) {
		if (buf[i + 3] == 0xEB) {
			addr = (uint32_t)buf[i] | ((uint32_t)buf[i + 1] << 8)
					| ((uint32_t)buf[i + 2] << 16);
			addr <<= 2;
			addr -= s->pos + (uint32_t)i + 8;
			addr >>= 2;
			buf[i] = (uint8_t)addr;
			buf[i + 1] = (uint8_t)(addr >> 8);
			buf[i + 2] = (uint8_t)(addr >> 16);
		}
	}
	return i;
}
#endif
#ifdef XZ_DEC_ARMTHUMB
static size_t bcj_armthumb(struct xz_dec_bcj *s, uint8_t *buf, size_t size)
{
	size_t i;
	uint32_t addr;
	if (size < 4)
		return 0;
	size -= 4;
	for (i = 0; i <= size; i += 2) {
		if ((buf[i + 1] & 0xF8) == 0xF0
				&& (buf[i + 3] & 0xF8) == 0xF8) {
			addr = (((uint32_t)buf[i + 1] & 0x07) << 19)
					| ((uint32_t)buf[i] << 11)
					| (((uint32_t)buf[i + 3] & 0x07) << 8)
					| (uint32_t)buf[i + 2];
			addr <<= 1;
			addr -= s->pos + (uint32_t)i + 4;
			addr >>= 1;
			buf[i + 1] = (uint8_t)(0xF0 | ((addr >> 19) & 0x07));
			buf[i] = (uint8_t)(addr >> 11);
			buf[i + 3] = (uint8_t)(0xF8 | ((addr >> 8) & 0x07));
			buf[i + 2] = (uint8_t)addr;
			i += 2;
		}
	}
	return i;
}
#endif
#ifdef XZ_DEC_SPARC
static size_t bcj_sparc(struct xz_dec_bcj *s, uint8_t *buf, size_t size)
{
	size_t i;
	uint32_t instr;
	size &= ~(size_t)3;
	for (i = 0; i < size; i += 4) {
		instr = get_unaligned_be32(buf + i);
		if ((instr >> 22) == 0x100 || (instr >> 22) == 0x1FF) {
			instr <<= 2;
			instr -= s->pos + (uint32_t)i;
			instr >>= 2;
			instr = ((uint32_t)0x40000000 - (instr & 0x400000))
					| 0x40000000 | (instr & 0x3FFFFF);
			put_unaligned_be32(instr, buf + i);
		}
	}
	return i;
}
#endif
#ifdef XZ_DEC_ARM64
static size_t bcj_arm64(struct xz_dec_bcj *s, uint8_t *buf, size_t size)
{
	size_t i;
	uint32_t instr;
	uint32_t addr;
	size &= ~(size_t)3;
	for (i = 0; i < size; i += 4) {
		instr = get_unaligned_le32(buf + i);
		if ((instr >> 26) == 0x25) {
			addr = instr - ((s->pos + (uint32_t)i) >> 2);
			instr = 0x94000000 | (addr & 0x03FFFFFF);
			put_unaligned_le32(instr, buf + i);
		} else if ((instr & 0x9F000000) == 0x90000000) {
			addr = ((instr >> 29) & 3) | ((instr >> 3) & 0x1FFFFC);
			if ((addr + 0x020000) & 0x1C0000)
				continue;
			addr -= (s->pos + (uint32_t)i) >> 12;
			instr &= 0x9000001F;
			instr |= (addr & 3) << 29;
			instr |= (addr & 0x03FFFC) << 3;
			instr |= (0U - (addr & 0x020000)) & 0xE00000;
			put_unaligned_le32(instr, buf + i);
		}
	}
	return i;
}
#endif
#ifdef XZ_DEC_RISCV
static size_t bcj_riscv(struct xz_dec_bcj *s, uint8_t *buf, size_t size)
{
	size_t i;
	uint32_t b1;
	uint32_t b2;
	uint32_t b3;
	uint32_t instr;
	uint32_t instr2;
	uint32_t instr2_rs1;
	uint32_t addr;
	if (size < 8)
		return 0;
	size -= 8;
	for (i = 0; i <= size; i += 2) {
		instr = buf[i];
		if (instr == 0xEF) {
			b1 = buf[i + 1];
			if ((b1 & 0x0D) != 0)
				continue;
			b2 = buf[i + 2];
			b3 = buf[i + 3];
			addr = ((b1 & 0xF0) << 13) | (b2 << 9) | (b3 << 1);
			addr -= s->pos + (uint32_t)i;
			buf[i + 1] = (uint8_t)((b1 & 0x0F)
					| ((addr >> 8) & 0xF0));
			buf[i + 2] = (uint8_t)(((addr >> 16) & 0x0F)
					| ((addr >> 7) & 0x10)
					| ((addr << 4) & 0xE0));
			buf[i + 3] = (uint8_t)(((addr >> 4) & 0x7F)
					| ((addr >> 13) & 0x80));
			i += 4 - 2;
		} else if ((instr & 0x7F) == 0x17) {
			instr |= (uint32_t)buf[i + 1] << 8;
			instr |= (uint32_t)buf[i + 2] << 16;
			instr |= (uint32_t)buf[i + 3] << 24;
			if (instr & 0xE80) {
				instr2 = get_unaligned_le32(buf + i + 4);
				if (((instr << 8) ^ (instr2 - 3)) & 0xF8003) {
					i += 6 - 2;
					continue;
				}
				addr = (instr & 0xFFFFF000) + (instr2 >> 20);
				instr = 0x17 | (2 << 7) | (instr2 << 12);
				instr2 = addr;
			} else {
				instr2_rs1 = instr >> 27;
				if ((uint32_t)((instr - 0x3117) << 18)
						>= (instr2_rs1 & 0x1D)) {
					i += 4 - 2;
					continue;
				}
				addr = get_unaligned_be32(buf + i + 4);
				addr -= s->pos + (uint32_t)i;
				instr2 = (instr >> 12) | (addr << 20);
				instr = 0x17 | (instr2_rs1 << 7)
					| ((addr + 0x800) & 0xFFFFF000);
			}
			put_unaligned_le32(instr, buf + i);
			put_unaligned_le32(instr2, buf + i + 4);
			i += 8 - 2;
		}
	}
	return i;
}
#endif
static void bcj_apply(struct xz_dec_bcj *s,
		      uint8_t *buf, size_t *pos, size_t size)
{
	size_t filtered;
	buf += *pos;
	size -= *pos;
	switch (s->type) {
#ifdef XZ_DEC_X86
	case BCJ_X86:
		filtered = bcj_x86(s, buf, size);
		break;
#endif
#ifdef XZ_DEC_POWERPC
	case BCJ_POWERPC:
		filtered = bcj_powerpc(s, buf, size);
		break;
#endif
#ifdef XZ_DEC_IA64
	case BCJ_IA64:
		filtered = bcj_ia64(s, buf, size);
		break;
#endif
#ifdef XZ_DEC_ARM
	case BCJ_ARM:
		filtered = bcj_arm(s, buf, size);
		break;
#endif
#ifdef XZ_DEC_ARMTHUMB
	case BCJ_ARMTHUMB:
		filtered = bcj_armthumb(s, buf, size);
		break;
#endif
#ifdef XZ_DEC_SPARC
	case BCJ_SPARC:
		filtered = bcj_sparc(s, buf, size);
		break;
#endif
#ifdef XZ_DEC_ARM64
	case BCJ_ARM64:
		filtered = bcj_arm64(s, buf, size);
		break;
#endif
#ifdef XZ_DEC_RISCV
	case BCJ_RISCV:
		filtered = bcj_riscv(s, buf, size);
		break;
#endif
	default:
		filtered = 0;
		break;
	}
	*pos += filtered;
	s->pos += filtered;
}
static void bcj_flush(struct xz_dec_bcj *s, struct xz_buf *b)
{
	size_t copy_size;
	copy_size = min_t(size_t, s->temp.filtered, b->out_size - b->out_pos);
	memcpy(b->out + b->out_pos, s->temp.buf, copy_size);
	b->out_pos += copy_size;
	s->temp.filtered -= copy_size;
	s->temp.size -= copy_size;
	memmove(s->temp.buf, s->temp.buf + copy_size, s->temp.size);
}
XZ_EXTERN enum xz_ret xz_dec_bcj_run(struct xz_dec_bcj *s,
				     struct xz_dec_lzma2 *lzma2,
				     struct xz_buf *b)
{
	size_t out_start;
	if (s->temp.filtered > 0) {
		bcj_flush(s, b);
		if (s->temp.filtered > 0)
			return XZ_OK;
		if (s->ret == XZ_STREAM_END)
			return XZ_STREAM_END;
	}
	if (s->temp.size < b->out_size - b->out_pos || s->temp.size == 0) {
		out_start = b->out_pos;
		memcpy(b->out + b->out_pos, s->temp.buf, s->temp.size);
		b->out_pos += s->temp.size;
		s->ret = xz_dec_lzma2_run(lzma2, b);
		if (s->ret != XZ_STREAM_END
				&& (s->ret != XZ_OK || s->single_call))
			return s->ret;
		bcj_apply(s, b->out, &out_start, b->out_pos);
		if (s->ret == XZ_STREAM_END)
			return XZ_STREAM_END;
		s->temp.size = b->out_pos - out_start;
		b->out_pos -= s->temp.size;
		memcpy(s->temp.buf, b->out + b->out_pos, s->temp.size);
		if (b->out_pos + s->temp.size < b->out_size)
			return XZ_OK;
	}
	if (b->out_pos < b->out_size) {
		s->out = b->out;
		s->out_pos = b->out_pos;
		s->out_size = b->out_size;
		b->out = s->temp.buf;
		b->out_pos = s->temp.size;
		b->out_size = sizeof(s->temp.buf);
		s->ret = xz_dec_lzma2_run(lzma2, b);
		s->temp.size = b->out_pos;
		b->out = s->out;
		b->out_pos = s->out_pos;
		b->out_size = s->out_size;
		if (s->ret != XZ_OK && s->ret != XZ_STREAM_END)
			return s->ret;
		bcj_apply(s, s->temp.buf, &s->temp.filtered, s->temp.size);
		if (s->ret == XZ_STREAM_END)
			s->temp.filtered = s->temp.size;
		bcj_flush(s, b);
		if (s->temp.filtered > 0)
			return XZ_OK;
	}
	return s->ret;
}
XZ_EXTERN struct xz_dec_bcj *xz_dec_bcj_create(bool single_call)
{
	struct xz_dec_bcj *s = kmalloc(sizeof(*s), GFP_KERNEL);
	if (s != NULL)
		s->single_call = single_call;
	return s;
}
XZ_EXTERN enum xz_ret xz_dec_bcj_reset(struct xz_dec_bcj *s, uint8_t id)
{
	switch (id) {
#ifdef XZ_DEC_X86
	case BCJ_X86:
#endif
#ifdef XZ_DEC_POWERPC
	case BCJ_POWERPC:
#endif
#ifdef XZ_DEC_IA64
	case BCJ_IA64:
#endif
#ifdef XZ_DEC_ARM
	case BCJ_ARM:
#endif
#ifdef XZ_DEC_ARMTHUMB
	case BCJ_ARMTHUMB:
#endif
#ifdef XZ_DEC_SPARC
	case BCJ_SPARC:
#endif
#ifdef XZ_DEC_ARM64
	case BCJ_ARM64:
#endif
#ifdef XZ_DEC_RISCV
	case BCJ_RISCV:
#endif
		break;
	default:
		return XZ_OPTIONS_ERROR;
	}
	s->type = id;
	s->ret = XZ_OK;
	s->pos = 0;
	s->x86_prev_mask = 0;
	s->temp.filtered = 0;
	s->temp.size = 0;
	return XZ_OK;
}
#endif
