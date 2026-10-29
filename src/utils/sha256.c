#include "lib.h"

typedef struct
{
	uint32_t state[8];
	uint8_t block[64];
	size_t block_len;
	uint64_t total_len;
} Sha256;

static const uint32_t sha256_k[64] = {
	0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
	0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
	0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
	0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
	0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
	0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
	0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
	0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
};

#define ROTR(x_, n_) (((x_) >> (n_)) | ((x_) << (32 - (n_))))

static void sha256_init(Sha256 *ctx)
{
	*ctx = (Sha256) { .state = { 0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
								 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19 } };
}

// Mix one 64 byte block into the state.
static void sha256_block(Sha256 *ctx, const uint8_t *p)
{
	uint32_t w[64];
	for (int i = 0; i < 16; i++)
	{
		w[i] = (uint32_t)p[i * 4] << 24 | (uint32_t)p[i * 4 + 1] << 16 | (uint32_t)p[i * 4 + 2] << 8 | p[i * 4 + 3];
	}
	for (int i = 16; i < 64; i++)
	{
		uint32_t s0 = ROTR(w[i - 15], 7) ^ ROTR(w[i - 15], 18) ^ (w[i - 15] >> 3);
		uint32_t s1 = ROTR(w[i - 2], 17) ^ ROTR(w[i - 2], 19) ^ (w[i - 2] >> 10);
		w[i] = w[i - 16] + s0 + w[i - 7] + s1;
	}
	uint32_t a = ctx->state[0], b = ctx->state[1], c = ctx->state[2], d = ctx->state[3];
	uint32_t e = ctx->state[4], f = ctx->state[5], g = ctx->state[6], h = ctx->state[7];
	for (int i = 0; i < 64; i++)
	{
		uint32_t t1 = h + (ROTR(e, 6) ^ ROTR(e, 11) ^ ROTR(e, 25)) + ((e & f) ^ (~e & g)) + sha256_k[i] + w[i];
		uint32_t t2 = (ROTR(a, 2) ^ ROTR(a, 13) ^ ROTR(a, 22)) + ((a & b) ^ (a & c) ^ (b & c));
		h = g; g = f; f = e; e = d + t1;
		d = c; c = b; b = a; a = t1 + t2;
	}
	ctx->state[0] += a; ctx->state[1] += b; ctx->state[2] += c; ctx->state[3] += d;
	ctx->state[4] += e; ctx->state[5] += f; ctx->state[6] += g; ctx->state[7] += h;
}

static void sha256_update(Sha256 *ctx, const uint8_t *data, size_t len)
{
	ctx->total_len += len;
	while (len)
	{
		size_t take = 64 - ctx->block_len;
		if (take > len) take = len;
		memcpy(ctx->block + ctx->block_len, data, take);
		ctx->block_len += take;
		data += take;
		len -= take;
		if (ctx->block_len == 64)
		{
			sha256_block(ctx, ctx->block);
			ctx->block_len = 0;
		}
	}
}

// Pad the message and return the digest as 64 lowercase hex characters.
static const char *sha256_final_hex(Sha256 *ctx)
{
	uint64_t bits = ctx->total_len * 8;
	uint8_t pad[72] = { 0x80 };
	size_t pad_len = (ctx->block_len < 56 ? 56 : 120) - ctx->block_len;
	for (int i = 0; i < 8; i++) pad[pad_len + i] = (uint8_t)(bits >> (56 - 8 * i));
	sha256_update(ctx, pad, pad_len + 8);

	char *hex = malloc_arena(65);
	for (int i = 0; i < 8; i++) snprintf(hex + i * 8, 9, "%08x", ctx->state[i]);
	return hex;
}

const char *sha256_hex(const void *data, size_t len)
{
	Sha256 ctx;
	sha256_init(&ctx);
	sha256_update(&ctx, data, len);
	return sha256_final_hex(&ctx);
}

const char *sha256_file_hex(const char *path)
{
	FILE *file = file_open_read(path);
	if (!file) return NULL;
	Sha256 ctx;
	sha256_init(&ctx);
	uint8_t buffer[16384];
	size_t read;
	while ((read = fread(buffer, 1, sizeof(buffer), file)) > 0) sha256_update(&ctx, buffer, read);
	bool failed = ferror(file);
	fclose(file);
	return failed ? NULL : sha256_final_hex(&ctx);
}
