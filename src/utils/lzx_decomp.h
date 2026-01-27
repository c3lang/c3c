#ifndef C3_LZX_DECOMP_H
#define C3_LZX_DECOMP_H

#include <stdint.h>

// LZX decompression
typedef struct lzx_decomp_state lzx_decomp_state;

lzx_decomp_state *lzx_decomp_create(int window);
void lzx_decomp_free(lzx_decomp_state *state);
int lzx_decomp_run(lzx_decomp_state *state, uint8_t *in, int inlen, uint8_t *out, int outlen);

#endif // C3_LZX_DECOMP_H
