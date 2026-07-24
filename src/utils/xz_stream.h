#pragma once
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <xz.h>

typedef struct {
	struct xz_dec *decoder;
	struct xz_buf buf;
} XzStream;

bool xz_stream_init(XzStream *stream);
void xz_stream_free(const XzStream *stream);

void xz_stream_in(XzStream *stream, uint8_t *buf, size_t len);
void xz_stream_out(XzStream *stream, uint8_t *buf, size_t len);

bool xz_stream_decompress(XzStream *stream);