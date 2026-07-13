#include "xz_stream.h"

#include "common.h"

bool xz_stream_init(XzStream *stream)
{
	stream->decoder = xz_dec_init(XZ_PREALLOC, 1 << 24);
	return stream->decoder != NULL;
}

void xz_stream_free(const XzStream *stream)
{
	xz_dec_end(stream->decoder);
}

void xz_stream_in(XzStream *stream, uint8_t *buf, size_t len)
{
	stream->buf.in = buf;
	stream->buf.in_pos = 0;
	stream->buf.in_size = len;
}

void xz_stream_out(XzStream *stream, uint8_t *buf, size_t len)
{
	stream->buf.out = buf;
	stream->buf.out_pos = 0;
	stream->buf.out_size = len;
}

bool xz_stream_decompress(XzStream *stream)
{
	const enum xz_ret ret = xz_dec_catrun(stream->decoder, &stream->buf, false);

	if (ret == XZ_MEMLIMIT_ERROR)
	{
		error_exit("Xz needs more memory, please increase the number in"
			"src/utils/xz_stream.c:7");
	}

	return ret == XZ_OK || ret == XZ_STREAM_END;
}
