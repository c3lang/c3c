#include "pbzx.h"

#include "cpio.h"
#include "lib.h"
#include "xz_stream.h"

#define IN_SIZE		4096
#define OUT_SIZE	IN_SIZE * 4

static uint64_t read_be_uint64(FILE *file)
{
	uint64_t val = (uint64_t) fgetc(file) << 56;
	val |= (uint64_t) fgetc(file) << 48;
	val |= (uint64_t) fgetc(file) << 40;
	val |= (uint64_t) fgetc(file) << 32;
	val |= (uint64_t) fgetc(file) << 24;
	val |= (uint64_t) fgetc(file) << 16;
	val |= (uint64_t) fgetc(file) << 8;

	return val | (uint64_t) fgetc(file);
}

bool pbzx_extract(const XarFile *file, Cpio *cpio)
{
	fseek(file->file, (long) file->offset, SEEK_SET);
	char header[5] = {0};
	fread(header, sizeof(char), 4, file->file);

	if (!str_eq(header, "pbzx")) error_exit("Expected 'pbzx' header.");

	XzStream stream;
	bool error = false;
	uint64_t len, last = 0;
	uint8_t in_buf[IN_SIZE];
	uint8_t *out_buf = malloc((size_t) OUT_SIZE);

	if (!xz_stream_init(&stream)) error_exit("Failed to initialize XzStream.");

	uint64_t flg = read_be_uint64(file->file);

	while (flg & 1 << 24)
	{
		flg = read_be_uint64(file->file);
		len = read_be_uint64(file->file);
		bool plain = len == 0x1000000;
		uint64_t min = MIN(IN_SIZE, len);
		fread(in_buf, sizeof(uint8_t), min, file->file);

		if (!plain && strncmp((char *) in_buf, "\xfd""7zXZ\0", 6) != 0)
		{
			error = true;
			goto cleanup;
		}
		while (len)
		{
			if (plain)
			{
				cpio_push(cpio, in_buf, min);
			}
			else
			{
				xz_stream_in(&stream, in_buf, min);

				while (stream.buf.in_pos < stream.buf.in_size)
				{
					xz_stream_out(&stream, out_buf, (size_t) OUT_SIZE);

					if (!xz_stream_decompress(&stream))
					{
						error = true;
						goto cleanup;
					}

					cpio_push(cpio, out_buf, stream.buf.out_pos);
				}
			}

			last = min;
			len -= min;
			min = MIN(IN_SIZE, len);
			if (min == 0) break;
			fread(in_buf, sizeof(uint8_t), min, file->file);
		}
		if (!plain && strncmp((char *) in_buf + last - 2, "YZ", 2) != 0)
		{
			error = true;
			goto cleanup;
		}
	}

cleanup:
	free(out_buf);
	xz_stream_free(&stream);

	return !error;
}
