#include "xar.h"

#include "lib.h"

static uint16_t read_be_uint16(FILE *file)
{
	const uint8_t hi = fgetc(file);
	const uint8_t lo = fgetc(file);

	return (uint16_t) (hi << 8 | lo);
}

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

XarHeader xar_header(FILE *file)
{
	XarHeader header = { 0 };
	fread(&header.signature, 4, 1, file);

	header.hsize = read_be_uint16(file);
	header.version = read_be_uint16(file);

	header.compressed_len = read_be_uint64(file);
	header.len = read_be_uint64(file);

	uint32_t algo;
	fread(&algo, sizeof(algo), 1, file);

	return header;
}

/* from dependencies/miniz/zlib_compat.c */
int uncompress(unsigned char *dest, unsigned long *destLen, const unsigned char
	*source, unsigned long sourceLen);

void *xar_toc(const XarHeader *header, FILE *file)
{
	unsigned long consumed = header->len;
	uint8_t *src = calloc_arena(header->compressed_len);
	uint8_t *dst = calloc_arena(header->len + 1);
	fread(src, sizeof(*src), header->compressed_len, file);

	uncompress(dst, &consumed, src, header->compressed_len);

	dst[consumed] = 0;

	return dst;
}