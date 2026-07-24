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
	header.file = file;
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

XarFile xar_open(const XarHeader *header, const char *filename)
{
	unsigned long consumed = header->len;
	uint8_t *src = calloc_arena(header->compressed_len);
	uint8_t *dst = calloc_arena(header->len + 1);
	fread(src, sizeof(*src), header->compressed_len, header->file);

	uncompress(dst, &consumed, src, header->compressed_len);
	dst[consumed] = 0;

	const size_t off = ftell(header->file);
	const char *start = (char*) dst;
	while ((start = strstr(start, "<file")))
	{
		char *end = strstr(start, "</file");
		if (!end) error_exit("Missing </file> in xar TOC");
		*end = 0;

		const char *name = strstr(start, "<name>");
		if (!name) error_exit("Missing <name> in xar TOC");
		name += 6;
		char *name_end = strstr(name, "</name");
		if (!name_end) error_exit("Missing </name> in xar TOC");
		*name_end = 0;

		if (str_eq(name, filename))
		{
			*name_end = '<';
			XarFile file = { header->file, off, 0 };

			const char *length = strstr(start, "<length>");
			if (!length) error_exit("Missing <length> in xar TOC");
			length += 8;
			file.to_read = (int64_t) strtoll(length, NULL, 10);

			const char *offset = strstr(start, "<offset>");
			if (!offset) error_exit("Missing <offset> in xar TOC");
			offset += 8;
			file.offset += (int64_t) strtoll(offset, NULL, 10);

			return file;
		}

		start = end + 1;
	}

	return (XarFile) { NULL, 0, 0 };
}