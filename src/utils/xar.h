#pragma once

#include <stdint.h>
#include <stdio.h>

typedef struct {
	FILE *file;

	char signature[4];
	uint16_t hsize, version;
	uint64_t compressed_len, len;
	/* checksum algorithms omitted */
} XarHeader;

typedef struct {
	FILE *file;

	size_t offset;
	int64_t to_read;
} XarFile;

XarHeader xar_header(FILE *file);

XarFile xar_open(const XarHeader *header, const char *filename);