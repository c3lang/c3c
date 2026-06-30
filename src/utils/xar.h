#pragma once

#include <stdint.h>
#include <stdio.h>

typedef struct {
	char signature[4];
	uint16_t hsize, version;
	uint64_t compressed_len, len;
	/* checksum algorithms omitted */
} XarHeader;

XarHeader xar_header(FILE *file);

void *xar_toc(const XarHeader *header, FILE *file);