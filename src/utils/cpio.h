#pragma once
#include <stdio.h>

#include "byte_buffer.h"

typedef enum {
	NONE,
	DIR,
	REGULAR,
	SYMBOLIC_LINK
} CpioFile;

typedef struct {
	char *name;
	char *link;
	CpioFile type;
	size_t name_size, size;
} CpioHeader;

typedef struct {
	ByteBuffer buffer;
	const char *shallow;

	int state;
	CpioHeader file;

	size_t to_read;
	FILE *out;
} Cpio;

void cpio_init(Cpio *cpio, const char *shallowify);
void cpio_free(Cpio *cpio);

void cpio_push(Cpio *cpio, uint8_t *buffer, size_t len);