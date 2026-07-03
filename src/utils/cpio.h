#pragma once
#include <stdio.h>

#include "byte_buffer.h"

typedef enum {
	NONE,
	DIRECTORY,
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
	size_t dot_at;

	int state;
	CpioHeader file;

	size_t to_read;
	FILE *out;

#ifdef _WIN32
	char **exclude;
	size_t exclude_count;
	bool keep_sdk;

	char *sdk;
#endif
} Cpio;

void cpio_init(Cpio *cpio, const char *shallowify);
void cpio_free(Cpio *cpio);

void cpio_push(Cpio *cpio, uint8_t *buffer, size_t len);