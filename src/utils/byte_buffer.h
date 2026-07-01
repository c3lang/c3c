#pragma once
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef struct {
	size_t max_read;
	struct {
		uint8_t *ptr;
		size_t len;
	} bytes;
	size_t read_idx, write_idx;
	bool has_last;
} ByteBuffer;

#define byte_buffer_available(self)	(self.write_idx - self.read_idx)

void byte_buffer_init(ByteBuffer *self, size_t max_read);
void byte_buffer_free(ByteBuffer *self);

size_t byte_buffer_write(ByteBuffer *self, const uint8_t *buffer, size_t len);
size_t byte_buffer_read(ByteBuffer *self, uint8_t *buffer, size_t len);