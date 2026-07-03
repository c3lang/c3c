#include "byte_buffer.h"

#include <stdlib.h>

#include "lib.h"

static void grow(ByteBuffer *self, size_t capacity)
{
	capacity = next_highest_power_of_2(self->bytes.len + capacity);
	self->bytes.ptr = realloc(self->bytes.ptr, capacity);
	self->bytes.len = capacity;
}

static void shrink(ByteBuffer *self)
{
	if (self->read_idx >= self->max_read)
	{
		const size_t readable = self->write_idx - self->read_idx;
		memmove(self->bytes.ptr, self->bytes.ptr + self->read_idx - 1,
			readable + 1);
		self->write_idx = readable + 1;
		self->read_idx = 1;
	}
}

void byte_buffer_init(ByteBuffer *self, size_t max_read)
{
	self->max_read = max_read;
	self->write_idx = 0;
	self->read_idx = 0;
	self->bytes.len = 0;
	self->bytes.ptr = NULL;
	grow(self, 16);
}

void byte_buffer_free(ByteBuffer *self)
{
	if (self->bytes.ptr != NULL) free(self->bytes.ptr);
	self->max_read = 0;
	self->bytes.len = 0;
	self->bytes.ptr = NULL;
	self->read_idx = 0;
	self->write_idx = 0;
	self->has_last = false;
}

size_t byte_buffer_write(ByteBuffer *self, const uint8_t *buffer,
	const size_t len)
{
	const size_t cap = self->bytes.len - self->write_idx;
	if (cap < len) grow(self, len);
	memcpy(self->bytes.ptr + self->write_idx, buffer, len);
	self->write_idx += len;
	return len;
}

size_t byte_buffer_read(ByteBuffer *self, uint8_t *buffer, size_t len)
{
	size_t readable = self->write_idx - self->read_idx;
	if (readable == 0)
	{
		self->has_last = false;
		return 0;
	}
	size_t n = MIN(readable, len);
	memcpy(buffer, self->bytes.ptr + self->read_idx, n);
	self->read_idx += n;
	self->has_last = n > 0;
	shrink(self);
	return n;
}