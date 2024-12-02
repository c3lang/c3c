/**
 * This code is indebted to the
 * JUnzip library by Joonas Pihlajamaa (firstname.lastname@iki.fi).
 */
#include "lib.h"
#include "miniz.h"

#define ZIP_BUFFER_SIZE 65536
#define FILE_OUTBUF_LEN 65536

uint8_t internal_buffer[ZIP_BUFFER_SIZE]; // limits maximum zip descriptor size
uint8_t file_out_buffer[FILE_OUTBUF_LEN];

typedef PACK(struct
{
	uint32_t signature; // 0x04034B50
	uint16_t version_needed_to_extract; // unsupported
	uint16_t general_purpose_bit_flag; // unsupported
	uint16_t compression_method;
	uint16_t last_mod_file_time;
	uint16_t last_mod_file_date;
	uint32_t crc32;
	uint32_t compressed_size;
	uint32_t uncompressed_size;
	uint16_t filename_len;
	uint16_t extra_field_len; // unsupported
}) JZLocalFileHeader;

typedef PACK(struct
{
	uint32_t signature; // 0x02014B50
	uint16_t version_made_by; // unsupported
	uint16_t version_needed_to_extract; // unsupported
	uint16_t general_purpose_bit_flag; // unsupported
	uint16_t compression_method;
	uint16_t last_mod_file_time;
	uint16_t last_mod_file_date;
	uint32_t crc32;
	uint32_t compressed_size;
	uint32_t uncompressed_size;
	uint16_t file_name_len;
	uint16_t extra_field_len; // unsupported
	uint16_t comment_len; // unsupported
	uint16_t disk_number_start; // unsupported
	uint16_t internal_file_attributes; // unsupported
	uint32_t external_file_attributes; // unsupported
	uint32_t relative_offset_of_local_header;
}) ZipGlobalFileHeader;


typedef PACK(struct
{
	uint32_t signature; // 0x06054b50
	uint16_t disk_number; // unsupported
	uint16_t central_dir_disk_number; // unsupported
	uint16_t num_entries_this_disk; // unsupported
	uint16_t num_entries;
	uint32_t central_dir_size;
	uint32_t central_dir_offset;
	uint16_t zip_comment_len;
	// Followed by .ZIP file comment (variable size)
}) ZipEndRecord;

INLINE bool read_all(FILE *file, void *buffer, size_t len)
{
	size_t read = fread(buffer, 1, len, file);
	return read == len;
}

const char *zip_dir_iterator(FILE *zip, ZipDirIterator *iterator)
{
	if (fseek(zip, 0, SEEK_END)) return "Couldn't move to end of .c3l file!";

	long file_size = ftell(zip);
	if (file_size <= sizeof(ZipEndRecord)) return "Too small to be a .c3l";

	long read_bytes = file_size < sizeof(internal_buffer) ? file_size : sizeof(internal_buffer);

	if (fseek(zip, file_size - read_bytes, SEEK_SET)) return "Cannot seek in .c3l file";

	if (!read_all(zip, internal_buffer, read_bytes)) return "Couldn't read end of .c3l file";

	// Naively assume signature can only be found in one place...
	ZipEndRecord *er;
	long i = 0;
	for (i = read_bytes - sizeof(ZipEndRecord); i >= 0; i--)
	{
		er = (ZipEndRecord *)(internal_buffer + i);
		if (er->signature == 0x06054B50) break;
	}

	if (i < 0) return "End record signature not found in .c3l file";

	ZipEndRecord record;
	memcpy(&record, er, sizeof(ZipEndRecord));

	if (record.disk_number || record.central_dir_disk_number ||
			record.num_entries != record.num_entries_this_disk)
	{
		return "Unsupported .c3l structure";
	}

	iterator->offset = record.central_dir_offset;
	iterator->files = record.num_entries;
	iterator->current_file = 0;
	iterator->file = zip;
	return NULL;
}

const char *zip_dir_iterator_next(ZipDirIterator *iterator, ZipFile *file)
{
	ASSERT0(iterator->current_file < iterator->files);
	iterator->current_file++;
	FILE *zip = iterator->file;
	if (fseek(zip, iterator->offset, SEEK_SET)) return "Cannot seek in c3l file!";
	ZipGlobalFileHeader file_header;
	if (!read_all(zip, &file_header, sizeof(ZipGlobalFileHeader)))
	{
		return str_printf("Couldn't read file header %d!", iterator->current_file);
	}

	if (file_header.signature != 0x02014B50)
	{
		return str_printf("Invalid file header signature %d!", iterator->current_file);
	}

	if (ZIP_MAX_NAME < file_header.file_name_len + 1)
	{
		return str_printf("Filename too long %d", iterator->current_file);
	}
	if (!read_all(zip, file->name, file_header.file_name_len))
	{
		return str_printf("Couldn't read filename %d!", iterator->current_file);
	}

	file->name[file_header.file_name_len] = '\0';
	if (fseek(zip, file_header.extra_field_len, SEEK_CUR) ||
		fseek(zip, file_header.comment_len, SEEK_CUR))
	{
		return str_printf("Couldn't skip extra field or file comment %s", file->name);
	}
	if (file_header.compression_method != 0 && file_header.compression_method != 8)
	{
		return str_printf("Illegal compression method '%s'", file->name);
	}
	if (file_header.compression_method == 0 &&
		(file_header.compressed_size != file_header.uncompressed_size))
	{
		return str_printf("Invalid compression '%s'", file->name);
	}
	file->uncompressed_size = file_header.uncompressed_size;
	file->compressed_size = file_header.compressed_size;
	file->offset = file_header.relative_offset_of_local_header + sizeof(JZLocalFileHeader) - 4;
	file->file_crc32 = file_header.crc32;
	file->compression_method = file_header.compression_method;
	iterator->offset = ftell(zip);
	return NULL;
}

INLINE const char *zip_prepare_zip_for_read(FILE *zip, ZipFile *file)
{
	if (fseek(zip, file->offset, SEEK_SET)) return "Failed to search in file.";

	uint16_t field1;
	uint16_t field2;
	if (!read_all(zip, &field1, 2)) return "Failed to read name len";
	if (!read_all(zip, &field2, 2)) return "Failed to read extra len";
	if (fseek(zip, field1 + field2, SEEK_CUR)) return "Failed to skip len";
	return NULL;
}

const char *zip_file_read(FILE *zip, ZipFile *file, void **buffer_ptr)
{
	const char *error = zip_prepare_zip_for_read(zip, file);
	if (error) return error;

	unsigned char *bytes = MALLOC(file->uncompressed_size + 1);
	bytes[file->uncompressed_size] = 0;
	*buffer_ptr = bytes;

	// Uncompressed
	if (file->compression_method == 0)
	{
		if (!read_all(zip, bytes, file->uncompressed_size) || ferror(zip)) return "Failed to read data.";
		return NULL;
	}

	// Only deflate supported.
	ASSERT0(file->compression_method == 8 && "Should already be checked.");

	// Deflate - using zlib
	z_stream strm = { .zalloc = Z_NULL, .zfree = Z_NULL, .opaque = Z_NULL, .avail_in = 0, .next_in = Z_NULL };

	// Use inflateInit2 with negative window bits to indicate raw data
	if (inflateInit2(&strm, -MAX_WBITS) != Z_OK) return "Failed to init zlib";

	// Inflate compressed data
	long compressed_left = file->compressed_size;
	long uncompressed_left = file->uncompressed_size;
	while (compressed_left && uncompressed_left)
	{
		size_t to_read = ZIP_BUFFER_SIZE < compressed_left ? ZIP_BUFFER_SIZE : compressed_left;
		strm.avail_in = fread(internal_buffer, 1, to_read, zip);
		if (strm.avail_in == 0 || ferror(zip))
		{
			inflateEnd(&strm);
			return "Failed to read zip";
		}
		strm.next_in = internal_buffer;
		strm.avail_out = uncompressed_left;
		strm.next_out = bytes;

		compressed_left -= strm.avail_in;

		switch (inflate(&strm, Z_NO_FLUSH))
		{
			case Z_STREAM_ERROR:
				return "Unexpected inflate error";
			case Z_NEED_DICT:
			case Z_DATA_ERROR:
				return "Inflate data error";
			case Z_MEM_ERROR:
				return "Inflate memory error";
			case Z_STREAM_END:
				goto END;
			default:
				break;
		}
		bytes += uncompressed_left - strm.avail_out;
		uncompressed_left = strm.avail_out;
	}
END:
	inflateEnd(&strm);
	return NULL;
}

const char *zip_file_write(FILE *zip, ZipFile *file, const char *dir, bool overwrite)
{
	const char *error = zip_prepare_zip_for_read(zip, file);
	if (error) return error;

	char *file_name;
	char *dir_path;
	if (!file_namesplit(file->name, &file_name, &dir_path)) return "Failed to split file name";

	if (dir_path)
	{
		char *new_path = str_printf("%s/%s", dir, dir_path);
		dir_make_recursive(new_path);
		dir = new_path;
	}

	const char *out_file_name = file_append_path(dir, file_name);
	if (!overwrite && file_exists(out_file_name)) return NULL;

	FILE *f = fopen(out_file_name, "wb");
	if (!f) return "Failed to open file output path.";

	// Uncompressed
	if (file->compression_method == 0)
	{
		size_t left_to_read = file->uncompressed_size;
		while (left_to_read)
		{
			size_t amount_to_read = left_to_read < ZIP_BUFFER_SIZE ? left_to_read : ZIP_BUFFER_SIZE;
			ASSERT0(amount_to_read > 0);
			if (!read_all(zip, internal_buffer, amount_to_read))
			{
				fclose(f);
				return "Failed to read data";
			}
			size_t written = fwrite(internal_buffer, 1, amount_to_read, f);
			if (written != amount_to_read)
			{
				fclose(f);
				return "Failed to write";
			}
			left_to_read -= written;
		}
		return NULL;
	}

	// Only deflate supported.
	ASSERT0(file->compression_method == 8 && "Should already be checked.");

	// Deflate - using zlib
	z_stream strm = { .zalloc = Z_NULL, .zfree = Z_NULL, .opaque = Z_NULL, .avail_in = 0, .next_in = Z_NULL };

	// Use inflateInit2 with negative window bits to indicate raw data
	if (inflateInit2(&strm, -MAX_WBITS) != Z_OK) return "Failed to init zlib";

	// Inflate compressed data
	long compressed_left = file->compressed_size;
	while (compressed_left)
	{
		size_t to_read = ZIP_BUFFER_SIZE < compressed_left ? ZIP_BUFFER_SIZE : compressed_left;
		strm.avail_in = fread(internal_buffer, 1, to_read, zip);
		if (strm.avail_in == 0 || ferror(zip))
		{
			inflateEnd(&strm);
			fclose(f);
			return "Failed to read zip";
		}

		strm.next_in = internal_buffer;
		while (strm.avail_in)
		{
			strm.avail_out = FILE_OUTBUF_LEN;
			strm.next_out = file_out_buffer;

			bool stream_end = false;
			switch (inflate(&strm, Z_NO_FLUSH))
			{
				case Z_STREAM_ERROR:
					fclose(f);
					return "Unexpected inflate error";
				case Z_NEED_DICT:
				case Z_DATA_ERROR:
					fclose(f);
					return "Inflate data error";
				case Z_MEM_ERROR:
					fclose(f);
					return "Inflate memory error";
				case Z_STREAM_END:
					stream_end = true;
					FALLTHROUGH;
				default:
					break;
			}

			size_t to_write = FILE_OUTBUF_LEN - strm.avail_out;

			if (to_write > 0 && to_write != fwrite(file_out_buffer, 1, to_write, f))
			{
				fclose(f);
				return "Failed to write";
			}
			if (stream_end) break;
		}
		compressed_left = file->compressed_size - strm.total_in;
	}
	fclose(f);
	inflateEnd(&strm);
	return NULL;
}

