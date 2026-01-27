#include "lib.h"
#include "lzx_decomp.h"
#include "json.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "miniz.h"

#define OLE2_MAGIC "\xD0\xCF\x11\xE0\xA1\xB1\x1A\xE1"
#define OLE2_MINI_STREAM_CUTOFF 4096

typedef struct
{
	FILE *f;
	size_t size;
	uint32_t sector_size;
	uint32_t mini_sector_size;
	uint32_t *fat;
	uint32_t fat_entries;
	uint32_t *mini_fat;
	uint32_t mini_fat_entries;
	uint8_t *mini_stream;
	size_t mini_stream_size;
	uint32_t directory_first_sector;
} Ole2;

static uint32_t read4(const uint8_t *d) { return d[0] | (d[1] << 8) | (d[2] << 16) | (d[3] << 24); }
static uint16_t read2(const uint8_t *d) { return d[0] | (d[1] << 8); }

static void ole2_free(Ole2 *ole)
{
	if (!ole) return;
	if (ole->f) fclose(ole->f);
	free(ole->fat);
	free(ole->mini_fat);
	free(ole->mini_stream);
	free(ole);
}

static uint8_t *ole2_read_stream(Ole2 *ole, uint32_t first_sector, uint32_t stream_size, size_t *out_size)
{
	if (stream_size == 0)
	{
		*out_size = 0;
		return NULL;
	}
	uint8_t *res = cmalloc(stream_size);
	if (!res)
	{
		*out_size = 0;
		return NULL;
	}
	uint32_t remaining = stream_size;
	uint32_t current = first_sector;
	uint8_t *p = res;

	if (stream_size >= OLE2_MINI_STREAM_CUTOFF)
	{
		while (remaining > 0 && current < 0xFFFFFFFD)
		{
			if (current >= ole->fat_entries) break;
			uint32_t to_read = (remaining < ole->sector_size) ? remaining : ole->sector_size;
			fseek(ole->f, (long)(current + 1) * (long)ole->sector_size, SEEK_SET);
			if (fread(p, 1, to_read, ole->f) != to_read) break;
			p += to_read;
			remaining -= to_read;
			current = ole->fat[current];
		}
	}
	else
	{
		while (remaining > 0 && current < 0xFFFFFFFD)
		{
			if (current >= ole->mini_fat_entries) break;
			uint32_t to_read = (remaining < ole->mini_sector_size) ? remaining : ole->mini_sector_size;
			size_t offset = (size_t)current * ole->mini_sector_size;
			if (offset + to_read <= ole->mini_stream_size)
			{
				memcpy(p, ole->mini_stream + offset, to_read);
			}
			p += to_read;
			remaining -= to_read;
			current = ole->mini_fat[current];
		}
	}
	*out_size = stream_size - remaining;
	return res;
}

static Ole2 *ole2_open(const char *path)
{
	FILE *f = file_open_read(path);
	if (!f) return NULL;

	uint8_t header[512];
	if (fread(header, 1, 512, f) != 512 || memcmp(header, OLE2_MAGIC, 8) != 0)
	{
		fclose(f);
		return NULL;
	}

	Ole2 *ole = ccalloc(1, sizeof(Ole2));
	if (!ole)
	{
		fclose(f);
		return NULL;
	}
	ole->f = f;
	fseek(f, 0, SEEK_END);
	ole->size = (size_t)ftell(f);
	ole->sector_size = 1U << read2(header + 30);
	ole->mini_sector_size = 1U << read2(header + 32);

	if (ole->sector_size < 512 || ole->sector_size > 4096)
	{
		ole2_free(ole);
		return NULL;
	}

	uint32_t fat_sectors = read4(header + 44);
	ole->directory_first_sector = read4(header + 48);
	uint32_t mini_fat_first_sector = read4(header + 60);
	uint32_t mini_fat_sectors = read4(header + 64);
	uint32_t difat_first_sector = read4(header + 68);
	uint32_t difat_sectors = read4(header + 72);

	uint32_t entries_per_sector = ole->sector_size / 4;
	size_t total_possible_sectors = ole->size / ole->sector_size + 1;
	ole->fat_entries = (uint32_t)total_possible_sectors;
	if (ole->fat_entries < 109 * entries_per_sector) ole->fat_entries = 109 * entries_per_sector;

	ole->fat = cmalloc(ole->fat_entries * 4);
	if (!ole->fat)
	{
		ole2_free(ole);
		return NULL;
	}
	memset(ole->fat, 0xFF, ole->fat_entries * 4);

	uint32_t fat_idx = 0;
	for (int i = 0; i < 109 && fat_idx < fat_sectors; i++)
	{
		uint32_t s = read4(header + 76 + i * 4);
		if (s >= 0xFFFFFFFD) continue;
		fseek(f, (long)(s + 1) * (long)ole->sector_size, SEEK_SET);
		uint32_t target_idx = fat_idx * entries_per_sector;
		if (target_idx + entries_per_sector <= ole->fat_entries)
		{
			if (fread(ole->fat + target_idx, 1, ole->sector_size, f) != ole->sector_size) break;
			fat_idx++;
		}
	}

	uint32_t curr_difat_s = difat_first_sector;
	uint8_t *difat_buf = cmalloc(ole->sector_size);
	while (curr_difat_s < 0xFFFFFFFD && fat_idx < fat_sectors && difat_buf)
	{
		fseek(f, (long)(curr_difat_s + 1) * (long)ole->sector_size, SEEK_SET);
		if (fread(difat_buf, 1, ole->sector_size, f) != ole->sector_size) break;
		for (uint32_t i = 0; i < entries_per_sector - 1 && fat_idx < fat_sectors; i++)
		{
			uint32_t s = read4(difat_buf + i * 4);
			if (s >= 0xFFFFFFFD) continue;
			fseek(f, (long)(s + 1) * (long)ole->sector_size, SEEK_SET);
			uint32_t target_idx = fat_idx * entries_per_sector;
			if (target_idx + entries_per_sector <= ole->fat_entries)
			{
				if (fread(ole->fat + target_idx, 1, ole->sector_size, f) != ole->sector_size) break;
				fat_idx++;
			}
		}
		curr_difat_s = read4(difat_buf + ole->sector_size - 4);
	}
	free(difat_buf);

	ole->mini_fat_entries = mini_fat_sectors * entries_per_sector;
	if (ole->mini_fat_entries > 0)
	{
		ole->mini_fat = cmalloc(ole->mini_fat_entries * 4);
		if (ole->mini_fat)
		{
			memset(ole->mini_fat, 0xFF, ole->mini_fat_entries * 4);
			uint32_t m_fat_idx = 0;
			uint32_t curr = mini_fat_first_sector;
			while (curr < 0xFFFFFFFD && m_fat_idx < mini_fat_sectors)
			{
				if (curr >= ole->fat_entries) break;
				fseek(f, (long)(curr + 1) * (long)ole->sector_size, SEEK_SET);
				if (fread(ole->mini_fat + (size_t)m_fat_idx * entries_per_sector, 1, ole->sector_size, f) != ole->sector_size) break;
				m_fat_idx++;
				curr = ole->fat[curr];
			}
		}
	}

	fseek(f, (long)(ole->directory_first_sector + 1) * (long)ole->sector_size, SEEK_SET);
	uint8_t root_entry[128];
	if (fread(root_entry, 1, 128, f) == 128)
	{
		uint32_t mini_stream_first_sector = read4(root_entry + 116);
		uint32_t mini_stream_size = read4(root_entry + 120);
		if (mini_stream_size > 0)
		{
			size_t actual;
			ole->mini_stream = ole2_read_stream(ole, mini_stream_first_sector, mini_stream_size, &actual);
			ole->mini_stream_size = actual;
		}
	}

	return ole;
}

static int msi_mime2utf(int x)
{
	if (x < 10) return x + '0';
	if (x < 36) return x - 10 + 'A';
	if (x < 62) return x - 36 + 'a';
	if (x == 62) return '.';
	if (x == 63) return '_';
	return 0;
}

static void decode_msi_stream_name(const uint16_t *in, char *out)
{
	while (*in)
	{
		uint16_t ch = *in++;
		if (ch >= 0x3800 && ch < 0x4800)
		{
			*out++ = (char)msi_mime2utf((ch - 0x3800) & 0x3f);
			*out++ = (char)msi_mime2utf(((ch - 0x3800) >> 6) & 0x3f);
		}
		else if (ch >= 0x4800 && ch < 0x4840)
		{
			*out++ = (char)msi_mime2utf((ch - 0x4800) & 0x3f);
		}
		else
		{
			*out++ = (char)ch;
		}
	}
	*out = 0;
}

typedef struct {
    char name[256];
    uint32_t first_sector;
    uint32_t size;
} StreamInfo;

static int msi_find_streams(Ole2 *ole, StreamInfo *infos, int max_infos)
{
	uint32_t curr = ole->directory_first_sector;
	int count = 0;
	uint8_t *sec = cmalloc(ole->sector_size);
	if (!sec) return 0;
	while (curr < 0xFFFFFFFD && count < max_infos)
	{
		if (curr >= ole->fat_entries) break;
		fseek(ole->f, (long)(curr + 1) * (long)ole->sector_size, SEEK_SET);
		if (fread(sec, 1, ole->sector_size, ole->f) != ole->sector_size) break;
		for (int i = 0; i < (int)(ole->sector_size / 128); i++)
		{
			uint8_t *entry = sec + i * 128;
			uint16_t name_len = read2(entry + 64);
			if (name_len > 2 && (entry[66] == 2 || entry[66] == 1))
			{
				uint16_t wname[64];
				memcpy(wname, entry, 64);
				decode_msi_stream_name(wname, infos[count].name);
				infos[count].first_sector = read4(entry + 116);
				infos[count].size = read4(entry + 120);
				count++;
				if (count >= max_infos) break;
			}
		}
		curr = ole->fat[curr];
	}
	free(sec);
	return count;
}

typedef struct
{
	char *name;
	uint32_t usize;
	uint32_t uoffset;
	uint16_t folder_idx;
} CabFileInfo;

typedef struct
{
	uint32_t data_offset;
	uint16_t data_blocks;
	uint16_t comp_type;
} CabFolderInfo;

static bool cab_extract_buffer(uint8_t *data, size_t size, const char *out_root, JSONObject *name_map, bool verbose)
{
	if (size < 36 || memcmp(data, "MSCF", 4) != 0) return false;

	uint16_t num_folders = read2(data + 26);
	uint16_t num_files = read2(data + 28);
	uint32_t file_offset = read4(data + 16);
	uint32_t header_ptr = 36;

	uint16_t flags = read2(data + 30);
	if (flags & 4)
	{
		if (header_ptr + 4 > size) return false;
		header_ptr += 4 + data[header_ptr] + data[header_ptr + 1];
	}

	if (num_folders > 0x1000 || (size_t)header_ptr + num_folders * 8 > size) return false;
	CabFolderInfo *folders = cmalloc(num_folders * sizeof(CabFolderInfo));
	if (!folders) return false;
	for (int i = 0; i < num_folders; i++)
	{
		folders[i].data_offset = read4(data + header_ptr);
		folders[i].data_blocks = read2(data + header_ptr + 4);
		folders[i].comp_type = read2(data + header_ptr + 6);
		header_ptr += 8;
	}

	const char *main_algo = "unknown";
	if (num_folders > 0)
	{
		int comp_type = folders[0].comp_type & 0x0F;
		if (comp_type == 0) main_algo = "none";
		else if (comp_type == 1) main_algo = "MSZIP";
		else if (comp_type == 3) main_algo = "LZX";
	}
	if (verbose) printf("    CAB: %u folders, %u files, algo: %s\n", num_folders, num_files, main_algo);

	if (file_offset >= size)
	{
		free(folders);
		return false;
	}
	CabFileInfo *files = cmalloc(num_files * sizeof(CabFileInfo));
	if (!files)
	{
		free(folders);
		return false;
	}
	uint32_t f_ptr = file_offset;
	for (int i = 0; i < num_files; i++)
	{
		if (f_ptr + 16 >= size)
		{
			num_files = (uint16_t)i;
			break;
		}
		files[i].usize = read4(data + f_ptr);
		files[i].uoffset = read4(data + f_ptr + 4);
		files[i].folder_idx = read2(data + f_ptr + 8);
		files[i].name = str_dup((char *)data + f_ptr + 16);
		f_ptr += 16 + (uint32_t)strlen(files[i].name) + 1;
	}

	lzx_decomp_state *lzx = NULL;
	for (int i = 0; i < num_folders; i++)
	{
		uint32_t total_usize = 0;
		for (int j = 0; j < num_files; j++)
		{
			if (files[j].folder_idx == i)
			{
				uint32_t end = files[j].uoffset + files[j].usize;
				if (end > total_usize) total_usize = end;
			}
		}
		if (total_usize == 0 || total_usize > 0x7FFFFFFF) continue;
		uint8_t *ubuf = cmalloc(total_usize);
		if (!ubuf) continue;
		uint32_t uptr = 0;
		uint32_t d_ptr = folders[i].data_offset;
		int comp_type = folders[i].comp_type & 0x0F;

		for (int b = 0; b < folders[i].data_blocks; b++)
		{
			if (d_ptr + 8 > size) break;
			uint16_t csize = read2(data + d_ptr + 4);
			uint16_t usize = read2(data + d_ptr + 6);
			if ((size_t)d_ptr + 8 + csize > size || uptr + usize > total_usize) break;

			if (comp_type == 1)
			{ // MSZIP
				mz_ulong destLen = usize;
				mz_uncompress(ubuf + uptr, &destLen, data + d_ptr + 10, csize - 2);
			}
			else if (comp_type == 3)
			{ // LZX
				if (!lzx) lzx = lzx_decomp_create((folders[i].comp_type >> 8) & 0x1F);
				if (lzx) lzx_decomp_run(lzx, data + d_ptr + 8, (int)csize, ubuf + uptr, (int)usize);
			}
			else if (comp_type == 0)
			{ // NO COMP
				memcpy(ubuf + uptr, data + d_ptr + 8, usize);
			}
			uptr += usize;
			d_ptr += 8 + csize;
		}

		for (int j = 0; j < num_files; j++)
		{
			if (files[j].folder_idx == i)
			{
				JSONObject *entry = name_map ? json_map_get(name_map, files[j].name) : NULL;
				if (entry)
				{
					const char *real_path = entry->str;
					char *norm_path = str_dup(real_path);
					for (char *p = norm_path; *p; p++)
						if (*p == '\\') *p = '/';
					char *full_dst = (char *)file_append_path(out_root, norm_path);
					file_create_folders(full_dst);
					if (files[j].uoffset + files[j].usize <= total_usize)
					{
						file_write_all(full_dst, (const char *)ubuf + files[j].uoffset, files[j].usize);
					}
				}
			}
		}
		free(ubuf);
	}

	if (lzx) lzx_decomp_free(lzx);
	free(files);
	free(folders);
	return true;
}

static bool cab_extract(const char *path, const char *out_root, JSONObject *name_map, bool verbose)
{
	FILE *f = file_open_read(path);
	if (!f) return false;
	fseek(f, 0, SEEK_END);
	size_t size = (size_t)ftell(f);
	fseek(f, 0, SEEK_SET);
	uint8_t *data = cmalloc(size);
	if (data)
	{
		if (fread(data, 1, size, f) == size)
		{
			cab_extract_buffer(data, size, out_root, name_map, verbose);
		}
	}
	free(data);
	fclose(f);
	return true;
}

typedef struct
{
	char **strings;
	uint32_t count;
} StringTable;

static void free_string_table(StringTable *st)
{
	if (!st) return;
	for (uint32_t i = 0; i < st->count; i++)
		free(st->strings[i]);
	free(st->strings);
	free(st);
}

static StringTable *msi_load_string_table(Ole2 *ole, const StreamInfo *infos, int stream_count, int *bytes_per_strref_out)
{
	uint8_t *pool_data = NULL;
	size_t pool_size = 0;
	uint8_t *str_data = NULL;
	size_t str_size = 0;

	for (int i = 0; i < stream_count; i++)
	{
		if (strcmp(infos[i].name, "@_StringPool") == 0)
		{
			pool_data = ole2_read_stream(ole, infos[i].first_sector, infos[i].size, &pool_size);
		}
		else if (strcmp(infos[i].name, "@_StringData") == 0)
		{
			str_data = ole2_read_stream(ole, infos[i].first_sector, infos[i].size, &str_size);
		}
	}

	if (!pool_data || !str_data)
	{
		free(pool_data);
		free(str_data);
		return NULL;
	}

	int bytes_per_strref = 2;
	if (pool_size > 4 && (read2(pool_data + 2) & 0x8000))
	{
		bytes_per_strref = 4;
	}
	if (bytes_per_strref_out) *bytes_per_strref_out = bytes_per_strref;

	uint32_t count = (uint32_t)pool_size / 4;
	if (count == 0 || count > 0x1000000)
	{
		free(pool_data);
		free(str_data);
		return NULL;
	}

	StringTable *st = ccalloc(1, sizeof(StringTable));
	st->count = count;
	st->strings = ccalloc(count, sizeof(char *));

	uint32_t offset = 0;
	uint32_t n = 1;
	for (uint32_t i = 1; i < count;)
	{
		uint16_t len = read2(pool_data + i * 4);
		uint16_t refs = read2(pool_data + i * 4 + 2);
		if (len == 0 && refs == 0)
		{
			i++;
			n++;
			continue;
		}
		if (len == 0)
		{
			if ((i + 1) * 4 + 4 <= pool_size)
			{
				uint32_t len_high = read2(pool_data + (i + 1) * 4 + 2);
				uint32_t len_low = read2(pool_data + (i + 1) * 4);
				len = (uint16_t)((len_high << 16) | len_low);
				i += 2;
			}
			else
			{
				i++;
			}
		}
		else
		{
			i += 1;
		}
		if (offset + len <= str_size && n < count)
		{
			st->strings[n] = cmalloc((size_t)len + 1);
			memcpy(st->strings[n], str_data + offset, len);
			st->strings[n][len] = 0;
			offset += len;
		}
		n++;
	}
	free(pool_data);
	free(str_data);
	return st;
}

static const char *st_get(StringTable *st, uint32_t i)
{
	if (!st || i >= st->count) return NULL;
	return st->strings[i];
}

static char *msi_get_filename(const char *msi_val)
{
	if (!msi_val) return NULL;
	const char *p = strchr(msi_val, '|');
	return str_dup(p ? p + 1 : msi_val);
}

typedef struct
{
	uint8_t *f_data, *d_data, *c_data, *med_data;
	size_t f_size, d_size, c_size, med_size;
	bool is_compressed;
	int s_sz;
	StringTable *st;
} MsiTables;

static void parse_directory_table(MsiTables *t, JSONObject *dir_map)
{
	uint32_t d_row_sz = 3 * t->s_sz;
	uint32_t d_N = (uint32_t)(t->d_size / d_row_sz);
	if (d_N == 0) return;

	for (int pass = 0; pass < 20; pass++)
	{
		for (uint32_t i = 0; i < d_N; i++)
		{
			uint32_t id_i, par_i, val_i;
			if (t->is_compressed)
			{
				id_i = (t->s_sz == 4) ? read4(t->d_data + (0 * d_N + i) * 4) : read2(t->d_data + (0 * d_N + i) * 2);
				par_i = (t->s_sz == 4) ? read4(t->d_data + (1 * d_N + i) * 4) : read2(t->d_data + (1 * d_N + i) * 2);
				val_i = (t->s_sz == 4) ? read4(t->d_data + (2 * d_N + i) * 4) : read2(t->d_data + (2 * d_N + i) * 2);
			}
			else
			{
				uint8_t *r = t->d_data + i * d_row_sz;
				id_i = (t->s_sz == 4) ? read4(r) : read2(r);
				par_i = (t->s_sz == 4) ? read4(r + t->s_sz) : read2(r + t->s_sz);
				val_i = (t->s_sz == 4) ? read4(r + 2 * t->s_sz) : read2(r + 2 * t->s_sz);
			}
			const char *id = st_get(t->st, id_i);
			const char *parent = st_get(t->st, par_i);
			const char *val = st_get(t->st, val_i);
			if (id && val)
			{
				char *name = msi_get_filename(val);
				if (par_i == 0 || (parent && strcmp(id, parent) == 0) || strcmp(id, "TARGETDIR") == 0 || strcmp(id, "SourceDir") == 0)
				{
					if (!json_map_get(dir_map, id)) json_map_set(dir_map, id, json_new_string("."));
				}
				else if (parent)
				{
					JSONObject *p_path = json_map_get(dir_map, parent);
					if (p_path)
					{
						if (!json_map_get(dir_map, id))
						{
							char *full = (strcmp(p_path->str, ".") == 0) ? str_dup(name) : str_printf("%s/%s", p_path->str, name);
							json_map_set(dir_map, id, json_new_string(full));
						}
					}
				}
			}
		}
	}
}

static void parse_component_table(MsiTables *t, JSONObject *dir_map, JSONObject *comp_map)
{
	uint32_t c_row_sz = (t->s_sz == 2) ? 12 : 20;
	uint32_t c_N = (uint32_t)(t->c_size / c_row_sz);
	if (c_N == 0) return;

	for (uint32_t i = 0; i < c_N; i++)
	{
		uint32_t id_i, dir_i;
		if (t->is_compressed)
		{
			id_i = (t->s_sz == 4) ? read4(t->c_data + (0 * c_N + i) * 4) : read2(t->c_data + (0 * c_N + i) * 2);
			dir_i = (t->s_sz == 4) ? read4(t->c_data + (2 * c_N + i) * 4) : read2(t->c_data + (2 * c_N + i) * 2);
		}
		else
		{
			uint8_t *r = t->c_data + i * c_row_sz;
			id_i = (t->s_sz == 4) ? read4(r) : read2(r);
			dir_i = (t->s_sz == 4) ? read4(r + 2 * t->s_sz) : read2(r + 2 * t->s_sz);
		}
		const char *id = st_get(t->st, id_i);
		const char *dir_id = st_get(t->st, dir_i);
		if (id && dir_id)
		{
			JSONObject *dir_path = json_map_get(dir_map, dir_id);
			if (dir_path) json_map_set(comp_map, id, json_new_string(dir_path->str));
		}
	}
}

static void parse_file_table(MsiTables *t, JSONObject *comp_map, JSONObject *name_map)
{
	uint32_t f_row_sz = (t->s_sz == 2) ? 20 : 32;
	uint32_t f_N = (uint32_t)(t->f_size / f_row_sz);
	if (f_N == 0) return;

	for (uint32_t i = 0; i < f_N; i++)
	{
		uint32_t id_idx, comp_idx, name_idx;
		if (t->is_compressed)
		{
			id_idx = (t->s_sz == 4) ? read4(t->f_data + (0 * f_N + i) * 4) : read2(t->f_data + (0 * f_N + i) * 2);
			comp_idx = (t->s_sz == 4) ? read4(t->f_data + (1 * f_N + i) * 4) : read2(t->f_data + (1 * f_N + i) * 2);
			name_idx = (t->s_sz == 4) ? read4(t->f_data + (2 * f_N + i) * 4) : read2(t->f_data + (2 * f_N + i) * 2);
		}
		else
		{
			uint8_t *r = t->f_data + i * f_row_sz;
			id_idx = (t->s_sz == 4) ? read4(r) : read2(r);
			comp_idx = (t->s_sz == 4) ? read4(r + t->s_sz) : read2(r + t->s_sz);
			name_idx = (t->s_sz == 4) ? read4(r + 2 * t->s_sz) : read2(r + 2 * t->s_sz);
		}
		const char *id = st_get(t->st, id_idx);
		const char *comp_id = st_get(t->st, comp_idx);
		const char *file_val = st_get(t->st, name_idx);
		if (id && comp_id && file_val)
		{
			char *fname = msi_get_filename(file_val);
			JSONObject *c_path = json_map_get(comp_map, comp_id);
			if (c_path)
			{
				char *full = str_printf("%s/%s", c_path->str, fname);
				json_map_set(name_map, id, json_new_string(full));
				const char *p = strchr(file_val, '|');
				if (p)
				{
					char *short_name = str_copy(file_val, p - file_val);
					json_map_set(name_map, short_name, json_new_string(full));
				}
			}
		}
	}
}

bool msi_extract(const char *msi_path, const char *out_root, const char *cab_dir, bool verbose)
{
	if (file_is_dir(msi_path)) return false;
	Ole2 *ole = ole2_open(msi_path);
	if (!ole) return false;

	StreamInfo *streams = cmalloc(8192 * sizeof(StreamInfo));
	if (!streams)
	{
		ole2_free(ole);
		return false;
	}
	int stream_count = msi_find_streams(ole, streams, 8192);

	int bytes_per_strref = 2;
	StringTable *st = msi_load_string_table(ole, streams, stream_count, &bytes_per_strref);
	if (!st)
	{
		free(streams);
		ole2_free(ole);
		return false;
	}

	MsiTables t = {0};
	t.s_sz = bytes_per_strref;
	t.st = st;
	JSONObject *name_map = json_new_object(J_OBJECT);

	for (int i = 0; i < stream_count; i++)
	{
		const char *name = streams[i].name;
		const char *pname = name;
		if (*pname == '@' || *pname == '!')
		{
			t.is_compressed = (*pname == '@');
			pname++;
		}
		while (*pname && !isalnum(*pname) && *pname != '_')
			pname++;

		if (strcmp(pname, "File") == 0) t.f_data = ole2_read_stream(ole, streams[i].first_sector, streams[i].size, &t.f_size);
		else if (strcmp(pname, "Directory") == 0) t.d_data = ole2_read_stream(ole, streams[i].first_sector, streams[i].size, &t.d_size);
		else if (strcmp(pname, "Component") == 0) t.c_data = ole2_read_stream(ole, streams[i].first_sector, streams[i].size, &t.c_size);
		else if (strcmp(pname, "Media") == 0) t.med_data = ole2_read_stream(ole, streams[i].first_sector, streams[i].size, &t.med_size);
	}

	if (t.f_data && t.d_data && t.c_data)
	{
		JSONObject *dir_map = json_new_object(J_OBJECT);
		parse_directory_table(&t, dir_map);

		JSONObject *comp_map = json_new_object(J_OBJECT);
		parse_component_table(&t, dir_map, comp_map);

		parse_file_table(&t, comp_map, name_map);
	}

	if (t.med_data && cab_dir)
	{
		uint32_t m_row_sz = (t.s_sz == 2) ? 14 : 26;
		uint32_t m_N = (uint32_t)(t.med_size / m_row_sz);
		for (uint32_t i = 0; i < m_N; i++)
		{
			uint32_t cab_i;
			if (t.is_compressed)
			{
				cab_i = (t.s_sz == 4) ? read4(t.med_data + (10 * m_N + i * 4)) : read2(t.med_data + (8 * m_N + i * 2));
			}
			else
			{
				cab_i = (t.s_sz == 4) ? read4(t.med_data + i * m_row_sz + 12) : read2(t.med_data + i * m_row_sz + 8);
			}
			const char *cab_name = st_get(st, cab_i);
			if (cab_name && cab_name[0] != '#')
			{
				char *cp = (char *)file_append_path(cab_dir, cab_name);
				if (!file_exists(cp))
				{
					char *with_ext = str_printf("%s.cab", cp);
					if (file_exists(with_ext))
					{
						cp = with_ext;
					}
				}
				if (file_exists(cp))
				{
					if (verbose) printf("    Extracting external CAB: %s\n", cab_name);
					cab_extract(cp, out_root, name_map, verbose);
				}
			}
		}
	}

	for (int i = 0; i < stream_count; i++)
	{
		size_t s_size;
		uint8_t *s_data = ole2_read_stream(ole, streams[i].first_sector, streams[i].size, &s_size);
		if (s_data && s_size > 4 && memcmp(s_data, "MSCF", 4) == 0)
		{
			if (verbose) printf("    Extracting embedded CAB from stream: %s\n", streams[i].name);
			cab_extract_buffer(s_data, s_size, out_root, name_map, verbose);
		}
		free(s_data);
	}

	free(t.f_data);
	free(t.d_data);
	free(t.c_data);
	free(t.med_data);
	free_string_table(st);
	free(streams);
	ole2_free(ole);
	return true;
}
