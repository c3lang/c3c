<*
 ZIP archive module for reading and writing ZIP files.
 Supports STORE and DEFLATE compression methods.
*>
module std::compression::zip;
import std::core::mem, std::io, std::collections::list, std::hash::crc32, std::time, std::math;
import std::compression::deflate;

faultdef
	INVALID_ARGUMENT,
	IO_ERROR,
	CORRUPTED_DATA,
	ENTRY_NOT_FOUND;

struct ZipEntry
{
	String name;
	ulong uncompressed_size;
	ulong compressed_size;
	bool is_directory;
	uint crc32;
	ulong offset;
	ushort method;
}

alias ZipEntryList = List{ZipEntry};

struct ZipArchive
{
	File* file;
	Allocator allocator;
	ZipEntryList entries;
	String mode;
	String path;
}

struct ZipLFH @packed
{
	uint signature;
	ushort version_needed;
	ushort flags;
	ushort method;
	ushort last_mod_time;
	ushort last_mod_date;
	uint crc32;
	uint compressed_size;
	uint uncompressed_size;
	ushort filename_len;
	ushort extra_field_len;
}

struct ZipCDH @packed
{
	uint signature;
	ushort version_made_by;
	ushort version_needed;
	ushort flags;
	ushort method;
	ushort last_mod_time;
	ushort last_mod_date;
	uint crc32;
	uint compressed_size;
	uint uncompressed_size;
	ushort filename_len;
	ushort extra_field_len;
	ushort comment_len;
	ushort disk_number_start;
	ushort internal_attr;
	uint external_attr;
	uint relative_offset;
}

struct ZipEOCD @packed
{
	uint signature;
	ushort disk_number;
	ushort cd_disk_number;
	ushort num_entries_this_disk;
	ushort num_entries;
	uint cd_size;
	uint cd_offset;
	ushort comment_len;
}

const uint ZIP_LFH_SIG = 0x04034B50;
const uint ZIP_CDH_SIG = 0x02014B50;
const uint ZIP_EOCD_SIG = 0x06054B50;

const ushort ZIP_METHOD_STORE = 0;
const ushort ZIP_METHOD_DEFLATE = 8;

fn char[]? decompress(char[] compressed, Allocator allocator = mem) @private
{
	return deflate::inflate(compressed, allocator);
}

<*
 Opens a ZIP archive.
 @param path : `The path to the ZIP file.`
 @param mode : `Opening mode ("r", "w", "w+").`
 @param allocator : `The allocator to use.`
 @return `The opened archive.`
 @require mode == "r" || mode == "w" || mode == "w+"
*>
fn ZipArchive? open(String path, String mode = "r", Allocator allocator = mem)
{
	if (mode == "w" || mode == "w+")
	{
		File f = file::open(path, mode == "w+" ? "w+" : "w+b")!;

		ZipArchive archive;
		archive.allocator = allocator;
		archive.file = allocator::new(allocator, File, f);
		archive.entries.init(allocator);
		archive.path = String.copy(path, allocator);
		archive.mode = String.copy(mode, allocator);
		return archive;
	}
	else if (mode == "r")
	{
		File f = file::open(path, "rb")!;
		defer (catch err) (void)f.close();

		f.seek(0, Seek.END)!;
		usz file_size = (usz)f.seek(0, Seek.CURSOR)!;

		if (file_size < ZipEOCD.sizeof) return CORRUPTED_DATA~;

		f.seek((isz)(file_size - ZipEOCD.sizeof), Seek.SET)!;
		ZipEOCD eocd;
		if (f.read(((char*)&eocd)[:ZipEOCD.sizeof])! != ZipEOCD.sizeof || eocd.signature != ZIP_EOCD_SIG)
		{
			return CORRUPTED_DATA~;
		}

		ZipArchive archive;
		archive.allocator = allocator;
		archive.file = allocator::new(allocator, File, f);
		archive.entries.init(allocator);
		archive.path = String.copy(path, allocator);
		archive.mode = String.copy(mode, allocator);
		defer (catch err) (void)archive.close();

		archive.file.seek((isz)eocd.cd_offset, Seek.SET)!;

		for (ushort i = 0; i < eocd.num_entries; i++)
		{
			ZipCDH cdh;
			if (archive.file.read(((char*)&cdh)[:ZipCDH.sizeof])! != ZipCDH.sizeof) break;
			if (cdh.signature != ZIP_CDH_SIG) break;

			char[] name = allocator::malloc(allocator, cdh.filename_len)[:cdh.filename_len];
			if (archive.file.read(name)! != (usz)cdh.filename_len)
			{
				allocator::free(allocator, name.ptr);
				break;
			}

			(void)archive.file.seek((isz)(cdh.extra_field_len + cdh.comment_len), Seek.CURSOR);

			ZipEntry entry;
			entry.name = (String)name;
			entry.uncompressed_size = cdh.uncompressed_size;
			entry.compressed_size = cdh.compressed_size;
			entry.crc32 = cdh.crc32;
			entry.offset = cdh.relative_offset;
			entry.method = cdh.method;
			entry.is_directory = entry.name.ends_with("/");

			archive.entries.push(entry);
		}

		return archive;
	}
	return INVALID_ARGUMENT~;
}

<*
 Closes the ZIP archive, writing the central directory if in write mode.
*>
fn void? ZipArchive.close(&self)
{
	defer {
		(void)self.file.close();

		for (usz i = 0; i < self.entries.len(); i++)
		{
			allocator::free(self.allocator, self.entries.get(i).name.ptr);
		}
		self.entries.free();
		allocator::free(self.allocator, self.mode.ptr);
		allocator::free(self.allocator, self.path.ptr);
		if (self.file) allocator::free(self.allocator, self.file);
	}

	if (self.mode.starts_with("w"))
	{
		self.file.flush()!;
		ulong cd_offset = (ulong)self.file.seek(0, Seek.CURSOR)!;
		uint cd_size = 0;

		for (usz i = 0; i < self.entries.len(); i++)
		{
			ZipEntry* entry = self.entries.get_ref(i);
			ZipCDH cdh;
			mem::set(&cdh, 0, ZipCDH.sizeof);
			cdh.signature = ZIP_CDH_SIG;
			cdh.version_made_by = 20;
			cdh.version_needed = 20;
			cdh.method = entry.method;
			cdh.crc32 = entry.crc32;
			cdh.compressed_size = (uint)entry.compressed_size;
			cdh.uncompressed_size = (uint)entry.uncompressed_size;
			cdh.filename_len = (ushort)entry.name.len;
			cdh.relative_offset = (uint)entry.offset;

			self.file.write(((char*)&cdh)[:ZipCDH.sizeof])!;
			self.file.write(entry.name.ptr[:entry.name.len])!;
			cd_size += (uint)(ZipCDH.sizeof + entry.name.len);
		}

		ZipEOCD eocd;
		mem::set(&eocd, 0, ZipEOCD.sizeof);
		eocd.signature = ZIP_EOCD_SIG;
		eocd.num_entries_this_disk = (ushort)self.entries.len();
		eocd.num_entries = (ushort)self.entries.len();
		eocd.cd_size = cd_size;
		eocd.cd_offset = (uint)cd_offset;

		self.file.write(((char*)&eocd)[:ZipEOCD.sizeof])!;
	}
}

<*
 @return `The number of entries in the archive.`
*>
fn usz ZipArchive.count(&self)
{
	return self.entries.len();
}

<*
 Returns metadata for the entry at the given index.
 @require index < self.count()
*>
fn ZipEntry? ZipArchive.stat_at(&self, usz index)
{
	if (index >= self.entries.len()) return ENTRY_NOT_FOUND~;
	return self.entries.get(index);
}

<*
 Returns metadata for the entry with the given filename.
*>
fn ZipEntry? ZipArchive.stat(&self, String filename)
{
	for (usz i = 0; i < self.entries.len(); i++)
	{
		ZipEntry entry = self.entries.get(i);
		if (entry.name == filename) return entry;
	}
	return ENTRY_NOT_FOUND~;
}

struct ZipEntryReader (InStream)
{
	ZipArchive* archive;
	ulong start_offset;
	ulong size;
	ulong pos;
	ushort method;
}

fn usz? ZipEntryReader.read(&self, char[] buffer) @dynamic
{
	if (self.pos >= self.size) return io::EOF~;
	if (self.method != ZIP_METHOD_STORE) return io::UNSUPPORTED_OPERATION~;

	usz to_read = (usz)math::min((ulong)buffer.len, self.size - self.pos);
	if (to_read == 0) return 0;

	self.archive.file.flush()!;
	self.archive.file.seek((isz)(self.start_offset + self.pos), Seek.SET)!;
	usz n = self.archive.file.read(buffer[:to_read])!;
	self.pos += n;
	return n;
}

fn char? ZipEntryReader.read_byte(&self) @dynamic
{
	char[1] b;
	usz? n = self.read(&b);
	if (@ok(n))
	{
		if (n! == 0) return io::EOF~;
		return b[0];
	}
	if (catch err = n) return err~;
	return io::EOF~;
}

fn usz ZipEntryReader.len(&self) @dynamic
{
	return (usz)self.size;
}

fn usz? ZipEntryReader.available(&self) @dynamic
{
	return (usz)(self.size - self.pos);
}

fn usz? ZipEntryReader.seek(&self, isz offset, Seek seek) @dynamic
{
	switch (seek)
	{
		case SET:
			self.pos = (ulong)offset;
		case CURSOR:
			self.pos = (ulong)((isz)self.pos + offset);
		case END:
			self.pos = (ulong)((isz)self.size + offset);
	}
	if (self.pos > self.size) self.pos = self.size;
	return (usz)self.pos;
}

<*
 Opens a reader for an entry.
 @param filename : `The name of the file to read.`
 @return `A reader for the entry's data.`
*>
fn ZipEntryReader? ZipArchive.open_reader(&self, String filename)
{
	ZipEntry? opt = self.stat(filename);
	if (!@ok(opt)) return ENTRY_NOT_FOUND~;
	ZipEntry entry = opt!;

	self.file.flush()!;
	self.file.seek((isz)entry.offset, Seek.SET)!;
	ZipLFH lfh;
	self.file.read(((char*)&lfh)[:ZipLFH.sizeof])!;
	if (lfh.signature != ZIP_LFH_SIG) return CORRUPTED_DATA~;

	ZipEntryReader reader;
	reader.archive = (ZipArchive*)self;
	reader.start_offset = entry.offset + ZipLFH.sizeof + lfh.filename_len + lfh.extra_field_len;
	reader.size = entry.uncompressed_size;
	reader.pos = 0;
	reader.method = entry.method;

	return reader;
}

struct ZipEntryWriter (OutStream)
{
	ZipArchive* archive;
	ZipEntry entry;
	Crc32 crc;
	ZipLFH lfh;
	ulong lfh_offset;
	char[] buffer;
}

fn usz? ZipEntryWriter.write(&self, char[] bytes) @dynamic
{
	self.crc.update(bytes);
	self.entry.uncompressed_size += (ulong)bytes.len;

	if (self.entry.method == ZIP_METHOD_STORE)
	{
		usz n = self.archive.file.write(bytes)!;
		self.entry.compressed_size += n;
		return n;
	}
	else
	{
		usz new_len = self.buffer.len + bytes.len;
		char* p = allocator::realloc_array(self.archive.allocator, self.buffer.ptr, char, new_len);
		mem::copy(p + self.buffer.len, bytes.ptr, bytes.len);
		self.buffer = p[:new_len];
		return bytes.len;
	}
}

fn void? ZipEntryWriter.write_byte(&self, char c) @dynamic
{
	char[1] b = { c };
	(void)self.write(&b)!;
}

fn void? ZipEntryWriter.close(&self) @dynamic
{
	if (self.entry.method == ZIP_METHOD_DEFLATE)
	{
		if (self.buffer.len > 0)
		{
			char[]? compressed = deflate::compress(self.buffer, self.archive.allocator);
			if (!@ok(compressed)) return IO_ERROR~;
			char[] data = compressed!;
			self.archive.file.write(data)!;
			self.entry.compressed_size = (ulong)data.len;

			allocator::free(self.archive.allocator, data.ptr);
			allocator::free(self.archive.allocator, self.buffer.ptr);
			self.buffer = {};
		}
	}

	self.entry.crc32 = ~self.crc.result;

	self.lfh.crc32 = self.entry.crc32;
	self.lfh.compressed_size = (uint)self.entry.compressed_size;
	self.lfh.uncompressed_size = (uint)self.entry.uncompressed_size;

	ulong end_pos = (ulong)self.archive.file.seek(0, Seek.CURSOR)!;

	self.archive.file.seek((isz)self.lfh_offset, Seek.SET)!;
	self.archive.file.write(((char*)&self.lfh)[:ZipLFH.sizeof])!;

	self.archive.file.seek((isz)end_pos, Seek.SET)!;

	self.archive.entries.push(self.entry);
}

<*
 Opens a writer for a new entry.
 @param filename : `The name of the file to create in the archive.`
 @param method : `Compression method.`
 @return `A writer for the new entry.`
 @require method == ZIP_METHOD_STORE || method == ZIP_METHOD_DEFLATE
*>
fn ZipEntryWriter? ZipArchive.open_writer(&self, String filename, ushort method = ZIP_METHOD_STORE)
{
	if (!self.mode.starts_with("w")) return IO_ERROR~;

	ZipEntryWriter writer;
	writer.archive = (ZipArchive*)self;
	writer.entry.name = String.copy(filename, self.allocator);
	defer (catch err) allocator::free(self.allocator, writer.entry.name.ptr);

	writer.entry.method = method;
	writer.entry.offset = (ulong)self.file.seek(0, Seek.CURSOR)!;
	writer.entry.uncompressed_size = 0;
	writer.entry.compressed_size = 0;
	writer.crc.init();
	writer.lfh_offset = writer.entry.offset;

	mem::set(&writer.lfh, 0, ZipLFH.sizeof);
	writer.lfh.signature = ZIP_LFH_SIG;
	writer.lfh.version_needed = 20;
	writer.lfh.method = method;
	writer.lfh.filename_len = (ushort)filename.len;

	Time now = time::now();
	writer.lfh.last_mod_time = time_to_dos_time(now);
	writer.lfh.last_mod_date = time_to_dos_date(now);

	self.file.write(((char*)&writer.lfh)[:ZipLFH.sizeof])!;
	self.file.write(filename.ptr[:filename.len])!;

	writer.buffer = {};

	return writer;
}

<*
 Reads an entire file from the archive.
 @param filename : `The name of the file to read.`
 @param allocator : `The allocator to use.`
 @return `The uncompressed file data.`
*>
fn char[]? ZipArchive.read_file(&self, String filename, Allocator allocator = mem)
{
	ZipEntryReader reader = self.open_reader(filename)!;
	ZipEntry entry = self.stat(filename)!;

	char[] data;
	if (reader.method == ZIP_METHOD_STORE)
	{
		data = allocator::malloc(allocator, (usz)reader.size)[:(usz)reader.size];
	}
	else
	{
		self.file.seek((isz)reader.start_offset, Seek.SET)!;
		char[] compressed = allocator::malloc(allocator, (usz)entry.compressed_size)[:(usz)entry.compressed_size];
		defer allocator::free(allocator, compressed.ptr);
		self.file.read(compressed)!;
		data = decompress(compressed, allocator)!;
	}
	defer (catch err) allocator::free(allocator, data.ptr);

	if (reader.method == ZIP_METHOD_STORE)
	{
		reader.read(data)!;
	}

	Crc32 crc;
	crc.init();
	crc.update(data);
	if (~crc.result != entry.crc32) return CORRUPTED_DATA~;

	return data;
}

<*
 Writes an entire file to the archive.
 @param filename : `The name of the file to create.`
 @param data : `The data to write.`
 @param method : `Compression method.`
*>
fn void? ZipArchive.write_file(&self, String filename, char[] data, ushort method = ZIP_METHOD_DEFLATE)
{
	ZipEntryWriter? writer_opt = self.open_writer(filename);
	if (catch err = writer_opt) return err~;
	ZipEntryWriter writer = writer_opt;

	writer.write(data)!;
	writer.close()!;
}

<*
 Adds a directory entry to the archive.
*>
fn void? ZipArchive.add_directory(&self, String dirname)
{
	if (!self.mode.starts_with("w")) return IO_ERROR~;

	String dir_name;
	if (dirname.ends_with("/"))
	{
		dir_name = dirname;
	}
	else
	{
		usz len = dirname.len + 1;
		char* buf = allocator::malloc(self.allocator, len);
		mem::copy(buf, dirname.ptr, dirname.len);
		buf[dirname.len] = '/';
		dir_name = (String)buf[:len];
	}
	defer if (dir_name.ptr != dirname.ptr) allocator::free(self.allocator, dir_name.ptr);

	ZipEntry entry;
	entry.name = String.copy(dir_name, self.allocator);
	defer (catch err) allocator::free(self.allocator, entry.name.ptr);

	entry.method = ZIP_METHOD_STORE;
	entry.offset = (ulong)self.file.seek(0, Seek.CURSOR)!;
	entry.uncompressed_size = 0;
	entry.compressed_size = 0;
	entry.crc32 = 0;
	entry.is_directory = true;

	ZipLFH lfh;
	mem::set(&lfh, 0, ZipLFH.sizeof);
	lfh.signature = ZIP_LFH_SIG;
	lfh.version_needed = 20;
	lfh.method = ZIP_METHOD_STORE;
	lfh.filename_len = (ushort)dir_name.len;

	Time now = time::now();
	lfh.last_mod_time = time_to_dos_time(now);
	lfh.last_mod_date = time_to_dos_date(now);

	self.file.write(((char*)&lfh)[:ZipLFH.sizeof])!;
	self.file.write(dir_name.ptr[:dir_name.len])!;

	self.entries.push(entry);
}

fn ushort time_to_dos_time(Time t) @private
{
	long seconds = (long)t / 1_000_000;
	int s = (int)(seconds % 60);
	int m = (int)((seconds / 60) % 60);
	int h = (int)((seconds / 3600) % 24);
	return (ushort)((h << 11) | (m << 5) | (s / 2));
}

fn ushort time_to_dos_date(Time t) @private
{
	long seconds = (long)t / 1_000_000;
	long days = seconds / 86400;

	if (days < 3652) return (ushort)((0 << 9) | (1 << 5) | 1); // 1980-01-01

	days -= 3652;
	int year = 0;
	while (true)
	{
		int y = 1980 + year;
		bool is_leap = (y % 4 == 0 && y % 100 != 0) || (y % 400 == 0);
		int year_days = is_leap ? 366 : 365;
		if (days < (long)year_days) break;
		days -= (long)year_days;
		year++;
	}

	int[12] month_days = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
	int y = 1980 + year;
	if ((y % 4 == 0 && y % 100 != 0) || (y % 400 == 0)) month_days[1] = 29;

	int month = 0;
	while (days >= (long)month_days[month])
	{
		days -= (long)month_days[month];
		month++;
	}

	int day = (int)days + 1;
	month++;
	return (ushort)((year << 9) | (month << 5) | day);
}
