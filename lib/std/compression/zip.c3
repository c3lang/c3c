<*
 ZIP archive module (STORE and DEFLATE).

 API:
 - fn ZipArchive? open(Allocator allocator, String path, String mode = "r")
 - fn ZipArchive? recover(Allocator allocator, String path, )
 - fn void?       ZipArchive.extract(&self, String output_dir)
 - fn ZipEntry?   ZipArchive.stat(&self, String filename)
 - fn char[]?     ZipArchive.read_file_all(&self, Allocator allocator, String filename)
 - fn void?       ZipArchive.close(&self)
*>
module std::compression::zip;
import std::io, std::collections::list, std::hash::crc32, std::time, std::math;
import std::encoding::codepage, std::compression::deflate;
import std::crypto::aes, std::hash::sha1;
import libc;

faultdef
	INVALID_ARGUMENT,
	IO_ERROR,
	CORRUPTED_DATA,
	ENTRY_NOT_FOUND,
	ENCRYPTED_FILE,
	WRONG_PASSWORD;

<*
 Describes a single entry within a ZIP archive.
*>
struct ZipEntry
{
	String name;
	ulong uncompressed_size;
	ulong compressed_size;
	bool is_directory;
	bool is_encrypted;
	uint crc32;
	ulong offset;
	ZipMethod method;
	ushort last_mod_time;
	ushort last_mod_date;
}

fn Time ZipEntry.time(&self) => dos_date_time_to_time(self.last_mod_date, self.last_mod_time);

alias ZipEntryList = List{ZipEntry};

struct ZipArchive
{
	File* file;
	Allocator allocator;
	ZipEntryList entries;
	String mode;
	String path;
	String comment;
}

constdef ZipMethod : UShortLE
{
	STORE = {0},
	DEFLATE = {8},
	AEX = {99},
}

<*
 Opens a ZIP archive.
 @param allocator : `The allocator to use.`
 @param path : `The path to the ZIP file.`
 @param mode : `Opening mode ("r", "w", "w+").`
 @return `The opened archive.`
 @require mode == "r" || mode == "w" || mode == "w+"
*>
fn ZipArchive? open(Allocator allocator, String path, String mode = "r")
{
	if (mode == "w" || mode == "w+")
	{
		File f = file::open(path, mode == "w+" ? "w+" : "w+b")!;

		ZipArchive archive;
		archive.allocator = allocator;
		archive.file = allocator::new(allocator, File, f);
		archive.entries.init(allocator);
		archive.path = path.copy(allocator);
		archive.mode = mode.copy(allocator);
		return archive;
	}
	if (mode != "r") return INVALID_ARGUMENT~;

	File f = file::open(path, "rb")!;
	defer (catch err) (void)f.close();

	ulong file_size = f.size()!!;

	if (file_size < ZipEOCD.sizeof) return CORRUPTED_DATA~;

	ulong search_start = file_size > (ulong)(ZipEOCD.sizeof + 65535)
		? file_size - (ulong)(ZipEOCD.sizeof + 65535)
		: 0;

	ZipEOCD eocd;
	bool found = false;

	for (ulong pos = file_size - (ulong)ZipEOCD.sizeof; pos >= search_start; pos--)
	{
		if (pos > (ulong)isz.max) return io::OVERFLOW~;
		f.set_cursor(pos)!;
		UIntLE sig;
		if (io::read_any(&f, &sig)! != 4) break;

		if (sig.val == ZIP_EOCD_SIG)
		{
			f.set_cursor(pos)!;
			if (io::read_any(&f, &eocd)! == ZipEOCD.sizeof)
			{
				ulong expected_end = pos + ZipEOCD.sizeof + eocd.comment_len.val;
				if (expected_end == file_size)
				{
					found = true;
					break;
				}
			}
		}
		if (pos == 0) break;
	}

	if (!found) return CORRUPTED_DATA~;

	ZipArchive archive;
	archive.allocator = allocator;
	archive.file = allocator::new(allocator, File, f);
	archive.entries.init(allocator);
	archive.path = path.copy(allocator);
	archive.mode = mode.copy(allocator);

	if (eocd.comment_len.val > 0)
	{
		char[] comment_data = allocator::alloc_array(allocator, char, (usz)eocd.comment_len.val);
		defer allocator::free(allocator, comment_data);
		if (archive.file.read(comment_data)! == (usz)eocd.comment_len.val)
		{
			archive.comment = codepage::decode(allocator, comment_data, CP437)!!;
		}
	}

	defer catch (void)archive.close();

	if (eocd.cd_offset.val > (uint)isz.max) return io::OVERFLOW~;
	archive.file.set_cursor(eocd.cd_offset.val)!;

	usz num_entries = eocd.num_entries.val;


	// ZIP64 check
	if (eocd.num_entries.val == 0xFFFF || eocd.cd_offset.val == 0xFFFFFFFF)
	{
		isz locator_pos = (isz)file_size - ZipEOCD.sizeof - Zip64Locator.sizeof;
		if (locator_pos >= 0)
		{
			archive.file.set_cursor(locator_pos)!;
			Zip64Locator locator;
			if (try n = archive.file.read(((char*)&locator)[:Zip64Locator.sizeof]))
			{
				if (n == Zip64Locator.sizeof && locator.signature.val == ZIP64_LOCATOR_SIG)
				{
					if (locator.offset_eocd.val > (ulong)isz.max) return io::OVERFLOW~;
					archive.file.set_cursor(locator.offset_eocd.val)!;
					Zip64EOCD eocd64;
					io::read_any(archive.file, &eocd64)!;
					if (eocd64.signature.val == ZIP64_EOCD_SIG)
					{
						if (eocd64.offset_cd.val > (ulong)isz.max) return io::OVERFLOW~;
						archive.file.set_cursor(eocd64.offset_cd.val)!!;
						num_entries = (usz)eocd64.count_total.val;
					}
				}
			}
		}
	}

	for (usz i = 0; i < num_entries; i++)
	{
		ZipCDH cdh;
		if (io::read_any(archive.file, &cdh)! != ZipCDH.sizeof) break;
		if (cdh.signature.val != ZIP_CDH_SIG) break;

		char[] raw_name = allocator::alloc_array(allocator, char, cdh.filename_len.val);
		if (archive.file.read(raw_name)! != (usz)cdh.filename_len.val)
		{
			allocator::free(allocator, raw_name);
			break;
		}

		String name;
		bool is_utf8 = (cdh.flags.val & 0x0800) != 0;
		if (is_utf8 || is_valid_utf8(raw_name))
		{
			name = (String)raw_name;
		}
		else
		{
			name = (String)codepage::decode(allocator, raw_name, CP437)!!;
			allocator::free(allocator, raw_name.ptr);
		}

		char[] extra_field;
		if (cdh.extra_field_len.val > 0)
		{
			extra_field = allocator::alloc_array(allocator, char, cdh.extra_field_len.val);
			archive.file.read(extra_field)!;
		}

		archive.file.set_cursor(cdh.comment_len.val, FROM_CURSOR)!;

		ulong uncompressed_size = cdh.uncompressed_size.val;
		ulong compressed_size = cdh.compressed_size.val;
		ulong offset = cdh.relative_offset.val;

		if (cdh.uncompressed_size.val == 0xFFFFFFFF || cdh.compressed_size.val == 0xFFFFFFFF || cdh.relative_offset.val == 0xFFFFFFFF)
		{
			ByteReader reader = { .bytes = extra_field };
			while (reader.available()! >= 4)
			{
				ushort id = io::read_le_ushort(&reader)!;
				ushort size = io::read_le_ushort(&reader)!;

				if (id == ZIP64_EXTRA_ID)
				{
					int remaining = size;
					if (cdh.uncompressed_size.val == 0xFFFFFFFF && remaining >= 8)
					{
						uncompressed_size = io::read_le_ulong(&reader)!;
						remaining -= 8;
					}
					if (cdh.compressed_size.val == 0xFFFFFFFF && remaining >= 8)
					{
						compressed_size = io::read_le_ulong(&reader)!;
						remaining -= 8;
					}
					if (cdh.relative_offset.val == 0xFFFFFFFF && remaining >= 8)
					{
						offset = io::read_le_ulong(&reader)!;
						remaining -= 8;
					}
					break;
				}
				reader.set_cursor(size, FROM_CURSOR)!;
			}
		}
		if (extra_field.len > 0) allocator::free(allocator, extra_field);

		bool is_directory = name.ends_with("/") || name.ends_with("\\");
		if (!is_directory)
		{
			ushort host_system = cdh.version_made_by.val >> 8;
			if (host_system == 0 || host_system == 10) // MS-DOS or NTFS
			{
				if ((cdh.external_attr.val & 0x10) != 0) is_directory = true;
			}
			else if (host_system == 3) // Unix
			{
				if (((cdh.external_attr.val >> 16) & 0x4000) != 0) is_directory = true;
			}
		}

		ZipEntry entry = {
			.name = name,
			.uncompressed_size = uncompressed_size,
			.compressed_size = compressed_size,
			.crc32 = cdh.crc32.val,
			.offset = offset,
			.method = cdh.method,
			.last_mod_time = cdh.last_mod_time.val,
			.last_mod_date = cdh.last_mod_date.val,
			.is_directory = is_directory,
			.is_encrypted = (cdh.flags.val & 1) != 0
		};

		archive.entries.push(entry);
	}

	return archive;
}

<*
 Recovers a ZIP archive by scanning for Local File Headers
 when the Central Directory is missing or corrupted.
 @param path : `The path to the broken ZIP file.`
 @param allocator : `The allocator to use.`
 @return `The recovered archive.`
*>
fn ZipArchive? recover(Allocator allocator, String path)
{
	File f = file::open(path, "rb")!;
	defer (catch err) (void)f.close();

	ZipArchive archive = {
		.allocator = allocator,
		.file = allocator::new(allocator, File, f),
		.path = path.copy(allocator),
		.mode = "r".copy(allocator)
	};
	archive.entries.init(allocator);
	defer (catch err) (void)archive.close();

	char[4] sig_buf;
	while (true)
	{
		long offset = archive.file.cursor()!!;
		usz n = archive.file.read(sig_buf[..])!;
		if (n < 4) break;

		if (bitorder::read(sig_buf, UIntLE) != ZIP_LFH_SIG)
		{
			archive.file.set_cursor(-3, FROM_CURSOR)!!;
			continue;
		}

		ZipLFH lfh;
		if (archive.file.read(((char*)&lfh.version_needed)[:ZipLFH.sizeof - 4])! != ZipLFH.sizeof - 4) break;
		lfh.signature.val = ZIP_LFH_SIG;

		char[] raw_name = allocator::alloc_array(allocator, char, lfh.filename_len.val);
		if (archive.file.read(raw_name)! != (usz)lfh.filename_len.val)
		{
			allocator::free(allocator, raw_name);
			break;
		}

		String name;
		if ((lfh.flags.val & 0x0800) != 0 || is_valid_utf8(raw_name))
		{
			name = (String)raw_name;
		}
		else
		{
			name = (String)codepage::decode(allocator, raw_name, CP437)!!;
			allocator::free(allocator, raw_name.ptr);
		}

		archive.file.set_cursor(lfh.extra_field_len.val, FROM_CURSOR)!!;

		ZipEntry entry = {
			.name = name,
			.compressed_size = lfh.compressed_size.val,
			.uncompressed_size = lfh.uncompressed_size.val,
			.crc32 = lfh.crc32.val,
			.offset = offset,
			.method = lfh.method,
			.last_mod_time = lfh.last_mod_time.val,
			.last_mod_date = lfh.last_mod_date.val,
			.is_directory = name.ends_with("/") || name.ends_with("\\"),
			.is_encrypted = (lfh.flags.val & 1) != 0
		};

		archive.entries.push(entry);

		if (lfh.compressed_size.val > 0 && (ulong)lfh.compressed_size.val > (ulong)isz.max) return io::OVERFLOW~;
		archive.file.set_cursor(lfh.compressed_size.val, FROM_CURSOR)!!;
	}

	if (archive.entries.len() == 0) return CORRUPTED_DATA~;
	return archive;
}

<*
 Closes the ZIP archive, writing the central directory if in write mode.
*>
fn void? ZipArchive.close(&self)
{
	defer {
		(void)self.file.close();

		foreach (&entry : self.entries)
		{
			allocator::free(self.allocator, entry.name);
		}
		self.entries.free();
		allocator::free(self.allocator, self.mode);
		allocator::free(self.allocator, self.path);
		if (self.comment.len > 0) allocator::free(self.allocator, self.comment);
		if (self.file) allocator::free(self.allocator, self.file);
	}

	if (self.mode.starts_with("w"))
	{
		self.file.flush()!;
		ulong cd_offset = self.file.cursor()!;
		ulong cd_size = 0;

		for (usz i = 0; i < self.entries.len(); i++)
		{
			ZipEntry* entry = self.entries.get_ref(i);
			ZipCDH cdh = {
				.signature.val = ZIP_CDH_SIG,
				.version_made_by.val = 45, // 4.5 for ZIP64
				.version_needed.val = 45,
				.method = entry.method,
				.last_mod_time.val = entry.last_mod_time,
				.last_mod_date.val = entry.last_mod_date,
				.crc32.val = entry.crc32,
				.filename_len.val = (ushort)entry.name.len,
			};

			bool is_zip64 = entry.uncompressed_size >= 0xFFFFFFFF || entry.compressed_size >= 0xFFFFFFFF || entry.offset >= 0xFFFFFFFF;

			char[] extra_data;
			if (is_zip64)
			{
				cdh.compressed_size.val = 0xFFFFFFFF;
				cdh.uncompressed_size.val = 0xFFFFFFFF;
				cdh.relative_offset.val = 0xFFFFFFFF;

				// Header(4) + Uncomp(8) + Comp(8) + Offset(8)
				ushort extra_size = 28;
				extra_data = allocator::alloc_array(self.allocator, char, extra_size);
				bitorder::write(ZIP64_EXTRA_ID, extra_data[:2], UShortLE);
				bitorder::write((ushort)(extra_size - 4), extra_data[2:2], UShortLE);
				bitorder::write(entry.uncompressed_size, extra_data[4:8], ULongLE);
				bitorder::write(entry.compressed_size, extra_data[12:8], ULongLE);
				bitorder::write(entry.offset, extra_data[20:8], ULongLE);

				cdh.extra_field_len.val = extra_size;
			}
			else
			{
				cdh.compressed_size.val = (uint)entry.compressed_size;
				cdh.uncompressed_size.val = (uint)entry.uncompressed_size;
				cdh.relative_offset.val = (uint)entry.offset;
			}

			// Set external attributes (MS-DOS compatibility).
			// 0x10 is the DOS directory attribute.
			cdh.external_attr.val = (uint)(entry.is_directory ? 0x10 : 0);

			io::write_any(self.file, &cdh)!;
			self.file.write(entry.name)!;
			if (is_zip64)
			{
				self.file.write(extra_data)!;
				allocator::free(self.allocator, extra_data);
			}

			ulong entry_record_size = (ulong)(ZipCDH.sizeof + entry.name.len + cdh.extra_field_len.val);
			if (cd_size > (ulong.max - entry_record_size)) return io::OVERFLOW~;
			cd_size += entry_record_size;
		}

		bool cd_zip64 = self.entries.len() >= 0xFFFF || cd_size >= 0xFFFFFFFF || cd_offset >= 0xFFFFFFFF;

		if (cd_zip64)
		{
			ulong eocd64_offset = self.file.cursor()!;

			Zip64EOCD eocd64 = {
				.signature.val = ZIP64_EOCD_SIG,
				.size.val = (ulong)(Zip64EOCD.sizeof - 12),
				.version_made.val = 45,
				.version_needed.val = 45,
				.count_this_disk.val = (ulong)self.entries.len(),
				.count_total.val = (ulong)self.entries.len(),
				.size_cd.val = cd_size,
				.offset_cd.val = cd_offset,
			};

			io::write_any(self.file, &eocd64)!;

			Zip64Locator locator = {
				.signature.val = ZIP64_LOCATOR_SIG,
				.disk_start.val = 0,
				.offset_eocd.val = eocd64_offset,
				.total_disks.val = 1,
			};

			io::write_any(self.file, &locator)!;
		}

		char[] encoded_comment;
		if (self.comment.len > 0)
		{
			char[]? res = codepage::encode(self.allocator, self.comment, CodePage.CP437);
			if (try res)
			{
				encoded_comment = res;
				if (encoded_comment.len > 0xFFFF)
				{
					allocator::free(self.allocator, encoded_comment.ptr);
					return INVALID_ARGUMENT~;
				}
			}
		}
		defer if (encoded_comment.ptr) allocator::free(self.allocator, encoded_comment);

		ZipEOCD eocd = {
			.signature.val = ZIP_EOCD_SIG,
			.num_entries_this_disk.val = (ushort)(self.entries.len() >= 0xFFFF ? 0xFFFF : (ushort)self.entries.len()),
			.num_entries.val = (ushort)(self.entries.len() >= 0xFFFF ? 0xFFFF : (ushort)self.entries.len()),
			.cd_size.val = (uint)(cd_size >= 0xFFFFFFFF ? 0xFFFFFFFF : (uint)cd_size),
			.cd_offset.val = (uint)(cd_offset >= 0xFFFFFFFF ? 0xFFFFFFFF : (uint)cd_offset),
			.comment_len.val = (ushort)encoded_comment.len,
		};

		io::write_any(self.file, &eocd)!;

		if (encoded_comment.len > 0)
		{
			self.file.write(encoded_comment)!;
		}
	}
}

<*
 Extracts the entire archive to the specified directory.
 @param output_dir : `The directory to extract to.`
 @param password : `The password if a file is encrypted.`
*>
fn void? ZipArchive.extract(&self, String output_dir, String password = "") => @pool()
{
	for (usz i = 0; i < self.count(); i++)
	{
		ZipEntry entry = self.stat_at(i) ?? ENTRY_NOT_FOUND~!;
		String out_path_str;
		if (try tmp = path::temp(output_dir))
		{
			if (try combined = tmp.tappend(entry.name))
			{
				out_path_str = combined.str_view();
			}
			else
			{
				return IO_ERROR~;
			}
		}
		else
		{
			return IO_ERROR~;
		}

		if (entry.is_directory)
		{
			(void)path::mkdir(out_path_str, true);
		}
		else
		{
			if (try tmp = path::temp(out_path_str))
			{
				if (try parent = tmp.parent())
				{
					(void)path::mkdir(parent.str_view(), true);
				}
			}

			ZipEntryReader reader = self.open_reader(entry.name, password)!;
			defer (void)reader.close();

			File f = file::open(out_path_str, "wb")!;
			defer (void)f.close();
			char[65536] buf;
			while (true)
			{
				usz? res = reader.read(&buf);
				if (catch excuse = res)
				{
					if (excuse == io::EOF) break;
					return excuse~;
				}
				usz n = res;
				if (n == 0) break;
				f.write(buf[:n])!;
			}
			f.close()!;

			file::set_modified_time(out_path_str, (Time_t)entry.time().to_seconds())!;
		}
	}

	// Set directory timestamps (reverse order for subdirectories)
	for (usz i = self.count(); i > 0; i--)
	{
		ZipEntry entry;
		if (try res = self.stat_at(i - 1))
		{
			entry = res;
		}
		else
		{
			continue;
		}
		if (!entry.is_directory) continue;

		if (try tmp = path::temp(output_dir))
		{
			if (try combined = tmp.tappend(entry.name))
			{
				String out_path_str = (String)combined.str_view();
				file::set_modified_time(out_path_str, (Time_t)entry.time().to_seconds())!;
			}
		}
	}
}

fn usz ZipArchive.count(&self) => self.entries.len();

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


<*
 Reads an entire file from the archive.
 @param allocator : `The allocator to use.`
 @param filename : `The name of the file to read.`
 @param password : `The password if file is encrypted.`
 @return `The uncompressed file data.`
*>
fn char[]? ZipArchive.read_file_all(&self, Allocator allocator, String filename, String password = "")
{
	ZipEntryReader reader = self.open_reader(filename, password)!;
	defer (void)reader.close();
	ZipEntry entry = self.stat(filename)!;

	char[] data;
	defer catch if (data.ptr) allocator::free(allocator, data);

	data = io::read_fully(allocator, &reader)!;

	if (reader.adapter.aex == 2) return data;

	Crc32 crc;
	crc.init();
	crc.update(data);
	if (~crc.result != entry.crc32) return CORRUPTED_DATA~;

	return data;
}


<*
 Adds a directory entry to the archive.
*>
fn void? ZipArchive.add_directory(&self, String dirname)
{
	String dir_name = dirname;
	if (!dirname.ends_with("/"))
	{
		dir_name = string::tformat("%s/", dirname);
	}

	ZipEntryWriter? writer_opt = self.open_writer(dir_name, STORE);
	if (catch err = writer_opt) return err~;

	ZipEntryWriter writer = writer_opt;
	writer.entry.is_directory = true;
	writer.close()!;
}

<*
 Writes an entire file to the archive.
 @param filename : `The name of the file to create.`
 @param data : `The data to write.`
 @param method : `Compression method.`
*>
fn void? ZipArchive.write_file(&self, String filename, char[] data, ZipMethod method = DEFLATE)
{
	ZipEntryWriter writer = self.open_writer(filename, method)!;

	writer.write(data)!;
	writer.close()!;
}

struct ZipEntryReader (InStream)
{
	ulong size; // Uncompressed size
	ulong pos;  // Uncompressed position
	ZipMethod method;
	// For DEFLATE
	Inflater* inflater;
	ArchiveStreamAdapter adapter;
	char* bit_buf;
}

fn usz? ZipEntryReader.read(&self, char[] buffer) @dynamic
{
	if (self.method == STORE)
	{
		usz n = self.adapter.read(buffer)!;
		if (n == 0) return io::EOF~;
		self.pos += n;
		return n;
	}
	else if (self.method == DEFLATE)
	{
		if (self.inflater == null)
		{
			self.inflater = allocator::new(self.adapter.archive.allocator, Inflater);
			self.bit_buf = allocator::alloc_array(self.adapter.archive.allocator, char, 8192);
			self.inflater.init(&self.adapter, self.bit_buf[:8192]);
		}

		usz n = self.inflater.read(buffer)!;
		if (n == 0) return io::EOF~;
		self.pos += n;
		return n;
	}

	return io::UNSUPPORTED_OPERATION~;
}

fn void? ZipEntryReader.close(&self) @dynamic
{
	if (self.method == DEFLATE && self.inflater != null)
	{
		allocator::free(self.adapter.archive.allocator, self.bit_buf);
		allocator::free(self.adapter.archive.allocator, self.inflater);
		self.inflater = null;
		self.bit_buf = null;
	}
	return;
}

fn char? ZipEntryReader.read_byte(&self) @dynamic
{
	char[1] b;
	usz n = self.read(&b)!;
	if (n == 0) return io::EOF~;
	return b[0];
}

fn usz ZipEntryReader.len(&self) @dynamic
{
	if (self.size > (ulong)usz.max) return usz.max;
	return (usz)self.size;
}

fn ulong? ZipEntryReader.available(&self) @dynamic
{
	return self.size - self.pos;
}

fn usz? ZipEntryReader.seek(&self, isz offset, Seek seek) @dynamic
{
	self.set_cursor((long)offset, (SeekOrigin)seek.ordinal)!;
	long size = self.cursor()!;
	if (size > (ulong)usz.max) return io::OVERFLOW~;
	return (usz)size;
}

fn long? ZipEntryReader.cursor(&self) @dynamic
{
	return self.pos;
}

fn void? ZipEntryReader.set_cursor(&self, long offset, SeekOrigin seek) @dynamic
{
	if (self.method == DEFLATE) return io::UNSUPPORTED_OPERATION~;

	ulong new_pos = self.pos;
	switch (seek)
	{
		case FROM_START:
			if (offset < 0) return io::INVALID_ARGUMENT~;
			new_pos = offset;
		case FROM_CURSOR:
			new_pos = new_pos + offset;
		case FROM_END:
			new_pos = self.size + offset;
	}
	if (new_pos > self.size) new_pos = self.size;
	self.pos = new_pos;
	self.adapter.pos = new_pos;
}

<*
 Opens a reader for an entry.
 @param filename : `The name of the file to read.`
 @param password : `The password if file is encrypted.`
 @return `A reader for the entry's data.`
*>
fn ZipEntryReader? ZipArchive.open_reader(&self, String filename, String password = "")
{
	ZipEntry? entry = self.stat(filename);
	if (catch entry) return ENTRY_NOT_FOUND~;


	self.file.flush()!;
	if (entry.offset > (ulong)isz.max) return io::OVERFLOW~;
	self.file.set_cursor(entry.offset)!;
	ZipLFH lfh;
	io::read_any(self.file, &lfh)!;
	if (lfh.signature.val != ZIP_LFH_SIG) return CORRUPTED_DATA~;

	ZipEntryReader reader;

	reader.adapter.archive = self;
	ulong start_offset = entry.offset + ZipLFH.sizeof + lfh.filename_len.val + lfh.extra_field_len.val;
	if (start_offset > (ulong)isz.max) return io::OVERFLOW~;
	reader.adapter.start_offset = start_offset;
	// For STORE: adapter.size is uncompressed size.
	// For DEFLATE: adapter.size is compressed size.
	reader.adapter.size = entry.method == STORE
		? entry.uncompressed_size
		: entry.compressed_size;
	reader.size = entry.uncompressed_size;
	reader.method = entry.method;

	// AE-X decryption.
	if (entry.is_encrypted)
	{
		if (entry.method != AEX) return ENCRYPTED_FILE~;

		char aex_strength;
		ushort aex_method;
		ushort aex_version;

		self.file.set_cursor(lfh.filename_len.val, FROM_CURSOR)!;
		long end = self.file.cursor()! + lfh.extra_field_len.val;
		while (self.file.cursor()! < end)
		{
			ushort id = io::read_le_ushort(self.file)!;
			ushort size = io::read_le_ushort(self.file)!;

			if (id == AEX_EXTRA_ID)
			{
				aex_version = io::read_le_ushort(self.file)!;
				io::read_le_ushort(self.file)!;
				aex_strength = self.file.read_byte()!;
				aex_method = io::read_le_ushort(self.file)!;
				continue;
			}
			self.file.set_cursor(size, FROM_CURSOR)!;
		}
		if (!aex_version) return CORRUPTED_DATA~;

		AesType aes_type;
		usz key_len;
		switch (aex_strength)
		{
			case 1: key_len = 16; aes_type = AES128;
			case 2: key_len = 24; aes_type = AES192;
			case 3: key_len = 32; aes_type = AES256;
			default: return CORRUPTED_DATA~;
		}
		usz salt_len = key_len / 2;

		@pool()
		{
			char[] buf = mem::temp_array(char, salt_len + 2);
			self.file.read(buf)!;

			if (buf.len < salt_len + 2) return CORRUPTED_DATA~;

			char[] salt        = buf[0:salt_len];
			char[] pv_stored   = buf[salt_len:2];

			usz cipher_len = (usz)reader.adapter.size - salt_len - 2 - 10;
			char[] cipher = mem::temp_array(char, cipher_len);
			self.file.read(cipher)!;

			char[] hmac_stored = mem::temp_array(char, 10);
			self.file.read(hmac_stored)!;

			usz dk_len = key_len * 2 + 2;
			char[] dk = mem::temp_array(char, dk_len);
			sha1::pbkdf2(password, salt, 1000, dk);

			char[] aes_key = dk[0:key_len ];
			char[] mac_key = dk[key_len:key_len];
			char[] pv_derived = dk[key_len*2:2];

			if (pv_stored != pv_derived) return WRONG_PASSWORD~;

			char[*] hmac_calc = sha1::hmac(mac_key, cipher);
			if (hmac_stored[:10] != hmac_calc[:10]) return CORRUPTED_DATA~;

			reader.adapter.aes.init(aes_type, aes_key, {[0]=1}, CTR);
		};

		reader.adapter.aex = aex_version;
		reader.adapter.start_offset += (ulong)salt_len + 2;
		reader.adapter.size = entry.compressed_size - ((ulong)salt_len + 2 + 10);
		reader.method = (ZipMethod)aex_method;
	}

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
	usz capacity;
}

fn usz? ZipEntryWriter.write(&self, char[] bytes) @dynamic
{
	if (bytes.len == 0) return 0;

	self.crc.update(bytes);
	self.entry.uncompressed_size += (ulong)bytes.len;

	if (self.entry.method == STORE)
	{
		usz n = self.archive.file.write(bytes)!;
		self.entry.compressed_size += n;
		return n;
	}
	else
	{
		usz new_len = self.buffer.len + bytes.len;
		if (new_len > self.capacity)
		{
			usz new_cap = self.capacity;
			if (new_cap < 4096) new_cap = 4096;
			while (new_cap < new_len)
			{
				if (new_cap > usz.max / 2) { new_cap = new_len; break; }
				new_cap *= 2;
			}

			char* p = allocator::realloc_array(self.archive.allocator, self.buffer.ptr, char, new_cap);
			self.buffer = p[:self.buffer.len]; // Keep length as used size
			self.capacity = new_cap;
		}

		mem::copy(self.buffer.ptr + self.buffer.len, bytes.ptr, bytes.len);
		// Update slice length
		self.buffer = self.buffer.ptr[:new_len];
		return bytes.len;
	}
}

fn void? ZipEntryWriter.write_byte(&self, char c) @dynamic
{
	char[1] b = { c };
	self.write(&b)!;
}

fn void? ZipEntryWriter.close(&self) @dynamic
{
	if (self.entry.method == DEFLATE)
	{
		if (self.buffer.len > 0)
		{
			char[]? data = deflate::compress(self.archive.allocator, self.buffer);
			if (catch data) return IO_ERROR~;
			self.archive.file.write(data)!;
			self.entry.compressed_size = data.len;

			allocator::free(self.archive.allocator, data);
			allocator::free(self.archive.allocator, self.buffer);
			self.buffer = {};
		}
	}

	self.entry.crc32 = ~self.crc.result;

	self.lfh.crc32.val = self.entry.crc32;

	// ZIP64 sentinel: readers typically fallback to Central Directory for actual sizes.
	self.lfh.compressed_size.val = (uint)math::min(self.entry.compressed_size, (ulong)0xFFFFFFFF);
	self.lfh.uncompressed_size.val = (uint)math::min(self.entry.uncompressed_size, (ulong)0xFFFFFFFF);

	long end_pos = self.archive.file.cursor()!;

	if (self.lfh_offset > (long)isz.max) return io::OVERFLOW~;
	self.archive.file.set_cursor(self.lfh_offset)!;
	io::write_any(self.archive.file, &self.lfh)!;

	self.archive.file.set_cursor(end_pos)!;

	self.archive.entries.push(self.entry);
}

<*
 Opens a writer for a new entry.
 @param filename : `The name of the file to create in the archive.`
 @param method : `Compression method.`
 @return `A writer for the new entry.`
*>
fn ZipEntryWriter? ZipArchive.open_writer(&self, String filename, ZipMethod method = DEFLATE)
{
	if (!self.mode.starts_with("w")) return IO_ERROR~;

	ZipEntryWriter writer;
	writer.archive = self;
	writer.entry.name = filename.copy(self.allocator);
	defer catch allocator::free(self.allocator, writer.entry.name);

	writer.entry.method = method;
	writer.entry.offset = self.file.cursor()!;
	writer.crc.init();
	writer.lfh_offset = writer.entry.offset;

	writer.lfh = {
		.signature.val = ZIP_LFH_SIG,
		.version_needed.val = 20,
		.method = method,
		.flags.val = 0x0800, // UTF-8 flag
		.filename_len.val = (ushort)filename.len,
	};

	Time now = time::now();
	writer.entry.last_mod_time = time_to_dos_time(now);
	writer.entry.last_mod_date = time_to_dos_date(now);
	writer.lfh.last_mod_time.val = writer.entry.last_mod_time;
	writer.lfh.last_mod_date.val = writer.entry.last_mod_date;

	io::write_any(self.file, &writer.lfh)!;
	self.file.write(filename)!;

	return writer;
}

// -----------------------------------------------------------------------------
// PRIVATE IMPLEMENTATION
// -----------------------------------------------------------------------------

struct ArchiveStreamAdapter (InStream) @private
{
	ZipArchive* archive;
	ulong start_offset;
	ulong size;
	ulong pos;

	// For AEX decryption
	ushort aex; // 0: unencrypted, 1: AE-1, 2: AE-2
	Aes aes;
}

fn usz? ArchiveStreamAdapter.read(&self, char[] buffer) @dynamic
{
	if (self.pos >= self.size) return 0;

	usz to_read = (usz)math::min((ulong)buffer.len, self.size - self.pos);
	if (to_read == 0) return 0;

	ulong abs_pos = self.start_offset + self.pos;
	if (abs_pos > (ulong)isz.max) return io::OVERFLOW~;

	// Note: ZipArchive shared file handle access is not thread-safe.
	self.archive.file.set_cursor(abs_pos)!;
	usz n = self.archive.file.read(buffer[:to_read])!;
	if (self.aex) self.aes.decrypt_buffer(buffer[:n], buffer[:n]);
	self.pos += n;
	return n;
}

fn char? ArchiveStreamAdapter.read_byte(&self) @dynamic
{
	char[1] b;
	if (try n = self.read(b[..]))
	{
		if (n == 0) return io::EOF~;
		return b[0];
	}
	return io::EOF~;
}

struct ZipLFH @packed @private
{
	UIntLE signature;
	UShortLE version_needed;
	UShortLE flags;
	ZipMethod method;
	UShortLE last_mod_time;
	UShortLE last_mod_date;
	UIntLE crc32;
	UIntLE compressed_size;
	UIntLE uncompressed_size;
	UShortLE filename_len;
	UShortLE extra_field_len;
}

struct ZipCDH @packed @private
{
	UIntLE signature;
	UShortLE version_made_by;
	UShortLE version_needed;
	UShortLE flags;
	ZipMethod method;
	UShortLE last_mod_time;
	UShortLE last_mod_date;
	UIntLE crc32;
	UIntLE compressed_size;
	UIntLE uncompressed_size;
	UShortLE filename_len;
	UShortLE extra_field_len;
	UShortLE comment_len;
	UShortLE disk_number_start;
	UShortLE internal_attr;
	UIntLE external_attr;
	UIntLE relative_offset;
}

struct ZipEOCD @packed @private
{
	UIntLE signature;
	UShortLE disk_number;
	UShortLE cd_disk_number;
	UShortLE num_entries_this_disk;
	UShortLE num_entries;
	UIntLE cd_size;
	UIntLE cd_offset;
	UShortLE comment_len;
}

const uint ZIP_LFH_SIG @private = 0x04034B50;
const uint ZIP_CDH_SIG @private = 0x02014B50;
const uint ZIP_EOCD_SIG @private = 0x06054B50;
const uint ZIP64_EOCD_SIG @private = 0x06064B50;
const uint ZIP64_LOCATOR_SIG @private = 0x07064B50;

const ushort ZIP64_EXTRA_ID @private = 0x0001;
const ushort AEX_EXTRA_ID @private = 0x9901;

struct Zip64EOCD @packed @private
{
	UIntLE signature;
	ULongLE size; // Size of remaining record
	UShortLE version_made;
	UShortLE version_needed;
	UIntLE disk_num;
	UIntLE disk_start;
	ULongLE count_this_disk;
	ULongLE count_total;
	ULongLE size_cd;
	ULongLE offset_cd;
	// char[] custom_data;
}

struct Zip64Locator @packed @private
{
	UIntLE signature;
	UIntLE disk_start;
	ULongLE offset_eocd;
	UIntLE total_disks;
}

struct Zip64ExtraField @private
{
	ushort header_id;
	ushort size;
	ulong uncompressed_size;
	ulong compressed_size;
	ulong offset;
	uint disk_start;
}

fn ushort time_to_dos_time(Time t) @private
{
	long seconds = (long)t / 1_000_000;
	int s = (int)(seconds % 60);
	int m = (int)((seconds / 60) % 60);
	int h = (int)((seconds / 3600) % 24);
	return (ushort)(((h & 0x1F) << 11) | ((m & 0x3F) << 5) | ((s / 2) & 0x1F));
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
	return (ushort)(((year & 0x7F) << 9) | ((month & 0xF) << 5) | (day & 0x1F));
}

fn Time dos_date_time_to_time(ushort dos_date, ushort dos_time) @private
{
	int sec = (int)((dos_time & 0x1F) * 2);
	int min = (int)((dos_time >> 5) & 0x3F);
	int hour = (int)((dos_time >> 11) & 0x1F);

	int day = (int)(dos_date & 0x1F);
	int month = (int)((dos_date >> 5) & 0xF);
	int year = (int)((dos_date >> 9) & 0x7F) + 1980;

	if (day == 0) day = 1;
	if (month == 0) month = 1;
	if (month > 12) month = 12;

	// 1970 to 1980 is 3652 days
	long total_days = 3652;

	for (int y = 1980; y < year; y++)
	{
		bool is_leap = (y % 4 == 0 && y % 100 != 0) || (y % 400 == 0);
		total_days += is_leap ? 366 : 365;
	}

	bool is_leap_year = (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
	int[12] mdays = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
	if (is_leap_year) mdays[1] = 29;

	for (int i = 0; i < month - 1; i++)
	{
		total_days += mdays[i];
	}

	total_days += (long)(day - 1);

	long total_seconds = total_days * 86400;
	total_seconds += (long)hour * 3600;
	total_seconds += (long)min * 60;
	total_seconds += (long)sec;

	return (Time)(total_seconds * 1_000_000);
}

fn bool is_valid_utf8(char[] bytes) @private
{
	usz i = 0;
	while (i < bytes.len)
	{
		char lead = bytes[i];
		switch (lead)
		{
			case 0x00..0x7F:
				i++;
			case 0xC2..0xDF:
				if (i + 1 >= bytes.len || (bytes[i + 1] & 0xC0) != 0x80) return false;
				i += 2;
			case 0xE0..0xEF:
				if (i + 2 >= bytes.len) return false;
				char b1 = bytes[i + 1];
				char b2 = bytes[i + 2];
				if ((b1 & 0xC0) != 0x80 || (b2 & 0xC0) != 0x80) return false;
				if (lead == 0xE0 && b1 < 0xA0) return false;
				if (lead == 0xED && b1 >= 0xA0) return false;
				i += 3;
			case 0xF0..0xF4:
				if (i + 3 >= bytes.len) return false;
				char b1 = bytes[i + 1];
				char b2 = bytes[i + 2];
				char b3 = bytes[i + 3];
				if ((b1 & 0xC0) != 0x80 || (b2 & 0xC0) != 0x80 || (b3 & 0xC0) != 0x80) return false;
				if (lead == 0xF0 && b1 < 0x90) return false;
				if (lead == 0xF4 && b1 >= 0x90) return false;
				i += 4;
			default:
				return false;
		}
	}
	return true;
}
