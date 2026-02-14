<*
 ZIP archive module (STORE and DEFLATE).

 API:
 - fn ZipArchive? open(String path, String mode = "r", Allocator allocator = mem)
 - fn ZipArchive? recover(String path, Allocator allocator = mem)
 - fn void?       ZipArchive.extract(&self, String output_dir)
 - fn ZipEntry?   ZipArchive.stat(&self, String filename)
 - fn char[]?     ZipArchive.read_file_all(&self, String filename, Allocator allocator = mem)
 - fn void?       ZipArchive.close(&self)
*>
module std::compression::zip;
import std::core::mem, std::io, std::collections::list, std::hash::crc32, std::time, std::math;
import std::io::file, std::io::path;
import std::encoding::codepage;
import libc;
import std::core::bitorder;
import std::compression::deflate;

faultdef
	INVALID_ARGUMENT,
	IO_ERROR,
	CORRUPTED_DATA,
	ENTRY_NOT_FOUND,
	ENCRYPTED_FILE;

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
	ushort method;
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

const ushort ZIP_METHOD_STORE = 0;
const ushort ZIP_METHOD_DEFLATE = 8;

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
		ulong file_size = (ulong)f.seek(0, Seek.CURSOR)!;

		if (file_size < ZipEOCD.sizeof) return CORRUPTED_DATA~;

		ulong search_start = file_size > (ulong)(ZipEOCD.sizeof + 65535)
			? file_size - (ulong)(ZipEOCD.sizeof + 65535)
			: 0;

		ZipEOCD eocd;
		bool found = false;

		for (ulong pos = file_size - (ulong)ZipEOCD.sizeof; pos >= search_start; pos--)
		{
			if (pos > (ulong)isz.max) return io::OVERFLOW~;
			f.seek((isz)pos, Seek.SET)!;
			UIntLE sig;
			if (f.read(((char*)&sig)[:4])! != 4) break;

			if (sig.val == ZIP_EOCD_SIG)
			{
				f.seek((isz)pos, Seek.SET)!;
				if (f.read(((char*)&eocd)[:ZipEOCD.sizeof])! == ZipEOCD.sizeof)
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
		archive.path = String.copy(path, allocator);
		archive.mode = String.copy(mode, allocator);

		if (eocd.comment_len.val > 0)
		{
			char[] comment_data = allocator::malloc(allocator, (usz)eocd.comment_len.val)[:(usz)eocd.comment_len.val];
			defer allocator::free(allocator, comment_data.ptr);
			if (archive.file.read(comment_data)! == (usz)eocd.comment_len.val)
			{
				archive.comment = codepage::decode(allocator, comment_data, CodePage.CP437)!!;
			}
		}

		defer (catch err) (void)archive.close();

		if (eocd.cd_offset.val > (uint)isz.max) return io::OVERFLOW~;
		archive.file.seek((isz)eocd.cd_offset.val, Seek.SET)!;

		usz num_entries = eocd.num_entries.val;


		// ZIP64 check
		if (eocd.num_entries.val == 0xFFFF || eocd.cd_offset.val == 0xFFFFFFFF)
		{
			isz locator_pos = (isz)file_size - ZipEOCD.sizeof - Zip64Locator.sizeof;
			if (locator_pos >= 0)
			{
				archive.file.seek(locator_pos, Seek.SET)!;
				Zip64Locator locator;
				if (try n = archive.file.read(((char*)&locator)[:Zip64Locator.sizeof]))
				{
					if (n == Zip64Locator.sizeof && locator.signature.val == ZIP64_LOCATOR_SIG)
					{
						archive.file.seek((isz)locator.offset_eocd.val, Seek.SET)!;
						Zip64EOCD eocd64;
						archive.file.read(((char*)&eocd64)[:Zip64EOCD.sizeof])!;
						if (eocd64.signature.val == ZIP64_EOCD_SIG)
						{
							if (eocd64.offset_cd.val > (ulong)isz.max) return io::OVERFLOW~;
							archive.file.seek((isz)eocd64.offset_cd.val, Seek.SET)!;
							num_entries = (usz)eocd64.count_total.val;
						}
					}
				}
			}
		}

		for (usz i = 0; i < num_entries; i++)
		{
			ZipCDH cdh;
			if (archive.file.read(((char*)&cdh)[:ZipCDH.sizeof])! != ZipCDH.sizeof) break;
			if (cdh.signature.val != ZIP_CDH_SIG) break;

			char[] raw_name = allocator::malloc(allocator, cdh.filename_len.val)[:cdh.filename_len.val];
			if (archive.file.read(raw_name)! != (usz)cdh.filename_len.val)
			{
				allocator::free(allocator, raw_name.ptr);
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
				name = (String)codepage::decode(allocator, raw_name, CodePage.CP437)!!;
				allocator::free(allocator, raw_name.ptr);
			}

			char[] extra_field;
			if (cdh.extra_field_len.val > 0)
			{
				extra_field = allocator::malloc(allocator, cdh.extra_field_len.val)[:cdh.extra_field_len.val];
				archive.file.read(extra_field)!;
			}

			archive.file.seek((isz)cdh.comment_len.val, Seek.CURSOR)!;

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
					reader.seek(size, Seek.CURSOR)!;
				}
			}
			if (extra_field.len > 0) allocator::free(allocator, extra_field.ptr);

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
				.method = cdh.method.val,
				.last_mod_time = cdh.last_mod_time.val,
				.last_mod_date = cdh.last_mod_date.val,
				.is_directory = is_directory,
				.is_encrypted = (cdh.flags.val & 1) != 0
			};

			archive.entries.push(entry);
		}

		return archive;
	}
	return INVALID_ARGUMENT~;
}

<*
 Recovers a ZIP archive by scanning for Local File Headers
 when the Central Directory is missing or corrupted.
 @param path : `The path to the broken ZIP file.`
 @param allocator : `The allocator to use.`
 @return `The recovered archive.`
*>
fn ZipArchive? recover(String path, Allocator allocator = mem)
{
	File f = file::open(path, "rb")!;
	defer (catch err) (void)f.close();

	ZipArchive archive = {
		.allocator = allocator,
		.file = allocator::new(allocator, File, f),
		.path = String.copy(path, allocator),
		.mode = String.copy("r", allocator)
	};
	archive.entries.init(allocator);
	defer (catch err) (void)archive.close();

	char[4] sig_buf;
	while (true)
	{
		ulong offset = (ulong)archive.file.seek(0, Seek.CURSOR)!;
		usz n = archive.file.read(sig_buf[..])!;
		if (n < 4) break;

		if (bitorder::read(sig_buf, UIntLE) != ZIP_LFH_SIG)
		{
			archive.file.seek(-3, Seek.CURSOR)!;
			continue;
		}

		ZipLFH lfh;
		if (archive.file.read(((char*)&lfh.version_needed)[:ZipLFH.sizeof - 4])! != ZipLFH.sizeof - 4) break;
		lfh.signature.val = ZIP_LFH_SIG;

		char[] raw_name = allocator::malloc(allocator, lfh.filename_len.val)[:lfh.filename_len.val];
		if (archive.file.read(raw_name)! != (usz)lfh.filename_len.val)
		{
			allocator::free(allocator, raw_name.ptr);
			break;
		}

		String name;
		if ((lfh.flags.val & 0x0800) != 0 || is_valid_utf8(raw_name))
		{
			name = (String)raw_name;
		}
		else
		{
			name = (String)codepage::decode(allocator, raw_name, CodePage.CP437)!!;
			allocator::free(allocator, raw_name.ptr);
		}

		archive.file.seek((isz)lfh.extra_field_len.val, Seek.CURSOR)!;

		ZipEntry entry = {
			.name = name,
			.compressed_size = lfh.compressed_size.val,
			.uncompressed_size = lfh.uncompressed_size.val,
			.crc32 = lfh.crc32.val,
			.offset = offset,
			.method = lfh.method.val,
			.last_mod_time = lfh.last_mod_time.val,
			.last_mod_date = lfh.last_mod_date.val,
			.is_directory = name.ends_with("/") || name.ends_with("\\"),
			.is_encrypted = (lfh.flags.val & 1) != 0
		};

		archive.entries.push(entry);

		if (lfh.compressed_size.val > 0 && (ulong)lfh.compressed_size.val > (ulong)isz.max) return io::OVERFLOW~;
		archive.file.seek((isz)lfh.compressed_size.val, Seek.CURSOR)!;
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

		for (usz i = 0; i < self.entries.len(); i++)
		{
			allocator::free(self.allocator, self.entries.get(i).name.ptr);
		}
		self.entries.free();
		allocator::free(self.allocator, self.mode.ptr);
		allocator::free(self.allocator, self.path.ptr);
		if (self.comment.len > 0) allocator::free(self.allocator, self.comment.ptr);
		if (self.file) allocator::free(self.allocator, self.file);
	}

	if (self.mode.starts_with("w"))
	{
		self.file.flush()!;
		ulong cd_offset = self.file.seek(0, Seek.CURSOR)!;
		ulong cd_size = 0;

		for (usz i = 0; i < self.entries.len(); i++)
		{
			ZipEntry* entry = self.entries.get_ref(i);
			ZipCDH cdh;
			mem::set(&cdh, 0, ZipCDH.sizeof);

			cdh.signature.val = ZIP_CDH_SIG;
			cdh.version_made_by.val = 45; // 4.5 for ZIP64
			cdh.version_needed.val = 45;
			cdh.method.val = entry.method;
			cdh.last_mod_time.val = entry.last_mod_time;
			cdh.last_mod_date.val = entry.last_mod_date;
			cdh.crc32.val = entry.crc32;
			cdh.filename_len.val = (ushort)entry.name.len;

			bool is_zip64 = entry.uncompressed_size >= 0xFFFFFFFF || entry.compressed_size >= 0xFFFFFFFF || entry.offset >= 0xFFFFFFFF;

			char[] extra_data;
			if (is_zip64)
			{
				cdh.compressed_size.val = 0xFFFFFFFF;
				cdh.uncompressed_size.val = 0xFFFFFFFF;
				cdh.relative_offset.val = 0xFFFFFFFF;

				// Header(4) + Uncomp(8) + Comp(8) + Offset(8)
				ushort extra_size = 28;
				extra_data = allocator::malloc(self.allocator, extra_size)[:extra_size];
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

			self.file.write(((char*)&cdh)[:ZipCDH.sizeof])!;
			self.file.write(entry.name.ptr[:entry.name.len])!;
			if (is_zip64)
			{
				self.file.write(extra_data)!;
				allocator::free(self.allocator, extra_data.ptr);
			}

			ulong entry_record_size = (ulong)(ZipCDH.sizeof + entry.name.len + cdh.extra_field_len.val);
			if (cd_size > (ulong.max - entry_record_size)) return io::OVERFLOW~;
			cd_size += entry_record_size;
		}

		bool cd_zip64 = self.entries.len() >= 0xFFFF || cd_size >= 0xFFFFFFFF || cd_offset >= 0xFFFFFFFF;

		if (cd_zip64)
		{
			ulong eocd64_offset = (ulong)self.file.seek(0, Seek.CURSOR)!;

			Zip64EOCD eocd64;
			mem::set(&eocd64, 0, Zip64EOCD.sizeof);
			eocd64.signature.val = ZIP64_EOCD_SIG;
			eocd64.size.val = (ulong)(Zip64EOCD.sizeof - 12);
			eocd64.version_made.val = 45;
			eocd64.version_needed.val = 45;
			eocd64.count_this_disk.val = (ulong)self.entries.len();
			eocd64.count_total.val = (ulong)self.entries.len();
			eocd64.size_cd.val = cd_size;
			eocd64.offset_cd.val = cd_offset;

			self.file.write(((char*)&eocd64)[:Zip64EOCD.sizeof])!;

			Zip64Locator locator;
			locator.signature.val = ZIP64_LOCATOR_SIG;
			locator.disk_start.val = 0;
			locator.offset_eocd.val = eocd64_offset;
			locator.total_disks.val = 1;

			self.file.write(((char*)&locator)[:Zip64Locator.sizeof])!;
		}

		char[] encoded_comment;
		if (self.comment.len > 0)
		{
			char[]? res = codepage::encode(self.allocator, self.comment, CodePage.CP437);
			if (@ok(res))
			{
				encoded_comment = res!;
				if (encoded_comment.len > 0xFFFF)
				{
					allocator::free(self.allocator, encoded_comment.ptr);
					return INVALID_ARGUMENT~;
				}
			}
		}
		defer if (encoded_comment.ptr) allocator::free(self.allocator, encoded_comment.ptr);

		ZipEOCD eocd;
		mem::set(&eocd, 0, ZipEOCD.sizeof);
		eocd.signature.val = ZIP_EOCD_SIG;
		eocd.num_entries_this_disk.val = (ushort)(self.entries.len() >= 0xFFFF ? 0xFFFF : (ushort)self.entries.len());
		eocd.num_entries.val = (ushort)(self.entries.len() >= 0xFFFF ? 0xFFFF : (ushort)self.entries.len());
		eocd.cd_size.val = (uint)(cd_size >= 0xFFFFFFFF ? 0xFFFFFFFF : (uint)cd_size);
		eocd.cd_offset.val = (uint)(cd_offset >= 0xFFFFFFFF ? 0xFFFFFFFF : (uint)cd_offset);
		eocd.comment_len.val = (ushort)encoded_comment.len;

		self.file.write(((char*)&eocd)[:ZipEOCD.sizeof])!;

		if (encoded_comment.len > 0)
		{
			self.file.write(encoded_comment)!;
		}
	}
}

<*
 Extracts the entire archive to the specified directory.
 @param output_dir : `The directory to extract to.`
*>
fn void? ZipArchive.extract(&self, String output_dir)
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
					(void)path::mkdir((String)parent.str_view(), true);
				}
			}

			ZipEntryReader reader = self.open_reader(entry.name)!;
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

			file::set_modified_time(out_path_str, (libc::Time_t)entry.time().to_seconds())!;
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
				file::set_modified_time(out_path_str, (libc::Time_t)entry.time().to_seconds())!;
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
 @param filename : `The name of the file to read.`
 @param allocator : `The allocator to use.`
 @return `The uncompressed file data.`
*>
fn char[]? ZipArchive.read_file_all(&self, String filename, Allocator allocator = mem)
{
	ZipEntryReader reader = self.open_reader(filename)!;
	defer (void)reader.close();
	ZipEntry entry = self.stat(filename)!;

	char[] data;
	defer (catch err) if (data.ptr) allocator::free(allocator, data.ptr);

	if (reader.method == ZIP_METHOD_STORE)
	{
		if (reader.size > (ulong)usz.max) return io::OVERFLOW~;
		data = allocator::malloc(allocator, (usz)reader.size)[:(usz)reader.size];
		reader.read(data)!;
	}
	else
	{
		if (reader.adapter.start_offset > (ulong)isz.max) return io::OVERFLOW~;
		self.file.seek((isz)reader.adapter.start_offset, Seek.SET)!;
		if (entry.compressed_size > (ulong)usz.max) return io::OVERFLOW~;
		char[] compressed = allocator::malloc(allocator, (usz)entry.compressed_size)[:(usz)entry.compressed_size];
		defer allocator::free(allocator, compressed.ptr);
		self.file.read(compressed)!;
		data = deflate::decompress(compressed, allocator)!;
	}

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

	ZipEntryWriter? writer_opt = self.open_writer(dir_name, ZIP_METHOD_STORE);
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
fn void? ZipArchive.write_file(&self, String filename, char[] data, ushort method = ZIP_METHOD_DEFLATE)
{
	ZipEntryWriter? writer_opt = self.open_writer(filename, method);
	if (catch err = writer_opt) return err~;
	ZipEntryWriter writer = writer_opt;

	writer.write(data)!;
	writer.close()!;
}

struct ZipEntryReader (InStream)
{
	ulong size; // Uncompressed size
	ulong pos;  // Uncompressed position
	ushort method;
	// For DEFLATE
	deflate::Inflater* inflater;
	ArchiveStreamAdapter adapter;
	char* bit_buf;
}

fn usz? ZipEntryReader.read(&self, char[] buffer) @dynamic
{
	if (self.method == ZIP_METHOD_STORE)
	{
		usz n = self.adapter.read(buffer)!;
		if (n == 0) return io::EOF~;
		self.pos += n;
		return n;
	}
	else if (self.method == ZIP_METHOD_DEFLATE)
	{
		if (self.inflater == null)
		{
			self.inflater = allocator::new(self.adapter.archive.allocator, deflate::Inflater);
			self.bit_buf = (char*)allocator::malloc(self.adapter.archive.allocator, 8192);
			self.inflater.init(&self.adapter, self.bit_buf[0:8192]);
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
	if (self.method == ZIP_METHOD_DEFLATE && self.inflater != null)
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
	if (self.size > (ulong)usz.max) return usz.max;
	return (usz)self.size;
}

fn usz? ZipEntryReader.available(&self) @dynamic
{
	ulong avail = self.size - self.pos;
	if (avail > (ulong)usz.max) return usz.max;
	return (usz)avail;
}

fn usz? ZipEntryReader.seek(&self, isz offset, Seek seek) @dynamic
{
	if (self.method == ZIP_METHOD_DEFLATE) return io::UNSUPPORTED_OPERATION~;

	ulong new_pos = self.pos;
	switch (seek)
	{
		case SET:
			if (offset < 0) return io::INVALID_ARGUMENT~;
			new_pos = (ulong)offset;
		case CURSOR:
			new_pos = (ulong)((isz)new_pos + offset);
		case END:
			new_pos = (ulong)((isz)self.size + offset);
	}
	if (new_pos > self.size) new_pos = self.size;
	self.pos = new_pos;
	self.adapter.pos = new_pos;

	if (self.pos > (ulong)usz.max) return usz.max;
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

	if (entry.is_encrypted) return ENCRYPTED_FILE~;

	self.file.flush()!;
	if (entry.offset > (ulong)isz.max) return io::OVERFLOW~;
	self.file.seek((isz)entry.offset, Seek.SET)!;
	ZipLFH lfh;
	self.file.read(((char*)&lfh)[:ZipLFH.sizeof])!;
	if (lfh.signature.val != ZIP_LFH_SIG) return CORRUPTED_DATA~;

	ZipEntryReader reader;
	mem::set(&reader, 0, ZipEntryReader.sizeof);

	reader.adapter.archive = (ZipArchive*)self;
	ulong start_offset = entry.offset + ZipLFH.sizeof + lfh.filename_len.val + lfh.extra_field_len.val;
	if (start_offset > (ulong)isz.max) return io::OVERFLOW~;
	reader.adapter.start_offset = start_offset;
	// For STORE: adapter.size is uncompressed size.
	// For DEFLATE: adapter.size is compressed size.
	reader.adapter.size = entry.method == ZIP_METHOD_STORE
		? entry.uncompressed_size
		: entry.compressed_size;
	reader.adapter.pos = 0;

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
	usz capacity;
}

fn usz? ZipEntryWriter.write(&self, char[] bytes) @dynamic
{
	if (bytes.len == 0) return 0;

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

	self.lfh.crc32.val = self.entry.crc32;

	// ZIP64 sentinel: readers typically fallback to Central Directory for actual sizes.
	self.lfh.compressed_size.val = (uint)math::min(self.entry.compressed_size, (ulong)0xFFFFFFFF);
	self.lfh.uncompressed_size.val = (uint)math::min(self.entry.uncompressed_size, (ulong)0xFFFFFFFF);

	ulong end_pos = (ulong)self.archive.file.seek(0, Seek.CURSOR)!;

	if (self.lfh_offset > (ulong)isz.max) return io::OVERFLOW~;
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
fn ZipEntryWriter? ZipArchive.open_writer(&self, String filename, ushort method = ZIP_METHOD_DEFLATE)
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
	writer.lfh.signature.val = ZIP_LFH_SIG;
	writer.lfh.version_needed.val = 20;
	writer.lfh.method.val = method;
	writer.lfh.flags.val = 0x0800; // UTF-8 flag
	writer.lfh.filename_len.val = (ushort)filename.len;

	Time now = time::now();
	writer.entry.last_mod_time = time_to_dos_time(now);
	writer.entry.last_mod_date = time_to_dos_date(now);
	writer.lfh.last_mod_time.val = writer.entry.last_mod_time;
	writer.lfh.last_mod_date.val = writer.entry.last_mod_date;

	self.file.write(((char*)&writer.lfh)[:ZipLFH.sizeof])!;
	self.file.write(filename.ptr[:filename.len])!;

	writer.buffer = {};

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
}

fn usz? ArchiveStreamAdapter.read(&self, char[] buffer) @dynamic
{
	if (self.pos >= self.size) return 0;

	usz to_read = (usz)math::min((ulong)buffer.len, self.size - self.pos);
	if (to_read == 0) return 0;

	ulong abs_pos = self.start_offset + self.pos;
	if (abs_pos > (ulong)isz.max) return io::OVERFLOW~;

	// Note: ZipArchive shared file handle access is not thread-safe.
	self.archive.file.seek((isz)abs_pos, Seek.SET)!;
	usz n = self.archive.file.read(buffer[:to_read])!;
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
	UShortLE method;
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
	UShortLE method;
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
