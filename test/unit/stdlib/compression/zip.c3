module zip_test @test;

import std::io;
import std::compression::zip;

fn void test_zip_store()
{
	@pool()
	{
		// Create archive with uncompressed file
		ZipArchive zip = zip::open(mem, "unittest_store.zip", "w")!!;
		zip.write_file("test.txt", "Hello, World!", STORE)!!;
		(void)zip.close();
		defer (void)file::delete("unittest_store.zip");

		// Read and verify
		ZipArchive read_zip = zip::open(mem, "unittest_store.zip", "r")!!;
		defer (void)read_zip.close();

		assert(read_zip.count() == 1, "Expected 1 entry");

		ZipEntry entry = read_zip.stat("test.txt")!!;
		assert(entry.method == STORE, "Expected STORE method");
		assert(entry.uncompressed_size == 13, "Expected 13 bytes");

		char[] data = read_zip.read_file_all(mem, "test.txt")!!;
		defer free(data);
		assert((String)data == "Hello, World!", "Data mismatch");

	};
}

fn void test_zip_deflate()
{
	@pool()
	{
		// Create archive with compressed file
		ZipArchive zip = zip::open(mem, "unittest_deflate.zip", "w")!!;

		ZipEntryWriter writer = zip.open_writer("compressed.txt", DEFLATE)!!;
		String data = "This is a test. ";
		for (int i = 0; i < 100; i++)
		{
			writer.write((char[])data)!!;
		}
		writer.close()!!;
		(void)zip.close();
		defer (void)file::delete("unittest_deflate.zip");

		// Read and verify
		ZipArchive read_zip = zip::open(mem, "unittest_deflate.zip", "r")!!;
		defer (void)read_zip.close();

		assert(read_zip.count() == 1, "Expected 1 entry");

		ZipEntry entry = read_zip.stat("compressed.txt")!!;
		assert(entry.method == DEFLATE, "Expected DEFLATE method");
		assert(entry.uncompressed_size == 1600, "Expected 1600 bytes");

		char[] decompressed = read_zip.read_file_all(mem, "compressed.txt")!!;
		defer free(decompressed);
		assert(decompressed.len == 1600, "Decompressed size mismatch");

	};
}

fn void test_zip_directory()
{
	@pool()
	{
		// Create archive with directory
		ZipArchive zip = zip::open(mem, "unittest_dir.zip", "w")!!;
		zip.add_directory("docs")!!;
		zip.write_file("docs/readme.txt", "README")!!;
		(void)zip.close();
		defer (void)file::delete("unittest_dir.zip");

		// Read and verify
		ZipArchive read_zip = zip::open(mem, "unittest_dir.zip", "r")!!;
		defer (void)read_zip.close();

		assert(read_zip.count() == 2, "Expected 2 entries");

		ZipEntry dir_entry = read_zip.stat("docs/")!!;
		assert(dir_entry.is_directory, "Expected directory");
		assert(dir_entry.uncompressed_size == 0, "Directory should have 0 size");

		ZipEntry file_entry = read_zip.stat("docs/readme.txt")!!;
		assert(!file_entry.is_directory, "Expected file");

		char[] data = read_zip.read_file_all(mem, "docs/readme.txt")!!;
		defer free(data);
		assert((String)data == "README", "Data mismatch");

	};
}

fn void test_zip_crc32_verification()
{
	@pool()
	{
		// Create archive
		ZipArchive zip = zip::open(mem, "unittest_crc.zip", "w")!!;
		zip.write_file("data.txt", "Test data for CRC32")!!;
		(void)zip.close();
		defer (void)file::delete("unittest_crc.zip");

		ZipArchive read_zip = zip::open(mem, "unittest_crc.zip", "r")!!;
		defer (void)read_zip.close();

		char[] data = read_zip.read_file_all(mem, "data.txt")!!;
		defer free(data);
		assert((String)data == "Test data for CRC32", "Data mismatch");

	};
}

fn void test_zip_multiple_files()
{
	@pool()
	{
		// Create archive with multiple files
		ZipArchive zip = zip::open(mem, "unittest_multi.zip", "w")!!;
		zip.write_file("file1.txt", "First file")!!;
		zip.write_file("file2.txt", "Second file")!!;
		zip.write_file("file3.txt", "Third file")!!;
		(void)zip.close();
		defer (void)file::delete("unittest_multi.zip");

		// Read and verify
		ZipArchive read_zip = zip::open(mem, "unittest_multi.zip", "r")!!;
		defer (void)read_zip.close();

		assert(read_zip.count() == 3, "Expected 3 entries");

		for (usz i = 0; i < read_zip.count(); i++)
		{
			ZipEntry entry = read_zip.stat_at(i)!!;
			assert(!entry.is_directory, "Expected files only");
		}

		char[] data1 = read_zip.read_file_all(mem, "file1.txt")!!;
		defer free(data1);
		assert((String)data1 == "First file", "File1 mismatch");

		char[] data2 = read_zip.read_file_all(mem, "file2.txt")!!;
		defer free(data2);
		assert((String)data2 == "Second file", "File2 mismatch");

		char[] data3 = read_zip.read_file_all(mem, "file3.txt")!!;
		defer free(data3);
		assert((String)data3 == "Third file", "File3 mismatch");

	};
}

fn void test_zip_streaming()
{
	@pool()
	{
		// Test streaming write
		ZipArchive zip = zip::open(mem, "unittest_stream.zip", "w")!!;

		ZipEntryWriter writer = zip.open_writer("stream.txt", DEFLATE)!!;
		writer.write("Part 1. ")!!;
		writer.write("Part 2. ")!!;
		writer.write("Part 3.")!!;
		writer.close()!!;

		(void)zip.close();
		defer (void)file::delete("unittest_stream.zip");

		// Read and verify
		ZipArchive read_zip = zip::open(mem, "unittest_stream.zip", "r")!!;
		defer (void)read_zip.close();

		char[] data = read_zip.read_file_all(mem, "stream.txt")!!;
		defer free(data);
		assert((String)data == "Part 1. Part 2. Part 3.", "Streaming write failed");

	};
}
fn void test_zip_invalid_access()
{
	@pool()
	{
		// Test non-existent archive
		ZipArchive? opt = zip::open(mem, "non_existent.zip", "r");
		assert(!@ok(opt), "Expected error when opening non-existent file");

		// Test non-existent entry
		ZipArchive zip = zip::open(mem, "unittest_edge.zip", "w")!!;
		zip.write_file("exists.txt", "data")!!;
		(void)zip.close();
		defer (void)file::delete("unittest_edge.zip");

		ZipArchive read_zip = zip::open(mem, "unittest_edge.zip", "r")!!;
		defer (void)read_zip.close();

		ZipEntry? entry_opt = read_zip.stat("does_not_exist.txt");
		assert(!@ok(entry_opt), "Expected ENTRY_NOT_FOUND");

		char[]? data_opt = read_zip.read_file_all(mem, "does_not_exist.txt");
		assert(!@ok(data_opt), "Expected error when reading non-existent file");
		assert(!@ok(data_opt), "Expected error when reading non-existent file");

	};
}

fn void test_zip_empty_archive()
{
	@pool()
	{
		// Create empty archive
		ZipArchive zip = zip::open(mem, "unittest_empty.zip", "w")!!;
		(void)zip.close();
		defer (void)file::delete("unittest_empty.zip");

		// Read empty archive
		ZipArchive read_zip = zip::open(mem, "unittest_empty.zip", "r")!!;
		defer (void)read_zip.close();

		assert(read_zip.count() == 0, "Expected 0 entries");
	};
}

fn void test_zip_recovery()
{
	@pool()
	{
		String path = "unittest_embedded_broken.zip";
		// Create a "broken" ZIP (LFH + Data, but no Central Directory)
		// Filename: "a", Data: "bc"
		char[] broken_zip = {0x50,0x4B,0x03,0x04,0x14,0x00,0x00,0x08,0x00,0x00,0x00,0x00,0x00,0x00,0x38,0x2B,0xA9,0xC2,0x02,0x00,0x00,0x00,0x02,0x00,0x00,0x00,0x01,0x00,0x00,0x00,0x61,0x62,0x63};
		file::save(path, broken_zip)!!;
		defer (void)file::delete(path);

		ZipArchive? normal = zip::open(mem, path, "r");
		assert(!@ok(normal), "Normal open should fail on broken ZIP");

		ZipArchive recovered = zip::recover(mem, path)!!;
		defer (void)recovered.close();

		assert(recovered.count() == 1, "Should have recovered 1 file");
		char[] data = recovered.read_file_all(mem, "a")!!;
		defer free(data);
		assert((String)data == "bc", "Recovered data mismatch");
	};
}

fn void test_zip_cp437()
{
	@pool()
	{
		String path = "unittest_embedded_cp437.zip";
		// Create a ZIP with CP437 encoding (Bit 11 NOT set)
		// Filename: 0x80 (Ç in CP437), Data: "x"
		char[] cp437_zip = {0x50,0x4B,0x03,0x04,0x14,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x83,0x16,0xDC,0x8C,0x01,0x00,0x00,0x00,0x01,0x00,0x00,0x00,0x01,0x00,0x00,0x00,0x80,0x78};

		file::save(path, cp437_zip)!!;
		defer (void)file::delete(path);

		ZipArchive recovered = zip::recover(mem, path)!!;
		defer (void)recovered.close();

		ZipEntry entry = recovered.stat_at(0)!!;
		assert(entry.name == "Ç", "CP437 decoding failed");

		char[] data = recovered.read_file_all(mem, "Ç")!!;
		defer free(data);
		assert((String)data == "x", "Data mismatch in CP437 test");
	};
}

fn void test_zip_with_comment()
{
	@pool()
	{
		// Create a ZIP file with a comment
		ZipArchive zip = zip::open(mem, "unittest_comment.zip", "w")!!;
		zip.write_file("test.txt", "Hello, World!")!!;
		(void)zip.close();
		defer (void)file::delete("unittest_comment.zip");

		char[] zip_data = file::load(mem, "unittest_comment.zip")!!;
		defer free(zip_data);

		isz eocd_pos = -1;
		for (isz i = (isz)zip_data.len - 22; i >= 0; i--)
		{
			uint sig = mem::load((uint*)&zip_data[i], 1);
			if (sig == 0x06054b50)
			{
				eocd_pos = i;
				break;
			}
		}
		assert(eocd_pos >= 0, "EOCD not found");

		String comment = "This is a test comment!";
		mem::store((ushort*)&zip_data[eocd_pos + 20], (ushort)comment.len, 1);

		char[] new_zip = mem::new_array(char, zip_data.len + comment.len);
		defer free(new_zip);
		mem::copy(new_zip.ptr, zip_data.ptr, zip_data.len);
		mem::copy(new_zip.ptr + zip_data.len, comment.ptr, comment.len);

		file::save("unittest_comment.zip", new_zip[:zip_data.len + comment.len])!!;

		// Try to open it
		ZipArchive read_zip = zip::open(mem, "unittest_comment.zip", "r")!!;
		defer (void)read_zip.close();

		assert(read_zip.count() == 1, "Expected 1 entry");
		assert(read_zip.comment == comment, "Comment mismatch");

		char[] data = read_zip.read_file_all(mem, "test.txt")!!;
		defer free(data);
		assert((String)data == "Hello, World!", "Data mismatch with comment");
	};
}

fn void test_zip_write_comment()
{
	@pool()
	{
		ZipArchive zip = zip::open(mem, "unittest_write_comment.zip", "w")!!;
		zip.comment = String.copy("Created by C3 ZIP library", zip.allocator);
		zip.write_file("test.txt", "Hello!")!!;
		(void)zip.close();
		defer (void)file::delete("unittest_write_comment.zip");

		ZipArchive read_zip = zip::open(mem, "unittest_write_comment.zip", "r")!!;
		defer (void)read_zip.close();

		assert(read_zip.comment == "Created by C3 ZIP library", "Comment not preserved");
		assert(read_zip.count() == 1, "Expected 1 entry");
	};
}

fn void test_zip64_headers()
{
	@pool()
	{
		String filename = "unittest_zip64.zip";
		ZipArchive zip = zip::open(mem, filename, "w")!!;

		ZipEntryWriter writer = zip.open_writer("large.txt", STORE)!!;
		writer.write("data")!!;

		// Manually set the size to > 4GB to trigger ZIP64 headers in the Central Directory.
		// This tests the fix for ZIP64 extra field serialization (ensuring no byte truncation).
		writer.entry.uncompressed_size = 0x100000001;
		writer.entry.compressed_size = 0x100000001;

		writer.close()!!;
		(void)zip.close();
		defer (void)file::delete(filename);

		ZipArchive read_zip = zip::open(mem, filename, "r")!!;
		defer (void)read_zip.close();

		ZipEntry entry = read_zip.stat("large.txt")!!;
		assert(entry.uncompressed_size == 0x100000001, "Failed to read ZIP64 uncompressed size");
		assert(entry.compressed_size == 0x100000001, "Failed to read ZIP64 compressed size");
	};
}

fn void test_zip_utf8()
{
	@pool()
	{
		String filename = "unittest_utf8.zip";
		String utf8_name = "测试_🚀.txt";
		ZipArchive zip = zip::open(mem, filename, "w")!!;
		zip.write_file(utf8_name, "content")!!;
		(void)zip.close();
		defer (void)file::delete(filename);

		ZipArchive read_zip = zip::open(mem, filename, "r")!!;
		defer (void)read_zip.close();

		ZipEntry entry = read_zip.stat(utf8_name)!!;
		assert(entry.name == utf8_name, "UTF-8 filename mismatch");
	};
}

fn void test_zip_zero_length()
{
	@pool()
	{
		String filename = "unittest_zero.zip";
		ZipArchive zip = zip::open(mem, filename, "w")!!;
		zip.write_file("empty.txt", "")!!;
		(void)zip.close();
		defer (void)file::delete(filename);

		ZipArchive read_zip = zip::open(mem, filename, "r")!!;
		defer (void)read_zip.close();

		ZipEntry entry = read_zip.stat("empty.txt")!!;
		assert(entry.uncompressed_size == 0, "Size should be 0");

		char[] data = read_zip.read_file_all(mem, "empty.txt")!!;
		defer free(data);
		assert(data.len == 0, "Read data should be empty");
	};
}

fn void test_zip64_offset()
{
	@pool()
	{
		String filename = "unittest_zip64_offset.zip";
		ZipArchive zip = zip::open(mem, filename, "w")!!;

		ZipEntryWriter writer = zip.open_writer("offset_test.txt", STORE)!!;
		writer.write("data")!!;

		// Manually set offset to > 4GB to trigger ZIP64 headers in the Central Directory
		writer.entry.offset = 0x100000005;

		writer.close()!!;
		(void)zip.close();
		defer (void)file::delete(filename);

		ZipArchive read_zip = zip::open(mem, filename, "r")!!;
		defer (void)read_zip.close();

		ZipEntry entry = read_zip.stat("offset_test.txt")!!;
		assert(entry.offset == 0x100000005, "Failed to read ZIP64 offset");
	};
}

fn void test_zip_reader_pos_and_seek()
{
	@pool()
	{
		String path = "unittest_reader.zip";
		ZipArchive zip = zip::open(mem, path, "w")!!;
		zip.write_file("test.txt", "0123456789", STORE)!!;
		(void)zip.close();
		defer (void)file::delete(path);

		ZipArchive read_zip = zip::open(mem, path, "r")!!;
		defer (void)read_zip.close();

		ZipEntryReader reader = read_zip.open_reader("test.txt")!!;
		defer (void)reader.close();

		assert(reader.len() == 10, "Expected length 10");
		assert(reader.available()!! == 10, "Expected 10 bytes available");
		assert(reader.pos == 0, "Expected pos 0");

		char[3] buf;
		assert(reader.read(buf[..])!! == 3);
		assert((String)buf[..] == "012", "Expected '012'");
		assert(reader.pos == 3, "Expected pos 3");
		assert(reader.available()!! == 7, "Expected 7 bytes available");

		assert(reader.seek(2, Seek.CURSOR)!! == 5, "Expected seek to 5");
		assert(reader.pos == 5, "Expected pos 5 after seek");
		assert(reader.available()!! == 5, "Expected 5 bytes available after seek");

		assert(reader.read(buf[..])!! == 3);
		assert((String)buf[..] == "567", "Expected '567'");

		assert(reader.seek(1, Seek.SET)!! == 1, "Expected seek to 1");
		assert(reader.read(buf[..])!! == 3);
		assert((String)buf[..] == "123", "Expected '123'");

		assert(reader.seek(-2, Seek.END)!! == 8, "Expected seek to 8");
		assert(reader.read(buf[..])!! == 2);
		assert((String)buf[:2] == "89", "Expected '89'");
		assert(reader.available()!! == 0, "Expected 0 bytes available at end");

		// Edge case: Negative seek SET
		assert(!@ok(reader.seek(-1, Seek.SET)), "Negative seek SET should fail");

		// Edge case: Seek past end
		assert(reader.seek(100, Seek.SET)!! == 10, "Seek past end should cap at size");
		assert(reader.pos == 10, "Pos should be 10");
	};
}

fn void test_zip_comment_boundary()
{
	@pool()
	{
		String filename = "unittest_comment_limit.zip";

		// 1. Test exactly 65535 bytes (Should pass)
		{
			ZipArchive zip = zip::open(mem, filename, "w")!!;
			char[] huge_comment = allocator::malloc(tmem, 65535)[:65535];
			mem::set(huge_comment.ptr, (char)'C', 65535);
			zip.comment = String.copy((String)huge_comment, zip.allocator);
			zip.write_file("t.txt", "d")!!;
			(void)zip.close();

			ZipArchive read_zip = zip::open(mem, filename, "r")!!;
			assert(read_zip.comment.len == 65535, "Comment length mismatch at 65535");
			(void)read_zip.close();
			(void)file::delete(filename);
		}

		// 2. Test 65536 bytes (Should fail with INVALID_ARGUMENT)
		{
			ZipArchive zip = zip::open(mem, filename, "w")!!;
			char[] too_huge = allocator::malloc(tmem, 65536)[:65536];
			mem::set(too_huge.ptr, (char)'X', 65536);
			zip.comment = String.copy((String)too_huge, zip.allocator);
			zip.write_file("t.txt", "d")!!;

			fault res = @catch(zip.close());
			assert(res == zip::INVALID_ARGUMENT, "Expected INVALID_ARGUMENT for 64k+1 comment");
			(void)file::delete(filename);
		}
	};
}

fn void test_zip_reader_available_capping()
{
	@pool()
	{
		// We manually construct a reader to test the capping logic for huge entry sizes
		// that might exist on 32-bit systems (where usz < 64-bit).
		ZipEntryReader reader;
		mem::set(&reader, 0, ZipEntryReader.sizeof);
		reader.size = 0xFFFFFFFFFFFFFFFF;
		reader.pos = 0;

		usz avail = reader.available()!!;
		assert(avail == usz.max, "Expected available to be capped at usz.max");

		reader.pos = 100;
		avail = reader.available()!!;
		if (usz.max < 0xFFFFFFFFFFFFFFFF)
		{
			// triggers on 32-bit
			assert(avail == usz.max, "Expected available to still be capped at usz.max");
		}
		else
		{
			// on 64-bit
			assert(avail == usz.max - (usz)100, "Expected available size to be correct on 64-bit");
		}
	};
}

fn void test_zip_ae2_decryption()
{
	char[*] ae2 = x`504b0304330001006300d95d555c00000000260000000a00000009000b00
		68656c6c6f2e747874019907000200414503000064566142d317c189266a
		1d2c30e6da4e1da17c6b47dab1b39a04dbc52442e9e1a57147386575504b
		01023f00330001006300d95d555c00000000260000000a00000009002f00
		000000000000200000000000000068656c6c6f2e7478740a002000000000
		000100180069cf9e611fa3dc010000000000000000000000000000000001
		99070002004145030000504b050600000000010001006600000058000000
		0000`;

	// Create archive with encrypted data
	String name = "ae2_encrypted.zip";
	File f = file::open(name, "wb+")!!;
	f.write(&ae2)!!;
	f.close()!!;
	defer (void)file::delete(name);

	// Read and verify
	ZipArchive read_zip = zip::open(mem, name, "r")!!;
	defer (void)read_zip.close();

	assert(read_zip.count() == 1, "Expected 1 entry");

	ZipEntry entry = read_zip.stat("hello.txt")!!;

	char[] decompressed = read_zip.read_file_all(mem, "hello.txt", "password")!!;
	defer free(decompressed);

	assert(decompressed == "HELLOWORLD", "Decompressed content mismatch; got='%s'", (String)decompressed[..]);
}
