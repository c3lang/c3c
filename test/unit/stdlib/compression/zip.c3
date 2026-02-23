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

fn void test_zip_ae2_decryption_deflate()
{
	char[*] ae2 = x`
504b03043300010063001b52565c00000000212000008b84000006000b00
7a69702e633301990700020041450308005c0c003967245d00868f20654f
09d01817b78e585be39f26c3b86f89b32b971ea9899503cb2d1beadcee86
ade29e4c40dc5d4bdef7aff2d9a60bdf9dd749a8d1b7c3f3ec278bc8c135
279a71e7bbafae4d76de1f0ba7e965426ff78cce0841ac636a13c4581f71
94b5c2880e813457a330d7215dfa80604491ad71d29a630818496fe3d00a
f8780c3dd9fd9bcd66e7f20b0c1393877214fad3ad7ddea00e93ac81b619
cbc14c3cf7d15b0c9dae4838501281f0156e9349a7ea9072c072835d5630
b35408db3d6951aca0d8e0cb23634fea06b31a957d5118cdf8672d0b4de4
7e978c809ea35fa0966034dbbe4e59d090c53d932f7a4337fbb2d311f03a
d42e9398c1917e7c3996fff3ed526ce2175525005a72b4c739586aac3aca
d91adafbd28b78a7ab944beea5b308f2a4750b57fa78cf51163f0c0234c0
d7431772704388cc0b92465c31326c2854e3d892a31332d5747ec4444bd3
2a9dfd77055ac93a9e5b79a0bc36cb1bf0957061e17a50209ee84f724175
c1a3b238f939a0a82007660fb8f629e58eea1543ab25a734e8ff452bd4a7
e3ae193eb40e98b3305bb9922662065d85801f682733b8b5a9db6ac7f7b1
de2461ef3733ba01b3e88862f953066c0d4847fab270849c60df249be4b4
84a19056e684aaabf09a5907327edaa55f8da51fb26d9196d8f5de2d4e43
487c65c3a646f686cbac939c663e21c3985448126bdc7019c652c327a8e3
e35a808f90b67f3c8979339f2eb980533046a4f81ee78d2ad968fc05bca4
dc47d820fdd247dfb7bc26f1bab42d9405c9a173e8ef9b56c819b8657550
1a58f751d91c9ee568a8419c18c5173381833470a0cbe5872384d2bf4be6
7d86fc6159aeb88faa6f6f6ca34501228e7fdb0fc29654e64ffb395f2264
363ea6c1f35a8a76907e44fa4ce8fa51f688b17afeedb847180a60b50b76
4986d34a9410cf9d1a474d395df00e9b51bef93009c90f0c9859d7eaa2f8
4761fb0fbcd9249193b0cdb8d2460d22e08cec6bddc9b007e017bc6a6e4b
83a8cd3058c0f7ab1d1c94d51efc6a7a35c4672739f2dc7f11693439ed46
fb503ebd4efd85a9364f5b0c723b8a0f473611cb6ae92f3542f58a50e4c4
e1d13284bf6b5c0ffca8359b9749d9787be25fdd59a6adb638cdab9271c2
dba036b7d5947bcc5e659146f46db71b047bcec95bdac910fa400b41aa5d
0b2c6243ebedc991310653a6b9ec3300800f9f9b4bc4daae32e06e174993
f466bb14d14f283b4385f13e5140efe805b4b982970ec7c924edfd6cf1e5
497b8e9ddc9f3a465e80d867d03efcc1d8a64682ba1a58f7d4a3c5e28967
4a1687bc73299fc510cd66a11bfb9fc85107afaabcb355a9be2d0eaa30f7
6d9673cad137b50fa9e0903873e78e7c2f6d566499edc124d4a2ece561ac
59388543411e8d4f5d6457648936d06abbf1209cc30a7e4d10d71e77bb0c
a6d458eea09d26f84b92a84182e98b01caad8886258ec785b8e83dea0884
01c83c0055c66de52324426921a10b1fa4c86d11b3d343c64850752fddf5
dedd8d298f258dbe3a4e51367c9c95493de430fba0e5f782023e1986dddc
491482b17c17e6cbfd501bc56b819d3c825d130050f0f587dd818618b074
071f572a995e30853bb19f609d011c069e0e8e85e229ead957b97dfa74fd
7d35bdc2bafe171d41847f93e32e28d1e9131bab007788dcfa828b925109
fda1a8d754bfb773775d81b3c82fe0bd0fb7d4b6fecab7b8b0d2a24f1e58
e59e43973e761585461c4348c431d6f01d960e80f9d8409236902cd780a7
4e95e3365696227c67c2f20d1b43966a62317a3f0b3614fb902a191ef954
ae444ca7e9e94e107fe1fb306e6d11620d21e8603144e4bd5732d9927b7c
170420ff5ceffbac893268b2073ae5af60988efd9074dc3843a85ee4aabd
5ee7140e7ce749de0599dd44b5c5081e01a537424207e4f10f8bfe81d944
bbcb59ee1a4c4f0b3063eaa4a16dc581609605df836aec5bde6ce71060a6
d15b05dc1beb7028473dffea1d43ecc9a6d881f3239008647a4de953eedf
975aac3b4042054a133230a3a750d727f9a40052f6b7395e645428d315b5
abfd4f21904adc3f1071df0f59aff5e1117da61ff94f35455769f0519049
612612c3bb91a464f2d841ee1425ca4ff1ad79dacdb2533a8de0180b9def
f0da23478ff1d2212a5c778fc3d534783c8fbc53933a4e235767d09d566b
01217856c826335b4ab6ff3630299e7d2eaa36b1ee6bad6b6313fa99f0ae
34aa29d126c2812da4fea97fcc42973c853eb18fc3ffed9cd2bdc6e15ec7
bc793721f1ea88e867f305dbdbd67e70d5de7c9831fd3a485c97165aae07
c7b102e3860293dcacfa1951a7afcf1310817eb1431662e13e4780fbf7bf
dc9f065a0073be029267d228465e344c978515c4ce61944f0e6ad011049c
2419e390809d259fb0e5abbca8ecd50c63b7c2fe22df20e1c514651aebcf
c3da3188134626928b65dea62683fa52548247bbe6ab3f9dbd1176385061
bf93877683991fa56090da71c7f7b6eac62d0cc7bdff23580a5cb0bb37d0
0e01dc104be7a8576282cd8b9975335864e7b66e8a555d33d6aecf26c333
f38814cafd95080250597609e296447c6eaf20c32e15609de7a28c8ba520
1a5138ff367a48c8ca97fc12f425c1704bcabe4460935214dbe2d4eeba9a
4981f461116dc25188c891502628fd84f590cc1065874ed298befa3f2cf8
8b8db0c1d5f41193111ee81f8a77635092681d0778773bceef16a2d2f94f
ac6aadd0cb7376e38aaf22f9f9d65184fd75afe782f5323fdaff397ac201
ad15223349828e7fd9991a8f1b4cf2841784212f739b3832ffe7529dec75
9598d922033f8d6d1bf2bb191a61fbd8a90ce4a93c3ca478ff73c3d1a6b3
e87046b70b593621f0ec3a175823b1229d751445b42379b6c976806b6987
33759a0998f5157dc98e633e0e056ccf88c69bdd700f8f03b4fb9ceee83a
6150cd88948b6b45f5e8118946787fe8813f7eb47fb17ab533a34c7b959e
b8b48c4395a64574c6d249aecf9d293857fe6b2ff04c163e02526088ae86
755ea851fdebd32de963230b9caf68c1154bf0070d8f33da90d0db696fc5
0744e27d013158906265fcecbc1b38f3166666234f12593e8a2dbce0c656
b42a4840a4c393adc28dcba93fc49f6a2ff2eb9a3b8e67ea1d3711caa95b
5934e5326df6e9ac457ce9fe58f3b10afd5c9b075471e447c56149ece605
5e59e8e06a17b8b632967dd42e2b39be10f54cd8169aece309142dab6e88
76457a338c3cf6a940910ce4ccdc204fc2cd5d11d5cb8db6e5301d4bfaaa
b73777b9f0e6448e80dee6b07268c1dc0fb78e397b9c6c9de16ee7baff44
05b46eb246bb2037bb9c82d538f2c5dd512ac81910753759fa06a3813f77
41b936b4b0a9c9664ddeef64a9b32d584d363d987a7c97d539f199b1363f
dbd748864051da56cb7caf8c23c68421034392ef56b1edc2e6c64b5ff799
2714fb86cb7623810727221a0de1e72f04751773289a6392180bd265e577
eb31ada778b06334494f0b4001db6f68baf71cbbf048338fc2077574b966
b7295b7fe90ea71da785a58185017a156a305eaddcbe1d2ccda641e97e8a
4380cbb4d0e69b21840d6b30ea1d7d9e5f7e4fcf19c033482b59d343ba8c
d5198e3a39d840fa12a7e4815c246941e2d4a427303343d7d91954fd3ff2
dc79c34d224f3d03cc5fa3f71d6cabadcb2119348dbb036f3c77b9dd2f7e
222f86a4f26e3b7e520361842ee80d19cdd6491e41bf9502b13060b6a5dc
3f2f46828d530126825f4ffeaaf48f571a596b9a481789c01bfdafb52913
7aa487f72b687de5508ed29012a3c6537d3bd4de9790a224e3be05743912
18100323662d534ca1599fda948dd9c9258ee27e2016c5c400bdaf5dd079
f1f250d2bd74ca5e1bb52601d311800aaf03b6f73486c335892965654feb
8657628d095e2def4adece5dda2c859e0dec69f8b4ea72f9b887656de057
8051e3e6d74760ab33ca2b5fdcf59dc7cdf6fde7df6487b5e77020475413
4cc77f16cf1c4d992b643125155290aad1c921962d248ca610139e1df5fe
c6ddff3697f53056cae236ac9e483c8d736e9520d1fb985b4c91f531ffe2
e622ee6488197fed00a13f2c85e6180fda6451995f0148ced1c6364abba8
08a06e8975a257bb14d17d36f609ef30d7f459e56ea6dd437645b23e369e
bec33516cfc244614f55464eb3af86c34ee83dd17be673b8806bc421325a
1797eb85c4e37f5fdb054e4a857d41bd9bd9fa272c9b0465e2c1a796ca87
19bb9ad5abbc038ea78cd2004e0e851ea50cfa836da52345dc1dd2d41b68
4c03ef92876f64e1fc0692d3eb773f32dc2774260e357b477551d381079d
0c1170195cb2ad35c59bdc971cb48a45e29c90937e6326e9f378a9dc88c7
f4db10b2abe59fe607624831a232c88cc8a1b6a1a749f7913591582b4abc
af8aaff1ea45fd3e214737d63d3d63c2029b9262d51d39c3a0c519f9f2ec
b2539ac73ee0067d76622ed9bdb0830ee274de0464058b1cb8946f01382d
c69f6d2acbc1c273309d8aa7c5404bd6fcd9665cc79cce8ee82c2f5d666a
15adfd2950323b5d278bd25515a015c6ead9425ee6073c72ebc542a1b847
3e58e1dd792d4da98db10bf446713ef76b66e1820aa5ebc3ecde8c227a3e
0f9f352ae6b4eca6e23ccac5819e9c392719a697d205ec7c99b19b9cac3c
c7bd653efab9e3b383ffa5d9cf4df4de61c9d5659035efc4044afb07c0ee
b7e7ae95c9ef569e03014180dfcfd25b9831b2a6582a9707dae3b9fa2636
bb06da0d60d5135f7da9b088df8737834344ffaceb9dfdddcdd2cd05fb0a
1da2aaf943133b4ab5ecf3a49b8fe673d6b332c33781467b01f98babf082
16d5cd408383f59151ae3fc7dff842564fdb6707a750b4079a83cc7dfcd4
1e8fadc8e16f294c6e39820bfbebbcdab3c3b4dff286b327a35dd28b55d1
8953360983f6c972e5f6735e19d85b606207bbe7389d0603fceff3c1ced7
1fd060549e6b9e3f61457d10fbe17ffad74d062fbec1851d220253dec1d5
ba4c202d24672c643eaa90c34505a9cb566c54f78d5f0e1e7e711cc87a39
0cabb5e0f0fbe45b273f97650fc55fcae82c1e896720112c3c124bae00dc
a823658759acbc907920caae67cbca2f19b88c68f5ec195d6903a013b441
c09b32a4115cef4fdd86686b6d0095779d3dce124ef9c2595748ef0de0de
11ae6c41664316883d7e8f0461777489164a1bf230399b9a7b066f9afecf
aa171a6a85de89caa233a51e218f429d143ca30477f09f8b116e78b8ec4b
287fd44838fcd43ff3151976ad90606b983ec9ed357f93d9b4ef7b65fc37
0ac972e5e6737eb498dc2650822fca904abace038e226de332cd871a5f3d
683800fd0ec5c5cf5404b3de9e7b37e9ae72f9fca563864e0cf46c8cf991
6611ab8ee5e4b487827caddc9109c526c96a02eae9d55efd8f121426cbce
fcd3230f71966c787a28ad8c35e71c130ae9f811d6b14ff5a55f44ab25ef
bb85e9e7f381233571c6ba12c4fa89ab665c314cef24324a917372d6b9b0
59f1f278d42dd9dc00bbc1faa0b3b23eb101908a3d7efd83867bcb17164f
238c71f69ee492140d66b0964809adb3736d223073ea978d171f4cb412e5
827f9f53f97f4abf296b4999574859aad9c5ef99024bd736ae8c44833491
b2e3504eb93c215fb4469be2c8690597e9f8419e20ff57d0e62f90da331d
fb8c6601bbeb44aa67529e01d6dd56fdf6bf4bf6590bc58179fc1aa2fe64
f64862783f62b4176f9bf5676577990fd2c6024b8e255b9e841ac7c87224
d06549a61f6b988e301d4a7760de8aa472cfd04def2a05b21a30de40d985
f6dce303acf8a5ed8bb73b192cda56fa198e45702c66697dbf076bff21df
eaca01ef3c924cda22fa25b1def7d4babaf4dc1ce2d8dc306009cd3b5ea1
9be690a0c5321e3b6294d0a05ab991d3af215d600f0e76a882d08ed2f133
9bb24c138b2a164e83cc0b98840b849a8e0a71dc3c6befbed253334ded3d
3d6c26e09fcd3bdda68ac622e7fa1ee16298d4e0cdfc052bfc4d27893a77
53fb6234ee770dd10e0286eac4b324c012188718760d32a9ddbec0d8f406
8e7316b49fcbe265f88e8f246277dc68a85fe325f5a44b7019ea70d404b7
5e1cd8f29109d94cbfa793d1cfd84ac21fe4091972a6e4a55985567857ba
995ccdd82285062da65f59aa8ceae229959b211fd1e1d3a01ab4cd1ab137
78d23c068ab9a56c5d576ef8cfabca1e597864308a5af4ea4d358ba0d06c
97fb6e4e30392ba03364607acaf964da4b716811658a1feb796a1897ca74
65141ef4a93b26707d9494c9b3c1ebc1f562b1550a54102ccb0e047e2062
df968c882a79a34cf781e958c9d526650597728524e3a20f15844d4c6682
c8de23958694a94427989977bb606d2b867ff5dfa3b2a2b97209afba948e
172cb5a3e49a6199608b9d89d025bb37b479bbe4e6bac67f82c63f821cbb
d3587a884e958633d192d688ad812c839ddcc1dcf44a57853e2e00ca2514
32485ede82c4e595243d7d92041c9b18720695ab4f42b0d5a4ea13b5272e
0780adc74f0a21b3eb7f03ec93c084540cdcbd38c4fa613807aaf7bbab79
f5799133535091b731211405bd14b7ec9a0ea41037803807862536a5a7f1
42d9d0af0ee81c2730026f46b3990c08ec4f2f0ef0b6856da4cd4ae8ad4b
7367459c483ccac7ab8d894b445af8cf6b08ea73afaa7fa3a47678cf34c2
1b6bb8e0c45645d8276f17ad025bc6ab5912e1bd8522def2a6df63591e6f
6d78c14ac24e83908119f77be210296fb3c1d3ff2d05c164c8c4fc3f2468
34aaa7e138e2b43d0f99c98e4b981a00e5ea343b2b33d1e1dee73fe08be8
cf8b7b1616dad2f685f559e7844452c3f3ddb0f90894ebeb6598be72edf4
10005a4d99ae7cea31cce8389f92e05fbb28016859b29a28feb39ee419d5
1b9510f1c896a89b8bbdc973b2b721755f30625cf35b5e8067d74e20486a
ba13642a7f1f8457403e33862ddc4ec0a468ea687773d51e52831704e79a
dedbebbe2718c09f3edf2155ba57d8ccb459983fbb8d7f547cb332c18298
5833d5e9535ab0b7144317bc987bebf69ac187745ee6eb70b76c52db92e3
5484e5891ecd8e87e03ad389f0cc525e67eedf495402360517ac42493ccf
ea58fc406b75e3d0431905a62e0efe4265c076516750296e959cb82a33b7
9ac324d5ae7fa71531ca541977b5a42537522c86636e6e4090eb354dc968
29510f5166edf6e7b99826c37d044b178d337597d8cf5ae7676ba214692b
265fc8d3fc781c1e46225add856f28243693a5b97ac7b1f75988e6329915
5c8756ebd6d75fb413f5ca3c959ff82fe9b82d122388951e33d6ab45983e
a3cd1209eedca3ac679c1cb4d1ce06d10cc1656536a9c592caf483fa9f9a
dbd287ac6461cf8cdbf5e22da67df8705aa01fb2e7ce6c06521bdd9e9729
3e1df6f7619c66d3930a86d5c7565287a36547906ca3b73d86469e4af61f
bf63bc70582f3081546b22df082a674dcc4d96dde1c24e0869da11263d1a
439b61a45186f652557bee42020361aabc5c5965824fdd644003f0a14134
747cafc63b51e80a7ac662cc14d6a476ae6035134a860325252681f6ede4
53f635dd5ae7cba5c53f4e0441441080e05deda22ae8cbdcf05e9d425bec
2670b5da3ebfc3f7b7daf2bd39391e1b15aed631b3d695cfedaf4b8b3252
939ed64bbbc6cd67d4c0ebbb1fddc5433a4b4ae9cc8954ac85d00274c78e
7a4d4422bdca7a40ca410d17243e55a2ccf9b8c20bd34d93621b5ea35cf7
2ae43cec37125d6b872a9db1e8569ed20240eac23a134a0f6e72c13b9771
39c728200bab527bcc35e8f38691700585659f5e79e44c0f7fab5917722e
92f298f14a7ca3a4f67193f67324a17de4f24e557629348c3456825ba3a7
d7d61e869faba18b411e3851426fc5b986e5f5199e7968baa1aec8917d79
6888a84a75f03b4ca07858676ae7fecf3342a5987e0f21951604e7bba324
64c3384206bb5667d89961e4e8dc3b714bbe1d7d902a179041ad8607d10f
08b0d17962743379dc1a043e67e875cf7bbd51a01d0bf54d2ef5aad937c9
c9aad8ee23c4f2ade9689bcb3da9d08851ca2ec95c8e52b0d5669a7b76cf
39d36179485754bd5afdabafc4c2303c0cc2e9d74c36765f7bf69e2b8466
82350dad8e3308297e9c2b3388dbbba0d6087eefebb67b3e5a19e08b2272
0d8b3fbd91d3d7424c196e4b54554fd7beefbfda1113417120c43a98cd23
230c3331eeb5e10a50be646fb2e024c5e0a7450fbb327431a2aa3d928f01
f7b6203ea8e7293c5dd83b2c3e8599341bc533ed768c5c413272fee1e1ca
788f6efaaf0b13faca20a2d5e9e6acefbac64e2721123a3e0f77eb5876c0
950fd35cf509878abfef0883d33cf8a583213ea963489880c4d55ef7b891
0a2de48e4df4d2716b444c1d734710921bad48109fc7a3debc0dbf72439b
e2d381cca5b6f78debb0a73e1b838d25f8d6867a960b4b43e5c4c2671157
1470ce3b4d06bdc00d2f867db87b070ad8c2a35147799f371c6478d93cc5
d3a869f515d7151e861bbac6c906b61ae1d935b63508e183ba0d76b96f8e
15aad190ef4f8f419312f88c157eb5213b1dad8bde04fc7cac2b10006d7f
b4f1cdf6ad7b17247a9076f63853a962aed9d4bf77ff28e8c56910044286
1ba5b0a3dd3a89ecc2a0f75733d5f6a07fd6642626313937be3890727984
536e357798727f4c6f5d6d705e357f34792c05c862710e91db894e935a3f
b887c828931552a613390896daf081fa28a627065471384835c173783c56
dc62c60aa3c7b4422810f69470ebbb58ed58d0974fb23c5ec8915474543b
ac00f00202b90d2e715ecc25d99b4a65e8ea4b89ebca992a9d5d2a172fa2
9ee5624ddf5bfdcf123d20acc473500c9a82d08666a135dc915a14e9a6a7
9c5741885a20e5fd71687bb1036134226757bc5ad529437bfd49efe2f656
dc25dfcd65824f45841f4b5fe7d1da0b1dfbae5bd834097173f3bd0e9435
174dba8837bb11683a1db317b72fea32dd25c96ddd4d1accf1a2ebf9bcb2
0a32c6516f70408d5371582c7998a00a9bf7cfce188661153d211f6d0aa4
d4743cfba093e2cbe16fd2b04b40c6a4faab681fc6a22a061197a9e43f1b
7195ebd54aae47b04c16340e56e5a12a59d6e46d0e7187e36bcd1b93fd9a
3d1e47142de193078ae3192a72819fceddb5c5e18572cc4535aa9c4af173
3a35003cbf0cf85d82977ac8d6020d4c79806bdcc513fb1058b920fbee81
3657d84ab071bc11669230f2cb2a029d6138c845dcedd6572ef9c2945a72
2e74c0ce24f34bb6066dc42f6ff97f963b8c6f55c064b0883458f1e738eb
f7190aa598447f1b2d21c1bc9b65c0b739bbc6fd0a14fef26b4a7a077bff
57c43b5dece566f90bcdb9cdef6e9e48668c315afe8a263884cf20d86eec
4813aa14632767741c5665ae7b49a51f236fbfbdc3bfeec825314cf0ff38
dcfff9e22344fdc3b94b65a663be7db42115ae14703f754cbbc1aa3cbf9a
bf98de6bfd2337f7f8685a5c110f0302370c331dd76c4ff595805798e953
4e22cf42291de7597d2965e695d38a6deb16e7bace1ea5fd5e8dfe0989d6
e57e50aad0e8120685f8d07ab245dd5d56cb4856ec5354c1e4e1f6ac4146
ac611abd761ee89e42cc4091ecedcc26a09a65f9ed1127bf9b209225e14d
4e5dd815270014bb01b52553e14db3d02547b6659ce7a4dfb06bab814ee4
882d9425eaff99eacde243d81ce2b4da2e342cbb527eb960e488994b27ca
a3cee08720366dceecdfeb28198f6c4c444db09f12faf5f9766106a6d8cd
9f560dd83a8f4d63f88a6fc658b75e62b9bd673e1c0ef1be03969c384919
2be047a46a3ace051fe16e32c7cc5faa8c3d2eec723850426a0c5a8e5188
9c3ebb8ed1a9c4ea5e9678b8abfa971cfb2102ef163670eb9172919dd70d
b20659d6192a2e8b6e1cae475efd134c9f2caa5145f8821466e052db4d9d
6a3e56c97b09b5aa58655311735df08f5cf00cd61eef3ec4918861f9c8a0
2bf646d654636f1c804fd0ff33979faa2cdef995c0df437ba8a201fcfd69
a5b921f2fad474a9b824fdadb9ab946367238c96bf5816da907896e35fc6
b7909e60eeb68d08d5a228f5c5dcf05542dda1502b448e542aeb361a38b7
e1cccc2e53b6560b956117b1c5aa727b3a4ebb7f0beb807397a3d53f992f
025339ea420aba83ea947f254ff5a25a59736402b9ac0ee4e895bf03efb3
267b85e2860a37241126dcc12330ef77b53be666eca8f7917c2fb94f472d
6edd7278785998119da34cd318c0c74241761139a963fd92ec5d3c08df54
f86c2abb5ab05e046e4a9a5a31aa066e1c17815a46ab45ec81829dcebe98
e25c04b6120d16b4eab0458f37e3ed0d0a4fb4a592ace75299997e6f28c9
650d98024a68d2ce6250cd9fbc779b4042823c0c45b3b8ebcd4c2317b7a4
dc5d3baf96a6876bb9cea7ab42cf78c90422aca855eacd4d2fb7ee697b70
d2f9cff8d5753d773c3c2cdf38a2de4e235817b05e7584f478192901df09
c5b6b3512c201d5d958bcfae62c0369c4c23bd975c773f24f0c1cf6a87b9
99e258a8e710ae73b3dd4a88e70c6482daff44d8f8ecef80c4e064d32040
2ccc3c2f0479d3f859f7b9adc3e11345fa7160a4386241e680ee9f31ad40
7d868f1f6e992fb7b3535fd76ec98117d84ec4900a93858715ee00634bf7
196471c39b58a07a1edcb06058bfd11055ed601f21f1234bec76332b796f
66be465fd1b737df61b1bb6fa675395e1586eed1ac7d266385c7c263bb8b
73d1339864a16ad5000e273ae75568445fbeb242288cd488701c184ed916
c5d06d246130adbc1cd1828e10870cd0fb3d59a30e7c5827cf9ccb93e92d
70e194fd90e52bf05ba439d06c411471f6fb1f0b65bb7a1e858af91afbe2
54f5b201a7a7e851528ea92e044bce291cf97700e383505c3c4d164d7a2e
203c566cae8e57bd483b82c37a9f0089e2d6d3669fba4b983a1e92701d3b
c55ba466efafa1b43c8658fa453c86fb89dfe043f468fa0a651e5892f68c
93814e18c04ded49bc5c19bfb09bf874754cbb4582a17d7d447b588d7bfb
79eaf72109646bde50f5c5afce05664bb8cb9c58b02dfb6c0e7bdfebd082
a1b8e6fa36579617e78a7034365e1d83f7a1bbd0467ac70a7b716ae16f6a
ba933c1e2ee0b1fd760d2e4ac1bf1fa5b4183a342191527c8ffa4e2db28f
90e06fb2f3b9091bc407da475d048561075a06fa9b6985ec21251033a999
73ccef7d6c1caf136189920fd7ba817a8e7b2e03037008ac4215cf66a6d7
460f3ef7ec6f5291310694459bad04cde5c5b60c12044e535ceb3003c1f6
eabe482ee859e0ddec71aa614a1f1d96964871512e25b0a1c7ab953c144e
997094dad0f21a938fc282c7c613dbb09a730ace95ca8948cccd07d4d845
2bd89ed3239bffc832369d908ba750c2cc7c9c54f3c15675bb2c7ff09378
99dc9b28732b8109017ccd6cb565c97f336adc7fc77018113f08994f581e
1499a1ae9c99ba56555ac18628fee4599809b26cbc5cb473858573a910b2
609ea3246a7da00968473a48992f5a21268fbbd68af9504b01023f003300
010063001b52565c00000000212000008b84000006002f00000000000000
20000000000000007a69702e63330a0020000000000001001800f2df15fc
dba3dc010000000000000000000000000000000001990700020041450308
00504b0506000000000100010063000000502000000000
	`;

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

	ZipEntry entry = read_zip.stat("zip.c3")!!;

	char[] decompressed = read_zip.read_file_all(mem, "zip.c3", "password")!!;
	defer free(decompressed);
}
