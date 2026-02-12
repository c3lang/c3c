module zip_test @test;

import std::io::file;
import std::compression::zip;

fn void test_zip_store()
{
	@pool()
	{
		// Create archive with uncompressed file
		ZipArchive zip = zip::open("unittest_store.zip", "w")!!;
		zip.write_file("test.txt", "Hello, World!")!!;
		(void)zip.close();
		defer (void)file::delete("unittest_store.zip");

		// Read and verify
		ZipArchive read_zip = zip::open("unittest_store.zip", "r")!!;
		defer (void)read_zip.close();

		assert(read_zip.count() == 1, "Expected 1 entry");

		ZipEntry entry = read_zip.stat("test.txt")!!;
		assert(entry.method == zip::ZIP_METHOD_STORE, "Expected STORE method");
		assert(entry.uncompressed_size == 13, "Expected 13 bytes");

		char[] data = read_zip.read_file("test.txt")!!;
		defer free(data);
		assert((String)data == "Hello, World!", "Data mismatch");

	};
}

fn void test_zip_deflate()
{
	@pool()
	{
		// Create archive with compressed file
		ZipArchive zip = zip::open("unittest_deflate.zip", "w")!!;

		ZipEntryWriter writer = zip.open_writer("compressed.txt", zip::ZIP_METHOD_DEFLATE)!!;
		String data = "This is a test. ";
		for (int i = 0; i < 100; i++)
		{
			writer.write((char[])data)!!;
		}
		writer.close()!!;
		(void)zip.close();
		defer (void)file::delete("unittest_deflate.zip");

		// Read and verify
		ZipArchive read_zip = zip::open("unittest_deflate.zip", "r")!!;
		defer (void)read_zip.close();

		assert(read_zip.count() == 1, "Expected 1 entry");

		ZipEntry entry = read_zip.stat("compressed.txt")!!;
		assert(entry.method == zip::ZIP_METHOD_DEFLATE, "Expected DEFLATE method");
		assert(entry.uncompressed_size == 1600, "Expected 1600 bytes");

		char[] decompressed = read_zip.read_file("compressed.txt")!!;
		defer free(decompressed);
		assert(decompressed.len == 1600, "Decompressed size mismatch");

	};
}

fn void test_zip_directory()
{
	@pool()
	{
		// Create archive with directory
		ZipArchive zip = zip::open("unittest_dir.zip", "w")!!;
		zip.add_directory("docs")!!;
		zip.write_file("docs/readme.txt", "README")!!;
		(void)zip.close();
		defer (void)file::delete("unittest_dir.zip");

		// Read and verify
		ZipArchive read_zip = zip::open("unittest_dir.zip", "r")!!;
		defer (void)read_zip.close();

		assert(read_zip.count() == 2, "Expected 2 entries");

		ZipEntry dir_entry = read_zip.stat("docs/")!!;
		assert(dir_entry.is_directory, "Expected directory");
		assert(dir_entry.uncompressed_size == 0, "Directory should have 0 size");

		ZipEntry file_entry = read_zip.stat("docs/readme.txt")!!;
		assert(!file_entry.is_directory, "Expected file");

		char[] data = read_zip.read_file("docs/readme.txt")!!;
		defer free(data);
		assert((String)data == "README", "Data mismatch");

	};
}

fn void test_zip_crc32_verification()
{
	@pool()
	{
		// Create archive
		ZipArchive zip = zip::open("unittest_crc.zip", "w")!!;
		zip.write_file("data.txt", "Test data for CRC32")!!;
		(void)zip.close();
		defer (void)file::delete("unittest_crc.zip");

		ZipArchive read_zip = zip::open("unittest_crc.zip", "r")!!;
		defer (void)read_zip.close();

		char[] data = read_zip.read_file("data.txt")!!;
		defer free(data);
		assert((String)data == "Test data for CRC32", "Data mismatch");

	};
}

fn void test_zip_multiple_files()
{
	@pool()
	{
		// Create archive with multiple files
		ZipArchive zip = zip::open("unittest_multi.zip", "w")!!;
		zip.write_file("file1.txt", "First file")!!;
		zip.write_file("file2.txt", "Second file")!!;
		zip.write_file("file3.txt", "Third file")!!;
		(void)zip.close();
		defer (void)file::delete("unittest_multi.zip");

		// Read and verify
		ZipArchive read_zip = zip::open("unittest_multi.zip", "r")!!;
		defer (void)read_zip.close();

		assert(read_zip.count() == 3, "Expected 3 entries");

		for (usz i = 0; i < read_zip.count(); i++)
		{
			ZipEntry entry = read_zip.stat_at(i)!!;
			assert(!entry.is_directory, "Expected files only");
		}

		char[] data1 = read_zip.read_file("file1.txt")!!;
		defer free(data1);
		assert((String)data1 == "First file", "File1 mismatch");

		char[] data2 = read_zip.read_file("file2.txt")!!;
		defer free(data2);
		assert((String)data2 == "Second file", "File2 mismatch");

		char[] data3 = read_zip.read_file("file3.txt")!!;
		defer free(data3);
		assert((String)data3 == "Third file", "File3 mismatch");

	};
}

fn void test_zip_streaming()
{
	@pool()
	{
		// Test streaming write
		ZipArchive zip = zip::open("unittest_stream.zip", "w")!!;

		ZipEntryWriter writer = zip.open_writer("stream.txt", zip::ZIP_METHOD_STORE)!!;
		writer.write("Part 1. ")!!;
		writer.write("Part 2. ")!!;
		writer.write("Part 3.")!!;
		writer.close()!!;

		(void)zip.close();
		defer (void)file::delete("unittest_stream.zip");

		// Read and verify
		ZipArchive read_zip = zip::open("unittest_stream.zip", "r")!!;
		defer (void)read_zip.close();

		char[] data = read_zip.read_file("stream.txt")!!;
		defer free(data);
		assert((String)data == "Part 1. Part 2. Part 3.", "Streaming write failed");

	};
}
fn void test_zip_invalid_access()
{
	@pool()
	{
		// Test non-existent archive
		ZipArchive? opt = zip::open("non_existent.zip", "r");
		assert(!@ok(opt), "Expected error when opening non-existent file");

		// Test non-existent entry
		ZipArchive zip = zip::open("unittest_edge.zip", "w")!!;
		zip.write_file("exists.txt", "data")!!;
		(void)zip.close();
		defer (void)file::delete("unittest_edge.zip");

		ZipArchive read_zip = zip::open("unittest_edge.zip", "r")!!;
		defer (void)read_zip.close();

		ZipEntry? entry_opt = read_zip.stat("does_not_exist.txt");
		assert(!@ok(entry_opt), "Expected ENTRY_NOT_FOUND");

		char[]? data_opt = read_zip.read_file("does_not_exist.txt");
		assert(!@ok(data_opt), "Expected error when reading non-existent file");

	};
}

fn void test_zip_empty_archive()
{
	@pool()
	{
		// Create empty archive
		ZipArchive zip = zip::open("unittest_empty.zip", "w")!!;
		(void)zip.close();
		defer (void)file::delete("unittest_empty.zip");

		// Read empty archive
		ZipArchive read_zip = zip::open("unittest_empty.zip", "r")!!;
		defer (void)read_zip.close();

		assert(read_zip.count() == 0, "Expected 0 entries");

	};
}
