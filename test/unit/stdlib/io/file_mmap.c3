module std::io::file @if(env::LIBC &&& env::POSIX);

import std::io;

fn void write_file(String path, char[] data)
{
	File f = file::open(path, "wb")!!;
	defer f.close()!!;
	f.write(data)!!;
}

fn void map_read_only() @test
{
	const String FNAME = "tmp/map_test_ro";
	char[] msg = "Hello, world";
	write_file(FNAME, msg);

	File f = file::open(FNAME, "rb")!!;
	defer f.close()!!;

	FileMmap mapped_file = file::mmap_file(f, 0, 0, vm::VirtualMemoryAccess.READ)!!;
	defer mapped_file.destroy()!!;
	
	char[] view = mapped_file.bytes();
	assert(view.len == msg.len);
	for (usz i = 0; i < view.len; i += 1)
	{
		assert(view[i] == msg[i]);
	}
}

fn void map_read_write() @test
{
	const String FNAME = "tmp/map_test_rw";
	char[4] data = "ABCD";
	write_file(FNAME, data[..]);

	bool shared = true;
	File f = file::open(FNAME, "r+")!!;
	mmap::FileMmap region = file::mmap_file(f, 0, 0, vm::VirtualMemoryAccess.READWRITE, shared)!!;

	region.bytes()[1] = 'Z';

	region.destroy()!!;
	f.close()!!;

	File fr = file::open(FNAME, "r")!!;
	defer fr.close()!!;
	char[4] check;
	fr.read(check[..])!!;
	assert(check[0] == 'A' && check[1] == 'Z' && check[2] == 'C');
}

fn void map_with_offset(String fname, usz offset, usz size, char[] reference_msg)
{
	File f = file::open(fname, "r")!!;
	defer f.close()!!;

	mmap::FileMmap mapped_file = file::mmap_file(f, offset, size, vm::VirtualMemoryAccess.READ)!!;
	defer mapped_file.destroy()!!;

	usz expected_size = size;
	if (size == 0)
	{
		expected_size = reference_msg.len - offset;
	}
	char[] view = mapped_file.bytes();
	assert(view.len == expected_size);
	assert(view[0] == reference_msg[offset]);
}

fn void map_read_only_with_offset_and_size_zero() @test
{
	const String FNAME = "tmp/map_test_ro_with_offset";
	char[] msg = "Hello, world";
	write_file(FNAME, msg);

	usz map_size = 0;
	for (usz i = 0; i < msg.len; i += 1)
	{
		map_with_offset(FNAME, i, map_size, msg);
	}
}

fn void map_read_only_with_offset() @test
{
	const String FNAME = "tmp/map_test_ro_with_offset";
	char[] msg = "Hello, world";
	write_file(FNAME, msg);

	usz map_size = 3;
	for (usz i = 0; i < msg.len - map_size; i++)
	{
		map_with_offset(FNAME, i, map_size, msg);
	}
}

fn void map_from_filename() @test
{
	const String FNAME = "tmp/map_test_ro";
	char[] msg = "Hello, world";
	write_file(FNAME, msg);

	mmap::FileMmap mapped_file = file::mmap_open(FNAME, "rb", 0, 0, vm::VirtualMemoryAccess.READ)!!;
	defer mapped_file.destroy()!!;

	char[] view = mapped_file.bytes();
	test::eq(view.len, msg.len);
	for (usz i = 0; i < view.len; i++)
	{
		test::eq(view[i], msg[i]);
	}
}

