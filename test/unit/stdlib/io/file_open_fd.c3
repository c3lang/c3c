module std::io::file;
import std::io, libc, std::io::path, std::math;

extern fn CInt fd_test_dup(Fd fd) @cname(env::WIN32 ? "_dup" : "dup");

// Make an fd that outlives the File it came from.
fn Fd fd_test_dup_file(String filename, String mode)
{
	File f = file::open(filename, mode)!!;
	Fd fd = fd_test_dup(f.fd());
	assert(fd >= 0);
	f.close()!!;
	return fd;
}

fn String fd_test_name()
{
	String filename = string::tformat("c3_test_fd_%x", rand(0xFFFFFFF));
	return path::temp_directory(tmem).tappend(filename)!!;
}

fn void open_fd_read() @test
{
	String filename = fd_test_name();
	file::save(filename, "Hello, fd")!!;
	defer (void)file::delete(filename);

	File f = file::open_fd(fd_test_dup_file(filename, "rb"), "rb")!!;
	defer f.close()!!;

	char[32] buf;
	sz n = f.read(&buf)!!;
	assert((String)buf[:n] == "Hello, fd");
}

fn void open_fd_write() @test
{
	String filename = fd_test_name();
	file::save(filename, "")!!;
	defer (void)file::delete(filename);

	File f = file::open_fd(fd_test_dup_file(filename, "wb"), "wb")!!;
	f.write("Written via fd")!!;
	f.close()!!;

	assert((String)file::load_temp(filename)!! == "Written via fd");
}

fn void open_fd_append() @test
{
	String filename = fd_test_name();
	file::save(filename, "abc")!!;
	defer (void)file::delete(filename);

	File f = file::open_fd(fd_test_dup_file(filename, "ab"), "ab")!!;
	f.write("def")!!;
	f.close()!!;

	assert((String)file::load_temp(filename)!! == "abcdef");
}

fn void open_fd_read_write() @test
{
	String filename = fd_test_name();
	file::save(filename, "0123456789")!!;
	defer (void)file::delete(filename);

	File f = file::open_fd(fd_test_dup_file(filename, "r+b"), "r+b")!!;
	f.seek(4)!!;
	f.write("XX")!!;
	f.seek(0)!!;

	char[16] buf;
	sz n = f.read(&buf)!!;
	assert((String)buf[:n] == "0123XX6789");
	f.close()!!;

	assert((String)file::load_temp(filename)!! == "0123XX6789");
}

// The wrapped File must report the same fd it was created from.
fn void open_fd_keeps_fd() @test
{
	String filename = fd_test_name();
	file::save(filename, "x")!!;
	defer (void)file::delete(filename);

	Fd fd = fd_test_dup_file(filename, "rb");
	File f = file::open_fd(fd, "rb")!!;
	defer f.close()!!;
	assert(f.fd() == fd);
}

// The fd starts at the offset it was left at, not at the beginning.
fn void open_fd_continues_at_offset() @test
{
	String filename = fd_test_name();
	file::save(filename, "0123456789")!!;
	defer (void)file::delete(filename);

	File src = file::open(filename, "rb")!!;
	Fd fd = fd_test_dup(src.fd());
	assert(fd >= 0);
	char[4] skip;
	assert(libc::read(fd, &skip, 4) == 4);
	src.close()!!;

	File f = file::open_fd(fd, "rb")!!;
	defer f.close()!!;
	char[16] buf;
	sz n = f.read(&buf)!!;
	assert((String)buf[:n] == "456789");
}


// Closing the File closes the fd.
fn void open_fd_close_closes_fd() @test
{
	// Reading a closed fd triggers the CRT's invalid parameter handler on Windows.
	$if !env::WIN32:
		String filename = fd_test_name();
		file::save(filename, "x")!!;
		defer (void)file::delete(filename);

		Fd fd = fd_test_dup_file(filename, "rb");
		File f = file::open_fd(fd, "rb")!!;
		f.close()!!;

		char[1] buf;
		assert(libc::read(fd, &buf, 1) == -1);
		assert(libc::errno() == Errno.EBADF);
	$endif
}
