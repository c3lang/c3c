module std::io::fileinfo @test;
import std::io, std::math;

fn void test_native_is_file() @feat(LINUX, DARWIN, ANDROID)
{
	assert(os::native_is_file("/etc/passwd"));
	assert(!os::native_is_file("/dev/loop0"));
	assert(!os::native_is_file("/dev/null"));
}

fn void test_native_is_dir() @feat(LINUX, DARWIN, ANDROID)
{
	assert(os::native_is_dir("/"));
	assert(!os::native_is_file("/"));
	assert(!os::native_is_dir("/dev/loop0"));
	assert(!os::native_is_dir("/dev/null"));
}


fn void test_native_is_file() @feat(BSD)
{
	assert(os::native_is_file("/etc/passwd"));
	assert(!os::native_is_file("/dev/sd0a"));
	assert(!os::native_is_file("/dev/null"));
}

fn void test_native_is_dir() @feat(BSD)
{
	assert(os::native_is_dir("/"));
	assert(!os::native_is_file("/"));
	assert(!os::native_is_dir("/dev/sd0a"));
	assert(!os::native_is_dir("/dev/null"));
}

fn void test_native_get_modified_time() @feat(LINUX, DARWIN, ANDROID, BSD)
{
	String filename = string::tformat("c3_test_file_%x", rand(0xFFFFFFF));
	Path temp_dir = path::temp_directory(tmem).tappend(filename)!!;
	String temp_file = (String)temp_dir;
	
	file::save(temp_file, "test")!!;
	defer (void)file::delete(temp_file);
	
	assert(@ok(os::native_get_modified_time(temp_file)));
	assert(@catch(os::native_get_modified_time("/this/file/does/not/exist")));
}
