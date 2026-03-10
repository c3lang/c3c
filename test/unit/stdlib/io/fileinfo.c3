module std::io::fileinfo @test;
import std::io, std::math;

fn void test_native_is_file() @if(env::LINUX || env::DARWIN || env::ANDROID)
{
	assert(os::native_is_file("/etc/passwd"));
	assert(!os::native_is_file("/dev/loop0"));
	assert(!os::native_is_file("/dev/null"));
}

fn void test_native_is_dir() @if(env::LINUX || env::DARWIN || env::ANDROID)
{
	assert(os::native_is_dir("/"));
	assert(!os::native_is_file("/"));
	assert(!os::native_is_dir("/dev/loop0"));
	assert(!os::native_is_dir("/dev/null"));
}


fn void test_native_is_file() @if(env::BSD_FAMILY)
{
	assert(os::native_is_file("/etc/passwd"));
	assert(!os::native_is_file("/dev/sd0a"));
	assert(!os::native_is_file("/dev/null"));
}

fn void test_native_is_dir() @if(env::BSD_FAMILY)
{
	assert(os::native_is_dir("/"));
	assert(!os::native_is_file("/"));
	assert(!os::native_is_dir("/dev/sd0a"));
	assert(!os::native_is_dir("/dev/null"));
}

fn void test_native_get_modified_time() @if(env::LINUX || env::DARWIN || env::ANDROID || env::BSD_FAMILY)
{
	String filename = string::tformat("c3_test_file_%x", rand(0xFFFFFFF));
	Path temp_dir = path::temp_directory(tmem).tappend(filename)!!;
	String temp_file = temp_dir.str_view();
	
	file::save(temp_file, "test")!!;
	defer (void)file::delete(temp_file);
	
	assert(@ok(os::native_get_modified_time(temp_file)));
	assert(@catch(os::native_get_modified_time("/this/file/does/not/exist")));
}
