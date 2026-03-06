module std::io::fileinfo @test;
import std::io::os;

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
	assert(@ok(os::native_get_modified_time("/etc/passwd")));	
	assert(@catch(os::native_get_modified_time("/this/file/does/not/exist")));
}
