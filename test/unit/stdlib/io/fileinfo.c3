module std::io::fileinfo @test;
import std::io::os;

fn void test_native_is_file() @if(env::LINUX || env::DARWIN)
{
	assert(!os::native_is_file("/dev/loop0"));
	assert(!os::native_is_file("/dev/null"));
}

fn void test_native_is_dir() @if(env::LINUX || env::DARWIN)
{
	assert(os::native_is_dir("/"));
	assert(!os::native_is_file("/"));
	assert(!os::native_is_dir("/dev/loop0"));
	assert(!os::native_is_dir("/dev/null"));
}