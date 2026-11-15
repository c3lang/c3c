module std::io::chmod_test @test;
import std::io, std::math, libc;

macro String temp_file_name()
{
	String filename = string::tformat("c3_chmod_test_%x", rand(0xFFFFFFF));
	return (String)path::temp_directory(tmem).tappend(filename)!!;
}

macro uint file_mode(String path)
{
	Stat stat;
	os::native_stat(&stat, path)!!;
	return (uint)stat.st_mode & 0o7777;
}

fn void test_chmod_path() @feat(LINUX, DARWIN, ANDROID, BSD)
{
	String path = temp_file_name();
	file::save(path, "test")!!;
	defer (void)file::delete(path);

	file::chmod(path, USER_READ | USER_WRITE)!!;
	assert(file_mode(path) == 0o600);
	file::chmod(path, USER_READ | USER_WRITE | USER_EXEC | GROUP_READ | OTHER_READ)!!;
	assert(file_mode(path) == 0o744);
	file::chmod(path, (FilePermission)0o640)!!;
	assert(file_mode(path) == 0o640);
}

fn void test_chmod_file() @feat(LINUX, DARWIN, ANDROID, BSD)
{
	String path = temp_file_name();
	File f = file::open(path, "wb")!!;
	defer (void)file::delete(path);
	defer (void)f.close();

	f.chmod(USER_READ | GROUP_READ)!!;
	assert(file_mode(path) == 0o440);
	f.chmod(USER_READ | USER_WRITE)!!;
	assert(file_mode(path) == 0o600);
}

fn void test_chmod_read_only() @feat(WIN32)
{
	String path = temp_file_name();
	file::save(path, "test")!!;
	defer (void)file::delete(path);
	defer (void)file::chmod(path, USER_READ | USER_WRITE);

	file::chmod(path, USER_READ)!!;
	assert(@catch(file::open(path, "ab")) == io::NO_PERMISSION);
	file::chmod(path, USER_READ | USER_WRITE)!!;
	File f = file::open(path, "ab")!!;
	defer (void)f.close();

	f.chmod(USER_READ)!!;
	assert(@catch(file::open(path, "ab")) == io::NO_PERMISSION);
	f.chmod(USER_READ | USER_WRITE)!!;
	(void)file::open(path, "ab")!!.close();
}

fn void test_chmod_missing()
{
	assert(@catch(file::chmod("/this/file/does/not/exist", USER_READ)));
}
