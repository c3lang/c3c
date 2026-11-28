module std::io::symlink_test @test;
import std::io, std::math;

fn void test_symlink_file() @feat(LINUX, DARWIN, ANDROID, BSD)
{
	Path dir = path::temp_directory(tmem)!!;
	Path target = dir.tappend(string::tformat("c3_symlink_target_%x", rand(0xFFFFFFF)))!!;
	Path link = dir.tappend(string::tformat("c3_symlink_link_%x", rand(0xFFFFFFF)))!!;

	file::save((String)target, "test")!!;
	defer (void)file::delete((String)target);

	path::symlink(target, link)!!;
	defer (void)file::delete((String)link);

	assert(path::is_link(link));
	assert(!path::is_link(target));
	assert(path::treadlink(link)!! == target);
	assert(file::load_temp((String)link)!! == "test");

	assert(@catch(path::symlink(target, link)) == io::ALREADY_EXISTS);
	assert(@catch(path::treadlink(target)) == io::FILE_NOT_VALID);
	assert(@catch(path::treadlink((Path)"/this/file/does/not/exist")) == io::FILE_NOT_FOUND);
}

fn void test_symlink_relative_and_string() @feat(LINUX, DARWIN, ANDROID, BSD)
{
	Path dir = path::temp_directory(tmem)!!;
	String target_name = string::tformat("c3_symlink_rel_target_%x", rand(0xFFFFFFF));
	Path target = dir.tappend(target_name)!!;
	Path link = dir.tappend(string::tformat("c3_symlink_rel_link_%x", rand(0xFFFFFFF)))!!;

	path::mkdir(target)!!;
	defer (void)path::rmdir(target);

	path::symlink(target_name, (String)link)!!;
	defer (void)file::delete((String)link);

	assert(path::is_link(link));
	assert(path::is_dir(link));
	assert((String)path::treadlink(link)!! == target_name);
}

fn void test_symlink_dangling() @feat(LINUX, DARWIN, ANDROID, BSD)
{
	Path dir = path::temp_directory(tmem)!!;
	Path link = dir.tappend(string::tformat("c3_symlink_dangling_%x", rand(0xFFFFFFF)))!!;

	path::symlink("does_not_exist", link)!!;
	defer (void)file::delete((String)link);

	assert(path::is_link(link));
	assert(!path::exists(link));
	assert((String)path::treadlink(link)!! == "does_not_exist");
}
