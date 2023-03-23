module std::io::path @test;


fn void! test_parent()
{
	Path p = path::new("")!;
	assert(@catchof(p.parent()));
	p = path::new("/", .path_env = PathEnv.POSIX)!;
	assert(@catchof(p.parent()));
	p = path::new("/a/b/c", .path_env = PathEnv.POSIX)!;
	assert(p.parent().as_str()! == "/a/b");
	p = path::new("/a/b/c", .path_env = PathEnv.WIN32)!;
	assert(p.parent().as_str()! == `\a\b`);
}

fn void! test_path_normalized()
{
	assert(path::new("",  .path_env = PathEnv.WIN32).as_str()! == "");
	assert(@catchof(path::new("1:\\a\\b\\c.txt", .path_env = PathEnv.WIN32)));
	assert(@catchof(path::new(":", .path_env = PathEnv.WIN32)));
	assert(@catchof(path::new("1:", .path_env = PathEnv.WIN32)));
	assert(@catchof(path::new("1:a", .path_env = PathEnv.WIN32)));
//	assert(@catchof(path::new(`\\\a\b\c.txt`, .path_env = PathEnv.WIN32)));
	assert(@catchof(path::new(`\\server\a\b\..\..\..\c`, .path_env = PathEnv.WIN32)));

	assert(@catchof(path::new(`\\a`, .path_env = PathEnv.WIN32)));
	assert(@catchof(path::new(`/a/b/../../../c`, .path_env = PathEnv.WIN32)));
	assert(@catchof(path::new(`/a/b/../../../c`, .path_env = PathEnv.POSIX)));
	assert(@catchof(path::new(`/a/b/../../..`, .path_env = PathEnv.WIN32)));
	assert(@catchof(path::new(`/a/b/../../..`, .path_env = PathEnv.POSIX)));
	assert(@catchof(path::new(`/../a`, .path_env = PathEnv.WIN32)));
	assert(@catchof(path::new(`/../a`, .path_env = PathEnv.POSIX)));
	assert(@catchof(path::new(`/..`, .path_env = PathEnv.WIN32)));
	assert(@catchof(path::new(`/..`, .path_env = PathEnv.POSIX)));
	assert(@catchof(path::new(`C:/a/b/../../../c`, .path_env = PathEnv.WIN32)));
	assert(@catchof(path::new(`C:/../a`, .path_env = PathEnv.WIN32)));
	assert(@catchof(path::new(`C:/..`, .path_env = PathEnv.WIN32)));

	assert(path::new("/", .path_env = PathEnv.POSIX).as_str()! == "/");
	assert(path::new("/./", .path_env = PathEnv.POSIX).as_str()! == "/");
	assert(path::new("/foo/../", .path_env = PathEnv.POSIX).as_str()! == "/");
	assert(path::new("/foo/bar/../", .path_env = PathEnv.POSIX).as_str()! == "/foo");
	assert(path::new("/foo//bar", .path_env = PathEnv.POSIX).as_str()! == "/foo/bar");
	assert(path::new("/foo//bar/../", .path_env = PathEnv.POSIX).as_str()! == "/foo");
	assert(path::new("a\\b/c.txt", .path_env = PathEnv.WIN32).as_str()! == `a\b\c.txt`);
	assert(path::new("a\\b/c.txt", .path_env = PathEnv.POSIX).as_str()! == "a\\b/c.txt");
	assert(path::new("C:\\a\\b/c.txt", .path_env = PathEnv.WIN32).as_str()! == `C:\a\b\c.txt`);
	assert(path::new("C:\\a\\b/c.txt", .path_env = PathEnv.POSIX).as_str()! == "C:\\a\\b/c.txt");
	assert(path::new(`\\server\a\b/c.txt`, .path_env = PathEnv.WIN32).as_str()! == `\\server\a\b\c.txt`);
	assert(path::new(`\\server\a\b/c.txt`, .path_env = PathEnv.POSIX).as_str()! == `\\server\a\b/c.txt`);
	assert(path::new(`c:\hello//bar\\\\foo.txt`, .path_env = PathEnv.WIN32).as_str()! == `c:\hello\bar\foo.txt`);

	assert(path::new(`~\a\b/c.txt`, .path_env = PathEnv.WIN32).as_str()! == `~\a\b\c.txt`);
	assert(path::new(`~\a\b/c.txt`, .path_env = PathEnv.POSIX).as_str()! == `~\a\b/c.txt`);


	assert(path::new(`a/b/../../../c`, .path_env = PathEnv.WIN32).as_str()! == `..\c`);
	assert(path::new(`a/b/../../../c`, .path_env = PathEnv.POSIX).as_str()! == `../c`);
	assert(path::new(`a/b/../../..`, .path_env = PathEnv.WIN32).as_str()! == `..`);
	assert(path::new(`a/b/../../..`, .path_env = PathEnv.POSIX).as_str()! == `..`);
	assert(path::new(`../a`, .path_env = PathEnv.WIN32).as_str()! == `..\a`);
	assert(path::new(`../a`, .path_env = PathEnv.POSIX).as_str()! == `../a`);
	assert(path::new(`..`, .path_env = PathEnv.WIN32).as_str()! == `..`);
	assert(path::new(`..`, .path_env = PathEnv.POSIX).as_str()! == `..`);
	assert(path::new(`a/b/../c`, .path_env = PathEnv.WIN32).as_str()! == `a\c`);
	assert(path::new(`a/b/../c`, .path_env = PathEnv.POSIX).as_str()! == `a/c`);
	assert(path::new(`a/b/../../c`, .path_env = PathEnv.WIN32).as_str()! == `c`);
	assert(path::new(`a/b/../../c`, .path_env = PathEnv.POSIX).as_str()! == `c`);
	assert(path::new(`a/b/..`, .path_env = PathEnv.WIN32).as_str()! == `a`);
	assert(path::new(`a/b/..`, .path_env = PathEnv.POSIX).as_str()! == `a`);
	assert(path::new(`a/b/../`, .path_env = PathEnv.WIN32).as_str()! == `a`);
	assert(path::new(`a/b/../`, .path_env = PathEnv.POSIX).as_str()! == `a`);
	assert(path::new(`a/b/../..`, .path_env = PathEnv.WIN32).as_str()! == "");
	assert(path::new(`a/b/../..`, .path_env = PathEnv.POSIX).as_str()! == "");
	assert(path::new(`a/b/../../`, .path_env = PathEnv.WIN32).as_str()! == "");
	assert(path::new(`a/b/../../`, .path_env = PathEnv.POSIX).as_str()! == "");
	assert(path::new(`a/b/../c/../d`, .path_env = PathEnv.WIN32).as_str()! == `a\d`);
	assert(path::new(`a/b/../c/../d`, .path_env = PathEnv.POSIX).as_str()! == `a/d`);
	assert(path::new(`a/b/../c/../d/`, .path_env = PathEnv.WIN32).as_str()! == `a\d`);
	assert(path::new(`a/b/../c/../d/`, .path_env = PathEnv.POSIX).as_str()! == `a/d`);
	assert(path::new(`a/b//d`, .path_env = PathEnv.WIN32).as_str()! == `a\b\d`);
	assert(path::new(`a/b//d`, .path_env = PathEnv.POSIX).as_str()! == `a/b/d`);
	assert(path::new(`a/b/././.`, .path_env = PathEnv.WIN32).as_str()! == `a\b`);
	assert(path::new(`a/b/././.`, .path_env = PathEnv.POSIX).as_str()! == `a/b`);
	assert(path::new(`a/b/./././`, .path_env = PathEnv.WIN32).as_str()! == `a\b`);
	assert(path::new(`a/b/./././`, .path_env = PathEnv.POSIX).as_str()! == `a/b`);
	assert(path::new(`./a/`, .path_env = PathEnv.WIN32).as_str()! == `a`);
	assert(path::new(`./a/`, .path_env = PathEnv.POSIX).as_str()! == `a`);
	assert(path::new(`./`, .path_env = PathEnv.WIN32).as_str()! == ``);
	assert(path::new(`./`, .path_env = PathEnv.POSIX).as_str()! == ``);
	assert(path::new(`.`, .path_env = PathEnv.WIN32).as_str()! == ``);
	assert(path::new(`.`, .path_env = PathEnv.POSIX).as_str()! == ``);
	assert(path::new(``, .path_env = PathEnv.WIN32).as_str()! == ``);
	assert(path::new(``, .path_env = PathEnv.POSIX).as_str()! == ``);
	assert(path::new(`/a`, .path_env = PathEnv.WIN32).as_str()! == `\a`);
	assert(path::new(`/a`, .path_env = PathEnv.POSIX).as_str()! == `/a`);
	assert(path::new(`/a/`, .path_env = PathEnv.WIN32).as_str()! == `\a`);
	assert(path::new(`/a/`, .path_env = PathEnv.POSIX).as_str()! == `/a`);
	assert(path::new(`/a/b/../c`, .path_env = PathEnv.WIN32).as_str()! == `\a\c`);
	assert(path::new(`/a/b/../c`, .path_env = PathEnv.POSIX).as_str()! == `/a/c`);
	assert(path::new(`/a/b/../../c`, .path_env = PathEnv.WIN32).as_str()! == `\c`);
	assert(path::new(`/a/b/../../c`, .path_env = PathEnv.POSIX).as_str()! == `/c`);
	assert(path::new(`/a/b/..`, .path_env = PathEnv.WIN32).as_str()! == `\a`);
	assert(path::new(`/a/b/..`, .path_env = PathEnv.POSIX).as_str()! == `/a`);
	assert(path::new(`/a/b/../..`, .path_env = PathEnv.WIN32).as_str()! == `\`);
	assert(path::new(`/a/b/../..`, .path_env = PathEnv.POSIX).as_str()! == `/`);
	assert(path::new(`/a/b/../c/../d`, .path_env = PathEnv.WIN32).as_str()! == `\a\d`);
	assert(path::new(`/a/b/../c/../d`, .path_env = PathEnv.POSIX).as_str()! == `/a/d`);
	assert(path::new(`/a/b//d`, .path_env = PathEnv.WIN32).as_str()! == `\a\b\d`);
	assert(path::new(`/a/b//d`, .path_env = PathEnv.POSIX).as_str()! == `/a/b/d`);
	assert(path::new(`/./a/`, .path_env = PathEnv.WIN32).as_str()! == `\a`);
	assert(path::new(`/./a/`, .path_env = PathEnv.POSIX).as_str()! == `/a`);
	assert(path::new(`/./`, .path_env = PathEnv.WIN32).as_str()! == `\`);
	assert(path::new(`/./`, .path_env = PathEnv.POSIX).as_str()! == `/`);
	assert(path::new(`/.`, .path_env = PathEnv.WIN32).as_str()! == `\`);
	assert(path::new(`/.`, .path_env = PathEnv.POSIX).as_str()! == `/`);
	assert(path::new(`/`, .path_env = PathEnv.WIN32).as_str()! == `\`);
	assert(path::new(`/`, .path_env = PathEnv.POSIX).as_str()! == `/`);
	assert(path::new(`C:/a`, .path_env = PathEnv.WIN32).as_str()! == `C:\a`);
	assert(path::new(`C:/a`, .path_env = PathEnv.POSIX).as_str()! == `C:/a`);
	assert(path::new(`C:/a/b/../c`, .path_env = PathEnv.WIN32).as_str()! == `C:\a\c`);
	assert(path::new(`C:/a/b/../c`, .path_env = PathEnv.POSIX).as_str()! == `C:/a/c`);
	assert(path::new(`C:/a/b/../../c`, .path_env = PathEnv.WIN32).as_str()! == `C:\c`);
	assert(path::new(`C:/a/b/../../c`, .path_env = PathEnv.POSIX).as_str()! == `C:/c`);
	assert(path::new(`C:/a/b/../../../c`, .path_env = PathEnv.POSIX).as_str()! == `c`);
	assert(path::new(`C:/a/b/..`, .path_env = PathEnv.WIN32).as_str()! == `C:\a`);
	assert(path::new(`C:/a/b/..`, .path_env = PathEnv.POSIX).as_str()! == `C:/a`);
	assert(path::new(`C:/a/b/../..`, .path_env = PathEnv.WIN32).as_str()! == `C:\`);
	assert(path::new(`C:/a/b/../..`, .path_env = PathEnv.POSIX).as_str()! == `C:`);
	assert(path::new(`C:/a/b/../c/../d`, .path_env = PathEnv.WIN32).as_str()! == `C:\a\d`);
	assert(path::new(`C:/a/b/../c/../d`, .path_env = PathEnv.POSIX).as_str()! == `C:/a/d`);
	assert(path::new(`C:/a/b//d`, .path_env = PathEnv.WIN32).as_str()! == `C:\a\b\d`);
	assert(path::new(`C:/a/b//d`, .path_env = PathEnv.POSIX).as_str()! == `C:/a/b/d`);
	assert(path::new(`C:/a/b/././.`, .path_env = PathEnv.WIN32).as_str()! == `C:\a\b`);
	assert(path::new(`C:/a/b/././.`, .path_env = PathEnv.POSIX).as_str()! == `C:/a/b`);
	assert(path::new(`C:/./a`, .path_env = PathEnv.WIN32).as_str()! == `C:\a`);
	assert(path::new(`C:/./a`, .path_env = PathEnv.POSIX).as_str()! == `C:/a`);
	assert(path::new(`C:/./`, .path_env = PathEnv.WIN32).as_str()! == `C:\`);
	assert(path::new(`C:/./`, .path_env = PathEnv.POSIX).as_str()! == `C:`);
	assert(path::new(`C:/../a`, .path_env = PathEnv.POSIX).as_str()! == `a`);
	assert(path::new(`C:/..`, .path_env = PathEnv.POSIX).as_str()! == ``);
	assert(path::new(`C:/`, .path_env = PathEnv.WIN32).as_str()! == `C:\`);
	assert(path::new(`C:/`, .path_env = PathEnv.POSIX).as_str()! == `C:`);
	assert(path::new(`C:a`, .path_env = PathEnv.WIN32).as_str()! == `C:a`);
	assert(path::new(`C:a`, .path_env = PathEnv.POSIX).as_str()! == `C:a`);
	assert(path::new(`C:a/`, .path_env = PathEnv.WIN32).as_str()! == `C:a`);
	assert(path::new(`C:a/`, .path_env = PathEnv.POSIX).as_str()! == `C:a`);

	assert(path::new(`C:a/b/../c`, .path_env = PathEnv.WIN32).as_str()! == `C:a\c`);
	assert(path::new(`C:a/b/../c`, .path_env = PathEnv.POSIX).as_str()! == `C:a/c`);
	assert(path::new(`C:a/b/../../c`, .path_env = PathEnv.WIN32).as_str()! == `C:c`);
	assert(path::new(`C:a/b/../../c`, .path_env = PathEnv.POSIX).as_str()! == `c`);
	assert(path::new(`C:a/b/..`, .path_env = PathEnv.WIN32).as_str()! == `C:a`);
	assert(path::new(`C:a/b/..`, .path_env = PathEnv.POSIX).as_str()! == `C:a`);
	assert(path::new(`C:a/b/../..`, .path_env = PathEnv.WIN32).as_str()! == `C:`);
	assert(path::new(`C:a/b/../..`, .path_env = PathEnv.POSIX).as_str()! == ``);
	assert(path::new(`C:a/b/../c/../d`, .path_env = PathEnv.WIN32).as_str()! == `C:a\d`);
	assert(path::new(`C:a/b/../c/../d`, .path_env = PathEnv.POSIX).as_str()! == `C:a/d`);
	assert(path::new(`C:a/b//d`, .path_env = PathEnv.WIN32).as_str()! == `C:a\b\d`);
	assert(path::new(`C:a/b//d`, .path_env = PathEnv.POSIX).as_str()! == `C:a/b/d`);
	assert(path::new(`C:a/b/././.`, .path_env = PathEnv.WIN32).as_str()! == `C:a\b`);
	assert(path::new(`C:a/b/././.`, .path_env = PathEnv.POSIX).as_str()! == `C:a/b`);
	assert(path::new(`C:a/b/../../../c`, .path_env = PathEnv.WIN32).as_str()! == `C:..\c`);
	assert(path::new(`C:./a`, .path_env = PathEnv.WIN32).as_str()! == `C:a`);
	assert(path::new(`C:./a`, .path_env = PathEnv.POSIX).as_str()! == `C:./a`);
	assert(path::new(`C:./`, .path_env = PathEnv.WIN32).as_str()! == `C:`);
	assert(path::new(`C:./`, .path_env = PathEnv.POSIX).as_str()! == `C:.`);
	assert(path::new(`C:../a`, .path_env = PathEnv.POSIX).as_str()! == `C:../a`);
	assert(path::new(`C:../a`, .path_env = PathEnv.WIN32).as_str()! == `C:..\a`);
	assert(path::new(`C:..`, .path_env = PathEnv.POSIX).as_str()! == `C:..`);
	assert(path::new(`C:..`, .path_env = PathEnv.WIN32).as_str()! == `C:..`);
	assert(path::new(`C:`, .path_env = PathEnv.WIN32).as_str()! == `C:`);
	assert(path::new(`C:`, .path_env = PathEnv.POSIX).as_str()! == `C:`);

	assert(path::new(`\\server/a`, .path_env = PathEnv.WIN32).as_str()! == `\\server\a`);
	assert(path::new(`\\server/a`, .path_env = PathEnv.POSIX).as_str()! == `\\server/a`);
	assert(path::new(`\\server\a\b\..\c`, .path_env = PathEnv.WIN32).as_str()! == `\\server\a\c`);
	assert(path::new(`\\server\a\b\..\..\c`, .path_env = PathEnv.WIN32).as_str()! == `\\server\c`);
	assert(path::new(`\\server\a\b\..`, .path_env = PathEnv.WIN32).as_str()! == `\\server\a`);
	assert(path::new(`\\server\a\..`, .path_env = PathEnv.WIN32).as_str()! == `\\server\`);
	assert(path::new(`\\server\a\b\..\c\..\d`, .path_env = PathEnv.WIN32).as_str()! == `\\server\a\d`);
	assert(path::new(`\\server\a\b\\d`, .path_env = PathEnv.WIN32).as_str()! == `\\server\a\b\d`);
	assert(path::new(`\\server\a\b\.\.\.`, .path_env = PathEnv.WIN32).as_str()! == `\\server\a\b`);
	assert(path::new(`\\server\.\a`, .path_env = PathEnv.WIN32).as_str()! == `\\server\a`);
	assert(path::new(`\\server\.`, .path_env = PathEnv.WIN32).as_str()! == `\\server\`);
	assert(path::new(`\\server\`, .path_env = PathEnv.WIN32).as_str()! == `\\server\`);

}

fn void! test_extension()
{
	assert(@catchof(path::new(`C:`, .path_env = PathEnv.WIN32).extension()));
	assert(@catchof(path::new(`C:`, .path_env = PathEnv.POSIX).extension()));
	assert(@catchof(path::new(`file`, .path_env = PathEnv.WIN32).extension()));
	assert(@catchof(path::new(`file`, .path_env = PathEnv.POSIX).extension()));
	assert(@catchof(path::new(`C:\temp\foo.bar\README`, .path_env = PathEnv.WIN32).extension()));

	assert(path::new_windows("file.txt").extension()! == "txt");
	assert(path::new_posix("file.txt").extension()! == "txt");

	assert(path::new_windows("a/b/file.txt").extension()! == "txt");
	assert(path::new_posix("a/b/file.txt").extension()! == "txt");

	assert(path::new_windows("a\\b\\file.txt").extension()! == "txt");

	assert(path::new_windows("a.b/file.txt").extension()! == "txt");
	assert(path::new_posix("a.b/file.txt").extension()! == "txt");
	assert(path::new_windows("a.b/file.txt").extension()! == "txt");
	assert(path::new_posix("a.b/file.txt").extension()! == "txt");

	assert(path::new_windows("a.b\\file.txt").extension()! == "txt");

	assert(path::new_windows("domain.dot.com").extension()! == "com");
	assert(path::new_posix("domain.dot.com").extension()! == "com");

	assert(path::new_windows("image.jpeg").extension()! == "jpeg");
	assert(path::new_posix("image.jpeg").extension()! == "jpeg");

	assert(path::new_windows("../filename.ext").extension()! == "ext");
	assert(path::new_posix("../filename.ext").extension()! == "ext");

}

fn void! test_basename()
{

	assert(path::new_windows("file.txt").basename()! == "file.txt");
	assert(path::new_posix("file.txt").basename()! == "file.txt");

	assert(path::new_windows("a/b/file.txt").basename()! == "file.txt");
	assert(path::new_posix("a/b/file.txt").basename()! == "file.txt");

	assert(path::new_windows("a.b/file.txt").basename()! == "file.txt");
	assert(path::new_posix("a.b/file.txt").basename()! == "file.txt");

	assert(path::new_windows("a.b/file.txt").basename()! == "file.txt");
	assert(path::new_posix("a.b/file.txt").basename()! == "file.txt");

	assert(path::new_windows("../filename.ext").basename()! == "filename.ext");
	assert(path::new_posix("../filename.ext").basename()! == "filename.ext");

	assert(path::new_windows("C:").basename()! == "");
	assert(path::new_posix("C:").basename()! == "C:");

	assert(path::new_windows("../..").basename()! == "..");
	assert(path::new_posix("../..").basename()! == "..");

	assert(path::new_windows(`\\server\`).basename()! == "");
	assert(path::new_posix(`\\server\`).basename()! == `\\server\`);
}

fn void! test_dirname()
{

	assert(path::new_windows("file.txt").dirname()! == "");
	assert(path::new_posix("file.txt").dirname()! == "");

	assert(path::new_windows("a/b/file.txt").dirname()! == `a\b`);
	assert(path::new_posix("a/b/file.txt").dirname()! == "a/b");

	assert(path::new_windows("a.b/file.txt").dirname()! == "a.b");
	assert(path::new_posix("a.b/file.txt").dirname()! == "a.b");

	assert(path::new_windows("../filename.ext").dirname()! == "..");
	assert(path::new_posix("../filename.ext").dirname()! == "..");

	assert(path::new_windows("C:").dirname()! == "C:");
	assert(path::new_posix("C:").dirname()! == "");

	assert(path::new_windows("C:/").dirname()! == "C:\\");
	assert(path::new_posix("C:/").dirname()! == "");

	assert(path::new_windows("C:/a").dirname()! == "C:\\");
	assert(path::new_posix("C:/a").dirname()! == "C:");

	assert(path::new_windows("../..").dirname()! == "..");
	assert(path::new_posix("../..").dirname()! == "..");

	assert(path::new_windows(`\\server\`).dirname()! == `\\server\`);
	assert(path::new_posix(`\\server\`).dirname()! == ``);
}

fn void! test_path_volume()
{
	assert(path::new_windows(`C:\abs`).volume_name()! == `C:`);
	assert(path::new_windows(`C:abs`).volume_name()! == `C:`);
	assert(path::new_posix(`C:/abs`).volume_name()! == ``);
	assert(path::new_posix(`C:abs`).volume_name()! == ``);
	assert(path::new_windows(`\\server\`).volume_name()! == `\\server`);
    assert(path::new_windows(`\\server\abc`).volume_name()! == `\\server`);
}
