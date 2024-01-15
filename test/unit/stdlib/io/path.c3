module std::io::path @test;

fn void! test_parent()
{
	Path p = path::new("")!;
	assert(@catch(p.parent()));
	p = path::new("/", .path_env = PathEnv.POSIX)!;
	assert(@catch(p.parent()));
	p = path::new("/a/b/c", .path_env = PathEnv.POSIX)!;
	assert(p.parent().str_view()! == "/a/b");
	p = path::new("/a/b/c", .path_env = PathEnv.WIN32)!;
	assert(p.parent().str_view()! == `\a\b`);
}

fn void! test_path_normalized()
{
	assert(path::new("",  .path_env = PathEnv.WIN32).str_view()! == "");
	assert(@catch(path::new("1:\\a\\b\\c.txt", .path_env = PathEnv.WIN32)));
	assert(@catch(path::new(":", .path_env = PathEnv.WIN32)));
	assert(@catch(path::new("1:", .path_env = PathEnv.WIN32)));
	assert(@catch(path::new("1:a", .path_env = PathEnv.WIN32)));
//	assert(@catch(path::new(`\\\a\b\c.txt`, .path_env = PathEnv.WIN32)));
	assert(@catch(path::new(`\\server\a\b\..\..\..\c`, .path_env = PathEnv.WIN32)));

	assert(@catch(path::new(`\\a`, .path_env = PathEnv.WIN32)));
	assert(@catch(path::new(`/a/b/../../../c`, .path_env = PathEnv.WIN32)));
	assert(@catch(path::new(`/a/b/../../../c`, .path_env = PathEnv.POSIX)));
	assert(@catch(path::new(`/a/b/../../..`, .path_env = PathEnv.WIN32)));
	assert(@catch(path::new(`/a/b/../../..`, .path_env = PathEnv.POSIX)));
	assert(@catch(path::new(`/../a`, .path_env = PathEnv.WIN32)));
	assert(@catch(path::new(`/../a`, .path_env = PathEnv.POSIX)));
	assert(@catch(path::new(`/..`, .path_env = PathEnv.WIN32)));
	assert(@catch(path::new(`/..`, .path_env = PathEnv.POSIX)));
	assert(@catch(path::new(`C:/a/b/../../../c`, .path_env = PathEnv.WIN32)));
	assert(@catch(path::new(`C:/../a`, .path_env = PathEnv.WIN32)));
	assert(@catch(path::new(`C:/..`, .path_env = PathEnv.WIN32)));

	assert(path::new("/", .path_env = PathEnv.POSIX).str_view()! == "/");
	assert(path::new("/./", .path_env = PathEnv.POSIX).str_view()! == "/");
	assert(path::new("/foo/../", .path_env = PathEnv.POSIX).str_view()! == "/");
	assert(path::new("/foo/bar/../", .path_env = PathEnv.POSIX).str_view()! == "/foo");
	assert(path::new("/foo//bar", .path_env = PathEnv.POSIX).str_view()! == "/foo/bar");
	assert(path::new("/foo//bar/../", .path_env = PathEnv.POSIX).str_view()! == "/foo");
	assert(path::new("/foo/.bar", .path_env = PathEnv.POSIX).str_view()! == "/foo/.bar");
	assert(path::new(`\foo\.bar`, .path_env = PathEnv.WIN32).str_view()! == `\foo\.bar`);
	assert(path::new("a\\b/c.txt", .path_env = PathEnv.WIN32).str_view()! == `a\b\c.txt`);
	assert(path::new("a\\b/c.txt", .path_env = PathEnv.POSIX).str_view()! == "a\\b/c.txt");
	assert(path::new("C:\\a\\b/c.txt", .path_env = PathEnv.WIN32).str_view()! == `C:\a\b\c.txt`);
	assert(path::new("C:\\a\\b/c.txt", .path_env = PathEnv.POSIX).str_view()! == "C:\\a\\b/c.txt");
	assert(path::new(`\\server\a\b/c.txt`, .path_env = PathEnv.WIN32).str_view()! == `\\server\a\b\c.txt`);
	assert(path::new(`\\server\a\b/c.txt`, .path_env = PathEnv.POSIX).str_view()! == `\\server\a\b/c.txt`);
	assert(path::new(`c:\hello//bar\\\\foo.txt`, .path_env = PathEnv.WIN32).str_view()! == `c:\hello\bar\foo.txt`);

	assert(path::new(`~\a\b/c.txt`, .path_env = PathEnv.WIN32).str_view()! == `~\a\b\c.txt`);
	assert(path::new(`~\a\b/c.txt`, .path_env = PathEnv.POSIX).str_view()! == `~\a\b/c.txt`);


	assert(path::new(`a/b/../../../c`, .path_env = PathEnv.WIN32).str_view()! == `..\c`);
	assert(path::new(`a/b/../../../c`, .path_env = PathEnv.POSIX).str_view()! == `../c`);
	assert(path::new(`a/b/../../..`, .path_env = PathEnv.WIN32).str_view()! == `..`);
	assert(path::new(`a/b/../../..`, .path_env = PathEnv.POSIX).str_view()! == `..`);
	assert(path::new(`../a`, .path_env = PathEnv.WIN32).str_view()! == `..\a`);
	assert(path::new(`../a`, .path_env = PathEnv.POSIX).str_view()! == `../a`);
	assert(path::new(`..`, .path_env = PathEnv.WIN32).str_view()! == `..`);
	assert(path::new(`..`, .path_env = PathEnv.POSIX).str_view()! == `..`);
	assert(path::new(`a/b/../c`, .path_env = PathEnv.WIN32).str_view()! == `a\c`);
	assert(path::new(`a/b/../c`, .path_env = PathEnv.POSIX).str_view()! == `a/c`);
	assert(path::new(`a/b/../../c`, .path_env = PathEnv.WIN32).str_view()! == `c`);
	assert(path::new(`a/b/../../c`, .path_env = PathEnv.POSIX).str_view()! == `c`);
	assert(path::new(`a/b/..`, .path_env = PathEnv.WIN32).str_view()! == `a`);
	assert(path::new(`a/b/..`, .path_env = PathEnv.POSIX).str_view()! == `a`);
	assert(path::new(`a/b/../`, .path_env = PathEnv.WIN32).str_view()! == `a`);
	assert(path::new(`a/b/../`, .path_env = PathEnv.POSIX).str_view()! == `a`);
	assert(path::new(`a/b/../..`, .path_env = PathEnv.WIN32).str_view()! == "");
	assert(path::new(`a/b/../..`, .path_env = PathEnv.POSIX).str_view()! == "");
	assert(path::new(`a/b/../../`, .path_env = PathEnv.WIN32).str_view()! == "");
	assert(path::new(`a/b/../../`, .path_env = PathEnv.POSIX).str_view()! == "");
	assert(path::new(`a/b/../c/../d`, .path_env = PathEnv.WIN32).str_view()! == `a\d`);
	assert(path::new(`a/b/../c/../d`, .path_env = PathEnv.POSIX).str_view()! == `a/d`);
	assert(path::new(`a/b/../c/../d/`, .path_env = PathEnv.WIN32).str_view()! == `a\d`);
	assert(path::new(`a/b/../c/../d/`, .path_env = PathEnv.POSIX).str_view()! == `a/d`);
	assert(path::new(`a/b//d`, .path_env = PathEnv.WIN32).str_view()! == `a\b\d`);
	assert(path::new(`a/b//d`, .path_env = PathEnv.POSIX).str_view()! == `a/b/d`);
	assert(path::new(`a/b/././.`, .path_env = PathEnv.WIN32).str_view()! == `a\b`);
	assert(path::new(`a/b/././.`, .path_env = PathEnv.POSIX).str_view()! == `a/b`);
	assert(path::new(`a/b/./././`, .path_env = PathEnv.WIN32).str_view()! == `a\b`);
	assert(path::new(`a/b/./././`, .path_env = PathEnv.POSIX).str_view()! == `a/b`);
	assert(path::new(`./a/`, .path_env = PathEnv.WIN32).str_view()! == `a`);
	assert(path::new(`./a/`, .path_env = PathEnv.POSIX).str_view()! == `a`);
	assert(path::new(`./`, .path_env = PathEnv.WIN32).str_view()! == ``);
	assert(path::new(`./`, .path_env = PathEnv.POSIX).str_view()! == ``);
	assert(path::new(`.`, .path_env = PathEnv.WIN32).str_view()! == ``);
	assert(path::new(`.`, .path_env = PathEnv.POSIX).str_view()! == ``);
	assert(path::new(``, .path_env = PathEnv.WIN32).str_view()! == ``);
	assert(path::new(``, .path_env = PathEnv.POSIX).str_view()! == ``);
	assert(path::new(`/a`, .path_env = PathEnv.WIN32).str_view()! == `\a`);
	assert(path::new(`/a`, .path_env = PathEnv.POSIX).str_view()! == `/a`);
	assert(path::new(`/a/`, .path_env = PathEnv.WIN32).str_view()! == `\a`);
	assert(path::new(`/a/`, .path_env = PathEnv.POSIX).str_view()! == `/a`);
	assert(path::new(`/a/b/../c`, .path_env = PathEnv.WIN32).str_view()! == `\a\c`);
	assert(path::new(`/a/b/../c`, .path_env = PathEnv.POSIX).str_view()! == `/a/c`);
	assert(path::new(`/a/b/../../c`, .path_env = PathEnv.WIN32).str_view()! == `\c`);
	assert(path::new(`/a/b/../../c`, .path_env = PathEnv.POSIX).str_view()! == `/c`);
	assert(path::new(`/a/b/..`, .path_env = PathEnv.WIN32).str_view()! == `\a`);
	assert(path::new(`/a/b/..`, .path_env = PathEnv.POSIX).str_view()! == `/a`);
	assert(path::new(`/a/b/../..`, .path_env = PathEnv.WIN32).str_view()! == `\`);
	assert(path::new(`/a/b/../..`, .path_env = PathEnv.POSIX).str_view()! == `/`);
	assert(path::new(`/a/b/../c/../d`, .path_env = PathEnv.WIN32).str_view()! == `\a\d`);
	assert(path::new(`/a/b/../c/../d`, .path_env = PathEnv.POSIX).str_view()! == `/a/d`);
	assert(path::new(`/a/b//d`, .path_env = PathEnv.WIN32).str_view()! == `\a\b\d`);
	assert(path::new(`/a/b//d`, .path_env = PathEnv.POSIX).str_view()! == `/a/b/d`);
	assert(path::new(`/./a/`, .path_env = PathEnv.WIN32).str_view()! == `\a`);
	assert(path::new(`/./a/`, .path_env = PathEnv.POSIX).str_view()! == `/a`);
	assert(path::new(`/./`, .path_env = PathEnv.WIN32).str_view()! == `\`);
	assert(path::new(`/./`, .path_env = PathEnv.POSIX).str_view()! == `/`);
	assert(path::new(`/.`, .path_env = PathEnv.WIN32).str_view()! == `\`);
	assert(path::new(`/.`, .path_env = PathEnv.POSIX).str_view()! == `/`);
	assert(path::new(`/`, .path_env = PathEnv.WIN32).str_view()! == `\`);
	assert(path::new(`/`, .path_env = PathEnv.POSIX).str_view()! == `/`);
	assert(path::new(`C:/a`, .path_env = PathEnv.WIN32).str_view()! == `C:\a`);
	assert(path::new(`C:/a`, .path_env = PathEnv.POSIX).str_view()! == `C:/a`);
	assert(path::new(`C:/a/b/../c`, .path_env = PathEnv.WIN32).str_view()! == `C:\a\c`);
	assert(path::new(`C:/a/b/../c`, .path_env = PathEnv.POSIX).str_view()! == `C:/a/c`);
	assert(path::new(`C:/a/b/../../c`, .path_env = PathEnv.WIN32).str_view()! == `C:\c`);
	assert(path::new(`C:/a/b/../../c`, .path_env = PathEnv.POSIX).str_view()! == `C:/c`);
	assert(path::new(`C:/a/b/../../../c`, .path_env = PathEnv.POSIX).str_view()! == `c`);
	assert(path::new(`C:/a/b/..`, .path_env = PathEnv.WIN32).str_view()! == `C:\a`);
	assert(path::new(`C:/a/b/..`, .path_env = PathEnv.POSIX).str_view()! == `C:/a`);
	assert(path::new(`C:/a/b/../..`, .path_env = PathEnv.WIN32).str_view()! == `C:\`);
	assert(path::new(`C:/a/b/../..`, .path_env = PathEnv.POSIX).str_view()! == `C:`);
	assert(path::new(`C:/a/b/../c/../d`, .path_env = PathEnv.WIN32).str_view()! == `C:\a\d`);
	assert(path::new(`C:/a/b/../c/../d`, .path_env = PathEnv.POSIX).str_view()! == `C:/a/d`);
	assert(path::new(`C:/a/b//d`, .path_env = PathEnv.WIN32).str_view()! == `C:\a\b\d`);
	assert(path::new(`C:/a/b//d`, .path_env = PathEnv.POSIX).str_view()! == `C:/a/b/d`);
	assert(path::new(`C:/a/b/././.`, .path_env = PathEnv.WIN32).str_view()! == `C:\a\b`);
	assert(path::new(`C:/a/b/././.`, .path_env = PathEnv.POSIX).str_view()! == `C:/a/b`);
	assert(path::new(`C:/./a`, .path_env = PathEnv.WIN32).str_view()! == `C:\a`);
	assert(path::new(`C:/./a`, .path_env = PathEnv.POSIX).str_view()! == `C:/a`);
	assert(path::new(`C:/./`, .path_env = PathEnv.WIN32).str_view()! == `C:\`);
	assert(path::new(`C:/./`, .path_env = PathEnv.POSIX).str_view()! == `C:`);
	assert(path::new(`C:/../a`, .path_env = PathEnv.POSIX).str_view()! == `a`);
	assert(path::new(`C:/..`, .path_env = PathEnv.POSIX).str_view()! == ``);
	assert(path::new(`C:/`, .path_env = PathEnv.WIN32).str_view()! == `C:\`);
	assert(path::new(`C:/`, .path_env = PathEnv.POSIX).str_view()! == `C:`);
	assert(path::new(`C:a`, .path_env = PathEnv.WIN32).str_view()! == `C:a`);
	assert(path::new(`C:a`, .path_env = PathEnv.POSIX).str_view()! == `C:a`);
	assert(path::new(`C:a/`, .path_env = PathEnv.WIN32).str_view()! == `C:a`);
	assert(path::new(`C:a/`, .path_env = PathEnv.POSIX).str_view()! == `C:a`);

	assert(path::new(`C:a/b/../c`, .path_env = PathEnv.WIN32).str_view()! == `C:a\c`);
	assert(path::new(`C:a/b/../c`, .path_env = PathEnv.POSIX).str_view()! == `C:a/c`);
	assert(path::new(`C:a/b/../../c`, .path_env = PathEnv.WIN32).str_view()! == `C:c`);
	assert(path::new(`C:a/b/../../c`, .path_env = PathEnv.POSIX).str_view()! == `c`);
	assert(path::new(`C:a/b/..`, .path_env = PathEnv.WIN32).str_view()! == `C:a`);
	assert(path::new(`C:a/b/..`, .path_env = PathEnv.POSIX).str_view()! == `C:a`);
	assert(path::new(`C:a/b/../..`, .path_env = PathEnv.WIN32).str_view()! == `C:`);
	assert(path::new(`C:a/b/../..`, .path_env = PathEnv.POSIX).str_view()! == ``);
	assert(path::new(`C:a/b/../c/../d`, .path_env = PathEnv.WIN32).str_view()! == `C:a\d`);
	assert(path::new(`C:a/b/../c/../d`, .path_env = PathEnv.POSIX).str_view()! == `C:a/d`);
	assert(path::new(`C:a/b//d`, .path_env = PathEnv.WIN32).str_view()! == `C:a\b\d`);
	assert(path::new(`C:a/b//d`, .path_env = PathEnv.POSIX).str_view()! == `C:a/b/d`);
	assert(path::new(`C:a/b/././.`, .path_env = PathEnv.WIN32).str_view()! == `C:a\b`);
	assert(path::new(`C:a/b/././.`, .path_env = PathEnv.POSIX).str_view()! == `C:a/b`);
	assert(path::new(`C:a/b/../../../c`, .path_env = PathEnv.WIN32).str_view()! == `C:..\c`);
	assert(path::new(`C:./a`, .path_env = PathEnv.WIN32).str_view()! == `C:a`);
	assert(path::new(`C:./a`, .path_env = PathEnv.POSIX).str_view()! == `C:./a`);
	assert(path::new(`C:./`, .path_env = PathEnv.WIN32).str_view()! == `C:`);
	assert(path::new(`C:./`, .path_env = PathEnv.POSIX).str_view()! == `C:.`);
	assert(path::new(`C:../a`, .path_env = PathEnv.POSIX).str_view()! == `C:../a`);
	assert(path::new(`C:../a`, .path_env = PathEnv.WIN32).str_view()! == `C:..\a`);
	assert(path::new(`C:..`, .path_env = PathEnv.POSIX).str_view()! == `C:..`);
	assert(path::new(`C:..`, .path_env = PathEnv.WIN32).str_view()! == `C:..`);
	assert(path::new(`C:`, .path_env = PathEnv.WIN32).str_view()! == `C:`);
	assert(path::new(`C:`, .path_env = PathEnv.POSIX).str_view()! == `C:`);

	assert(path::new(`\\server/a`, .path_env = PathEnv.WIN32).str_view()! == `\\server\a`);
	assert(path::new(`\\server/a`, .path_env = PathEnv.POSIX).str_view()! == `\\server/a`);
	assert(path::new(`\\server\a\b\..\c`, .path_env = PathEnv.WIN32).str_view()! == `\\server\a\c`);
	assert(path::new(`\\server\a\b\..\..\c`, .path_env = PathEnv.WIN32).str_view()! == `\\server\c`);
	assert(path::new(`\\server\a\b\..`, .path_env = PathEnv.WIN32).str_view()! == `\\server\a`);
	assert(path::new(`\\server\a\..`, .path_env = PathEnv.WIN32).str_view()! == `\\server\`);
	assert(path::new(`\\server\a\b\..\c\..\d`, .path_env = PathEnv.WIN32).str_view()! == `\\server\a\d`);
	assert(path::new(`\\server\a\b\\d`, .path_env = PathEnv.WIN32).str_view()! == `\\server\a\b\d`);
	assert(path::new(`\\server\a\b\.\.\.`, .path_env = PathEnv.WIN32).str_view()! == `\\server\a\b`);
	assert(path::new(`\\server\.\a`, .path_env = PathEnv.WIN32).str_view()! == `\\server\a`);
	assert(path::new(`\\server\.`, .path_env = PathEnv.WIN32).str_view()! == `\\server\`);
	assert(path::new(`\\server\`, .path_env = PathEnv.WIN32).str_view()! == `\\server\`);

}

fn void! test_extension()
{
	assert(@catch(path::new(`C:`, .path_env = PathEnv.WIN32).extension()));
	assert(@catch(path::new(`C:`, .path_env = PathEnv.POSIX).extension()));
	assert(@catch(path::new(`file`, .path_env = PathEnv.WIN32).extension()));
	assert(@catch(path::new(`file`, .path_env = PathEnv.POSIX).extension()));
	assert(@catch(path::new(`C:\temp\foo.bar\README`, .path_env = PathEnv.WIN32).extension()));

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

fn void! test_path_is_absolute()
{
    assert(!path::new_posix("").is_absolute()!);
    assert(path::new_posix("/").is_absolute()!);
    assert(path::new_posix("/a/b").is_absolute()!);
    assert(!path::new_posix("a/b").is_absolute()!);

	assert(!path::new_windows(`C:`).is_absolute()!);
	assert(path::new_windows(`C:\abs`).is_absolute()!);
	assert(!path::new_windows(`C:abs`).is_absolute()!);
	assert(path::new_windows(`\\server\`).is_absolute()!);
	assert(path::new_windows(`\\server\abc`).is_absolute()!);
}

fn void! test_path_absolute()
{
    assert(path::new_posix("/").absolute()!.str_view() == "/");
    assert(path::new_posix(".").absolute()!.str_view() == path::getcwd(allocator::temp())!!.str_view());

	assert(path::new_windows(`C:\abs`).absolute()!.str_view() == `C:\abs`);
}