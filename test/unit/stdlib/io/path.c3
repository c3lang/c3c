module std::io::path @test;

fn void test_append()
{
	Path p = path::new(mem, "/", POSIX)!!;
	Path p2 = p.append(mem, "hello")!!;
	test::eq(p2.str_view(), "/hello");
	p.free();
	p2.free();
	p = path::new(mem, "c:\\", WIN32)!!;
	p2 = p.append(mem, "hello")!!;
	assert(p2.str_view() == "c:\\hello");
	p.free();
	p2.free();
}
fn void test_dot()
{
	Path p = path::new(mem, ".")!!;
	defer p.free();
	assert(@catch(p.parent()));
	// It must be possible to form the absolute version.
	Path p2 = p.absolute(mem)!!;
	p2.free();
	p2 = p.append(mem, "/hello/world")!!;
	if (p2.env == POSIX)
	{
		assert(p2.str_view() == "hello/world");
	}
	else
	{
		assert(p2.str_view() == `hello\world`);
	}
	p2.free();
}

fn void test_parent()
{
	Path p = path::new(mem, "")!!;
	assert(@catch(p.parent()));
	p.free();
	p = path::new(mem, "/", path_env: PathEnv.POSIX)!!;
	assert(@catch(p.parent()));
	p.free();
	p = path::new(mem, "/a/b/c", path_env: PathEnv.POSIX)!!;
	assert(p.parent().str_view()!! == "/a/b");
	p.free();
	p = path::new(mem, "/a/b/c", path_env: PathEnv.WIN32)!!;
	assert(p.parent().str_view()!! == `\a\b`);
	p.free();
	p = path::new(mem, "/a/", path_env: PathEnv.POSIX)!!;
	assert(p.parent().str_view()!! == "/");
	p.free();
	p = path::new(mem, `C:\foo`, path_env: WIN32)!!;
    assert(p.parent().str_view()!! == `C:\`);
    assert(@catch(p.parent().parent()));
    p.free();
}

fn void test_path_normalized() => mem::@scoped(tmem)
{
	assert(path::new(mem, "",  path_env: PathEnv.WIN32).str_view()!! == "");
	assert(@catch(path::new(mem, "1:\\a\\b\\c.txt", path_env: PathEnv.WIN32)));
	assert(@catch(path::new(mem, ":", path_env: PathEnv.WIN32)));
	assert(@catch(path::new(mem, "1:", path_env: PathEnv.WIN32)));
	assert(@catch(path::new(mem, "1:a", path_env: PathEnv.WIN32)));
//	assert(@catch(path::new(mem, `\\\a\b\c.txt`, path_env: PathEnv.WIN32)));
	assert(@catch(path::new(mem, `\\server\a\b\..\..\..\c`, path_env: PathEnv.WIN32)));

	assert(@catch(path::new(mem, `\\a`, path_env: PathEnv.WIN32)));
	assert(@catch(path::new(mem, `/a/b/../../../c`, path_env: PathEnv.WIN32)));
	assert(@catch(path::new(mem, `/a/b/../../../c`, path_env: PathEnv.POSIX)));
	assert(@catch(path::new(mem, `/a/b/../../..`, path_env: PathEnv.WIN32)));
	assert(@catch(path::new(mem, `/a/b/../../..`, path_env: PathEnv.POSIX)));
	assert(@catch(path::new(mem, `/../a`, path_env: PathEnv.WIN32)));
	assert(@catch(path::new(mem, `/../a`, path_env: PathEnv.POSIX)));
	assert(@catch(path::new(mem, `/..`, path_env: PathEnv.WIN32)));
	assert(@catch(path::new(mem, `/..`, path_env: PathEnv.POSIX)));
	assert(@catch(path::new(mem, `C:/a/b/../../../c`, path_env: PathEnv.WIN32)));
	assert(@catch(path::new(mem, `C:/../a`, path_env: PathEnv.WIN32)));
	assert(@catch(path::new(mem, `C:/..`, path_env: PathEnv.WIN32)));

	assert(path::new(mem, "/", path_env: PathEnv.POSIX).str_view()!! == "/");
	assert(path::new(mem, "/./", path_env: PathEnv.POSIX).str_view()!! == "/");
	assert(path::new(mem, "/foo/../", path_env: PathEnv.POSIX).str_view()!! == "/");
	assert(path::new(mem, "/foo/bar/../", path_env: PathEnv.POSIX).str_view()!! == "/foo");
	assert(path::new(mem, "/foo//bar", path_env: PathEnv.POSIX).str_view()!! == "/foo/bar");
	assert(path::new(mem, "/foo//bar/../", path_env: PathEnv.POSIX).str_view()!! == "/foo");
	assert(path::new(mem, "/foo/.bar", path_env: PathEnv.POSIX).str_view()!! == "/foo/.bar");
	assert(path::new(mem, `\foo\.bar`, path_env: PathEnv.WIN32).str_view()!! == `\foo\.bar`);
	assert(path::new(mem, "a\\b/c.txt", path_env: PathEnv.WIN32).str_view()!! == `a\b\c.txt`);
	assert(path::new(mem, "a\\b/c.txt", path_env: PathEnv.POSIX).str_view()!! == "a\\b/c.txt");
	assert(path::new(mem, "C:\\a\\b/c.txt", path_env: PathEnv.WIN32).str_view()!! == `C:\a\b\c.txt`);
	assert(path::new(mem, "C:\\a\\b/c.txt", path_env: PathEnv.POSIX).str_view()!! == "C:\\a\\b/c.txt");
	assert(path::new(mem, `\\server\a\b/c.txt`, path_env: PathEnv.WIN32).str_view()!! == `\\server\a\b\c.txt`);
	assert(path::new(mem, `\\server\a\b/c.txt`, path_env: PathEnv.POSIX).str_view()!! == `\\server\a\b/c.txt`);
	assert(path::new(mem, `c:\hello//bar\\\\foo.txt`, path_env: PathEnv.WIN32).str_view()!! == `c:\hello\bar\foo.txt`);

	assert(path::new(mem, `~\a\b/c.txt`, path_env: PathEnv.WIN32).str_view()!! == `~\a\b\c.txt`);
	assert(path::new(mem, `~\a\b/c.txt`, path_env: PathEnv.POSIX).str_view()!! == `~\a\b/c.txt`);


	assert(path::new(mem, `a/b/../../../c`, path_env: PathEnv.WIN32).str_view()!! == `..\c`);
	assert(path::new(mem, `a/b/../../../c`, path_env: PathEnv.POSIX).str_view()!! == `../c`);
	assert(path::new(mem, `a/b/../../..`, path_env: PathEnv.WIN32).str_view()!! == `..`);
	assert(path::new(mem, `a/b/../../..`, path_env: PathEnv.POSIX).str_view()!! == `..`);
	assert(path::new(mem, `../a`, path_env: PathEnv.WIN32).str_view()!! == `..\a`);
	assert(path::new(mem, `../a`, path_env: PathEnv.POSIX).str_view()!! == `../a`);
	assert(path::new(mem, `..`, path_env: PathEnv.WIN32).str_view()!! == `..`);
	assert(path::new(mem, `..`, path_env: PathEnv.POSIX).str_view()!! == `..`);
	assert(path::new(mem, `a/b/../c`, path_env: PathEnv.WIN32).str_view()!! == `a\c`);
	assert(path::new(mem, `a/b/../c`, path_env: PathEnv.POSIX).str_view()!! == `a/c`);
	assert(path::new(mem, `a/b/../../c`, path_env: PathEnv.WIN32).str_view()!! == `c`);
	assert(path::new(mem, `a/b/../../c`, path_env: PathEnv.POSIX).str_view()!! == `c`);
	assert(path::new(mem, `a/b/..`, path_env: PathEnv.WIN32).str_view()!! == `a`);
	assert(path::new(mem, `a/b/..`, path_env: PathEnv.POSIX).str_view()!! == `a`);
	assert(path::new(mem, `a/b/../`, path_env: PathEnv.WIN32).str_view()!! == `a`);
	assert(path::new(mem, `a/b/../`, path_env: PathEnv.POSIX).str_view()!! == `a`);
	assert(path::new(mem, `a/b/../..`, path_env: PathEnv.WIN32).str_view()!! == ".");
	assert(path::new(mem, `a/b/../..`, path_env: PathEnv.POSIX).str_view()!! == ".");
	assert(path::new(mem, `a/b/../../`, path_env: PathEnv.WIN32).str_view()!! == ".");
	assert(path::new(mem, `a/b/../../`, path_env: PathEnv.POSIX).str_view()!! == ".");
	assert(path::new(mem, `a/b/../c/../d`, path_env: PathEnv.WIN32).str_view()!! == `a\d`);
	assert(path::new(mem, `a/b/../c/../d`, path_env: PathEnv.POSIX).str_view()!! == `a/d`);
	assert(path::new(mem, `a/b/../c/../d/`, path_env: PathEnv.WIN32).str_view()!! == `a\d`);
	assert(path::new(mem, `a/b/../c/../d/`, path_env: PathEnv.POSIX).str_view()!! == `a/d`);
	assert(path::new(mem, `a/b//d`, path_env: PathEnv.WIN32).str_view()!! == `a\b\d`);
	assert(path::new(mem, `a/b//d`, path_env: PathEnv.POSIX).str_view()!! == `a/b/d`);
	assert(path::new(mem, `a/b/././.`, path_env: PathEnv.WIN32).str_view()!! == `a\b`);
	assert(path::new(mem, `a/b/././.`, path_env: PathEnv.POSIX).str_view()!! == `a/b`);
	assert(path::new(mem, `a/b/./././`, path_env: PathEnv.WIN32).str_view()!! == `a\b`);
	assert(path::new(mem, `a/b/./././`, path_env: PathEnv.POSIX).str_view()!! == `a/b`);
	assert(path::new(mem, `./a/`, path_env: PathEnv.WIN32).str_view()!! == `a`);
	assert(path::new(mem, `./a/`, path_env: PathEnv.POSIX).str_view()!! == `a`);
	assert(path::new(mem, `./`, path_env: PathEnv.WIN32).str_view()!! == `.`);
	assert(path::new(mem, `./`, path_env: PathEnv.POSIX).str_view()!! == `.`);
	assert(path::new(mem, `.`, path_env: PathEnv.WIN32).str_view()!! == `.`);
	assert(path::new(mem, `.`, path_env: PathEnv.POSIX).str_view()!! == `.`);
	assert(path::new(mem, ``, path_env: PathEnv.WIN32).str_view()!! == ``);
	assert(path::new(mem, ``, path_env: PathEnv.POSIX).str_view()!! == ``);
	assert(path::new(mem, `/a`, path_env: PathEnv.WIN32).str_view()!! == `\a`);
	assert(path::new(mem, `/a`, path_env: PathEnv.POSIX).str_view()!! == `/a`);
	assert(path::new(mem, `/a/`, path_env: PathEnv.WIN32).str_view()!! == `\a`);
	assert(path::new(mem, `/a/`, path_env: PathEnv.POSIX).str_view()!! == `/a`);
	assert(path::new(mem, `/a/b/../c`, path_env: PathEnv.WIN32).str_view()!! == `\a\c`);
	assert(path::new(mem, `/a/b/../c`, path_env: PathEnv.POSIX).str_view()!! == `/a/c`);
	assert(path::new(mem, `/a/b/../../c`, path_env: PathEnv.WIN32).str_view()!! == `\c`);
	assert(path::new(mem, `/a/b/../../c`, path_env: PathEnv.POSIX).str_view()!! == `/c`);
	assert(path::new(mem, `/a/b/..`, path_env: PathEnv.WIN32).str_view()!! == `\a`);
	assert(path::new(mem, `/a/b/..`, path_env: PathEnv.POSIX).str_view()!! == `/a`);
	assert(path::new(mem, `/a/b/../..`, path_env: PathEnv.WIN32).str_view()!! == `\`);
	assert(path::new(mem, `/a/b/../..`, path_env: PathEnv.POSIX).str_view()!! == `/`);
	assert(path::new(mem, `/a/b/../c/../d`, path_env: PathEnv.WIN32).str_view()!! == `\a\d`);
	assert(path::new(mem, `/a/b/../c/../d`, path_env: PathEnv.POSIX).str_view()!! == `/a/d`);
	assert(path::new(mem, `/a/b//d`, path_env: PathEnv.WIN32).str_view()!! == `\a\b\d`);
	assert(path::new(mem, `/a/b//d`, path_env: PathEnv.POSIX).str_view()!! == `/a/b/d`);
	assert(path::new(mem, `/./a/`, path_env: PathEnv.WIN32).str_view()!! == `\a`);
	assert(path::new(mem, `/./a/`, path_env: PathEnv.POSIX).str_view()!! == `/a`);
	assert(path::new(mem, `/./`, path_env: PathEnv.WIN32).str_view()!! == `\`);
	assert(path::new(mem, `/./`, path_env: PathEnv.POSIX).str_view()!! == `/`);
	assert(path::new(mem, `/.`, path_env: PathEnv.WIN32).str_view()!! == `\`);
	assert(path::new(mem, `/.`, path_env: PathEnv.POSIX).str_view()!! == `/`);
	assert(path::new(mem, `/`, path_env: PathEnv.WIN32).str_view()!! == `\`);
	assert(path::new(mem, `/`, path_env: PathEnv.POSIX).str_view()!! == `/`);
	assert(path::new(mem, `C:/a`, path_env: PathEnv.WIN32).str_view()!! == `C:\a`);
	assert(path::new(mem, `C:/a`, path_env: PathEnv.POSIX).str_view()!! == `C:/a`);
	assert(path::new(mem, `C:/a/b/../c`, path_env: PathEnv.WIN32).str_view()!! == `C:\a\c`);
	assert(path::new(mem, `C:/a/b/../c`, path_env: PathEnv.POSIX).str_view()!! == `C:/a/c`);
	assert(path::new(mem, `C:/a/b/../../c`, path_env: PathEnv.WIN32).str_view()!! == `C:\c`);
	assert(path::new(mem, `C:/a/b/../../c`, path_env: PathEnv.POSIX).str_view()!! == `C:/c`);
	assert(path::new(mem, `C:/a/b/../../../c`, path_env: PathEnv.POSIX).str_view()!! == `c`);
	assert(path::new(mem, `C:/a/b/..`, path_env: PathEnv.WIN32).str_view()!! == `C:\a`);
	assert(path::new(mem, `C:/a/b/..`, path_env: PathEnv.POSIX).str_view()!! == `C:/a`);
	assert(path::new(mem, `C:/a/b/../..`, path_env: PathEnv.WIN32).str_view()!! == `C:\`);
	assert(path::new(mem, `C:/a/b/../..`, path_env: PathEnv.POSIX).str_view()!! == `C:`);
	assert(path::new(mem, `C:/a/b/../c/../d`, path_env: PathEnv.WIN32).str_view()!! == `C:\a\d`);
	assert(path::new(mem, `C:/a/b/../c/../d`, path_env: PathEnv.POSIX).str_view()!! == `C:/a/d`);
	assert(path::new(mem, `C:/a/b//d`, path_env: PathEnv.WIN32).str_view()!! == `C:\a\b\d`);
	assert(path::new(mem, `C:/a/b//d`, path_env: PathEnv.POSIX).str_view()!! == `C:/a/b/d`);
	assert(path::new(mem, `C:/a/b/././.`, path_env: PathEnv.WIN32).str_view()!! == `C:\a\b`);
	assert(path::new(mem, `C:/a/b/././.`, path_env: PathEnv.POSIX).str_view()!! == `C:/a/b`);
	assert(path::new(mem, `C:/./a`, path_env: PathEnv.WIN32).str_view()!! == `C:\a`);
	assert(path::new(mem, `C:/./a`, path_env: PathEnv.POSIX).str_view()!! == `C:/a`);
	assert(path::new(mem, `C:/./`, path_env: PathEnv.WIN32).str_view()!! == `C:\`);
	assert(path::new(mem, `C:/./`, path_env: PathEnv.POSIX).str_view()!! == `C:`);
	assert(path::new(mem, `C:/../a`, path_env: PathEnv.POSIX).str_view()!! == `a`);
	assert(path::new(mem, `C:/..`, path_env: PathEnv.POSIX).str_view()!! == `.`);
	assert(path::new(mem, `C:/`, path_env: PathEnv.WIN32).str_view()!! == `C:\`);
	assert(path::new(mem, `C:/`, path_env: PathEnv.POSIX).str_view()!! == `C:`);
	assert(path::new(mem, `C:a`, path_env: PathEnv.WIN32).str_view()!! == `C:a`);
	assert(path::new(mem, `C:a`, path_env: PathEnv.POSIX).str_view()!! == `C:a`);
	assert(path::new(mem, `C:a/`, path_env: PathEnv.WIN32).str_view()!! == `C:a`);
	assert(path::new(mem, `C:a/`, path_env: PathEnv.POSIX).str_view()!! == `C:a`);

	assert(path::new(mem, `C:a/b/../c`, path_env: PathEnv.WIN32).str_view()!! == `C:a\c`);
	assert(path::new(mem, `C:a/b/../c`, path_env: PathEnv.POSIX).str_view()!! == `C:a/c`);
	assert(path::new(mem, `C:a/b/../../c`, path_env: PathEnv.WIN32).str_view()!! == `C:c`);
	assert(path::new(mem, `C:a/b/../../c`, path_env: PathEnv.POSIX).str_view()!! == `c`);
	assert(path::new(mem, `C:a/b/..`, path_env: PathEnv.WIN32).str_view()!! == `C:a`);
	assert(path::new(mem, `C:a/b/..`, path_env: PathEnv.POSIX).str_view()!! == `C:a`);
	assert(path::new(mem, `C:a/b/../..`, path_env: PathEnv.WIN32).str_view()!! == `C:`);
	assert(path::new(mem, `C:a/b/../..`, path_env: PathEnv.POSIX).str_view()!! == `.`);
	assert(path::new(mem, `C:a/b/../c/../d`, path_env: PathEnv.WIN32).str_view()!! == `C:a\d`);
	assert(path::new(mem, `C:a/b/../c/../d`, path_env: PathEnv.POSIX).str_view()!! == `C:a/d`);
	assert(path::new(mem, `C:a/b//d`, path_env: PathEnv.WIN32).str_view()!! == `C:a\b\d`);
	assert(path::new(mem, `C:a/b//d`, path_env: PathEnv.POSIX).str_view()!! == `C:a/b/d`);
	assert(path::new(mem, `C:a/b/././.`, path_env: PathEnv.WIN32).str_view()!! == `C:a\b`);
	assert(path::new(mem, `C:a/b/././.`, path_env: PathEnv.POSIX).str_view()!! == `C:a/b`);
	assert(path::new(mem, `C:a/b/../../../c`, path_env: PathEnv.WIN32).str_view()!! == `C:..\c`);
	assert(path::new(mem, `C:./a`, path_env: PathEnv.WIN32).str_view()!! == `C:a`);
	assert(path::new(mem, `C:./a`, path_env: PathEnv.POSIX).str_view()!! == `C:./a`);
	assert(path::new(mem, `C:./`, path_env: PathEnv.WIN32).str_view()!! == `C:`);
	assert(path::new(mem, `C:./`, path_env: PathEnv.POSIX).str_view()!! == `C:.`);
	assert(path::new(mem, `C:../a`, path_env: PathEnv.POSIX).str_view()!! == `C:../a`);
	assert(path::new(mem, `C:../a`, path_env: PathEnv.WIN32).str_view()!! == `C:..\a`);
	assert(path::new(mem, `C:..`, path_env: PathEnv.POSIX).str_view()!! == `C:..`);
	assert(path::new(mem, `C:..`, path_env: PathEnv.WIN32).str_view()!! == `C:..`);
	assert(path::new(mem, `C:`, path_env: PathEnv.WIN32).str_view()!! == `C:`);
	assert(path::new(mem, `C:`, path_env: PathEnv.POSIX).str_view()!! == `C:`);

	assert(path::new(mem, `\\server\foo/a`, path_env: PathEnv.WIN32).str_view()!! == `\\server\foo\a`);
	assert(path::new(mem, `\\server\foo/a`, path_env: PathEnv.POSIX).str_view()!! == `\\server\foo/a`);
	assert(path::new(mem, `\\server\foo\a\b\..\c`, path_env: PathEnv.WIN32).str_view()!! == `\\server\foo\a\c`);
	assert(path::new(mem, `\\server\foo\a\b\..\..\c`, path_env: PathEnv.WIN32).str_view()!! == `\\server\foo\c`);
	assert(path::new(mem, `\\server\foo\a\b\..`, path_env: PathEnv.WIN32).str_view()!! == `\\server\foo\a`);
	assert(path::new(mem, `\\server\foo\a\..`, path_env: PathEnv.WIN32).str_view()!! == `\\server\foo\`);
	assert(path::new(mem, `\\server\foo\a\b\..\c\..\d`, path_env: PathEnv.WIN32).str_view()!! == `\\server\foo\a\d`);
	assert(path::new(mem, `\\server\foo\a\b\\d`, path_env: PathEnv.WIN32).str_view()!! == `\\server\foo\a\b\d`);
	assert(path::new(mem, `\\server\foo\a\b\.\.\.`, path_env: PathEnv.WIN32).str_view()!! == `\\server\foo\a\b`);
	assert(path::new(mem, `\\server\foo\.\a`, path_env: PathEnv.WIN32).str_view()!! == `\\server\foo\a`);
	assert(path::new(mem, `\\server\foo\.`, path_env: PathEnv.WIN32).str_view()!! == `\\server\foo\`);
	assert(path::new(mem, `\\server\foo\`, path_env: PathEnv.WIN32).str_view()!! == `\\server\foo\`);

}

fn void test_extension() => mem::@scoped(tmem)
{
	assert(@catch(path::new(mem, `C:`, path_env: PathEnv.WIN32).extension()));
	assert(@catch(path::new(mem, `C:`, path_env: PathEnv.POSIX).extension()));
	assert(@catch(path::new(mem, `file`, path_env: PathEnv.WIN32).extension()));
	assert(@catch(path::new(mem, `file`, path_env: PathEnv.POSIX).extension()));
	assert(@catch(path::new(mem, `C:\temp\foo.bar\README`, path_env: PathEnv.WIN32).extension()));

	assert(path::for_windows(mem, "file.txt").extension()!! == "txt");
	assert(path::for_posix(mem, "file.txt").extension()!! == "txt");

	assert(path::for_windows(mem, "a/b/file.txt").extension()!! == "txt");
	assert(path::for_posix(mem, "a/b/file.txt").extension()!! == "txt");

	assert(path::for_windows(mem, "a\\b\\file.txt").extension()!! == "txt");

	assert(path::for_windows(mem, "a.b/file.txt").extension()!! == "txt");
	assert(path::for_posix(mem, "a.b/file.txt").extension()!! == "txt");
	assert(path::for_windows(mem, "a.b/file.txt").extension()!! == "txt");
	assert(path::for_posix(mem, "a.b/file.txt").extension()!! == "txt");

	assert(path::for_windows(mem, "a.b\\file.txt").extension()!! == "txt");

	assert(path::for_windows(mem, "domain.dot.com").extension()!! == "com");
	assert(path::for_posix(mem, "domain.dot.com").extension()!! == "com");

	assert(path::for_windows(mem, "image.jpeg").extension()!! == "jpeg");
	assert(path::for_posix(mem, "image.jpeg").extension()!! == "jpeg");

	assert(path::for_windows(mem, "../filename.ext").extension()!! == "ext");
	assert(path::for_posix(mem, "../filename.ext").extension()!! == "ext");

}

fn void test_has_extension() => mem::@scoped(tmem)
{
	assert(!path::new(mem, `C:\temp\foo.bar\README`, path_env: PathEnv.WIN32)!!.has_extension(`bar\README`));

	assert(path::for_windows(mem, "file.txt")!!.has_extension("txt"));
	assert(path::for_posix(mem, "file.txt")!!.has_extension("txt"));

	assert(path::for_windows(mem, "a/b/file.txt")!!.has_extension("txt"));
	assert(path::for_posix(mem, "a/b/file.txt")!!.has_extension("txt"));

	assert(path::for_windows(mem, "a\\b\\file.txt")!!.has_extension("txt"));

	assert(path::for_windows(mem, "a.b/file.txt")!!.has_extension("txt"));
	assert(path::for_posix(mem, "a.b/file.txt")!!.has_extension("txt"));
	assert(path::for_windows(mem, "a.b/file.txt")!!.has_extension("txt"));
	assert(path::for_posix(mem, "a.b/file.txt")!!.has_extension("txt"));

	assert(path::for_windows(mem, "a.b\\file.txt")!!.has_extension("txt"));

	assert(path::for_windows(mem, "domain.dot.com")!!.has_extension("com"));
	assert(path::for_posix(mem, "domain.dot.com")!!.has_extension("com"));

	assert(path::for_windows(mem, "image.jpeg")!!.has_extension("jpeg"));
	assert(path::for_posix(mem, "image.jpeg")!!.has_extension("jpeg"));

	assert(path::for_windows(mem, "../filename.ext")!!.has_extension("ext"));
	assert(path::for_posix(mem, "../filename.ext")!!.has_extension("ext"));

}

fn void test_basename() => mem::@scoped(tmem)
{
	assert(path::for_windows(mem, "file.txt").basename()!! == "file.txt");
	assert(path::for_posix(mem, "file.txt").basename()!! == "file.txt");

	assert(path::for_windows(mem, "a/b/file.txt").basename()!! == "file.txt");
	assert(path::for_posix(mem, "a/b/file.txt").basename()!! == "file.txt");

	assert(path::for_windows(mem, "a.b/file.txt").basename()!! == "file.txt");
	assert(path::for_posix(mem, "a.b/file.txt").basename()!! == "file.txt");

	assert(path::for_windows(mem, "a.b/file.txt").basename()!! == "file.txt");
	assert(path::for_posix(mem, "a.b/file.txt").basename()!! == "file.txt");

	assert(path::for_windows(mem, "../filename.ext").basename()!! == "filename.ext");
	assert(path::for_posix(mem, "../filename.ext").basename()!! == "filename.ext");

	assert(path::for_windows(mem, "C:").basename()!! == "");
	assert(path::for_posix(mem, "C:").basename()!! == "C:");

	assert(path::for_windows(mem, "../..").basename()!! == "..");
	assert(path::for_posix(mem, "../..").basename()!! == "..");

	assert(path::for_windows(mem, `\\server\abc`).basename()!! == "");
	assert(path::for_posix(mem, `\\server\abc`).basename()!! == `\\server\abc`);
}

fn void test_dirname() => mem::@scoped(tmem)
{
	assert(path::for_posix(mem, "").dirname()!! == ".");
	assert(path::for_posix(mem, "/file").dirname()!! == "/");
	assert(path::for_posix(mem, "///").dirname()!! == "/");
	assert(path::for_windows(mem, "d:").dirname()!! == "d:");
	assert(path::for_windows(mem, "d:file").dirname()!! == "d:");
	assert(path::for_windows(mem, `d:\file`).dirname()!! == `d:\`);

	assert(path::for_windows(mem, "file.txt").dirname()!! == ".");
	assert(path::for_posix(mem, "file.txt").dirname()!! == ".");

	assert(path::for_windows(mem, "a/b/file.txt").dirname()!! == `a\b`);
	assert(path::for_posix(mem, "a/b/file.txt").dirname()!! == "a/b");

	assert(path::for_windows(mem, "a.b/file.txt").dirname()!! == "a.b");
	assert(path::for_posix(mem, "a.b/file.txt").dirname()!! == "a.b");

	assert(path::for_windows(mem, "../filename.ext").dirname()!! == "..");
	assert(path::for_posix(mem, "../filename.ext").dirname()!! == "..");

	assert(path::for_windows(mem, "C:").dirname()!! == "C:");
	assert(path::for_posix(mem, "C:").dirname()!! == ".");

	assert(path::for_windows(mem, "C:/").dirname()!! == "C:\\");
	assert(path::for_posix(mem, "C:/").dirname()!! == ".");

	assert(path::for_windows(mem, "C:/a").dirname()!! == "C:\\");
	assert(path::for_posix(mem, "C:/a").dirname()!! == "C:");

	assert(path::for_windows(mem, "../..").dirname()!! == "..");
	assert(path::for_posix(mem, "../..").dirname()!! == "..");

	assert(path::for_windows(mem, `\\server\share\dir\file`).dirname()!! == `\\server\share\dir`);
	assert(path::for_windows(mem, `\\server\share\file`).dirname()!! == `\\server\share`);
	assert(path::for_windows(mem, `\\server\share\`).dirname()!! == `\\server\share`);
	assert(path::for_windows(mem, `\\server\share`).dirname()!! == `\\server\share`);
	assert(path::for_posix(mem, `\\server\`).dirname()!! == `.`);
}

fn void test_path_volume() => mem::@scoped(tmem)
{
	assert(path::for_windows(mem, `C:\abs`).volume_name()!! == `C:`);
	assert(path::for_windows(mem, `C:abs`).volume_name()!! == `C:`);
	assert(path::for_posix(mem, `C:/abs`).volume_name()!! == ``);
	assert(path::for_posix(mem, `C:abs`).volume_name()!! == ``);
	assert(path::for_windows(mem, `\\server\foo`).volume_name()!! == `\\server\foo`);
    assert(path::for_windows(mem, `\\server\foo\abc`).volume_name()!! == `\\server\foo`);
}

fn void test_path_is_absolute() => mem::@scoped(tmem)
{
    assert(!path::for_posix(mem, "").is_absolute()!!);
    assert(path::for_posix(mem, "/").is_absolute()!!);
    assert(path::for_posix(mem, "/a/b").is_absolute()!!);
    assert(!path::for_posix(mem, "a/b").is_absolute()!!);

	assert(!path::for_windows(mem, `C:`).is_absolute()!!);
	assert(path::for_windows(mem, `C:\abs`).is_absolute()!!);
	assert(!path::for_windows(mem, `C:abs`).is_absolute()!!);
	assert(path::for_windows(mem, `\\server\foo`).is_absolute()!!);
	assert(path::for_windows(mem, `\\server\foo\abc`).is_absolute()!!);
}

fn void test_path_absolute() => mem::@scoped(tmem)
{
$if env::WIN32:
	assert(path::for_windows(mem, `C:\abs`).absolute(mem, )!!.str_view() == `C:\abs`);
$else
    assert(path::for_posix(mem, "/").absolute(mem, )!!.str_view() == "/");
    assert(path::for_posix(mem, ".").absolute(mem, )!!.str_view() == path::tcwd()!!.str_view());
$endif
}