module std::io::path @test;

fn void test_append() => mem::@scoped(tmem)
{
	{
		PathPosix p = path::for_posix(mem, "/")!!;
		PathPosix p2 = p.append(mem, "hello")!!;
		test::eq(p2, "/hello");

		p = path::for_posix(mem, "")!!;
		p2 = p.append(mem, "a", "b")!!;
		test::eq(p2, "a/b");

		p = path::for_posix(mem, "/a/")!!;
		p2 = p.append(mem, "b", "c")!!;
		test::eq(p2, "/a/b/c");
		p2 = p.append(mem, "b", "c", extension: "txt")!!;
		test::eq(p2, "/a/b/c.txt");
		p2 = p.append(mem, "b", "", "c", "")!!;
		test::eq(p2, "/a/b/c");

	}
	{
		PathWin p = path::for_windows(mem, "c:\\")!!;
		PathWin p2 = p.append(mem, "hello")!!;
		assert(p2 == "c:\\hello");

		p = path::for_windows(mem, "")!!;
		p2 = p.append(mem, "a", "b")!!;
		test::eq(p2, "a\\b");

		p = path::for_windows(mem, "\\a\\")!!;
		p2 = p.append(mem, "b", "c")!!;
		test::eq(p2, "\\a\\b\\c");
		p2 = p.append(mem, "b", "c", extension: "txt")!!;
		test::eq(p2, "\\a\\b\\c.txt");
		p2 = p.append(mem, "b", "", "c", "")!!;
		test::eq(p2, "\\a\\b\\c");
	}
}
fn void test_dot()
{
	Path p = path::new(mem, ".")!!;
	defer p.free(mem);
	assert(@catch(p.parent()));
	// It must be possible to form the absolute version.
	Path p2 = p.absolute(mem)!!;
	p2.free(mem);
	p2 = p.append(mem, "/hello/world")!!;
	if (path::DEFAULT_ENV == POSIX)
	{
		assert(p2 == "hello/world");
	}
	else
	{
		assert(p2 == `hello\world`);
	}
	p2.free(mem);
}

fn void test_parent()
{
	{
		Path p = path::new(mem, "")!!;
		assert(@catch(p.parent()));
		p.free(mem);
	}	
	{
		PathPosix p = path::for_posix(mem, "/")!!;
    	assert(@catch(p.parent()));
    	p.free(mem);
    	p = path::for_posix(mem, "/a/b/c")!!;
    	assert(p.parent()!! == "/a/b");
    	p.free(mem);
		p = path::for_posix(mem, "/a/")!!;
		assert(p.parent()!! == "/");
		p.free(mem);
	}
	{
		PathWin p = path::for_windows(mem, "/a/b/c")!!;
		assert(p.parent()!! == `\a\b`);
		p.free(mem);
		p = path::for_windows(mem, `C:\foo`)!!;
		assert(p.parent()!! == `C:\`);
		assert(@catch(p.parent().parent()));
		p.free(mem);
    }

}

fn void test_path_normalized() => mem::@scoped(tmem)
{
	assert(path::for_windows(mem, "")!! == "");
	assert(@catch(path::for_windows(mem, "1:\\a\\b\\c.txt")));
	assert(@catch(path::for_windows(mem, ":")));
	assert(@catch(path::for_windows(mem, "1:")));
	assert(@catch(path::for_windows(mem, "1:a")));
//	assert(@catch(path::new(mem, `\\\a\b\c.txt`, path_env: PathEnv.WIN32)));
	assert(@catch(path::for_windows(mem, `\\server\a\b\..\..\..\c`)));

	assert(@catch(path::for_windows(mem, `\\a`)));
	assert(@catch(path::for_windows(mem, `/a/b/../../../c`)));
	assert(@catch(path::for_posix(mem, `/a/b/../../../c`)));
	assert(@catch(path::for_windows(mem, `/a/b/../../..`)));
	assert(@catch(path::for_posix(mem, `/a/b/../../..`)));
	assert(@catch(path::for_windows(mem, `/../a`)));
	assert(@catch(path::for_posix(mem, `/../a`)));
	assert(@catch(path::for_windows(mem, `/..`)));
	assert(@catch(path::for_posix(mem, `/..`)));
	assert(@catch(path::for_windows(mem, `C:/a/b/../../../c`)));
	assert(@catch(path::for_windows(mem, `C:/../a`)));
	assert(@catch(path::for_windows(mem, `C:/..`)));

	assert(path::for_posix(mem, "/")!! == "/");
	assert(path::for_posix(mem, "/./")!! == "/");
	assert(path::for_posix(mem, "/foo/../")!! == "/");
	assert(path::for_posix(mem, "/foo/bar/../")!! == "/foo");
	assert(path::for_posix(mem, "/foo//bar")!! == "/foo/bar");
	assert(path::for_posix(mem, "/foo//bar/../")!! == "/foo");
	assert(path::for_posix(mem, "/foo/.bar")!! == "/foo/.bar");
	assert(path::for_windows(mem, `\foo\.bar`)!! == `\foo\.bar`);
	assert(path::for_windows(mem, "a\\b/c.txt")!! == `a\b\c.txt`);
	assert(path::for_posix(mem, "a\\b/c.txt")!! == "a\\b/c.txt");
	assert(path::for_windows(mem, "C:\\a\\b/c.txt")!! == `C:\a\b\c.txt`);
	assert(path::for_posix(mem, "C:\\a\\b/c.txt")!! == "C:\\a\\b/c.txt");
	assert(path::for_windows(mem, `\\server\a\b/c.txt`)!! == `\\server\a\b\c.txt`);
	assert(path::for_posix(mem, `\\server\a\b/c.txt`)!! == `\\server\a\b/c.txt`);
	assert(path::for_windows(mem, `c:\hello//bar\\\\foo.txt`)!! == `c:\hello\bar\foo.txt`);

	assert(path::for_windows(mem, `~\a\b/c.txt`)!! == `~\a\b\c.txt`);
	assert(path::for_posix(mem, `~\a\b/c.txt`)!! == `~\a\b/c.txt`);


	assert(path::for_windows(mem, `a/b/../../../c`)!! == `..\c`);
	assert(path::for_posix(mem, `a/b/../../../c`)!! == `../c`);
	assert(path::for_windows(mem, `a/b/../../..`)!! == `..`);
	assert(path::for_posix(mem, `a/b/../../..`)!! == `..`);
	assert(path::for_windows(mem, `../a`)!! == `..\a`);
	assert(path::for_posix(mem, `../a`)!! == `../a`);
	assert(path::for_windows(mem, `..`)!! == `..`);
	assert(path::for_posix(mem, `..`)!! == `..`);
	assert(path::for_windows(mem, `a/b/../c`)!! == `a\c`);
	assert(path::for_posix(mem, `a/b/../c`)!! == `a/c`);
	assert(path::for_windows(mem, `a/b/../../c`)!! == `c`);
	assert(path::for_posix(mem, `a/b/../../c`)!! == `c`);
	assert(path::for_windows(mem, `a/b/..`)!! == `a`);
	assert(path::for_posix(mem, `a/b/..`)!! == `a`);
	assert(path::for_windows(mem, `a/b/../`)!! == `a`);
	assert(path::for_posix(mem, `a/b/../`)!! == `a`);
	assert(path::for_windows(mem, `a/b/../..`)!! == ".");
	assert(path::for_posix(mem, `a/b/../..`)!! == ".");
	assert(path::for_windows(mem, `a/b/../../`)!! == ".");
	assert(path::for_posix(mem, `a/b/../../`)!! == ".");
	assert(path::for_windows(mem, `a/b/../c/../d`)!! == `a\d`);
	assert(path::for_posix(mem, `a/b/../c/../d`)!! == `a/d`);
	assert(path::for_windows(mem, `a/b/../c/../d/`)!! == `a\d`);
	assert(path::for_posix(mem, `a/b/../c/../d/`)!! == `a/d`);
	assert(path::for_windows(mem, `a/b//d`)!! == `a\b\d`);
	assert(path::for_posix(mem, `a/b//d`)!! == `a/b/d`);
	assert(path::for_windows(mem, `a/b/././.`)!! == `a\b`);
	assert(path::for_posix(mem, `a/b/././.`)!! == `a/b`);
	assert(path::for_windows(mem, `a/b/./././`)!! == `a\b`);
	assert(path::for_posix(mem, `a/b/./././`)!! == `a/b`);
	assert(path::for_windows(mem, `./a/`)!! == `a`);
	assert(path::for_posix(mem, `./a/`)!! == `a`);
	assert(path::for_windows(mem, `./`)!! == `.`);
	assert(path::for_posix(mem, `./`)!! == `.`);
	assert(path::for_windows(mem, `.`)!! == `.`);
	assert(path::for_posix(mem, `.`)!! == `.`);
	assert(path::for_windows(mem, ``)!! == ``);
	assert(path::for_posix(mem, ``)!! == ``);
	assert(path::for_windows(mem, `/a`)!! == `\a`);
	assert(path::for_posix(mem, `/a`)!! == `/a`);
	assert(path::for_windows(mem, `/a/`)!! == `\a`);
	assert(path::for_posix(mem, `/a/`)!! == `/a`);
	assert(path::for_windows(mem, `/a/b/../c`)!! == `\a\c`);
	assert(path::for_posix(mem, `/a/b/../c`)!! == `/a/c`);
	assert(path::for_windows(mem, `/a/b/../../c`)!! == `\c`);
	assert(path::for_posix(mem, `/a/b/../../c`)!! == `/c`);
	assert(path::for_windows(mem, `/a/b/..`)!! == `\a`);
	assert(path::for_posix(mem, `/a/b/..`)!! == `/a`);
	assert(path::for_windows(mem, `/a/b/../..`)!! == `\`);
	assert(path::for_posix(mem, `/a/b/../..`)!! == `/`);
	assert(path::for_windows(mem, `/a/b/../c/../d`)!! == `\a\d`);
	assert(path::for_posix(mem, `/a/b/../c/../d`)!! == `/a/d`);
	assert(path::for_windows(mem, `/a/b//d`)!! == `\a\b\d`);
	assert(path::for_posix(mem, `/a/b//d`)!! == `/a/b/d`);
	assert(path::for_windows(mem, `/./a/`)!! == `\a`);
	assert(path::for_posix(mem, `/./a/`)!! == `/a`);
	assert(path::for_windows(mem, `/./`)!! == `\`);
	assert(path::for_posix(mem, `/./`)!! == `/`);
	assert(path::for_windows(mem, `/.`)!! == `\`);
	assert(path::for_posix(mem, `/.`)!! == `/`);
	assert(path::for_windows(mem, `/`)!! == `\`);
	assert(path::for_posix(mem, `/`)!! == `/`);
	assert(path::for_windows(mem, `C:/a`)!! == `C:\a`);
	assert(path::for_posix(mem, `C:/a`)!! == `C:/a`);
	assert(path::for_windows(mem, `C:/a/b/../c`)!! == `C:\a\c`);
	assert(path::for_posix(mem, `C:/a/b/../c`)!! == `C:/a/c`);
	assert(path::for_windows(mem, `C:/a/b/../../c`)!! == `C:\c`);
	assert(path::for_posix(mem, `C:/a/b/../../c`)!! == `C:/c`);
	assert(path::for_posix(mem, `C:/a/b/../../../c`)!! == `c`);
	assert(path::for_windows(mem, `C:/a/b/..`)!! == `C:\a`);
	assert(path::for_posix(mem, `C:/a/b/..`)!! == `C:/a`);
	assert(path::for_windows(mem, `C:/a/b/../..`)!! == `C:\`);
	assert(path::for_posix(mem, `C:/a/b/../..`)!! == `C:`);
	assert(path::for_windows(mem, `C:/a/b/../c/../d`)!! == `C:\a\d`);
	assert(path::for_posix(mem, `C:/a/b/../c/../d`)!! == `C:/a/d`);
	assert(path::for_windows(mem, `C:/a/b//d`)!! == `C:\a\b\d`);
	assert(path::for_posix(mem, `C:/a/b//d`)!! == `C:/a/b/d`);
	assert(path::for_windows(mem, `C:/a/b/././.`)!! == `C:\a\b`);
	assert(path::for_posix(mem, `C:/a/b/././.`)!! == `C:/a/b`);
	assert(path::for_windows(mem, `C:/./a`)!! == `C:\a`);
	assert(path::for_posix(mem, `C:/./a`)!! == `C:/a`);
	assert(path::for_windows(mem, `C:/./`)!! == `C:\`);
	assert(path::for_posix(mem, `C:/./`)!! == `C:`);
	assert(path::for_posix(mem, `C:/../a`)!! == `a`);
	assert(path::for_posix(mem, `C:/..`)!! == `.`);
	assert(path::for_windows(mem, `C:/`)!! == `C:\`);
	assert(path::for_posix(mem, `C:/`)!! == `C:`);
	assert(path::for_windows(mem, `C:a`)!! == `C:a`);
	assert(path::for_posix(mem, `C:a`)!! == `C:a`);
	assert(path::for_windows(mem, `C:a/`)!! == `C:a`);
	assert(path::for_posix(mem, `C:a/`)!! == `C:a`);

	assert(path::for_windows(mem, `C:a/b/../c`)!! == `C:a\c`);
	assert(path::for_posix(mem, `C:a/b/../c`)!! == `C:a/c`);
	assert(path::for_windows(mem, `C:a/b/../../c`)!! == `C:c`);
	assert(path::for_posix(mem, `C:a/b/../../c`)!! == `c`);
	assert(path::for_windows(mem, `C:a/b/..`)!! == `C:a`);
	assert(path::for_posix(mem, `C:a/b/..`)!! == `C:a`);
	assert(path::for_windows(mem, `C:a/b/../..`)!! == `C:`);
	assert(path::for_posix(mem, `C:a/b/../..`)!! == `.`);
	assert(path::for_windows(mem, `C:a/b/../c/../d`)!! == `C:a\d`);
	assert(path::for_posix(mem, `C:a/b/../c/../d`)!! == `C:a/d`);
	assert(path::for_windows(mem, `C:a/b//d`)!! == `C:a\b\d`);
	assert(path::for_posix(mem, `C:a/b//d`)!! == `C:a/b/d`);
	assert(path::for_windows(mem, `C:a/b/././.`)!! == `C:a\b`);
	assert(path::for_posix(mem, `C:a/b/././.`)!! == `C:a/b`);
	assert(path::for_windows(mem, `C:a/b/../../../c`)!! == `C:..\c`);
	assert(path::for_windows(mem, `C:./a`)!! == `C:a`);
	assert(path::for_posix(mem, `C:./a`)!! == `C:./a`);
	assert(path::for_windows(mem, `C:./`)!! == `C:`);
	assert(path::for_posix(mem, `C:./`)!! == `C:.`);
	assert(path::for_posix(mem, `C:../a`)!! == `C:../a`);
	assert(path::for_windows(mem, `C:../a`)!! == `C:..\a`);
	assert(path::for_posix(mem, `C:..`)!! == `C:..`);
	assert(path::for_windows(mem, `C:..`)!! == `C:..`);
	assert(path::for_windows(mem, `C:`)!! == `C:`);
	assert(path::for_posix(mem, `C:`)!! == `C:`);

	assert(path::for_windows(mem, `\\server\foo/a`)!! == `\\server\foo\a`);
	assert(path::for_posix(mem, `\\server\foo/a`)!! == `\\server\foo/a`);
	assert(path::for_windows(mem, `\\server\foo\a\b\..\c`)!! == `\\server\foo\a\c`);
	assert(path::for_windows(mem, `\\server\foo\a\b\..\..\c`)!! == `\\server\foo\c`);
	assert(path::for_windows(mem, `\\server\foo\a\b\..`)!! == `\\server\foo\a`);
	assert(path::for_windows(mem, `\\server\foo\a\..`)!! == `\\server\foo\`);
	assert(path::for_windows(mem, `\\server\foo\a\b\..\c\..\d`)!! == `\\server\foo\a\d`);
	assert(path::for_windows(mem, `\\server\foo\a\b\\d`)!! == `\\server\foo\a\b\d`);
	assert(path::for_windows(mem, `\\server\foo\a\b\.\.\.`)!! == `\\server\foo\a\b`);
	assert(path::for_windows(mem, `\\server\foo\.\a`)!! == `\\server\foo\a`);
	assert(path::for_windows(mem, `\\server\foo\.`)!! == `\\server\foo\`);
	assert(path::for_windows(mem, `\\server\foo\`)!! == `\\server\foo\`);

	assert(path::for_posix(mem, `.a/..`)!! == `.`);
	assert(path::for_posix(mem, `.x/..`)!! == `.`);
	assert(path::for_posix(mem, `.a/.b/..`)!! == `.a`);
	assert(path::for_posix(mem, `.a/.b/../..`)!! == `.`);
	assert(path::for_posix(mem, `.a/b/../..`)!! == `.`);
	assert(path::for_posix(mem, `/.a/..`)!! == `/`);
	assert(path::for_posix(mem, `foo/.a/..`)!! == `foo`);
	assert(path::for_windows(mem, `.a/..`)!! == `.`);
	assert(path::for_windows(mem, `.a\.b\..`)!! == `.a`);
	assert(path::for_windows(mem, `/.a/..`)!! == `\`);
	assert(path::for_windows(mem, `foo\.a\..`)!! == `foo`);
}

fn void test_extension() => mem::@scoped(tmem)
{
	assert(@catch(path::for_windows(mem, `C:`).extension()));
	assert(@catch(path::for_posix(mem, `C:`).extension()));
	assert(@catch(path::for_windows(mem, `file`).extension()));
	assert(@catch(path::for_posix(mem, `file`).extension()));
	assert(@catch(path::for_windows(mem, `C:\temp\foo.bar\README`).extension()));

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
	assert(!path::for_windows(mem, `C:\temp\foo.bar\README`)!!.has_extension(`bar\README`));

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
	assert(path::for_windows(mem, `C:\abs`).absolute(mem, )!! == `C:\abs`);
$else
    assert(path::for_posix(mem, "/").absolute(mem, )!! == "/");
    assert(path::for_posix(mem, ".").absolute(mem, )!! == path::tcwd()!!);
$endif
}

fn void test_ls_options() => mem::@scoped(tmem)
{
	Path root = path::temp_directory(mem).tappend("c3_ls_test")!!;
	if (path::exists(root)) (void)path::rmtree(root);
	path::mkdir(root, recursive: true)!!;
	defer (void)path::rmtree(root);
	path::mkdir(root.tappend("alpha.c3")!!, recursive: true)!!;
	path::mkdir(root.tappend("beta.txt")!!, recursive: true)!!;
	path::mkdir(root.tappend("sub", "gamma.c3")!!, recursive: true)!!;

	PathList files = path::ls(mem, root, mask: "*.c3")!!;
	bool found_alpha;
	bool found_beta;
	bool found_gamma;
	foreach (entry : files)
	{
		found_alpha = found_alpha || entry == path::tnew("alpha.c3")!!;
		found_beta = found_beta || entry == path::tnew("beta.txt")!!;
		found_gamma = found_gamma || entry == path::tnew("sub/gamma.c3")!!;
	}
	assert(found_alpha);
	assert(!found_beta);
	assert(!found_gamma);

	files = path::ls(mem, root, mask: "*.c3", recursive: true)!!;
	found_alpha = false;
	found_gamma = false;
	foreach (entry : files)
	{
		found_alpha = found_alpha || entry == path::tnew("alpha.c3")!!;
		found_gamma = found_gamma || entry == path::tnew("sub/gamma.c3")!!;
	}
	assert(found_alpha);
	assert(found_gamma);

	files = path::ls(mem, root, recursive: true, relative: false)!!;
	found_alpha = false;
	found_gamma = false;
	foreach (entry : files)
	{
		found_alpha = found_alpha || entry == root.tappend("alpha.c3")!!;
		found_gamma = found_gamma || entry == root.tappend("sub", "gamma.c3")!!;
	}
	assert(found_alpha);
	assert(found_gamma);

	files = path::ls(mem, root, recursive: true, relative: true, rebase: path::tnew("rebased")!!)!!;
	found_alpha = false;
	found_gamma = false;
	foreach (entry : files)
	{
		found_alpha = found_alpha || entry == path::tnew("rebased/alpha.c3")!!;
		found_gamma = found_gamma || entry == path::tnew("rebased/sub/gamma.c3")!!;
	}
	assert(found_alpha);
	assert(found_gamma);
}