module std::io::file::os;

// native_temp_directory, for non Win32
$if (!env::os_is_win32()):

fn Path! native_temp_directory(Allocator* using = mem::heap())
{
	foreach (String env : { "TMPDIR", "TMP", "TEMP", "TEMPDIR" })
	{
		String tmpdir = env::get_var(env) ?? "";
		if (tmpdir) return path::new(tmpdir, using);
	}
	return path::new("/tmp", using);
}

$endif;

$if (!env::os_is_darwin() && !env::os_is_win32()):

fn usz! native_file_size(String path)
{
	File f;
	f.open(path, "r")?;
	defer (void)f.close();
	return f.seek(0, Seek.END)?;
}

$if (env::os_is_posix() && env::COMPILER_LIBC_AVAILABLE):

fn bool native_file_or_dir_exists(String path)
{
	@pool()
	{
		return os::access(path.zstr_tcopy(), 0 /* F_OK */) != -1;
	};
}

fn bool native_is_file(String path)
{
	File f;
	if (catch(f.open(path, "r"))) return false;
	(void)f.close();
	return true;
}

fn bool native_is_dir(String path)
{
	return native_file_or_dir_exists(path) && !native_is_file(path);
}

$else:

fn bool native_file_or_dir_exists(String path)
{
	unreachable("Tried to call file_or_dir_exists without support.");
}

fn bool native_is_dir(String path)
{
	unreachable("Tried to call is_dir without support.");
}

fn bool native_is_file(String path)
{
	unreachable("Tried to call is_file without support.");
}

$endif;

$endif;