module std::io::os @if(env::LIBC);

fn Path! native_temp_directory(Allocator* using = mem::heap()) @if(!env::WIN32)
{
	foreach (String env : { "TMPDIR", "TMP", "TEMP", "TEMPDIR" })
	{
		String tmpdir = env::get_var(env) ?? "";
		if (tmpdir) return path::new(tmpdir, using);
	}
	return path::new("/tmp", using);
}

fn Path! native_temp_directory(Allocator* using = mem::heap()) @if(env::WIN32)
{
	@stack_mem(256; Allocator* mem)
	{
	    Win32_DWORD len = win32::getTempPathW(0, null);
		if (!len) return IoError.GENERAL_ERROR?;
		Char16[] buff = malloc(Char16, len + 1, .using = mem);
		if (!win32::getTempPathW(len, buff)) return IoError.GENERAL_ERROR?;
		return path::new(string::from_utf16(buff[:len], .using = mem), using);
	};
}

module std::io::os @if(env::NO_LIBC);

macro Path! native_temp_directory(Allocator* using = mem::heap())
{
	unreachable("Not available");
}
