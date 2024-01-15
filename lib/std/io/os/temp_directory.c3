module std::io::os @if(env::LIBC);

fn Path! native_temp_directory(Allocator* allocator = allocator::heap()) @if(!env::WIN32)
{
	foreach (String env : { "TMPDIR", "TMP", "TEMP", "TEMPDIR" })
	{
		String tmpdir = env::get_var(env) ?? "";
		if (tmpdir) return path::new(tmpdir, allocator);
	}
	return path::new("/tmp", allocator);
}

fn Path! native_temp_directory(Allocator* allocator = allocator::heap()) @if(env::WIN32)
{
	@pool(allocator)
	{
		Win32_DWORD len = win32::getTempPathW(0, null);
		if (!len) return IoError.GENERAL_ERROR?;
		Char16[] buff = mem::temp_alloc_array(Char16, len + (usz)1);
		if (!win32::getTempPathW(len, buff)) return IoError.GENERAL_ERROR?;
		return path::new(string::temp_from_utf16(buff[:len]), allocator);
	};
}

module std::io::os @if(env::NO_LIBC);

macro Path! native_temp_directory(Allocator* allocator = allocator::heap())
{
	return IoError.UNSUPPORTED_OPERATION?;
}
