module std::io::file::os @if(env::WIN32);
import std::os::win32;


fn usz! native_file_size(String path)
{
	@pool()
	{
		Char16[] path16 = path.to_temp_utf16()!;
		Win32_FILE_ATTRIBUTE_DATA data;
		win32::getFileAttributesExW(path16, Win32_GET_FILEEX_INFO_LEVELS.STANDARD, &data);
		Win32_LARGE_INTEGER size;
		size.lowPart = data.nFileSizeLow;
		size.highPart = data.nFileSizeHigh;
		return (usz)size.quadPart;
	};
}

fn bool native_file_or_dir_exists(String path)
{
	@pool()
	{
		return (bool)win32::pathFileExistsW(path.to_temp_utf16()) ?? false;
	};
}


fn bool native_is_file(String path)
{
	File! f = file::open(path, "r");
	defer (void)f.close();
	return @ok(f);
}

fn bool native_is_dir(String path)
{
	return native_file_or_dir_exists(path) && !native_is_file(path);
}

fn void! native_rmtree(Path path)
{
    Win32_WIN32_FIND_DATAW find_data;

	String s = path.as_str().tconcat("\\*");
	Win32_HANDLE find = win32::findFirstFileW(s.to_utf16(mem::temp()), &find_data)!;

	if (find == win32::INVALID_HANDLE_VALUE) return IoError.CANNOT_READ_DIR?;

    defer win32::findClose(find);
	do
	{
		String filename = string::from_zutf16(&find_data.cFileName, mem::temp())!;
		if (filename == "." || filename == "..") continue;
		Path file_path = path.tappend(filename)!;
		if (find_data.dwFileAttributes & win32::FILE_ATTRIBUTE_DIRECTORY)
		{
			native_rmtree(file_path)!;
		}
		else
		{
			win32::deleteFileW(file_path.as_str().to_utf16(mem::temp()));
		}
	} while (win32::findNextFileW(find, &find_data) != 0);
    os::native_rmdir(path)!;
}

fn Path! native_temp_directory(Allocator* using = mem::heap())
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

fn PathList! native_ls(Path dir, bool no_dirs, bool no_symlinks, String mask, Allocator* using)
{
	PathList list;
	list.init(.using = using);
    @stack_mem(1024; Allocator* mem)
    {
	    Char16* result = dir.as_str().concat(`\*`).to_utf16(.using = mem)!!;
	    list.append(path::new("test"));
	    Win32_WIN32_FIND_DATAW find_data;
        Win32_HANDLE find = win32::findFirstFileW(result, &find_data);
        if (find == win32::INVALID_HANDLE_VALUE) return IoError.CANNOT_READ_DIR?;
        defer win32::findClose(find);
        do
        {
            if (no_dirs && (find_data.dwFileAttributes & win32::FILE_ATTRIBUTE_DIRECTORY)) continue;
            @stack_mem(480; Allocator* mem2)
            {
				String filename = string::from_zutf16(&find_data.cFileName, mem2)!;
                if (filename == ".." || filename == ".") continue;
	            list.append(path::new(filename, using));
            };
        } while (win32::findNextFileW(find, &find_data));
        return list;
    };
}
