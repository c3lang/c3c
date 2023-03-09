module std::io::file::os;
import std::os::win32::files;

$if (env::os_is_win32()):

fn usz! native_file_size(String path)
{
	@pool()
	{
		Char16[] path16 = path.to_temp_utf16()?;
		Win32_FILE_ATTRIBUTE_DATA data;
		files::win32_GetFileAttributesExW(path16, Win32_GET_FILEEX_INFO_LEVELS.STANDARD, &data);
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
		return files::win32_PathFileExistsW(path.to_temp_utf16()) ?? false;
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

fn Path! native_temp_directory(Allocator* using = mem::heap())
{
	Char16[256] default_buffer;
	Char16* buff = &default_buffer;
	// Free if we allocated on the heap.
	defer if (buff != &default_buffer) free(buff);

    Win32_DWORD len = files::win32_GetTempPathW(default_buffer.len, buff);
	if (!len) return IoError.GENERAL_ERROR!;
	// Too long, allocate
	if (len > default_buffer.len)
	{
		// Allocate on the heap
		buff = tmalloc(Char16, len);
		if (!files::win32_GetTempPathW(len, buff)) return IoError.GENERAL_ERROR!;
	}
	return (Path)string::from_utf16(buff[:len], using);
}

/*

	}else if(method == file_size_methods::get_attributes){
		WIN32_FILE_ATTRIBUTE_DATA file_attr_data;
		if(GetFileAttributesEx(path, GetFileExInfoStandard, &file_attr_data)){
			file_size.LowPart = file_attr_data.nFileSizeLow;
			file_size.HighPart = file_attr_data.nFileSizeHigh;
		}
	}
*/
$endif;