module std::io::file::os @if(env::os_is_win32());
import std::os::win32;

const Win32_DWORD FILE_ATTRIBUTE_READONLY = 0x01;
const Win32_DWORD FILE_ATTRIBUTE_HIDDEN = 0x02;
const Win32_DWORD FILE_ATTRIBUTE_SYSTEM = 0x04;
const Win32_DWORD FILE_ATTRIBUTE_DIRECTORY = 0x10;
const Win32_DWORD FILE_ATTRIBUTE_ARCHIVE = 0x20;
const Win32_DWORD FILE_ATTRIBUTE_DEVICE = 0x40;
const Win32_DWORD FILE_ATTRIBUTE_NORMAL = 0x80;
const Win32_DWORD FILE_ATTRIBUTE_TEMPORARY = 0x100;
const Win32_DWORD FILE_ATTRIBUTE_SPARSE_FILE = 0x200;
const Win32_DWORD FILE_ATTRIBUTE_REPARSE_POINT = 0x400;
const Win32_DWORD FILE_ATTRIBUTE_COMPRESSED = 0x800;
const Win32_DWORD FILE_ATTRIBUTE_OFFLINE = 0x1000;
const Win32_DWORD FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = 0x2000;
const Win32_DWORD FILE_ATTRIBUTE_ENCRYPTED = 0x4000;
const Win32_DWORD FILE_ATTRIBUTE_INTEGRITY_STREAM = 0x8000;
const Win32_DWORD FILE_ATTRIBUTE_VIRTUAL = 0x10000;
const Win32_DWORD FILE_ATTRIBUTE_NO_SCRUB_DATA = 0x20000;
const Win32_DWORD FILE_ATTRIBUTE_EA = 0x40000;
const Win32_DWORD FILE_ATTRIBUTE_PINNED = 0x80000;
const Win32_DWORD FILE_ATTRIBUTE_UNPINNED = 0x100000;
const Win32_DWORD FILE_ATTRIBUTE_RECALL_ON_OPEN = 0x40000;
const Win32_DWORD FILE_ATTRIBUTE_RECALL_ON_DATA_ACCESS = 0x400000;

fn usz! native_file_size(String path)
{
	@pool()
	{
		Char16[] path16 = path.to_temp_utf16()!;
		Win32_FILE_ATTRIBUTE_DATA data;
		win32::win32_GetFileAttributesExW(path16, Win32_GET_FILEEX_INFO_LEVELS.STANDARD, &data);
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
		return (bool)win32::win32_PathFileExistsW(path.to_temp_utf16()) ?? false;
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
	Win32_HANDLE find = win32::win32_FindFirstFileW(s.to_utf16(mem::temp()), &find_data)!;

	if (find == win32::INVALID_HANDLE_VALUE) return IoError.CANNOT_READ_DIR?;

    defer win32::win32_FindClose(find);
	do
	{
		String filename = string::from_zutf16(&find_data.cFileName, mem::temp())!;
		if (filename == "." || filename == "..") continue;
		Path file_path = path.tappend(filename)!;
		if (find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
		{
			native_rmtree(file_path)!;
		}
		else
		{
			win32::win32_DeleteFileW(file_path.as_str().to_utf16(mem::temp()));
		}
	} while (win32::win32_FindNextFileW(find, &find_data) != 0);
    os::native_rmdir(path)!;
}

fn Path! native_temp_directory(Allocator* using = mem::heap())
{
	@stack_mem(256; Allocator* mem)
	{
	    Win32_DWORD len = win32::win32_GetTempPathW(0, null);
		if (!len) return IoError.GENERAL_ERROR?;
		Char16[] buff = malloc(Char16, len + 1, .using = mem);
		if (!win32::win32_GetTempPathW(len, buff)) return IoError.GENERAL_ERROR?;
		return path::new(string::from_utf16(buff[:len], .using = mem), using);
	};
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
