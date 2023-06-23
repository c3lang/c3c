module std::io::file::os @if(env::POSIX);
import libc;

/**
 * @require dir.as_str()
 **/
fn void! native_rmtree(Path dir)
{
	DIRPtr directory = posix::opendir(dir.as_zstr());
	defer if (directory) posix::closedir(directory);
	if (!directory) return path::is_dir(dir) ? IoError.CANNOT_READ_DIR? : IoError.FILE_NOT_DIR?;
	Posix_dirent* entry;
	while ((entry = posix::readdir(directory)))
	{
		@pool()
		{
			String name = ((ZString)&entry.name).as_str();
            if (!name || name == "." || name == "..") continue;
            Path new_path = dir.tappend(name)!;
            if (entry.d_type == posix::DT_DIR)
            {
            	native_rmtree(new_path)!;
            	continue;
            }
            if (libc::remove(new_path.as_zstr()))
            {
            	// TODO improve
            	return IoError.GENERAL_ERROR?;
            }
		};
	}
	os::native_rmdir(dir)!;
}

module std::io::os @if(env::WIN32);

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
