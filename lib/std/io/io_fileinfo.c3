module std::io::file;
import libc;

fn bool is_file(String path)
{
	return os::native_is_file(path);
}

fn usz! get_size(String path)
{
	return os::native_file_size(path);
}