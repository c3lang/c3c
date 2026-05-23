module std::io::os;
import libc, std::os;

const MAX_CWD = 64 * 1024;
macro String? getcwd(Allocator allocator)
{
	$switch:
		$case env::WIN32:
			@stack_mem(540; smem)
			{
				int size = 256;
				Char16[] buffer = alloc::alloc_array_try(smem, Char16, size)!;
				WString res @noinit;
				while ((res = win32::_wgetcwd(buffer, size)) == null)
				{
					// Improve error
					if (libc::errno() != ERANGE || size >= MAX_CWD) return io::GENERAL_ERROR~;
					size *= 2;
					buffer = alloc::alloc_array_try(smem, Char16, size)!;
				}
				Char16[] str16 = res[:win32::wcslen(res)];
				return string::from_utf16(allocator, str16);
			};
		$case env::POSIX:
			@stack_mem(280; smem)
			{
				sz size = 256;
				char[] buffer = alloc::alloc_array_try(smem, char, size)!;
				ZString res;
				while ((res = posix::getcwd(buffer, (usz)size)) == null)
				{
					// Improve error
					if (libc::errno() != ERANGE || size >= MAX_CWD) return io::GENERAL_ERROR~;
					size *= 2;
					buffer = alloc::alloc_array_try(smem, char, size)!;
				}
				return res.copy(allocator);
			};
		$default:
			return io::UNSUPPORTED_OPERATION~;
	$endswitch
}

