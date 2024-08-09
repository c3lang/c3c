module std::time::os @if(env::WIN32);
import std::os::win32;
import std::math;

extern fn void win32_GetSystemTimeAsFileTime(Win32_FILETIME* time) @extern("GetSystemTimeAsFileTime");
extern fn Win32_BOOL win32_QueryPerformanceFrequency(Win32_LARGE_INTEGER* lpFrequency) @extern("QueryPerformanceFrequency");
extern fn Win32_BOOL win32_QueryPerformanceCounter(Win32_LARGE_INTEGER* lpPerformanceCount) @extern("QueryPerformanceCounter");

const ulong WINDOWS_TICK_US @local = 10;
const ulong WIN_TO_UNIX_EPOCH_US @local = 116444736000000000u64 / WINDOWS_TICK_US;


fn Clock native_clock()
{
	static Win32_LARGE_INTEGER freq;
	static ulong div = 0;
	ulong mult = 0;
	if (!freq.quadPart)
	{
		if (!win32_QueryPerformanceFrequency(&freq)) return 0;
	}
	Win32_LARGE_INTEGER counter @noinit;
	if (!win32_QueryPerformanceCounter(&counter)) return 0;
	return (Clock)counter.quadPart.muldiv(1_000_000_000, freq.quadPart);
}

fn Time native_timestamp()
{
	Win32_FILETIME ft @noinit;
	win32_GetSystemTimeAsFileTime(&ft);
	ulong result = (ulong)ft.dwHighDateTime << 32 | ft.dwLowDateTime;
	return (Time)(result / WINDOWS_TICK_US - WIN_TO_UNIX_EPOCH_US);
}