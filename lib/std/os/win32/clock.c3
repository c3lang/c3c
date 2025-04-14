module std::os::win32 @if(env::WIN32);
import std::math;

extern fn void getSystemTimeAsFileTime(Win32_FILETIME* time) @extern("GetSystemTimeAsFileTime");
extern fn Win32_BOOL queryPerformanceFrequency(Win32_LARGE_INTEGER* lpFrequency) @extern("QueryPerformanceFrequency");
extern fn Win32_BOOL queryPerformanceCounter(Win32_LARGE_INTEGER* lpPerformanceCount) @extern("QueryPerformanceCounter");
