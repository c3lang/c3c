module std::os::win32 @feat(WIN32) @link("winmm");

alias Win32_MMRESULT = Win32_UINT;

extern fn Win32_MMRESULT timeBeginPeriod(Win32_UINT uPeriod);
extern fn Win32_MMRESULT timeEndPeriod(Win32_UINT uPeriod);

