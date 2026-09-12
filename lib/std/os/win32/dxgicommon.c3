module std::os::win32 @feat(WIN32);

struct Win32_DXGI_RATIONAL
{
	Win32_UINT numerator;
	Win32_UINT denominator;
}

struct Win32_DXGI_SAMPLE_DESC
{
	Win32_UINT count;
	Win32_UINT quality;
}

alias Win32_DXGI_USAGE = Win32_UINT;