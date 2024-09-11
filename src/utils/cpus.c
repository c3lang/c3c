#include "common.h"

#if PLATFORM_WINDOWS
#include <windows.h>
int cpus(void)
{
	SYSTEM_INFO sysinfo;
	GetSystemInfo(&sysinfo);
	return sysinfo.dwNumberOfProcessors;
}
#elif __APPLE__ || defined(__linux__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
#include <unistd.h>
int cpus(void)
{
	return sysconf(_SC_NPROCESSORS_ONLN);
}
#else
int cpus(void)
{
	REMINDER("Implement processor count for OS");
	return 1;
}
#endif
