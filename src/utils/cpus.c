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

// Host information for the compiler statistics, NULL or 0 when unknown.
static char host_buffer[256];

#if PLATFORM_WINDOWS
const char *host_os_name(void)
{
	return "Windows";
}

const char *host_cpu_name(void)
{
	return NULL;
}

int64_t host_memory(void)
{
	MEMORYSTATUSEX status = { .dwLength = sizeof(status) };
	return GlobalMemoryStatusEx(&status) ? (int64_t)status.ullTotalPhys : 0;
}
#else
#include <sys/utsname.h>
#if __APPLE__
#include <sys/sysctl.h>
#endif

const char *host_os_name(void)
{
	struct utsname name;
	if (uname(&name)) return NULL;
	snprintf(host_buffer, sizeof(host_buffer), "%s %s %s", name.sysname, name.release, name.machine);
	return host_buffer;
}

const char *host_cpu_name(void)
{
#if __APPLE__
	size_t len = sizeof(host_buffer);
	if (sysctlbyname("machdep.cpu.brand_string", host_buffer, &len, NULL, 0)) return NULL;
	return host_buffer;
#elif defined(__linux__)
	FILE *file = fopen("/proc/cpuinfo", "r");
	if (!file) return NULL;
	const char *result = NULL;
	char line[256];
	while (fgets(line, sizeof(line), file))
	{
		if (strncmp(line, "model name", 10) != 0) continue;
		char *value = strchr(line, ':');
		if (!value) break;
		value += strspn(value + 1, " \t") + 1;
		value[strcspn(value, "\n")] = '\0';
		snprintf(host_buffer, sizeof(host_buffer), "%s", value);
		result = host_buffer;
		break;
	}
	fclose(file);
	return result;
#else
	return NULL;
#endif
}

int64_t host_memory(void)
{
#if __APPLE__
	uint64_t memory = 0;
	size_t len = sizeof(memory);
	return sysctlbyname("hw.memsize", &memory, &len, NULL, 0) ? 0 : (int64_t)memory;
#elif defined(_SC_PHYS_PAGES) && defined(_SC_PAGESIZE)
	long pages = sysconf(_SC_PHYS_PAGES);
	long page_size = sysconf(_SC_PAGESIZE);
	return pages > 0 && page_size > 0 ? (int64_t)pages * (int64_t)page_size : 0;
#else
	return 0;
#endif
}
#endif
