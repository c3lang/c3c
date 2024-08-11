#include "../utils/common.h"
#include "../utils/lib.h"
#include <errno.h>
#include "subprocess.h"

#if PLATFORM_POSIX
#include <sys/wait.h>
#endif

#if PLATFORM_WINDOWS
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

int run_subprocess(const char *name, const char **args)
{
#if PLATFORM_WINDOWS
	// https://docs.microsoft.com/en-us/windows/win32/procthread/creating-a-child-process-with-redirected-input-and-output

	STARTUPINFOA siStartInfo;
	ZeroMemory(&siStartInfo, sizeof(siStartInfo));
	siStartInfo.cb = sizeof(STARTUPINFO);
	// NOTE: theoretically setting NULL to std handles should not be a problem
	// https://docs.microsoft.com/en-us/windows/console/getstdhandle?redirectedfrom=MSDN#attachdetach-behavior
	// TODO: check for errors in GetStdHandle
	siStartInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);
	siStartInfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
	siStartInfo.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
	siStartInfo.dwFlags |= STARTF_USESTDHANDLES;

	PROCESS_INFORMATION piProcInfo;
	ZeroMemory(&piProcInfo, sizeof(PROCESS_INFORMATION));

	scratch_buffer_clear();
	scratch_buffer_printf("%s", name);
	for (uint32_t i = 0; i < vec_size(args); i++)
	{
		scratch_buffer_append_char(' ');

		bool need_quoting = strpbrk(args[i], "\t\v ") != NULL || args[i][0] == 0;
		if (need_quoting) scratch_buffer_append_char('"');
		for (int j = 0; '\0' != args[i][j]; j++)
		{
			switch (args[i][j])
			{
				default:
					break;
				case '\\':
					if (args[i][j + 1] == '"')
					{
						scratch_buffer_append_char('\\');
					}

					break;
				case '"':
					scratch_buffer_append_char('\\');
					break;
			}

			scratch_buffer_append_char(args[i][j]);
		}

		if (need_quoting) scratch_buffer_append_char('"');
	}

	BOOL bSuccess = CreateProcessA(
		NULL,
		scratch_buffer_to_string(),
		NULL,
		NULL,
		TRUE,
		0,
		NULL,
		NULL,
		&siStartInfo,
		&piProcInfo);

	if (!bSuccess)
	{
		eprintf("Could not create child process: %lu", GetLastError());
		return -1;
	}

	CloseHandle(piProcInfo.hThread);

	DWORD result = WaitForSingleObject(
			piProcInfo.hProcess, // HANDLE hHandle,
			INFINITE // DWORD  dwMilliseconds
	);

	if (result == WAIT_FAILED)
	{
		eprintf("Could not wait on child process: %lu", GetLastError());
		return -1;
	}

	DWORD exit_status;
	if (!GetExitCodeProcess(piProcInfo.hProcess, &exit_status))
	{
		eprintf("Could not get process exit code: %lu", GetLastError());
		return -1;
	}

	CloseHandle(piProcInfo.hProcess);

	return exit_status;
#else
	pid_t cpid = fork();
	if (cpid < 0)
	{
		eprintf("Could not fork child process %s: %s\n", name, strerror(errno));
		return -1;
	}

	if (cpid == 0)
	{
		const char **args_null = NULL;
		vec_add(args_null, name);
		for (uint32_t i = 0; i < vec_size(args); ++i)
		{
			vec_add(args_null, args[i]);
		}
		vec_add(args_null, NULL);
		if (execvp(name, (char *const *)args_null) < 0)
		{
			eprintf("Could not exec child process %s: %s\n", name, strerror(errno));
			exit(1);
		}
		exit(0);
	}

	for (;;)
	{
		int wstatus = 0;
		if (waitpid(cpid, &wstatus, 0) < 0)
		{
			if (errno != EINTR)
			{
				eprintf("Could not wait on %s (pid %d): %s\n", name, cpid, strerror(errno));
				return -1;
			}
			continue;
		}

		if (WIFEXITED(wstatus)) return WEXITSTATUS(wstatus);

		if (WIFSIGNALED(wstatus))
		{
			eprintf("Program interrupted by signal %d.\n", WTERMSIG(wstatus));
			return -1;
		}
	}

	return cpid;
#endif
}
