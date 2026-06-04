module std::os::process @if(env::WIN32 || env::POSIX);
import std::io, libc, std::os, std::net::os;

// This code is based on https://github.com/sheredom/subprocess.h

faultdef
	FAILED_TO_CREATE_PIPE,
	FAILED_TO_OPEN_STDIN,
	FAILED_TO_OPEN_STDOUT,
	FAILED_TO_OPEN_STDERR,
	FAILED_TO_BIND_STDIN,
	FAILED_TO_BIND_STDOUT,
	FAILED_TO_BIND_STDERR,
	FAILED_TO_START_PROCESS,
	FAILED_TO_INITIALIZE_ACTIONS,
	PROCESS_JOIN_FAILED,
	PROCESS_TERMINATION_FAILED,
	READ_FAILED;

struct Process
{
	CFile stdin_file;
	CFile stdout_file;
	CFile stderr_file;
	
	Win32_HANDLE hProcess @if(env::WIN32);
	Win32_HANDLE hStdInput @if(env::WIN32);
	Win32_HANDLE hEventOutput @if(env::WIN32);
	Win32_HANDLE hEventError @if(env::WIN32);

	Pid_t child @if(!env::WIN32);
	int return_status @if(env::POSIX);

	bool is_alive;
}

constdef SpawnOptions
{
	STDERR_TO_STDOUT    = 1 << 0,
	INHERIT_ENV         = 1 << 1,
	ASYNC_OUTPUT        = 1 << 2,
	NO_WINDOW           = 1 << 3,
	NO_PATH_SEARCH      = 1 << 4,
	INHERIT_STDIO       = 1 << 5
}


alias RawFd = env::WIN32 ? Win32_HANDLE : CInt;

enum ProcessStdioAction
{
	DEFAULT,
	CAPTURE,
	INHERIT,
	BIND
}

struct ProcessStdio
{
	ProcessStdioAction action;
	RawFd fd;
}

const ProcessStdio CAPTURE = { .action = CAPTURE };
const ProcessStdio INHERIT = { .action = INHERIT };
const ProcessStdio DEFAULT = { .action = DEFAULT };
macro ProcessStdio bind(RawFd fd) => { .action = BIND, .fd = fd };

fn String? run_capture_stdout(char[] buffer, String[] command_line, SpawnOptions options = {}, String[] environment = {})
{
	Process process = process::spawn(command_line, options, environment)!;
	defer (void)process.destroy();
	process.join()!;
	sz len = process.read_stdout(buffer.ptr, buffer.len)!;
	if (len == 0) return "";
	return (String)buffer[:len];
}

fn int? run(String[] command_line, SpawnOptions options = {}, String[] environment = {},
            ProcessStdio in = DEFAULT, ProcessStdio out = DEFAULT, ProcessStdio err = DEFAULT)
{
	Process process = process::spawn(command_line, options, environment, in, out, err)!;
	defer (void)process.destroy();
	return process.join()!;
}


<*
 @require !environment || !(options & INHERIT_ENV)
*>
fn Process? spawn(String[] command_line, SpawnOptions options = {}, String[] environment = {},
                  ProcessStdio in = DEFAULT, ProcessStdio out = DEFAULT, ProcessStdio err = DEFAULT) @if(env::WIN32 || env::POSIX)
{
	update_default(&in, &out, &err, !!(options & INHERIT_STDIO));

$if env::WIN32:
	Win32_DWORD flags = win32::CREATE_UNICODE_ENVIRONMENT;
	Win32_PROCESS_INFORMATION process_info;
	Win32_SECURITY_ATTRIBUTES sa_attr = { Win32_SECURITY_ATTRIBUTES::size, null, 1 };
	Win32_STARTUPINFOW start_info = {
		.cb = Win32_STARTUPINFOW::size,
		.dwFlags = win32::STARTF_USESTDHANDLES
	};

	if (options & NO_WINDOW) flags |= win32::CREATE_NO_WINDOW;


	CFile stdin, stdout, stderr;
	void* rd, wr;
	void* event_output, event_error;

	switch (in.action)
	{
		case DEFAULT:
			unreachable();
		case CAPTURE:
			if (!win32::createPipe(&rd, &wr, &sa_attr, 0)) return FAILED_TO_CREATE_PIPE~;
			if (!win32::setHandleInformation(wr, win32::HANDLE_FLAG_INHERIT, 0)) return FAILED_TO_CREATE_PIPE~;

			int fd = win32::_open_osfhandle((iptr)wr, 0);
			if (fd != -1)
			{
				stdin = win32::_fdopen(fd, "wb");
				if (!stdin) return FAILED_TO_OPEN_STDIN~;
			}
			start_info.hStdInput = rd;
		case BIND:
			start_info.hStdInput = in.fd;
		case INHERIT:
			start_info.hStdInput = win32::getStdHandle(win32::STD_INPUT_HANDLE);
	}

	switch (out.action)
	{
		case DEFAULT:
			unreachable();
		case CAPTURE:
			if (options & ASYNC_OUTPUT)
			{
				create_named_pipe_helper(&rd, &wr)!;
				event_output = win32::createEventA(&sa_attr, 1, 1, null);
			}
			else
			{
				if (!win32::createPipe(&rd, &wr, &sa_attr, 0)) return FAILED_TO_CREATE_PIPE~;
			}
			if (!win32::setHandleInformation(rd, win32::HANDLE_FLAG_INHERIT, 0)) return FAILED_TO_CREATE_PIPE~;
		
			int fd = win32::_open_osfhandle((iptr)rd, 0);
			if (fd != -1)
			{
				stdout = win32::_fdopen(fd, "rb");
				if (!stdout) return FAILED_TO_OPEN_STDOUT~;
			}

			start_info.hStdOutput = wr;
		case BIND:
			start_info.hStdOutput = out.fd;
		case INHERIT:
			start_info.hStdOutput = win32::getStdHandle(win32::STD_OUTPUT_HANDLE);
	}

	if (options & STDERR_TO_STDOUT)
	{
		stderr = stdout;
		start_info.hStdError = start_info.hStdOutput;
	}
	else
	{
		switch (err.action)
		{
			case DEFAULT:
				unreachable();
			case CAPTURE:
				if (options & ASYNC_OUTPUT)
				{
					create_named_pipe_helper(&rd, &wr)!;
					event_error = win32::createEventA(&sa_attr, 1, 1, null);
				}
				else
				{
					if (!win32::createPipe(&rd, &wr, &sa_attr, 0)) return FAILED_TO_CREATE_PIPE~;
				}
				if (!win32::setHandleInformation(rd, win32::HANDLE_FLAG_INHERIT, 0)) return FAILED_TO_CREATE_PIPE~;

				int fd = win32::_open_osfhandle((iptr)rd, 0);
				if (fd != -1)
				{
					stderr = win32::_fdopen(fd, "rb");
					if (!stderr) return FAILED_TO_OPEN_STDERR~;
				}
				start_info.hStdError = wr;
			case BIND:
				start_info.hStdError = err.fd;
			case INHERIT:
				start_info.hStdError = win32::getStdHandle(win32::STD_ERROR_HANDLE);
		}
	}

	@stack_mem(2048; Allocator smem)
	{
		WString used_environment = null;
		if (!(options & INHERIT_ENV))
		{
			DString env = dstring::new_with_capacity(smem, 64);
			if (!environment.len)
			{
				env.append("\0");
			}
			foreach (String s : environment)
			{
				env.append(s);
				env.append("\0");
			}
			env.append("\0");
			used_environment = env.str_view().to_wstring(smem)!;
		}
	
		if (!win32::createProcessW(
			null,
			convert_command_line_win32(command_line),
			null,                   // process security attributes
			null,                   // primary thread security attributes
			1,                      // handles are inherited
			flags,                  // creation flags
			used_environment,       // environment
			null,                   // use parent dir
			&start_info,            // startup info ptr
			&process_info)) return FAILED_TO_START_PROCESS~;
	};

	// We don't need the handle of the primary thread in the called process.
	win32::closeHandle(process_info.hThread);

	if (out.action == CAPTURE) win32::closeHandle(start_info.hStdOutput);
	if (err.action == CAPTURE && start_info.hStdOutput != start_info.hStdError)
	{
		win32::closeHandle(start_info.hStdError);
	}

		
	return {
		.hProcess = process_info.hProcess,
		.hStdInput = in.action == CAPTURE ? start_info.hStdInput : null,
		.hEventOutput = event_output,
		.hEventError = event_error,
		.stdin_file = stdin,
		.stdout_file = stdout,
		.stderr_file = stderr,
		.is_alive = true,
	};
$else
	Posix_spawn_file_actions_t actions;
	if (posix::spawn_file_actions_init(&actions)) return FAILED_TO_INITIALIZE_ACTIONS~;
	defer posix::spawn_file_actions_destroy(&actions);

	// We should set FD_CLOEXEC to prevent leaking pipe handles between concurrent spawns
	const int F_SETFD = 2;
	const int FD_CLOEXEC = 1;

	CInt[2] stdinfd, stdoutfd, stderrfd;
	CFile stdin, stdout, stderr;

	switch (in.action)
	{
		case DEFAULT:
			unreachable();
		case CAPTURE:
			if (posix::pipe(&stdinfd)) return FAILED_TO_OPEN_STDIN~;
			os::fcntl((NativeSocket)stdinfd[0], F_SETFD, FD_CLOEXEC);
			os::fcntl((NativeSocket)stdinfd[1], F_SETFD, FD_CLOEXEC);
			if (posix::spawn_file_actions_adddup2(&actions, stdinfd[0], libc::STDIN_FD)) return FAILED_TO_OPEN_STDIN~;
		case BIND:
			if (posix::spawn_file_actions_adddup2(&actions, in.fd, libc::STDIN_FD)) return FAILED_TO_BIND_STDIN~;
		case INHERIT:
			break;
	}

	switch (out.action)
	{
		case DEFAULT:
			unreachable();
		case CAPTURE:
			if (posix::pipe(&stdoutfd)) return FAILED_TO_OPEN_STDOUT~;
			os::fcntl((NativeSocket)stdoutfd[0], F_SETFD, FD_CLOEXEC);
			os::fcntl((NativeSocket)stdoutfd[1], F_SETFD, FD_CLOEXEC);

			if (options & ASYNC_OUTPUT)
			{
				((NativeSocket)stdoutfd[0]).set_non_blocking(true)!;
				((NativeSocket)stdoutfd[1]).set_non_blocking(true)!;
			}
			if (posix::spawn_file_actions_adddup2(&actions, stdoutfd[1], libc::STDOUT_FD)) return FAILED_TO_OPEN_STDOUT~;
		case BIND:
			if (posix::spawn_file_actions_adddup2(&actions, out.fd, libc::STDOUT_FD)) return FAILED_TO_BIND_STDOUT~;
		case INHERIT:
			break;
	}

	if (options & STDERR_TO_STDOUT)
	{
		if (posix::spawn_file_actions_adddup2(&actions, libc::STDOUT_FD, libc::STDERR_FD)) return FAILED_TO_OPEN_STDERR~;
	}
	else
	{
		switch (err.action)
		{
			case DEFAULT:
				unreachable();
			case CAPTURE:
				if (posix::pipe(&stderrfd)) return FAILED_TO_OPEN_STDERR~;
				os::fcntl((NativeSocket)stderrfd[0], F_SETFD, FD_CLOEXEC);
				os::fcntl((NativeSocket)stderrfd[1], F_SETFD, FD_CLOEXEC);

				if (options & ASYNC_OUTPUT)
				{
					((NativeSocket)stderrfd[0]).set_non_blocking(true)!;
					((NativeSocket)stderrfd[1]).set_non_blocking(true)!;
				}
				if (posix::spawn_file_actions_adddup2(&actions, stderrfd[1], libc::STDERR_FD)) return FAILED_TO_OPEN_STDERR~;
			case BIND:
				if (posix::spawn_file_actions_adddup2(&actions, err.fd, libc::STDERR_FD)) return FAILED_TO_BIND_STDERR~;
			case INHERIT:
				break;
		}
	}

	Pid_t child;
	@stack_mem(2048; Allocator mem)
	{
		ZString* command_line_copy = copy_command_line(mem, command_line);
		ZString* used_environment = options & INHERIT_ENV ? posix::environ : copy_env(mem, environment);
		if (options & NO_PATH_SEARCH)
		{
			if (posix::spawn(&child, command_line_copy[0], &actions, null, command_line_copy, used_environment)) return FAILED_TO_START_PROCESS~;
		}
		else
		{
			if (posix::spawnp(&child, command_line_copy[0], &actions, null, command_line_copy, used_environment)) return FAILED_TO_START_PROCESS~;
		}
	};

	// Only set up file handles for when stdio is captured
	if (in.action  == CAPTURE)
	{
		libc::close(stdinfd[0]);
		stdin = libc::fdopen(stdinfd[1],  "wb");
	}
	if (out.action == CAPTURE)
	{
		libc::close(stdoutfd[1]);
		stdout = libc::fdopen(stdoutfd[0], "rb");
	}
	if (err.action == CAPTURE)
	{
		if (options & STDERR_TO_STDOUT)
		{
			stderr = stdout;
		}
		else
		{
			libc::close(stderrfd[1]);
			stderr = libc::fdopen(stderrfd[0], "rb");
		}
	}


	return {
		.stdin_file = stdin,
		.stdout_file = stdout,
		.stderr_file = stderr,
		.child = child,
		.is_alive = true,
	};
$endif
}





fn CInt? Process.join(&self) @if(env::POSIX)
{
	if (self.stdin_file)
	{
		libc::fclose(self.stdin_file);
		self.stdin_file = null;
	}
	CInt status;
	if (self.child && self.child != posix::waitpid(self.child, &status, 0)) return PROCESS_JOIN_FAILED~;

	self.child = 0;
	self.is_alive = false;

	return self.return_status = posix::wIFEXITED(status) ? posix::wEXITSTATUS(status) : libc::EXIT_FAILURE;
}

fn File Process.stdout(&self)
{
	if (!self.stdout_file) return (File){}; // Return an empty File struct
	return file::from_handle(self.stdout_file);
}

fn File Process.stderr(&self)
{
	if (!self.stderr_file) return (File){}; // Return an empty File struct
	return file::from_handle(self.stderr_file);
}

fn CInt? Process.join(&self) @if(env::WIN32)
{
	if (self.stdin_file)
	{
		libc::fclose(self.stdin_file);
		self.stdin_file = null;
	}
	if (self.hStdInput)
	{
		win32::closeHandle(self.hStdInput);
		self.hStdInput = null;
	}
	win32::waitForSingleObject(self.hProcess, win32::INFINITE);
	Win32_DWORD return_code @noinit;
	if (!win32::getExitCodeProcess(self.hProcess, &return_code)) return PROCESS_JOIN_FAILED~;
	self.is_alive = false;
	return (CInt)return_code;
}

fn bool Process.destroy(&self)
{
	if (self.stdin_file) libc::fclose(self.stdin_file);
	if (self.stdout_file) libc::fclose(self.stdout_file);
	if (self.stderr_file && self.stderr_file != self.stdout_file) libc::fclose(self.stderr_file);
	self.stdin_file = self.stdout_file = self.stderr_file = null;
	$if env::WIN32:
		if (self.hProcess) win32::closeHandle(self.hProcess);
		if (self.hStdInput) win32::closeHandle(self.hStdInput);
		if (self.hEventOutput) win32::closeHandle(self.hEventOutput);
		if (self.hEventError) win32::closeHandle(self.hEventError);
		self.hProcess = self.hStdInput = self.hEventOutput = self.hEventError = null;
	$endif;
	return true;
}

fn void? Process.terminate(&self)
{
	$if env::WIN32:
		if (!win32::terminateProcess(self.hProcess, 99)) return PROCESS_TERMINATION_FAILED~;
	$else
		if (posix::kill(self.child, 9)) return PROCESS_TERMINATION_FAILED~;
	$endif
}

<*
 @require size <= Win32_DWORD::max
*>
fn sz? read_from_file_win32(CFile file, Win32_HANDLE event_handle, char* buffer, sz size) @if(env::WIN32) @local
{
	CInt fd = libc::fileno(file);
	Win32_DWORD bytes_read = 0;
	Win32_OVERLAPPED overlapped = { .hEvent = event_handle };
	Win32_HANDLE handle = (Win32_HANDLE)win32::_get_osfhandle(fd);
	if (!win32::readFile(handle, buffer, (Win32_DWORD)size, &bytes_read, &overlapped))
	{
		// Means we've got an async read!
		if (win32::getLastError() == win32::ERROR_IO_PENDING)
		{
			if (!win32::getOverlappedResult(handle, &overlapped, &bytes_read, 1))
			{
				switch (win32::getLastError())
				{
					case win32::ERROR_IO_INCOMPLETE:
					case win32::ERROR_HANDLE_EOF:
						break;
					default:
						return READ_FAILED~;
				}
			}
		}
	}
	return bytes_read;
}

fn sz? read_from_file_posix(CFile file, char* buffer, sz size) @if(env::POSIX) @local
{
	sz bytes_read = libc::read(libc::fileno(file), buffer, (usz)size);
	if (bytes_read < 0)
	{
		if (libc::errno() == EAGAIN) return 0;   // nothing to read but O_NONBLOCK (async) is used
		return READ_FAILED~;
	}
	return bytes_read;
}

fn sz? Process.read_stdout(&self, char* buffer, sz size)
{
	if (!self.stdout_file) return 0; // No output available when inheriting stdio

	$if env::WIN32:
		return read_from_file_win32(self.stdout_file, self.hEventOutput, buffer, size);
	$else
		return read_from_file_posix(self.stdout_file, buffer, size);
	$endif
}


fn sz? Process.read_stderr(&self, char* buffer, sz size)
{
	if (!self.stderr_file) return 0; // No output available when inheriting stdio

	$if env::WIN32:
		return read_from_file_win32(self.stderr_file, self.hEventError, buffer, size);
	$else
		return read_from_file_posix(self.stderr_file, buffer, size);
	$endif
}

fn bool? Process.is_running(&self)
{
	if (!self.is_alive) return false;
	$if env::WIN32:
		bool is_alive = win32::waitForSingleObject(self.hProcess, 0) != win32::WAIT_OBJECT_0;
		if (!is_alive) self.is_alive = false;
		return is_alive;
	$else
		CInt status;
		bool is_alive = posix::waitpid(self.child, &status, posix::WNOHANG) == 0;
		if (is_alive) return true;
		self.is_alive = false;
		self.return_status = posix::wIFEXITED(status) ? posix::wEXITSTATUS(status) : libc::EXIT_FAILURE;
		self.child = 0;
		self.join()!;
		return false;
	$endif
}

fn sz? Process.write_stdin(&self, char[] buffer)
{
	if (!self.stdin_file) return FAILED_TO_OPEN_STDIN~;
	defer try (void)os::native_fflush(self.stdin_file);
	return os::native_fwrite(self.stdin_file, buffer);
}
fn void? create_named_pipe_helper(void** rd, void **wr) @local @if(env::WIN32)
{
	Win32_SECURITY_ATTRIBUTES sa_attr = { Win32_SECURITY_ATTRIBUTES::size, null, 1 };

	tlocal long index = 0;
	long unique = index++;
	String s = string::bformat(&&(char[128]){}, `\\.\pipe\c3_process.%08x.%08x.%d`, win32::getCurrentProcessId(), win32::getCurrentThreadId(), unique);
	Win32_LPCSTR str = (Win32_LPCSTR)s.ptr;
	*rd = win32::createNamedPipeA(
		str,
		win32::PIPE_ACCESS_INBOUND | win32::FILE_FLAG_OVERLAPPED,
		win32::PIPE_TYPE_BYTE | win32::PIPE_WAIT,
		1, 4096, 4096, 0, &sa_attr);
	if (win32::INVALID_HANDLE_VALUE == *rd) return FAILED_TO_CREATE_PIPE~;
	*wr = win32::createFileA(
		str, win32::GENERIC_WRITE, 0, &sa_attr,
		win32::OPEN_EXISTING, win32::FILE_ATTRIBUTE_NORMAL, null);
	if (win32::INVALID_HANDLE_VALUE == *wr) return FAILED_TO_CREATE_PIPE~;
}

fn WString convert_command_line_win32(String[] command_line) @inline @if(env::WIN32) @local
{
	DString str = dstring::temp_with_capacity(512);
	foreach LINE: (i, s : command_line)
	{
		if (i != 0) str.append(' ');

		do CHECK_WS:
		{
			foreach (c : s)
			{
				switch (c)
				{
					case '\t':
					case ' ':
					case '\v':
						break CHECK_WS;
				}
			}
			str.append(s);
			continue LINE;
		};
		str.append('"');
		foreach (j, c : s)
		{
			switch (c)
			{
				case '\\':
					if (j != s.len - 1 && s[j + 1] == '"') str.append('\\');
				case '"':
					str.append('\\');
			}
			str.append(c);
		}
		str.append('"');
	}
	str.append('\0');
	return str.str_view().to_temp_wstring()!!;
}

<*
 @require command_line.len > 0
*>
fn ZString* copy_command_line(Allocator mem, String[] command_line) @local @inline @if(env::POSIX)
{
	ZString* copy = alloc::alloc_array(mem, ZString, command_line.len + 1);
	foreach (i, str : command_line)
	{
		copy[i] = str.zstr_copy(mem);
	}
	copy[command_line.len] = null;
	return copy;
}

const ZString[1] EMPTY_ENVIRONMENT @if(env::POSIX) = { null };
fn ZString* copy_env(Allocator mem, String[] environment) @local @inline @if(env::POSIX)
{
	if (!environment.len) return &EMPTY_ENVIRONMENT;
	ZString* copy = alloc::alloc_array(mem, ZString, environment.len + 1);
	copy[environment.len] = null;
	foreach (i, str : environment)
	{
		copy[i] = str.zstr_copy(mem);
	}
	return copy;
}

macro update_default(ProcessStdio* in, ProcessStdio* out, ProcessStdio* err, bool inherit_by_default) @local
{
	ProcessStdioAction action = inherit_by_default ? INHERIT : CAPTURE;
	if (in.action == DEFAULT) in.action = action;
	if (out.action == DEFAULT) out.action = action;
	if (err.action == DEFAULT) err.action = action;
}