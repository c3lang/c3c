module std::os::process @feat(WIN32, POSIX);
import std::io, libc, std::os, std::net::os;

// This code is based on https://github.com/sheredom/subprocess.h

excuse
{
	FAILED_TO_CREATE_PIPE,
	FAILED_TO_OPEN_STDIN,
	FAILED_TO_OPEN_STDOUT,
	FAILED_TO_OPEN_STDERR,
	FAILED_TO_BIND_STDIN,
	FAILED_TO_BIND_STDOUT,
	FAILED_TO_BIND_STDERR,
	FAILED_TO_START_PROCESS,
	FAILED_TO_INITIALIZE_ACTIONS,
	FAILED_TO_SET_WORKING_DIRECTORY,
	PROCESS_JOIN_FAILED,
	PROCESS_TERMINATION_FAILED,
	READ_FAILED
}
struct Process
{
	CFile stdin_file;
	CFile stdout_file;
	CFile stderr_file;
	
	Win32_HANDLE hProcess @feat(WIN32);
	Win32_HANDLE hStdInput @feat(WIN32);
	Win32_HANDLE hEventOutput @feat(WIN32);
	Win32_HANDLE hEventError @feat(WIN32);

	Pid_t child @feat(!WIN32);
	int return_status @feat(POSIX);

	bool is_alive;
}

constset SpawnOptions
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

fn String? run_capture_stdout(char[] buffer, String[] command_line, SpawnOptions options = {}, String[] environment = {},
            String working_directory = "")
{
	Process process = process::spawn(command_line, options, environment, DEFAULT, DEFAULT, DEFAULT, working_directory)!;
	defer (void)process.destroy();
	process.join()!;
	sz len = process.read_stdout(buffer.ptr, buffer.len)!;
	if (len == 0) return "";
	return (String)buffer[:len];
}

fn int? run(String[] command_line, SpawnOptions options = {}, String[] environment = {},
            ProcessStdio in = DEFAULT, ProcessStdio out = DEFAULT, ProcessStdio err = DEFAULT,
            String working_directory = "")
{
	Process process = process::spawn(command_line, options, environment, in, out, err, working_directory)!;
	defer (void)process.destroy();
	return process.join()!;
}


<*
 @require !environment || !(options & INHERIT_ENV)
*>
fn Process? spawn(String[] command_line, SpawnOptions options = {}, String[] environment = {},
                  ProcessStdio in = DEFAULT, ProcessStdio out = DEFAULT, ProcessStdio err = DEFAULT,
                  String working_directory = "") @feat(WIN32, POSIX)
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
		WString cwd2 = working_directory.len ? working_directory.to_wstring(smem)! : null;
	
		if (!win32::createProcessW(
			null,
			convert_command_line_win32(command_line),
			null,                   // process security attributes
			null,                   // primary thread security attributes
			1,                      // handles are inherited
			flags,                  // creation flags
			used_environment,       // environment
			cwd2,                    // null uses the parent's dir
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
	// We should set FD_CLOEXEC to prevent leaking pipe handles between concurrent spawns
	const int F_SETFD = 2;
	const int FD_CLOEXEC = 1;

	CInt[2] stdinfd = { -1, -1 };
	CInt[2] stdoutfd = { -1, -1 };
	CInt[2] stderrfd = { -1, -1 };
	CFile stdin, stdout, stderr;
	// Don't leak the pipes if we fail before handing them over.
	defer catch
	{
		foreach (fd : (CInt[6]){ stdinfd[0], stdinfd[1], stdoutfd[0], stdoutfd[1], stderrfd[0], stderrfd[1] })
		{
			if (fd >= 0) libc::close(fd);
		}
	}

	// What the child gets as stdin, stdout and stderr: BIND dups the fd, anything else inherits.
	ProcessStdio child_in = INHERIT;
	ProcessStdio child_out = INHERIT;
	ProcessStdio child_err = INHERIT;

	switch (in.action)
	{
		case DEFAULT:
			unreachable();
		case CAPTURE:
			if (posix::pipe(&stdinfd)) return FAILED_TO_OPEN_STDIN~;
			os::fcntl((NativeSocket)stdinfd[0], F_SETFD, FD_CLOEXEC);
			os::fcntl((NativeSocket)stdinfd[1], F_SETFD, FD_CLOEXEC);
			child_in = bind(stdinfd[0]);
		case BIND:
			child_in = in;
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
			child_out = bind(stdoutfd[1]);
		case BIND:
			child_out = out;
		case INHERIT:
			break;
	}

	if (options & STDERR_TO_STDOUT)
	{
		// Applied after stdout is bound, so stderr follows it.
		child_err = bind(libc::STDOUT_FD);
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
				child_err = bind(stderrfd[1]);
			case BIND:
				child_err = err;
			case INHERIT:
				break;
		}
	}

	Pid_t child;
	@stack_mem(2048; Allocator mem)
	{
		ZString* command_line_copy = copy_command_line(mem, command_line);
		ZString* used_environment = options & INHERIT_ENV ? posix::environ : copy_env(mem, environment);
		ZString cwd = working_directory.len ? working_directory.zstr_copy(mem) : null;
		bool path_search = !(options & NO_PATH_SEARCH);

		// Like libuv: prefer posix_spawn, but if a working directory is requested and
		// the libc has no posix_spawn_file_actions_addchdir, fall back to fork + chdir + exec.
		SpawnFileActionsAddchdirFn addchdir = cwd ? posix::spawn_file_actions_addchdir : null;
		if (cwd && !addchdir)
		{
			child = spawn_fork(command_line_copy, used_environment, cwd, path_search, child_in, child_out, child_err)!;
		}
		else
		{
			child = spawn_posix(command_line_copy, used_environment, cwd, addchdir, path_search, child_in, child_out, child_err)!;
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

fn Pid_t? spawn_posix(ZString* argv, ZString* envp, ZString cwd, SpawnFileActionsAddchdirFn addchdir, bool path_search,
                      ProcessStdio in = DEFAULT, ProcessStdio out = DEFAULT, ProcessStdio err = DEFAULT) @local @feat(POSIX)
{
	Posix_spawn_file_actions_t actions;
	if (posix::spawn_file_actions_init(&actions)) return FAILED_TO_INITIALIZE_ACTIONS~;
	defer posix::spawn_file_actions_destroy(&actions);

	// The path is copied into the actions.
	if (cwd && addchdir(&actions, cwd)) return FAILED_TO_SET_WORKING_DIRECTORY~;
	// Order matters: err may be bound to STDOUT_FD, so it must come after out.
	if (in.action == BIND && posix::spawn_file_actions_adddup2(&actions, in.fd, libc::STDIN_FD)) return FAILED_TO_BIND_STDIN~;
	if (out.action == BIND && posix::spawn_file_actions_adddup2(&actions, out.fd, libc::STDOUT_FD)) return FAILED_TO_BIND_STDOUT~;
	if (err.action == BIND && posix::spawn_file_actions_adddup2(&actions, err.fd, libc::STDERR_FD)) return FAILED_TO_BIND_STDERR~;

	Pid_t child;
	CInt result = path_search
		? posix::spawnp(&child, argv[0], &actions, null, argv, envp)
		: posix::spawn(&child, argv[0], &actions, null, argv, envp);
	if (result) return FAILED_TO_START_PROCESS~;
	return child;
}

// Which step failed in the forked child, sent back to the parent over the error pipe.
constset ForkStage : inline CInt @local @feat(POSIX)
{
	BIND_STDIN = 1,
	BIND_STDOUT = 2,
	BIND_STDERR = 3,
	CHDIR = 4,
	EXEC = 5,
}

fn Pid_t? spawn_fork(ZString* argv, ZString* envp, ZString cwd, bool path_search,
                     ProcessStdio in = DEFAULT, ProcessStdio out = DEFAULT, ProcessStdio err = DEFAULT) @local @feat(POSIX)
{
	const int F_SETFD = 2;
	const int FD_CLOEXEC = 1;

	CInt[2] error_pipe;
	if (posix::pipe(&error_pipe)) return FAILED_TO_START_PROCESS~;
	os::fcntl((NativeSocket)error_pipe[0], F_SETFD, FD_CLOEXEC);
	os::fcntl((NativeSocket)error_pipe[1], F_SETFD, FD_CLOEXEC);
	defer libc::close(error_pipe[0]);

	// Block signals so none of our handlers run in the child before it execs.
	Sigset_t all_signals, old_mask;
	libc::sigfillset(&all_signals);
	libc::sigprocmask(libc::SIG_SETMASK, &all_signals, &old_mask);

	Pid_t pid = posix::fork();
	if (pid == 0)
	{
		fork_child(argv, envp, cwd, path_search, &old_mask, error_pipe[1], in, out, err);
		libc::_exit(127);
	}

	libc::sigprocmask(libc::SIG_SETMASK, &old_mask, null);
	libc::close(error_pipe[1]);
	if (pid < 0) return FAILED_TO_START_PROCESS~;

	CInt stage;
	sz read;
	do
	{
		read = libc::read(error_pipe[0], &stage, CInt::size);
	} while (read < 0 && libc::errno() == EINTR);

	// EOF: the pipe was closed by a successful exec.
	if (read == 0) return pid;

	// The child failed before exec, reap it.
	CInt status;
	while (posix::waitpid(pid, &status, 0) < 0 && libc::errno() == EINTR) {}

	if (read != CInt::size) return FAILED_TO_START_PROCESS~;
	switch ((ForkStage)stage)
	{
		case BIND_STDIN: return FAILED_TO_BIND_STDIN~;
		case BIND_STDOUT: return FAILED_TO_BIND_STDOUT~;
		case BIND_STDERR: return FAILED_TO_BIND_STDERR~;
		case CHDIR: return FAILED_TO_SET_WORKING_DIRECTORY~;
		default: return FAILED_TO_START_PROCESS~;
	}
}

fn void fork_child(ZString* argv, ZString* envp, ZString cwd, bool path_search, Sigset_t* signal_mask, CInt error_fd,
                   ProcessStdio in = DEFAULT, ProcessStdio out = DEFAULT, ProcessStdio err = DEFAULT) @local @feat(POSIX)
{
	// Order matters: err may be bound to STDOUT_FD, so it must come after out.
	if (in.action == BIND && posix::dup2(in.fd, libc::STDIN_FD) < 0) return fork_child_fail(error_fd, BIND_STDIN);
	if (out.action == BIND && posix::dup2(out.fd, libc::STDOUT_FD) < 0) return fork_child_fail(error_fd, BIND_STDOUT);
	if (err.action == BIND && posix::dup2(err.fd, libc::STDERR_FD) < 0) return fork_child_fail(error_fd, BIND_STDERR);

	if (posix::chdir(cwd)) return fork_child_fail(error_fd, CHDIR);

	// Same as libuv: execvp searches the PATH of the child's environment.
	posix::environ = envp;
	libc::sigprocmask(libc::SIG_SETMASK, signal_mask, null);
	if (path_search)
	{
		posix::execvp(argv[0], argv);
	}
	else
	{
		posix::execv(argv[0], argv);
	}
	fork_child_fail(error_fd, EXEC);
}

fn void fork_child_fail(CInt error_fd, ForkStage stage) @local @feat(POSIX)
{
	CInt value = (CInt)stage;
	while (libc::write(error_fd, &value, CInt::size) < 0 && libc::errno() == EINTR) {}
	libc::_exit(127);
}

fn CInt? Process.join(&self) @feat(POSIX)
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

fn CInt? Process.join(&self) @feat(WIN32)
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
fn sz? read_from_file_win32(CFile file, Win32_HANDLE event_handle, char* buffer, sz size) @feat(WIN32) @local
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

fn sz? read_from_file_posix(CFile file, char* buffer, sz size) @feat(POSIX) @local
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
fn void? create_named_pipe_helper(void** rd, void **wr) @local @feat(WIN32)
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

fn WString convert_command_line_win32(String[] command_line) @inline @feat(WIN32) @local
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
fn ZString* copy_command_line(Allocator mem, String[] command_line) @local @inline @feat(POSIX)
{
	ZString* copy = alloc::alloc_array(mem, ZString, command_line.len + 1);
	foreach (i, str : command_line)
	{
		copy[i] = str.zstr_copy(mem);
	}
	copy[command_line.len] = null;
	return copy;
}

const ZString[1] EMPTY_ENVIRONMENT @feat(POSIX) = { null };
fn ZString* copy_env(Allocator mem, String[] environment) @local @inline @feat(POSIX)
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
