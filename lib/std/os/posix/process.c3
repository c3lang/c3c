module std::os::posix @if(env::POSIX);
import libc, std::os::darwin;

struct Posix_spawn_file_actions_t
{
	int __allocated;
	int __used;
	void* __actions;
	int[16] __pad;
}

struct Posix_spawnattr_t
{
	void*[42] opaque;
}

extern fn CInt posix_spawn_file_actions_init(Posix_spawn_file_actions_t *file_actions);
extern fn CInt posix_spawn_file_actions_destroy(Posix_spawn_file_actions_t *file_actions);
extern fn CInt posix_spawn_file_actions_addclose(Posix_spawn_file_actions_t *file_actions, CInt fd);
extern fn CInt posix_spawn_file_actions_adddup2(Posix_spawn_file_actions_t *file_actions, CInt fd, CInt newfd);

extern fn CInt get_nprocs();
extern fn CInt get_nprocs_conf();

alias spawn_file_actions_init = posix_spawn_file_actions_init;
alias spawn_file_actions_destroy = posix_spawn_file_actions_destroy;
alias spawn_file_actions_addclose = posix_spawn_file_actions_addclose;
alias spawn_file_actions_adddup2 = posix_spawn_file_actions_adddup2;

extern fn CInt posix_spawnp(Pid_t* pid, char* file, Posix_spawn_file_actions_t* file_actions,
	Posix_spawnattr_t* attrp, ZString* argv, ZString* envp);
extern fn CInt posix_spawn(Pid_t* pid, char* file, Posix_spawn_file_actions_t* file_actions,
	Posix_spawnattr_t* attrp, ZString* argv, ZString* envp);
alias spawnp = posix_spawnp;
alias spawn = posix_spawn;

extern fn CInt getpid();
extern fn CInt kill(Pid_t pid, CInt sig);
extern fn Pid_t waitpid(Pid_t pid, CInt* stat_loc, int options);
extern fn CInt raise(CInt sig);
extern fn ZString* backtrace_symbols(void** buffer, CInt size);
extern fn void backtrace_symbols_fd(void** buffer, CInt size, CInt fd);
macro CInt wEXITSTATUS(CInt status) => (status & 0xff00) >> 8;
macro CInt wTERMSIG(CInt status) => status & 0x7f;
macro CInt wSTOPSIG(CInt status) => wEXITSTATUS(status);
macro bool wIFEXITED(CInt status) => wTERMSIG(status) == 0;
macro bool wIFSIGNALED(CInt status) => ((ichar) ((status & 0x7f) + 1) >> 1) > 0;
macro bool wIFSTOPPED(CInt status) => (status & 0xff) == 0x7f;
macro bool wIFCONTINUED(CInt status) => status == __W_CONTINUED;
macro CInt wWCOREDUMP(CInt status) => status & __WCOREFLAG;
macro CInt w_EXITCODE(CInt ret, CInt sig) => (ret << 8) | sig;
macro CInt w_STOPCODE(CInt sig) => (sig << 8) | 0x7f;
const CInt __WCOREFLAG = 0x80;
const CInt __W_CONTINUED = 0xffff;
const CInt WNOHANG = 1;
const CInt WUNTRACES = 2;

JmpBuf backtrace_jmpbuf @local;
alias BacktraceFn = fn CInt(void** buffer, CInt size);

extern fn CInt backtrace(void** buffer, CInt size) @if(env::OPENBSD);

fn void install_signal_handler(CInt signal, SigActionFunction func)
{
	Sigaction action = {
		.sa_sigaction = func,
	};
	Sigaction old;
	libc::sigaction(signal, &action, &old);
}

fn CInt backtrace(void** buffer, CInt size) @if(!env::OPENBSD)
{
	if (size < 1) return 0;
	void* handle = libc::dlopen("libc.so.6", libc::RTLD_LAZY|libc::RTLD_NODELETE);
	if (handle)
	{
		BacktraceFn backtrace_fn = libc::dlsym(handle, "backtrace");
		libc::dlclose(handle);
		if (backtrace_fn)
		{
			return backtrace_fn(buffer, size);
		}
	}
	// Loop through the return addresses until we hit a signal.
	// This avoids using the frame address.
	Sigaction restore_backtrace = {
    	.sa_sigaction = fn void(CInt, void*, void*) { libc::longjmp(&backtrace_jmpbuf, 1); },
    };
	Sigaction sig_bus, sig_segv, sig_ill;
	libc::sigaction(libc::SIGBUS, &restore_backtrace, &sig_bus);
	libc::sigaction(libc::SIGSEGV, &restore_backtrace, &sig_segv);
	libc::sigaction(libc::SIGILL, &restore_backtrace, &sig_ill);

	void*[128] buffer_first;
	int i = 0;
	for (i = 0; i < size; i++)
	{
		if (libc::setjmp(&backtrace_jmpbuf) == 1) break;
		buffer[i] = builtin::get_returnaddress(i);
		if (!buffer[i]) break;
	}

	Sigaction old;
	libc::sigaction(libc::SIGBUS, &sig_bus, &old);
	libc::sigaction(libc::SIGSEGV, &sig_segv, &old);
	libc::sigaction(libc::SIGILL, &sig_ill, &old);

	return i;
}

struct PosixUContext_t @if(env::DARWIN)
{
	int                     uc_onstack;
	__Darwin_sigset_t       uc_sigmask;     /* signal mask used by this context */
	__Darwin_sigaltstack    uc_stack;       /* stack used by this context */
	PosixUContext_t*        uc_link;        /* pointer to resuming context */
	__Darwin_size_t         uc_mcsize;      /* size of the machine context passed in */
	__Darwin_mcontext64*    uc_mcontext;   /* pointer to machine specific context */
}

alias PosixUContext_t @if(!env::DARWIN) = void;

macro void* stack_instruction(PosixUContext_t* uc)
{
	$switch:
		$case env::DARWIN && env::AARCH64:
			return (void*)uc.uc_mcontext.__ss.__pc + 1;
/*		$case env::DARWIN && env::X86_64:
			return uc.uc_mcontext.__ss.__rip;
		$case env::LINUX && env::X86:
			return uc.uc_mcontext.gregs[REG_EIP];
		$case env::LINUX && env::X86_64:
			return uc.uc_mcontext.gregs[REG_RIP];
		$case env::LINUX && env::AARCH64:
			return uc.uc_mcontext.pc;
		$case env::FREEBSD && env::X86_64:
			return uc.uc_mcontext.mc_rip;
		$case env::OPENBSD && env::X86_64:
			return uc.sc_rip;
		$case env::NETBSD:
			uc.uc_mcontext.__gregs[_REG_RIP];*/
		$default:
			return null;
	$endswitch
}
