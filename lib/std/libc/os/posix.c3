module libc @if(env::POSIX);

const CInt SHUT_RD = 0;
const CInt SHUT_WR = 1;
const CInt SHUT_RDWR = 2;
extern fn CInt shutdown(Fd sockfd, CInt how);

extern fn sz recv(Fd socket, void *buffer, usz length, CInt flags);
extern fn sz send(Fd socket, void *buffer, usz length, CInt flags);

extern fn void* dlopen(ZString path, int flags);
extern fn CInt dlclose(void*);
extern fn void* dlsym(void* handle, ZString symbol);

const int RTLD_LAZY     = 0x1;
const int RTLD_NOW      = 0x2;   // (bionic ILP32 is 0, but assuming LP64 Android)
const int RTLD_LOCAL    = env::DARWIN ? 0x4 : (env::NETBSD ? 0x200 : 0);
const int RTLD_GLOBAL   = env::DARWIN ? 0x8 : 0x100;
const int RTLD_NODELETE = env::DARWIN ? 0x80 : (env::OPENBSD ? 0x400 : 0x1000);

const int RTLD_NOLOAD   @if(!env::OPENBSD)                = env::DARWIN ? 0x10 : ((env::FREEBSD || env::NETBSD) ? 0x2000 : 0x4);
const int RTLD_DEEPBIND @if(env::LINUX)                   = 0x8;
const int RTLD_TRACE    @if(env::FREEBSD || env::OPENBSD) = 0x200;
const int RTLD_FIRST    @if(env::DARWIN)                  = 0x100;

const void* RTLD_NEXT      = (void*)(uptr)-1;
const void* RTLD_DEFAULT   = (env::DARWIN || env::BSD_FAMILY) ? (void*)(uptr)-2 : null;

const void* RTLD_SELF      @if(env::DARWIN || env::BSD_FAMILY) = (void*)(uptr)-3;
const void* RTLD_MAIN_ONLY @if(env::DARWIN)                    = (void*)(uptr)-5;

alias Pid_t = int;
alias Uid_t = uint;
alias Gid_t = uint;

const CUInt SA_ONSTACK       = env::LINUX || env::ANDROID ? 0x08000000 : 0x0001;
const CUInt SA_RESTART       = env::LINUX || env::ANDROID ? 0x10000000 : 0x0002;
const CUInt SA_RESETHAND     = env::LINUX || env::ANDROID ? 0x80000000 : 0x0004;
const CUInt SA_SIGINFO       = env::LINUX || env::ANDROID ? 0x00000004 : 0x0040;

alias Sigset_t @if(env::DARWIN || env::OPENBSD) = uint;
alias Sigset_t @if(env::NETBSD || env::FREEBSD) = uint[4];
alias Sigset_t @if(env::LINUX) = ulong[16];
alias Sigset_t @if(env::EMSCRIPTEN) = uint[2];
alias Sigset_t @if(env::ANDROID) = ulong;
alias SigActionFunction =  fn void(CInt, void*, void*);

struct Sigaction
{
	CInt sa_flags @if(env::ANDROID);
	union
	{
		SignalFunction      sa_handler;
		SigActionFunction   sa_sigaction;
	}

	CInt     sa_flags @if(env::FREEBSD);
	Sigset_t sa_mask  @if(env::FREEBSD);

	Sigset_t sa_mask     @if(env::LINUX || env::DARWIN  || env::EMSCRIPTEN || env::OPENBSD || env::NETBSD || env::ANDROID);
	CInt     sa_flags    @if(env::LINUX || env::DARWIN  || env::EMSCRIPTEN || env::OPENBSD || env::NETBSD);
	void*    sa_restorer @if(env::LINUX || env::ANDROID || env::EMSCRIPTEN);
}

extern fn CInt setpriority(CInt which, CInt who, CInt prio);
extern fn CInt getpriority(CInt which, CInt who);

extern fn CInt sched_get_priority_min(CInt);
extern fn CInt sched_get_priority_max(CInt);

struct Stack_t
{
	void*   ss_sp;
	struct @if(!env::LINUX && !env::ANDROID && !env::EMSCRIPTEN)
	{
		usz     ss_size;
		CInt    ss_flags;
	}
	struct @if(env::LINUX || env::ANDROID || env::EMSCRIPTEN)
	{
		CInt    ss_flags;
		usz     ss_size;
	}
}

extern fn CInt sigaltstack(Stack_t* ss, Stack_t* old_ss);
extern fn CInt sigaction(CInt signum, Sigaction *action, Sigaction *oldaction) @if(!env::NETBSD);
extern fn CInt sigaction(CInt signum, Sigaction *action, Sigaction *oldaction) @cname("__sigaction_siginfo") @if(env::NETBSD);
extern fn CInt sigemptyset(Sigset_t* set) @if(!env::NETBSD);
extern fn CInt sigemptyset(Sigset_t* set) @cname("__sigemptyset14") @if(env::NETBSD);

const CInt SIGHUP = 1;
const CInt SIGQUIT = 3;
const CInt SIGTRAP = 5;
const CInt SIGABRT = 6;
const CInt SIGKILL = 9;
const CInt SIGBUS = BSD_FLAVOR_SIG ? 10 : 7; // Or Mips
const CInt SIGSYS = BSD_FLAVOR_SIG ? 12 : 31;
const CInt SIGPIPE = 13;
const CInt SIGALRM = 14;
const CInt SIGURG = BSD_FLAVOR_SIG ? 16 : 23;
const CInt SIGSTOP = BSD_FLAVOR_SIG ? 17 : 19;
const CInt SIGTSTP = BSD_FLAVOR_SIG ? 18 : 20;
const CInt SIGCONT = BSD_FLAVOR_SIG ? 19 : 18;
const CInt SIGCHLD = BSD_FLAVOR_SIG ? 20 : 17;
const CInt SIGTTIN = 21;
const CInt SIGTTOU = 22;
const CInt SIGXCPU = 24;
const CInt SIGXFSZ = 25;
const CInt SIGVTALRM = 26;
const CInt SIGWINCH = 28;
const CInt SIGUSR1 = BSD_FLAVOR_SIG ? 30 : 10;
const CInt SIGUSR2 = BSD_FLAVOR_SIG ? 31 : 12;

const bool BSD_FLAVOR_SIG @local = env::DARWIN || env::BSD_FAMILY;
