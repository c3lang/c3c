module libc @if(env::POSIX);

def Pid_t = int;
def Uid_t = uint;
def Gid_t = uint;

const CInt SA_ONSTACK       = env::LINUX ? 0x08000000 : 0x0001;
const CInt SA_RESTART       = env::LINUX ? 0x10000000 : 0x0002;
const CInt SA_RESETHAND     = env::LINUX ? 0x80000000 : 0x0004;
const CInt SA_SIGINFO       = env::LINUX ? 0x00000004 : 0x0040;

def Sigset_t = uint @if(!env::LINUX);
def Sigset_t = ulong[16] @if(env::LINUX);
def SigActionFunction =  fn void(CInt, void*, void*);

struct Sigaction
{
	union
    {
    	SignalFunction      sa_handler;
    	SigActionFunction   sa_sigaction;
    }
	CInt            sa_flags @if(env::FREEBSD);
	Sigset_t        sa_mask; // 128
	CInt            sa_flags @if(!env::FREEBSD);
	void*           sa_restorer @if(env::LINUX);
}

struct Stack_t
{
	void*   ss_sp;
	usz     ss_size @if(!env::LINUX);
	CInt    ss_flags;
	usz     ss_size @if(env::LINUX);
}

extern fn CInt sigaltstack(Stack_t* ss, Stack_t* old_ss);
extern fn CInt sigaction(CInt signum, Sigaction *action, Sigaction *oldaction);
