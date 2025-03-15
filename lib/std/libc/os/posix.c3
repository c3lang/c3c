module libc @if(env::POSIX);

const CInt SHUT_RD = 0;
const CInt SHUT_WR = 1;
const CInt SHUT_RDWR = 2;
extern fn CInt shutdown(Fd sockfd, CInt how);

extern fn isz recv(Fd socket, void *buffer, usz length, CInt flags);
extern fn isz send(Fd socket, void *buffer, usz length, CInt flags);

extern fn void* dlopen(ZString path, int flags);
extern fn CInt dlclose(void*);
extern fn void* dlsym(void* handle, ZString symbol);

const int RTLD_LAZY = 0x1;
const int RTLD_NOW = 0x2;
const int RTLD_LOCAL = 0x4;
const int RTLD_GLOBAL = 0x8;
const int RTLD_NODELETE = 0x1000;

alias Pid_t = int;
alias Uid_t = uint;
alias Gid_t = uint;

const CUInt SA_ONSTACK       = env::LINUX ? 0x08000000 : 0x0001;
const CUInt SA_RESTART       = env::LINUX ? 0x10000000 : 0x0002;
const CUInt SA_RESETHAND     = env::LINUX ? 0x80000000 : 0x0004;
const CUInt SA_SIGINFO       = env::LINUX ? 0x00000004 : 0x0040;

alias Sigset_t @if(!env::LINUX) = uint;
alias Sigset_t @if(env::LINUX) = ulong[16];
alias SigActionFunction =  fn void(CInt, void*, void*);

struct Sigaction
{
	union
	{
		SignalFunction      sa_handler;
		SigActionFunction   sa_sigaction;
	}
	CInt                    sa_flags @if(env::BSD_FAMILY);
	Sigset_t                sa_mask; // 128
	CInt                    sa_flags @if(!env::BSD_FAMILY);
	void*                   sa_restorer @if(env::LINUX);
}

struct Stack_t
{
	void*   ss_sp;
	struct @if(!env::LINUX)
	{
		usz     ss_size;
		CInt    ss_flags;
	}
	struct @if(env::LINUX)
	{
		CInt    ss_flags;
		usz     ss_size;
	}
}

extern fn CInt sigaltstack(Stack_t* ss, Stack_t* old_ss);
extern fn CInt sigaction(CInt signum, Sigaction *action, Sigaction *oldaction);

module libc::termios @if(env::LIBC &&& env::POSIX);

typedef Cc = char;
typedef Speed = CUInt;
typedef Tcflags = CUInt;
typedef Tcactions = CInt;

const Tcactions TCOOFF = 0;
const Tcactions TCOON = 1;
const Tcactions TCIOFF = 2;
const Tcactions TCION = 3;
const Tcactions TCIFLUSH = 0;
const Tcactions TCOFLUSH = 1;
const Tcactions TCIOFLUSH = 2;
const Tcactions TCSANOW = 0;
const Tcactions TCSADRAIN = 1;
const Tcactions TCSAFLUSH = 2;
const Speed B0 = 0000000;
const Speed B50 = 0000001;
const Speed B75 = 0000002;
const Speed B110 = 0000003;
const Speed B134 = 0000004;
const Speed B150 = 0000005;
const Speed B200 = 0000006;
const Speed B300 = 0000007;
const Speed B600 = 0000010;
const Speed B1200 = 0000011;
const Speed B1800 = 0000012;
const Speed B2400 = 0000013;
const Speed B4800 = 0000014;
const Speed B9600 = 0000015;
const Speed B19200 = 0000016;
const Speed B38400 = 0000017;
const Speed B57600 = 0010001;
const Speed B115200 = 0010002;
const Speed B230400 = 0010003;
const Speed B460800 = 0010004;
const Speed B500000 = 0010005;
const Speed B576000 = 0010006;
const Speed B921600 = 0010007;
const Speed B1000000 = 0010010;
const Speed B1152000 = 0010011;
const Speed B1500000 = 0010012;
const Speed B2000000 = 0010013;
const Speed B2500000 = 0010014;
const Speed B3000000 = 0010015;
const Speed B3500000 = 0010016;
const Speed B4000000 = 0010017;
const Speed MAX_BAUD = B4000000;
const Tcflags VINTR = 0;
const Tcflags VQUIT = 1;
const Tcflags VERASE = 2;
const Tcflags VKILL = 3;
const Tcflags VEOF = 4;
const Tcflags VTIME = 5;
const Tcflags VMIN = 6;
const Tcflags VSWTC = 7;
const Tcflags VSTART = 8;
const Tcflags VSTOP = 9;
const Tcflags VSUSP = 10;
const Tcflags VEOL = 11;
const Tcflags VREPRINT = 12;
const Tcflags VDISCARD = 13;
const Tcflags VWERASE = 14;
const Tcflags VLNEXT = 15;
const Tcflags VEOL2 = 16;
const Tcflags ISIG = 0000001;
const Tcflags ICANON = 0000002;
const Tcflags ECHO = 0000010;
const Tcflags ECHOE = 0000020;
const Tcflags ECHOK = 0000040;
const Tcflags ECHONL = 0000100;
const Tcflags NOFLSH = 0000200;
const Tcflags TOSTOP = 0000400;
const Tcflags IEXTEN = 0100000;
const Tcflags CSIZE = 0000060;
const Tcflags CS5 = 0000000;
const Tcflags CS6 = 0000020;
const Tcflags CS7 = 0000040;
const Tcflags CS8 = 0000060;
const Tcflags CSTOPB = 0000100;
const Tcflags CREAD = 0000200;
const Tcflags PARENB = 0000400;
const Tcflags PARODD = 0001000;
const Tcflags HUPCL = 0002000;
const Tcflags CLOCAL = 0004000;
const Tcflags OPOST = 0000001;
const Tcflags OLCUC = 0000002;
const Tcflags ONLCR = 0000004;
const Tcflags OCRNL = 0000010;
const Tcflags ONOCR = 0000020;
const Tcflags ONLRET = 0000040;
const Tcflags OFILL = 0000100;
const Tcflags OFDEL = 0000200;
const Tcflags VTDLY = 0040000;
const Tcflags VT0 = 0000000;
const Tcflags VT1 = 0040000;
const Tcflags IGNBRK = 0000001;
const Tcflags BRKINT = 0000002;
const Tcflags IGNPAR = 0000004;
const Tcflags PARMRK = 0000010;
const Tcflags INPCK = 0000020;
const Tcflags ISTRIP = 0000040;
const Tcflags INLCR = 0000100;
const Tcflags IGNCR = 0000200;
const Tcflags ICRNL = 0000400;
const Tcflags IUCLC = 0001000;
const Tcflags IXON = 0002000;
const Tcflags IXANY = 0004000;
const Tcflags IXOFF = 0010000;
const Tcflags IMAXBEL = 0020000;
const Tcflags IUTF8 = 0040000;

extern fn CInt tcgetattr(Fd fd, Termios* self);
extern fn CInt tcsetattr(Fd fd, Tcactions optional_actions, Termios* self);
extern fn CInt tcsendbreak(Fd fd, CInt duration);
extern fn CInt tcdrain(Fd fd);
extern fn CInt tcflush(Fd fd, CInt queue_selector);
extern fn CInt tcflow(Fd fd, CInt action);
extern fn Speed cfgetospeed(Termios* self);
extern fn Speed cfgetispeed(Termios* self);
extern fn CInt cfsetospeed(Termios* self, Speed speed);
extern fn CInt cfsetispeed(Termios* self, Speed speed);

const CInt NCCS = 32;
struct Termios
{
	Tcflags c_iflag;
	Tcflags c_oflag;
	Tcflags c_cflag;
	Tcflags c_lflag;
	Cc c_line;
	Cc[NCCS] c_cc;
	Speed c_ispeed;
	Speed c_ospeed;
} 

