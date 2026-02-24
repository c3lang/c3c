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

const CUInt SA_ONSTACK       = env::LINUX || env::ANDROID ? 0x08000000 : 0x0001;
const CUInt SA_RESTART       = env::LINUX || env::ANDROID ? 0x10000000 : 0x0002;
const CUInt SA_RESETHAND     = env::LINUX || env::ANDROID ? 0x80000000 : 0x0004;
const CUInt SA_SIGINFO       = env::LINUX || env::ANDROID ? 0x00000004 : 0x0040;

alias Sigset_t @if((env::DARWIN || env::BSD_FAMILY) && !env::NETBSD) = uint;
alias Sigset_t @if(env::NETBSD) = uint[4];
alias Sigset_t @if(env::LINUX) = ulong[16];
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

    CInt       sa_flags @if(env::DARWIN || env::BSD_FAMILY );
    Sigset_t   sa_mask  @if(env::DARWIN || env::BSD_FAMILY);

    Sigset_t   sa_mask     @if(env::LINUX || env::ANDROID);
    CInt       sa_flags    @if(env::LINUX);
    void*      sa_restorer @if(env::LINUX || env::ANDROID);
}

struct Stack_t
{
	void*   ss_sp;
	struct @if(!env::LINUX && !env::ANDROID)
	{
		usz     ss_size;
		CInt    ss_flags;
	}
	struct @if(env::LINUX || env::ANDROID)
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


module libc::termios @if(env::LIBC &&& env::POSIX);

bitstruct Tc_iflags : CUInt
{
	bool ignbrk;
	bool brkint;
	bool ignpar;
	bool parmrk;
	bool inpck;
	bool istrip;
	bool inlcr;
	bool igncr;
	bool icrnl;
	bool iuclc;
	bool ixon;
	bool ixany;
	bool ixoff;
	bool imaxbel;
	bool iutf8;
}

bitstruct Tc_oflags : CUInt
{
	bool opost : 0;
	bool olcuc : 1;
	bool onlcr : 2;
	bool ocrnl : 3;
	bool onocr : 4;
	bool onlret : 5;
	bool ofill : 6;
	bool ofdel : 7;
	T_nldly nldly : 8..8;
	T_crdly crdly : 9..10;
	T_tabdly tabdly : 11..12;
	T_bsdly bsdly : 13..13;
	T_vtdly vtdly : 14..14;
	T_ffdly ffdly : 15..15;
}

bitstruct Tc_cflags : CUInt
{
	T_csize csize : 4..5;
	bool cstopb : 6;
	bool cread : 7;
	bool parenb : 8;
	bool parodd : 9;
	bool hupcl : 10;
	bool clocal : 11;
	bool addrb : 29;
}

bitstruct Tc_lflags : CUInt
{
	bool isig : 0;
	bool icanon : 1;
	bool xcase : 2;
	bool echo : 3;
	bool echoe : 4;
	bool echok : 5;
	bool echonl : 6;
	bool noflsh : 7;
	bool tostop : 8;
	bool echoctl : 9;
	bool echoprt : 10;
	bool echoke : 11;
	bool flusho : 12;
	bool pendin : 14;
	bool iexten : 15;
	bool extproc : 16;
}

constdef T_nldly : char
{
	NL0 = 0b0,
	NL1 = 0b1,
}

constdef T_crdly : char
{
	CR0 = 0b00,
	CR1 = 0b01,
	CR2 = 0b10,
	CR3 = 0b11,
}

constdef T_tabdly : char
{
	TAB0 = 0b00,
	TAB1 = 0b01,
	TAB2 = 0b10,
	TAB3 = 0b11,
	XTABS = TAB3,
}

constdef T_bsdly : char
{
	BS0 = 0b0,
	BS1 = 0b1,
}

constdef T_ffdly : char
{
	FF0 = 0b0,
	FF1 = 0b1,
}

constdef T_vtdly : char
{
	VT0 = 0b0,
	VT1 = 0b1,
}

constdef T_csize : char
{
	CS5 = 0b00,
	CS6 = 0b01,
	CS7 = 0b10,
	CS8 = 0b11,
}

constdef Speed : CUInt
{
	B0       = 0o0000000,
	B50      = 0o0000001,
	B75      = 0o0000002,
	B110     = 0o0000003,
	B134     = 0o0000004,
	B150     = 0o0000005,
	B200     = 0o0000006,
	B300     = 0o0000007,
	B600     = 0o0000010,
	B1200    = 0o0000011,
	B1800    = 0o0000012,
	B2400    = 0o0000013,
	B4800    = 0o0000014,
	B9600    = 0o0000015,
	B19200   = 0o0000016,
	B38400   = 0o0000017,
	B57600   = 0o0010001,
	B115200  = 0o0010002,
	B230400  = 0o0010003,
	B460800  = 0o0010004,
	B500000  = 0o0010005,
	B576000  = 0o0010006,
	B921600  = 0o0010007,
	B1000000 = 0o0010010,
	B1152000 = 0o0010011,
	B1500000 = 0o0010012,
	B2000000 = 0o0010013,
	B2500000 = 0o0010014,
	B3000000 = 0o0010015,
	B3500000 = 0o0010016,
	B4000000 = 0o0010017,
	MAX_BAUD = B4000000,
}

constdef Cc : inline char
{
	VINTR    = 0,
	VQUIT    = 1,
	VERASE   = 2,
	VKILL    = 3,
	VEOF     = 4,
	VTIME    = 5,
	VMIN     = 6,
	VSWTC    = 7,
	VSTART   = 8,
	VSTOP    = 9,
	VSUSP    = 10,
	VEOL     = 11,
	VREPRINT = 12,
	VDISCARD = 13,
	VWERASE  = 14,
	VLNEXT   = 15,
	VEOL2    = 16,
}

constdef Tcactions : CInt
{
	TCOOFF    = 0,
	TCOON     = 1,
	TCIOFF    = 2,
	TCION     = 3,
	TCIFLUSH  = 0,
	TCOFLUSH  = 1,
	TCIOFLUSH = 2,
	TCSANOW   = 0,
	TCSADRAIN = 1,
	TCSAFLUSH = 2,
}

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
	Tc_iflags c_iflag;
	Tc_oflags c_oflag;
	Tc_cflags c_cflag;
	Tc_lflags c_lflag;
	Cc c_line;
	Cc[NCCS] c_cc;
	Speed c_ispeed;
	Speed c_ospeed;
}

const Tcactions TCOOFF @deprecated = (Tcactions)0;
const Tcactions TCOON @deprecated = (Tcactions)1;
const Tcactions TCIOFF @deprecated = (Tcactions)2;
const Tcactions TCION @deprecated = (Tcactions)3;
const Tcactions TCIFLUSH @deprecated = (Tcactions)0;
const Tcactions TCOFLUSH @deprecated = (Tcactions)1;
const Tcactions TCIOFLUSH @deprecated = (Tcactions)2;
const Tcactions TCSANOW @deprecated = (Tcactions)0;
const Tcactions TCSADRAIN @deprecated = (Tcactions)1;
const Tcactions TCSAFLUSH @deprecated = (Tcactions)2;
const Speed B0 @deprecated = (Speed)0o0000000;
const Speed B50 @deprecated = (Speed)0o0000001;
const Speed B75 @deprecated = (Speed)0o0000002;
const Speed B110 @deprecated = (Speed)0o0000003;
const Speed B134 @deprecated = (Speed)0o0000004;
const Speed B150 @deprecated = (Speed)0o0000005;
const Speed B200 @deprecated = (Speed)0o0000006;
const Speed B300 @deprecated = (Speed)0o0000007;
const Speed B600 @deprecated = (Speed)0o0000010;
const Speed B1200 @deprecated = (Speed)0o0000011;
const Speed B1800 @deprecated = (Speed)0o0000012;
const Speed B2400 @deprecated = (Speed)0o0000013;
const Speed B4800 @deprecated = (Speed)0o0000014;
const Speed B9600 @deprecated = (Speed)0o0000015;
const Speed B19200 @deprecated = (Speed)0o0000016;
const Speed B38400 @deprecated = (Speed)0o0000017;
const Speed B57600 @deprecated = (Speed)0o0010001;
const Speed B115200 @deprecated = (Speed)0o0010002;
const Speed B230400 @deprecated = (Speed)0o0010003;
const Speed B460800 @deprecated = (Speed)0o0010004;
const Speed B500000 @deprecated = (Speed)0o0010005;
const Speed B576000 @deprecated = (Speed)0o0010006;
const Speed B921600 @deprecated = (Speed)0o0010007;
const Speed B1000000 @deprecated = (Speed)0o0010010;
const Speed B1152000 @deprecated = (Speed)0o0010011;
const Speed B1500000 @deprecated = (Speed)0o0010012;
const Speed B2000000 @deprecated = (Speed)0o0010013;
const Speed B2500000 @deprecated = (Speed)0o0010014;
const Speed B3000000 @deprecated = (Speed)0o0010015;
const Speed B3500000 @deprecated = (Speed)0o0010016;
const Speed B4000000 @deprecated = (Speed)0o0010017;
const Speed MAX_BAUD @deprecated = (Speed)0o0010017;
const Cc VINTR @deprecated = (Cc)0;
const Cc VQUIT @deprecated = (Cc)1;
const Cc VERASE @deprecated = (Cc)2;
const Cc VKILL @deprecated = (Cc)3;
const Cc VEOF @deprecated = (Cc)4;
const Cc VTIME @deprecated = (Cc)5;
const Cc VMIN @deprecated = (Cc)6;
const Cc VSWTC @deprecated = (Cc)7;
const Cc VSTART @deprecated = (Cc)8;
const Cc VSTOP @deprecated = (Cc)9;
const Cc VSUSP @deprecated = (Cc)10;
const Cc VEOL @deprecated = (Cc)11;
const Cc VREPRINT @deprecated = (Cc)12;
const Cc VDISCARD @deprecated = (Cc)13;
const Cc VWERASE @deprecated = (Cc)14;
const Cc VLNEXT @deprecated = (Cc)15;
const Cc VEOL2 @deprecated = (Cc)16;
const Tc_lflags ISIG @deprecated = {.isig};
const Tc_lflags ICANON @deprecated = {.icanon};
const Tc_lflags ECHO @deprecated = {.echo};
const Tc_lflags ECHOE @deprecated = {.echoe};
const Tc_lflags ECHOK @deprecated = {.echok};
const Tc_lflags ECHONL @deprecated = {.echonl};
const Tc_lflags NOFLSH @deprecated = {.noflsh};
const Tc_lflags TOSTOP @deprecated = {.tostop};
const Tc_lflags IEXTEN @deprecated = {.iexten};
const Tc_cflags CSIZE @deprecated = {.csize = CS8};
const Tc_cflags CS5 @deprecated = {.csize = CS5};
const Tc_cflags CS6 @deprecated = {.csize = CS6};
const Tc_cflags CS7 @deprecated = {.csize = CS7};
const Tc_cflags CS8 @deprecated = {.csize = CS8};
const Tc_cflags CSTOPB @deprecated = {.cstopb};
const Tc_cflags CREAD @deprecated = {.cread};
const Tc_cflags PARENB @deprecated = {.parenb};
const Tc_cflags PARODD @deprecated = {.parodd};
const Tc_cflags HUPCL @deprecated = {.hupcl};
const Tc_cflags CLOCAL @deprecated = {.clocal};
const Tc_oflags OPOST @deprecated = {.opost};
const Tc_oflags OLCUC @deprecated = {.olcuc};
const Tc_oflags ONLCR @deprecated = {.onlcr};
const Tc_oflags OCRNL @deprecated = {.ocrnl};
const Tc_oflags ONOCR @deprecated = {.onocr};
const Tc_oflags ONLRET @deprecated = {.onlret};
const Tc_oflags OFILL @deprecated = {.ofill};
const Tc_oflags OFDEL @deprecated = {.ofdel};
const Tc_oflags VTDLY @deprecated = {.vtdly = VT1};
const Tc_oflags VT0 @deprecated = {.vtdly = VT0};
const Tc_oflags VT1 @deprecated = {.vtdly = VT1};
const Tc_iflags IGNBRK @deprecated = {.ignbrk};
const Tc_iflags BRKINT @deprecated = {.brkint};
const Tc_iflags IGNPAR @deprecated = {.ignpar};
const Tc_iflags PARMRK @deprecated = {.parmrk};
const Tc_iflags INPCK @deprecated = {.inpck};
const Tc_iflags ISTRIP @deprecated = {.istrip};
const Tc_iflags INLCR @deprecated = {.inlcr};
const Tc_iflags IGNCR @deprecated = {.igncr};
const Tc_iflags ICRNL @deprecated = {.icrnl};
const Tc_iflags IUCLC @deprecated = {.iuclc};
const Tc_iflags IXON @deprecated = {.ixon};
const Tc_iflags IXANY @deprecated = {.ixany};
const Tc_iflags IXOFF @deprecated = {.ixoff};
const Tc_iflags IMAXBEL @deprecated = {.imaxbel};
const Tc_iflags IUTF8 @deprecated = {.iutf8};

