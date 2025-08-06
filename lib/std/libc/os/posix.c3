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
	bool creadb : 7;
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

enum T_nldly : const char
{
	NL0 = 0b0,
	NL1 = 0b1,
}

enum T_crdly : const char
{
	CR0 = 0b00,
	CR1 = 0b01,
	CR2 = 0b10,
	CR3 = 0b11,
}

enum T_tabdly : const char
{
	TAB0 = 0b00,
	TAB1 = 0b01,
	TAB2 = 0b10,
	TAB3 = 0b11,
	XTABS = TAB3,
}

enum T_bsdly : const char
{
	BS0 = 0b0,
	BS1 = 0b1,
}

enum T_ffdly : const char
{
	FF0 = 0b0,
	FF1 = 0b1,
}

enum T_vtdly : const char
{
	VT0 = 0b0,
	VT1 = 0b1,
}

enum T_csize : const char
{
	CS5 = 0b00,
	CS6 = 0b01,
	CS7 = 0b10,
	CS8 = 0b11,
}

enum Speed : const CUInt
{
	B0 = 0o0000000,
	B50 = 0o0000001,
	B75 = 0o0000002,
	B110 = 0o0000003,
	B134 = 0o0000004,
	B150 = 0o0000005,
	B200 = 0o0000006,
	B300 = 0o0000007,
	B600 = 0o0000010,
	B1200 = 0o0000011,
	B1800 = 0o0000012,
	B2400 = 0o0000013,
	B4800 = 0o0000014,
	B9600 = 0o0000015,
	B19200 = 0o0000016,
	B38400 = 0o0000017,
	B57600 = 0o0010001,
	B115200 = 0o0010002,
	B230400 = 0o0010003,
	B460800 = 0o0010004,
	B500000 = 0o0010005,
	B576000 = 0o0010006,
	B921600 = 0o0010007,
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

enum Cc : const char
{
	VINTR = 0,
	VQUIT = 1,
	VERASE = 2,
	VKILL = 3,
	VEOF = 4,
	VTIME = 5,
	VMIN = 6,
	VSWTC = 7,
	VSTART = 8,
	VSTOP = 9,
	VSUSP = 10,
	VEOL = 11,
	VREPRINT = 12,
	VDISCARD = 13,
	VWERASE = 14,
	VLNEXT = 15,
	VEOL2 = 16,
}

enum Tcactions : const CInt
{
	TCOOFF = 0,
	TCOON = 1,
	TCIOFF = 2,
	TCION = 3,
	TCIFLUSH = 0,
	TCOFLUSH = 1,
	TCIOFLUSH = 2,
	TCSANOW = 0,
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

typedef Tcflags = CUInt;

const Tcactions TCOOFF @deprecated = 0;
const Tcactions TCOON @deprecated = 1;
const Tcactions TCIOFF @deprecated = 2;
const Tcactions TCION @deprecated = 3;
const Tcactions TCIFLUSH @deprecated = 0;
const Tcactions TCOFLUSH @deprecated = 1;
const Tcactions TCIOFLUSH @deprecated = 2;
const Tcactions TCSANOW @deprecated = 0;
const Tcactions TCSADRAIN @deprecated = 1;
const Tcactions TCSAFLUSH @deprecated = 2;
const Speed B0 @deprecated = 0o0000000;
const Speed B50 @deprecated = 0o0000001;
const Speed B75 @deprecated = 0o0000002;
const Speed B110 @deprecated = 0o0000003;
const Speed B134 @deprecated = 0o0000004;
const Speed B150 @deprecated = 0o0000005;
const Speed B200 @deprecated = 0o0000006;
const Speed B300 @deprecated = 0o0000007;
const Speed B600 @deprecated = 0o0000010;
const Speed B1200 @deprecated = 0o0000011;
const Speed B1800 @deprecated = 0o0000012;
const Speed B2400 @deprecated = 0o0000013;
const Speed B4800 @deprecated = 0o0000014;
const Speed B9600 @deprecated = 0o0000015;
const Speed B19200 @deprecated = 0o0000016;
const Speed B38400 @deprecated = 0o0000017;
const Speed B57600 @deprecated = 0o0010001;
const Speed B115200 @deprecated = 0o0010002;
const Speed B230400 @deprecated = 0o0010003;
const Speed B460800 @deprecated = 0o0010004;
const Speed B500000 @deprecated = 0o0010005;
const Speed B576000 @deprecated = 0o0010006;
const Speed B921600 @deprecated = 0o0010007;
const Speed B1000000 @deprecated = 0o0010010;
const Speed B1152000 @deprecated = 0o0010011;
const Speed B1500000 @deprecated = 0o0010012;
const Speed B2000000 @deprecated = 0o0010013;
const Speed B2500000 @deprecated = 0o0010014;
const Speed B3000000 @deprecated = 0o0010015;
const Speed B3500000 @deprecated = 0o0010016;
const Speed B4000000 @deprecated = 0o0010017;
const Speed MAX_BAUD @deprecated = B4000000;
const Tcflags VINTR @deprecated = 0;
const Tcflags VQUIT @deprecated = 1;
const Tcflags VERASE @deprecated = 2;
const Tcflags VKILL @deprecated = 3;
const Tcflags VEOF @deprecated = 4;
const Tcflags VTIME @deprecated = 5;
const Tcflags VMIN @deprecated = 6;
const Tcflags VSWTC @deprecated = 7;
const Tcflags VSTART @deprecated = 8;
const Tcflags VSTOP @deprecated = 9;
const Tcflags VSUSP @deprecated = 10;
const Tcflags VEOL @deprecated = 11;
const Tcflags VREPRINT @deprecated = 12;
const Tcflags VDISCARD @deprecated = 13;
const Tcflags VWERASE @deprecated = 14;
const Tcflags VLNEXT @deprecated = 15;
const Tcflags VEOL2 @deprecated = 16;
const Tcflags ISIG @deprecated = 0o0000001;
const Tcflags ICANON @deprecated = 0o0000002;
const Tcflags ECHO @deprecated = 0o0000010;
const Tcflags ECHOE @deprecated = 0o0000020;
const Tcflags ECHOK @deprecated = 0o0000040;
const Tcflags ECHONL @deprecated = 0o0000100;
const Tcflags NOFLSH @deprecated = 0o0000200;
const Tcflags TOSTOP @deprecated = 0o0000400;
const Tcflags IEXTEN @deprecated = 0o0100000;
const Tcflags CSIZE @deprecated = 0o0000060;
const Tcflags CS5 @deprecated = 0o0000000;
const Tcflags CS6 @deprecated = 0o0000020;
const Tcflags CS7 @deprecated = 0o0000040;
const Tcflags CS8 @deprecated = 0o0000060;
const Tcflags CSTOPB @deprecated = 0o0000100;
const Tcflags CREAD @deprecated = 0o0000200;
const Tcflags PARENB @deprecated = 0o0000400;
const Tcflags PARODD @deprecated = 0o0001000;
const Tcflags HUPCL @deprecated = 0o0002000;
const Tcflags CLOCAL @deprecated = 0o0004000;
const Tcflags OPOST @deprecated = 0o0000001;
const Tcflags OLCUC @deprecated = 0o0000002;
const Tcflags ONLCR @deprecated = 0o0000004;
const Tcflags OCRNL @deprecated = 0o0000010;
const Tcflags ONOCR @deprecated = 0o0000020;
const Tcflags ONLRET @deprecated = 0o0000040;
const Tcflags OFILL @deprecated = 0o0000100;
const Tcflags OFDEL @deprecated = 0o0000200;
const Tcflags VTDLY @deprecated = 0o0040000;
const Tcflags VT0 @deprecated = 0o0000000;
const Tcflags VT1 @deprecated = 0o0040000;
const Tcflags IGNBRK @deprecated = 0o0000001;
const Tcflags BRKINT @deprecated = 0o0000002;
const Tcflags IGNPAR @deprecated = 0o0000004;
const Tcflags PARMRK @deprecated = 0o0000010;
const Tcflags INPCK @deprecated = 0o0000020;
const Tcflags ISTRIP @deprecated = 0o0000040;
const Tcflags INLCR @deprecated = 0o0000100;
const Tcflags IGNCR @deprecated = 0o0000200;
const Tcflags ICRNL @deprecated = 0o0000400;
const Tcflags IUCLC @deprecated = 0o0001000;
const Tcflags IXON @deprecated = 0o0002000;
const Tcflags IXANY @deprecated = 0o0004000;
const Tcflags IXOFF @deprecated = 0o0010000;
const Tcflags IMAXBEL @deprecated = 0o0020000;
const Tcflags IUTF8 @deprecated = 0o0040000;

fn Tc_iflags Tc_iflags.and(self, Tcflags other) @operator(&) @deprecated => (Tc_iflags)(((Tcflags)self) & other);
fn Tc_iflags Tc_iflags.or(self, Tcflags other) @operator(|) @deprecated => (Tc_iflags)(((Tcflags)self) | other);
fn Tc_iflags Tc_iflags.xor(self, Tcflags other) @operator(^) @deprecated => (Tc_iflags)(((Tcflags)self) ^ other);

fn Tc_oflags Tc_oflags.and(self, Tcflags other) @operator(&) @deprecated => (Tc_oflags)(((Tcflags)self) & other);
fn Tc_oflags Tc_oflags.or(self, Tcflags other) @operator(|) @deprecated => (Tc_oflags)(((Tcflags)self) | other);
fn Tc_oflags Tc_oflags.xor(self, Tcflags other) @operator(^) @deprecated => (Tc_oflags)(((Tcflags)self) ^ other);

fn Tc_cflags Tc_cflags.and(self, Tcflags other) @operator(&) @deprecated => (Tc_cflags)(((Tcflags)self) & other);
fn Tc_cflags Tc_cflags.or(self, Tcflags other) @operator(|) @deprecated => (Tc_cflags)(((Tcflags)self) | other);
fn Tc_cflags Tc_cflags.xor(self, Tcflags other) @operator(^) @deprecated => (Tc_cflags)(((Tcflags)self) ^ other);

fn Tc_lflags Tc_lflags.and(self, Tcflags other) @operator(&) @deprecated => (Tc_lflags)(((Tcflags)self) & other);
fn Tc_lflags Tc_lflags.or(self, Tcflags other) @operator(|) @deprecated => (Tc_lflags)(((Tcflags)self) | other);
fn Tc_lflags Tc_lflags.xor(self, Tcflags other) @operator(^) @deprecated => (Tc_lflags)(((Tcflags)self) ^ other);
