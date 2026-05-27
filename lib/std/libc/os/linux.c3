module libc @if(env::LINUX);

// Checked for x86_64, this is notoriously incorrect when comparing with Rust code etc

alias Blksize_t = env::X86_64 ? long : CInt;
alias Nlink_t = env::X86_64 ? ulong : CUInt;
alias Blkcnt_t = long;
alias Ino_t = ulong;
alias Dev_t = ulong;
alias Mode_t = uint;
alias Ino64_t = ulong;
alias Blkcnt64_t = long;

struct Stat @if(env::X86_64)
{
	Dev_t st_dev;
	Ino_t st_ino;
	Nlink_t st_nlink;
	Mode_t st_mode;
	Uid_t st_uid;
	Gid_t st_gid;
	CInt __pad0;
	Dev_t st_rdev;
	Off_t st_size;
	Blksize_t st_blksize;
	Blkcnt_t st_blocks;
	Time_t st_atime;
	long st_atime_nsec;
	Time_t st_mtime;
	long st_mtime_nsec;
	Time_t st_ctime;
	long st_ctime_nsec;
	long[3] __unused;
}

struct Stat @if(!env::X86_64)
{
	Dev_t st_dev;
	Ino_t st_ino;
	Mode_t st_mode;
	Nlink_t st_nlink;
	Uid_t st_uid;
	Gid_t st_gid;
	Dev_t st_rdev;
	CInt __pad1;
	Off_t st_size;
	Blksize_t st_blksize;
	CInt __pad2;
	Blkcnt_t st_blocks;
	Time_t st_atime;
	long st_atime_nsec;
	Time_t st_mtime;
	long st_mtime_nsec;
	Time_t st_ctime;
	long st_ctime_nsec;
	CInt[2] __unused;
}

extern fn CInt stat(ZString path, Stat* stat);

extern fn CInt get_nprocs();
extern fn CInt get_nprocs_conf();

// Additional Linux signals
const CInt SIGIOT = 6;
const CInt SIGSTKFLT = 16;
const CInt SIGPROF = 27;
const CInt SIGIO = 29;
const CInt SIGPOLL = 29;
const CInt SIGPWR = 30;
const CInt SIGUNUSED = 31;

module libc @if(env::LINUX || env::ANDROID);

const PRIO_MIN = -20;
const PRIO_MAX = 20;
const PRIO_PROCESS = 0;
const PRIO_PGRP = 1;
const PRIO_USER = 2;

const SCHED_OTHER = 0;
const SCHED_FIFO  = 1;
const SCHED_RR    = 2;
const SCHED_BATCH = 3;
const SCHED_IDLE  = 5;

const SYS_GETTID = gettid_code();

macro gettid_code() @const
{
	$switch env::ARCH_TYPE:
		$case X86_64:  return 186;
		$case X86:
		$case ARM:
		$case ARMB:    return 224;
		$case RISCV32:
		$case RISCV64:
		$case AARCH64: return 178;
		$default:      $error("Missing gettid code");
	$endswitch
}

module libc::termios @if(env::LIBC &&& env::LINUX);

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

constdef Control_Char : inline CInt
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

alias Cc = char;

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
