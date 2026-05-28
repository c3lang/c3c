module libc @if(env::DARWIN);

alias Dev_t = int;
alias Mode_t = ushort;
alias Nlink_t = ushort;
alias Blkcnt_t = long;
alias Blksize_t = int;
alias Ino_t = ulong;

struct Stat
{
	Dev_t st_dev;
	Mode_t st_mode;
	Nlink_t st_nlink;
	Ino_t st_ino;
	Uid_t st_uid;
	Gid_t st_gid;
	Dev_t st_rdev;

	<* time of last access *>
	TimeSpec st_atimespec;
	<* time of last data modification *>
	TimeSpec st_mtimespec;
	<* time of last status change *>
	TimeSpec st_ctimespec;
	<* time of file creation(birth) *>
	TimeSpec st_birthtimespec;
	<* file size, in bytes *>
	Off_t st_size;
	<* blocks allocated for file *>
	Blkcnt_t st_blocks;
	<* optimal blocksize for I/O *>
	Blksize_t st_blocksize;
	<* user defined flags for file *>
	uint st_flags;
	<* file generation number *>
	uint st_gen;
	<* RESERVED *>
	int st_lspare;
	<* RESERVED *>
	long[2] st_qspare;
}

extern fn int stat(ZString str, Stat* stat) @cname("stat64");

extern fn CInt sysctl(CInt *name, CUInt namelen, void *oldp, usz *oldlenp, void *newp, usz newlen);

const SCHED_OTHER = 1;
const SCHED_FIFO  = 4;
const SCHED_RR    = 2;

module libc::termios @if(env::LIBC &&& env::DARWIN);

bitstruct Tc_iflags : CULong
{
	bool ignbrk : 0;
	bool brkint : 1;
	bool ignpar : 2;
	bool parmrk : 3;
	bool inpck : 4;
	bool istrip : 5;
	bool inlcr : 6;
	bool igncr : 7;
	bool icrnl : 8;
	bool ixon : 9;
	bool ixoff : 10;
	bool ixany : 11;
	bool imaxbel : 13;
	bool iutf8 : 14;
}

bitstruct Tc_oflags : CULong
{
	bool opost : 0;
	bool onlcr : 1;
	bool oxtabs : 2;
	bool onoeot : 3;
	bool ocrnl : 4;
	bool onocr : 5;
	bool onlret : 6;
	bool ofill : 7;
	T_nldly nldly : 8..9;
	T_tabdly tabdly : 10..11;
	T_crdly crdly : 12..13;
	T_ffdly ffdly : 14..14;
	T_bsdly bsdly : 15..15;
	T_vtdly vtdly : 16..16;
	bool ofdel : 17;
}

bitstruct Tc_cflags : CULong
{
	bool cignore : 0;
	T_csize csize : 8..9;
	bool cstopb : 10;
	bool cread : 11;
	bool parenb : 12;
	bool parodd : 13;
	bool hupcl : 14;
	bool clocal : 15;
	bool ccts_oflow : 16;
	bool crts_iflow : 17;
	bool cdtr_iflow : 18;
	bool cdsr_oflow : 19;
	bool ccar_oflow : 20;
}

bitstruct Tc_lflags : CULong
{
	bool echoke : 0;
	bool echoe : 1;
	bool echok : 2;
	bool echo : 3;
	bool echonl : 4;
	bool echoprt : 5;
	bool echoctl : 6;
	bool isig : 7;
	bool icanon : 8;
	bool altwerase : 9;
	bool iexten : 10;
	bool extproc : 11;
	bool tostop : 22;
	bool flusho : 23;
	bool nokerninfo : 25;
	bool pendin : 29;
	bool noflsh : 31;
}

constdef T_nldly : char
{
	NL0 = 0b00,
	NL1 = 0b01,
	NL2 = 0b10,
	NL3 = 0b11,
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
	// Darwin's TAB3 is the OXTABS bit, exposed as Tc_oflags.oxtabs.
	TAB0 = 0b00,
	TAB1 = 0b01,
	TAB2 = 0b10,
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

constdef Speed : CULong
{
	B0      = 0,
	B50     = 50,
	B75     = 75,
	B110    = 110,
	B134    = 134,
	B150    = 150,
	B200    = 200,
	B300    = 300,
	B600    = 600,
	B1200   = 1200,
	B1800   = 1800,
	B2400   = 2400,
	B4800   = 4800,
	B9600   = 9600,
	B19200  = 19200,
	B38400  = 38400,
	B7200   = 7200,
	B14400  = 14400,
	B28800  = 28800,
	B57600  = 57600,
	B76800  = 76800,
	B115200 = 115200,
	B230400 = 230400,
	EXTA    = B19200,
	EXTB    = B38400,
}

constdef Control_Char : inline CInt
{
	VEOF     = 0,
	VEOL     = 1,
	VEOL2    = 2,
	VERASE   = 3,
	VWERASE  = 4,
	VKILL    = 5,
	VREPRINT = 6,
	VINTR    = 8,
	VQUIT    = 9,
	VSUSP    = 10,
	VDSUSP   = 11,
	VSTART   = 12,
	VSTOP    = 13,
	VLNEXT   = 14,
	VDISCARD = 15,
	VMIN     = 16,
	VTIME    = 17,
	VSTATUS  = 18,
}

constdef Tcactions : CInt
{
	TCSANOW   = 0,
	TCSADRAIN = 1,
	TCSAFLUSH = 2,
	TCSASOFT  = 0x10,
	TCIFLUSH  = 1,
	TCOFLUSH  = 2,
	TCIOFLUSH = 3,
	TCOOFF    = 1,
	TCOON     = 2,
	TCIOFF    = 3,
	TCION     = 4,
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
extern fn void cfmakeraw(Termios* self);
extern fn CInt cfsetspeed(Termios* self, Speed speed);
extern fn Pid_t tcgetsid(Fd fd);

alias Cc = char;

const CInt NCCS = 20;

struct Termios
{
	Tc_iflags c_iflag;
	Tc_oflags c_oflag;
	Tc_cflags c_cflag;
	Tc_lflags c_lflag;
	Cc[NCCS] c_cc;
	Speed c_ispeed;
	Speed c_ospeed;
}
