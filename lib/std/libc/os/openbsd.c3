module libc @feat(OPENBSD);

// Checked for x86_64

alias Blksize_t = int;
alias Nlink_t = env::X86_64 ? uint : CUInt;
alias Dev_t = int;
alias Ino_t = ulong;
alias Mode_t = uint;
alias Blkcnt_t = long;
alias Fflags_t = uint;

struct Stat @feat(X86_64)
{
	Mode_t st_mode;
	Dev_t st_dev;
	Ino_t st_ino;
	Nlink_t st_nlink;
	Uid_t st_uid;
	Gid_t st_gid;
	Dev_t st_rdev;
	TimeSpec st_atime;
	TimeSpec st_mtime;
	TimeSpec st_ctime;
	Off_t st_size;
	Blkcnt_t st_blocks;
	Blksize_t st_blksize;
	Fflags_t st_flags;
	uint st_gen;
	ulong[10] st_spare;
}

// TODO: Investigate if this needs to be fixed
struct Stat @feat(!X86_64)
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
extern fn CInt lstat(ZString path, Stat* stat);

extern fn CInt sysctl(CInt *name, CUInt namelen, void *oldp, usz *oldlenp, void *newp, usz newlen);

const SCHED_OTHER = 2;
const SCHED_FIFO  = 1;
const SCHED_RR    = 3;

extern fn CInt pledge(ZString promises, ZString execpromises);
extern fn CInt unveil(ZString path, ZString permissions);

module libc::termios @feat(OPENBSD);

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
	bool ixon;
	bool ixoff;
	bool ixany;
	bool iuclc;
	bool imaxbel;
}

bitstruct Tc_oflags : CUInt @overlap
{
	bool opost  : 0;
	bool onclr  : 1;
	bool tabdly : 2;
	bool tab0   : 0;
	bool tab3   : 2;
	bool oxtabs : 2;
	bool onoeot : 3;
	bool ocrnl  : 4;
	bool olcuc  : 5;
	bool onocr  : 6;
	bool onlret : 7;
}

bitstruct Tc_cflags : CUInt
{
	bool cignore  : 0;
	T_csize csize : 8..9;
	bool cstopb   : 10;
	bool cread    : 11;
	bool parenb   : 12;
	bool parodd   : 13;
	bool hupcl    : 14;
	bool clocal   : 15;
}

bitstruct Tc_lflags : CUInt
{
	bool echoke     : 0;
	bool echoe      : 1;
	bool echok      : 2;
	bool echo       : 3;
	bool echonl     : 4;
	bool echoprt    : 5;
	bool echoctl    : 6;
	bool isig       : 7;
	bool icanon     : 8;
	bool altwerase  : 9;
	bool iexten     : 10;
	bool extproc    : 11;
	bool tostop     : 22;
	bool flusho     : 23;
	bool xcase      : 24;
	bool nokerninfo : 25;
	bool pendin     : 29;
	bool noflsh     : 31;
}

constset T_csize : char
{
	CS5 = 0b00,
	CS6 = 0b01,
	CS7 = 0b10,
	CS8 = 0b11,
}

constset Speed : CUInt
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
}

constset Control_Char : inline CInt
{
	VEOF     = 0,
	VEOL     = 1,
	VEOL2    = 2,
	VERASE   = 3,
	VWERASE  = 4,
	VKILL    = 5,
	VREPRINT = 6,
	//         7,
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
	//         19,
}

constset Tcactions : CInt
{
	TCOOFF    = 1,
	TCOON     = 2,
	TCIOFF    = 3,
	TCION     = 4,
	TCIFLUSH  = 1,
	TCOFLUSH  = 2,
	TCIOFLUSH = 3,
	TCSANOW   = 0,
	TCSADRAIN = 1,
	TCSAFLUSH = 2,
}

extern fn CInt tcgetattr(Fd fd, Termios* self);
extern fn CInt tcsetattr(Fd fd, Tcactions optional_actions, Termios* self);
extern fn CInt tcsendbreak(Fd fd, CInt ignored);
extern fn CInt tcdrain(Fd fd);
extern fn CInt tcflush(Fd fd, int action);
extern fn CInt tcflow(Fd fd, int action);
extern fn Speed cfgetospeed(Termios* self);
extern fn Speed cfgetispeed(Termios* self);
extern fn CInt cfsetospeed(Termios* self, Speed speed);
extern fn CInt cfsetispeed(Termios* self, Speed speed);

alias Cc = char;

const CInt NCCS = 20;

struct Termios
{
	Tc_iflags c_iflag;
	Tc_oflags c_oflag;
	Tc_cflags c_cflag;
	Tc_lflags c_lflag;
	Cc[NCCS]  c_cc;
	Speed     c_ispeed;
	Speed     c_ospeed;
}
