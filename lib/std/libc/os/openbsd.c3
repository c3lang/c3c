module libc @if(env::OPENBSD);

// Checked for x86_64

alias Blksize_t = int;
alias Nlink_t = $typefrom(env::X86_64 ? uint.typeid : CUInt.typeid);
alias Dev_t = int;
alias Ino_t = ulong;
alias Mode_t = uint;
alias Blkcnt_t = long;
alias Fflags_t = uint;

struct Stat @if(env::X86_64)
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

extern fn CInt sysctl(CInt *name, CUInt namelen, void *oldp, usz *oldlenp, void *newp, usz newlen);

