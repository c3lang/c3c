module libc @if(env::NETBSD);

alias Blksize_t = int;
alias Nlink_t = uint;
alias Dev_t = long;
alias Ino_t = ulong;
alias Mode_t = uint;
alias Blkcnt_t = long;

struct Stat
{
	Dev_t st_dev;
	Mode_t st_mode;
	Ino_t st_ino;
	Nlink_t st_nlink;
	Uid_t st_uid;
	Gid_t st_gid;
	Dev_t st_rdev;
	Time_t st_atime;
	long st_atimensec;
	Time_t st_mtime;
	long st_mtimensec;
	Time_t st_ctime;
	long st_ctimensec;
	Time_t st_birthtime;
	long st_birthtimensec;
	Off_t st_size;
	Blkcnt_t st_blocks;
	Blksize_t st_blksize;
	uint st_flags;
	uint st_gen;
	uint[2] st_spare;
}

extern fn CInt stat(ZString path, Stat* stat) @cname("__stat50");

extern fn CInt sysctl(CInt *name, CUInt namelen, void *oldp, usz *oldlenp, void *newp, usz newlen);

