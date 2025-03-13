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

	TimeSpec st_atimespec; // time of last access
	TimeSpec st_mtimespec; // time of last data modification
	TimeSpec st_ctimespec; // time of last status change
	TimeSpec st_birthtimespec; // time of file creation(birth)
	Off_t st_size; // file size, in bytes
	Blkcnt_t st_blocks; // blocks allocated for file
	Blksize_t st_blocksize; // optimal blocksize for I/O
	uint st_flags; // user defined flags for file
	uint st_gen; // file generation number
	int st_lspare; // RESERVED
	long[2] st_qspare; // RESERVED
}

extern fn int stat(ZString str, Stat* stat) @extern("stat64");

extern fn CInt sysctl(CInt *name, CUInt namelen, void *oldp, usz *oldlenp, void *newp, usz newlen);
