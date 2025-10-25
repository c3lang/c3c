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
