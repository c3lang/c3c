module libc @if(env::LINUX);

// Checked for x86_64, this is notoriously incorrect when comparing with Rust code etc

alias Blksize_t = $typefrom(env::X86_64 ? long.typeid : CInt.typeid);
alias Nlink_t = $typefrom(env::X86_64 ? ulong.typeid : CUInt.typeid);
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
