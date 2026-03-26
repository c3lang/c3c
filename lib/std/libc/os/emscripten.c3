module libc @if(env::EMSCRIPTEN);

alias Blksize_t = CInt;
alias Nlink_t = CUInt;
alias Blkcnt_t = CInt;
alias Ino_t = ulong;
alias Dev_t = uint;
alias Mode_t = uint;
alias Ino64_t = ulong;
alias Blkcnt64_t = long;

struct Stat
{
	uint st_dev;
	uint st_mode;
	uint st_nlink;
	uint st_uid;
	uint st_gid;
	uint st_rdev;
	long st_size;
	uint st_blksize;
	uint st_blocks;
	long st_atime;
	CLong st_atime_nsec;
	long st_mtime;
	CLong st_mtime_nsec;
	long st_ctime;
	CLong st_ctime_nsec;
	ulong st_ino;
}

extern fn CInt stat(ZString path, Stat* stat);
extern fn CInt emscripten_num_logical_cores();
