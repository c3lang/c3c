module std::io::file::os @if(DARWIN_LIBC);
import libc;

struct DarwinTimespec @private
{
	long tv_sec;
	long tv_nsec;
}
struct Darwin64Stat @private
{
	int st_dev;
	ushort st_mode;
	ushort st_nlink;
	ulong st_ino;
	uint st_uid;
	uint st_gid;
	int st_rdev;
	DarwinTimespec st_atimespec; // time of last access
    DarwinTimespec st_mtimespec; // time of last data modification
    DarwinTimespec st_ctimespec; // time of last status change
    DarwinTimespec st_birthtimespec; // time of file creation(birth)
    long st_size;
    long st_blocks;
    int st_blocksize;
    uint st_flags;
    uint st_gen;
    int st_lspare;
    long[2] st_qspare;
}
extern fn int _stat(ZString str, Darwin64Stat* stat) @extern("stat64");

const S_IFMT   = 0o170000; // type of file mask
const S_IFIFO  = 0o010000; // named pipe (fifo)
const S_IFCHR  = 0o020000; // character special
const S_IFDIR  = 0o040000; // directory
const S_IFBLK  = 0o060000; // block special
const S_IFREG  = 0o100000; // regular
const S_IFLNK  = 0o120000; // symbolic link
const S_IFSOCK = 0o140000; // socket

fn usz! native_file_size(String path)
{
	Darwin64Stat stat;
	read_stat(&stat, path)!;
	return stat.st_size;
}

fn bool native_file_or_dir_exists(String path)
{
	Darwin64Stat stat;
	return @ok(read_stat(&stat, path));
}

fn bool native_is_file(String path)
{
	Darwin64Stat stat;
	return @ok(read_stat(&stat, path)) && stat.st_mode & S_IFREG;
}

fn bool native_is_dir(String path)
{
	Darwin64Stat stat;
	return @ok(read_stat(&stat, path)) && stat.st_mode & S_IFDIR;
}

fn void! read_stat(Darwin64Stat* stat, String path) @local
{
	@pool()
	{
		int res = _stat(path.zstr_tcopy(), stat);
		if (res != 0)
		{
			switch (libc::errno())
			{
				case errno::EBADF:
					return IoError.FILE_NOT_VALID?;
				case errno::EFAULT:
					unreachable("Invalid stat");
				case errno::EIO:
					return IoError.GENERAL_ERROR?;
				case errno::EACCES:
					return IoError.NO_PERMISSION?;
				case errno::ELOOP:
					return IoError.NO_PERMISSION?;
				case errno::ENAMETOOLONG:
					return IoError.NAME_TOO_LONG?;
				case errno::ENOENT:
					return IoError.FILE_NOT_FOUND?;
				case errno::ENOTDIR:
					return IoError.FILE_NOT_DIR?;
				case errno::EOVERFLOW:
					return IoError.GENERAL_ERROR?;
				default:
					return IoError.UNKNOWN_ERROR?;
			}
		}
	};
}