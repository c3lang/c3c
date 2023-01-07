module std::io::file;
import libc;

struct FileInfo
{
	ulong size;
}

$switch (env::OS_TYPE):
$case MACOSX:
$case IOS:
$case WATCHOS:
$case TVOS:
private struct DarwinTimespec
{
	long tv_sec;
	long tv_nsec;
}
private struct Darwin64Stat
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
extern fn int _stat(ZString str, Darwin64Stat* stat) @extname("stat64");
fn void! FileInfo.read(FileInfo* info, Path path)
{
	@pool()
	{
		Darwin64Stat stat;
		int res = _stat(str::tcopy_zstring((String)path), &stat);
		if (res != 0)
		{
			switch (libc::errno())
			{
				case errno::EBADF:
					return IoError.FILE_NOT_VALID!;
				case errno::EFAULT:
					unreachable("Invalid stat");
				case errno::EIO:
					return IoError.GENERAL_ERROR!;
				case errno::EACCES:
					return IoError.NO_PERMISSION!;
				case errno::ELOOP:
					return IoError.NO_PERMISSION!;
				case errno::ENAMETOOLONG:
					return IoError.NAME_TOO_LONG!;
				case errno::ENOENT:
					return IoError.FILE_NOT_FOUND!;
				case errno::ENOTDIR:
					return IoError.FILE_NOT_DIR!;
				case errno::EOVERFLOW:
					return IoError.GENERAL_ERROR!;
				default:
					return IoError.UNKNOWN_ERROR!;
			}
		}
		info.size = stat.st_size;
	};
}
$default:
macro void! FileInfo.read(FileInfo* info, Path path)
{
	$assert("Unsupported function");
}
$endswitch;