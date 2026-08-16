module std::os::posix @feat(POSIX);
import libc;

alias Mode_t = uint;
typedef DIRPtr = void*;

struct Posix_dirent
{
	Ino_t d_fileno;
	Off_t d_off @feat(!NETBSD);
	ushort d_reclen;
	ushort d_namelen @feat(DARWIN, NETBSD);
	char d_type;

	char d_namelen @feat(OPENBSD);
	char[4] d_pad0 @feat(OPENBSD);

	char d_pad0 @feat(FREEBSD);
	ushort d_namelen @feat(FREEBSD);
	ushort d_pad1 @feat(FREEBSD);

	char[255+1] name @feat(FREEBSD, OPENBSD);
	char[511+1] name @feat(NETBSD);
	char[1024] name @feat(DARWIN);
	char[*] name @feat(!DARWIN) @feat(!BSD);
}

extern fn int rmdir(ZString);
extern fn int mkdir(ZString, ushort mode_t);
extern fn int chdir(ZString);
extern fn ZString getcwd(char* pwd, usz len);
extern fn CInt pipe(CInt[2]* pipes);
extern fn CFile fdopen(CInt fd, ZString mode);
extern fn CInt open(ZString path, CInt flags, ...);
extern fn CInt access(ZString path, CInt mode);
extern fn CInt closedir(DIRPtr);
extern fn Posix_dirent* readdir(DIRPtr) @cname(readdir_cname());
extern fn DIRPtr opendir(ZString) @cname(env::NETBSD ??? "__opendir30" : "opendir");

const DT_UNKNOWN =  0;
const DT_FIFO    =  1;
const DT_CHR     =  2;
const DT_DIR     =  4;
const DT_BLK     =  6;
const DT_REG     =  8;
const DT_LNK     = 10;
const DT_SOCK    = 12;
const DT_WHT     = 14;


macro String readdir_cname() @local @const
{
	$switch:
		$case env::DARWIN && env::X86_64: return "readdir$INODE64";
		$case env::NETBSD: return "__readdir30";
		$default: return "readdir";
	$endswitch
}


// I dont know how to automate 
// and make crossplatform to glibc linux and musl linux
// In current stage, this only glibc linux.
module std::os::posix @feat(LINUX);
const O_RDONLY    = 0o0000;
const O_WRONLY    = 0o0001;
const O_RDWR      = 0o0002;

const O_ACCMODE   = 0o0003;

const O_CREAT     = 0o0100;
const O_EXCL      = 0o0200;

const O_NOCTTY    = 0o0400;

const O_TRUNC     = 0o01000;
const O_APPEND    = 0o02000;

const O_NONBLOCK  = 0o04000;
const O_NDELAY    = O_NONBLOCK;

const O_SYNC      = 0o4010000;
const O_ASYNC     = 0o0020000;
const O_RSYNC     = O_SYNC;
const O_FSYNC     = O_SYNC
const O_DSYNC     = 0o0010000;

const O_DIRECTORY = 0o0200000;
const O_NOFOLLOW  = 0o0400000;
const O_TMPFILE   = (0o020000000 | O_DIRECTORY);

const O_DIRECT    = 0o0040000;

const O_NOATIME   = 0o1000000;
const O_CLOEXEC   = 0o2000000;

const O_PATH      = 0o10000000;
