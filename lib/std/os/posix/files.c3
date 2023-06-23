module std::os::posix @if(env::POSIX);
import libc;

def Mode_t = uint;
def DIRPtr = distinct void*;

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

struct Posix_dirent
{
	Ino_t d_fileno;
	Off_t d_off;
	ushort d_reclen;
	ushort d_namelen @if(env::DARWIN);
	char d_type;
	char d_namelen @if(env::FREEBSD || env::OPENBSD);
	uint d_pad0 @if(env::FREEBSD);
	char[4] d_pad0 @if(env::OPENBSD);
	char[1024] name @if(env::DARWIN);
	char[*] name @if(!env::DARWIN);
}

extern fn int rmdir(ZString);
extern fn int mkdir(ZString, ushort mode_t);
extern fn int chdir(ZString);
extern fn ZString getcwd(char* pwd, usz len);
extern fn CInt pipe(CInt[2]* pipes);
extern fn CFile fdopen(CInt fd, ZString mode);
extern fn CInt access(ZString path, CInt mode);
extern fn Posix_dirent* readdir(DIRPtr) @extern("readdir") @if(!USE_DARWIN_INODE64) ;
extern fn DIRPtr opendir(ZString);
extern fn void closedir(DIRPtr);

const DT_UNKNOWN =  0;
const DT_FIFO    =  1;
const DT_CHR     =  2;
const DT_DIR     =  4;
const DT_BLK     =  6;
const DT_REG     =  8;
const DT_LNK     = 10;
const DT_SOCK    = 12;
const DT_WHT     = 14;

const USE_DARWIN_INODE64 = env::DARWIN && env::ARCH_TYPE == X86_64;
extern fn Posix_dirent* readdir(DIRPtr) @extern("readdir$INODE64") @if(USE_DARWIN_INODE64);

