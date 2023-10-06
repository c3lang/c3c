module std::os::posix @if(env::POSIX);
import libc;

def Mode_t = uint;
distinct DIRPtr = void*;

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

const USE_DARWIN_INODE64 = env::DARWIN && env::X86_64;
extern fn Posix_dirent* readdir(DIRPtr) @extern("readdir$INODE64") @if(USE_DARWIN_INODE64);

