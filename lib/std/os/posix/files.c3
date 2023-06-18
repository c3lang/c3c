module std::os::posix @if(POSIX_LIBC);
import libc;

extern fn int rmdir(ZString);
extern fn int mkdir(ZString, ushort mode_t);
extern fn int chdir(ZString);
extern fn ZString getcwd(char* pwd, usz len);
def Mode_t = uint;
def Pid_t = int;
extern fn CInt pipe(CInt[2]* pipes);
extern fn CFile fdopen(CInt fd, ZString mode);
