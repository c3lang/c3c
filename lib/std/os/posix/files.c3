module std::os::posix;

$if (env::os_is_posix() && env::COMPILER_LIBC_AVAILABLE)

extern fn int rmdir(ZString);
extern fn int mkdir(ZString, ushort mode_t);
extern fn int chdir(ZString);
extern fn ZString getcwd(char* pwd, usz len);

$endif