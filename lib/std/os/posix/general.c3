module std::os::posix @feat(POSIX);

extern ZString* environ;

extern fn CLong sysconf(CInt name);

const CInt _SC_PAGESIZE @feat(LINUX) = 30;
const CInt _SC_PAGESIZE @feat(DARWIN) = 29;
