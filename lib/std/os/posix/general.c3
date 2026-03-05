module std::os::posix @if(env::POSIX);

extern ZString* environ;

extern fn CLong sysconf(CInt name);

const CInt _SC_PAGESIZE @if(env::LINUX) = 30;
const CInt _SC_PAGESIZE @if(env::DARWIN) = 29;
