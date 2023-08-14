module std::os::macos @if(env::DARWIN);

extern fn CInt sysctl(CInt *name, CUInt namelen, void *oldp, usz *oldlenp, void *newp, usz newlen);
