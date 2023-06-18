module libc @if(env::WIN32);

extern fn int _fseeki64(CFile, long, int); def fseek = _fseeki64 @if(env::WIN32);
extern fn long _ftelli64(CFile); def ftell = _ftelli64 @if(env::WIN32);
extern fn CFile _wfopen(Char16*, Char16*);
extern fn CFile _wfreopen(Char16*, Char16*, CFile);
extern fn int _close(CInt fd); def close = _close;
extern fn CFile _fdopen(CInt fd, char* mode); def fdopen = _fdopen;
extern fn CInt _fileno(CFile stream); def fileno = _fileno;

