module libc @if(env::WIN32);


extern fn CFile __acrt_iob_func(CInt c);
extern fn CInt _close(Fd fd); def close = _close;
extern fn double _difftime64(Time_t time1, Time_t time2); def difftime = _difftime64;
extern fn CFile _fdopen(Fd fd, ZString mode); def fdopen = _fdopen;
extern fn CInt _fileno(CFile stream); def fileno = _fileno;
extern fn CInt _fseeki64(CFile, long, int); def fseek = _fseeki64;
extern fn CLong _ftelli64(CFile); def ftell = _ftelli64;
extern fn Errno _get_timezone(CLong *timezone);
extern fn Tm* _gmtime64_s(Tm* buf, Time_t *timer);
extern fn Tm* _localtime64_s(Tm* buf, Time_t *timer);
extern fn Time_t _mkgmtime64(Tm* timeptr); def timegm = _mkgmtime64;
extern fn Time_t _mktime64(Tm *timeptr); def mktime = _mktime64;
extern fn usz _msize(void* ptr);
extern fn CInt _read(Fd fd, void* buffer, CUInt buffer_size);
extern fn CInt _setjmp(void* frameptr, JmpBuf* buffer) @if(env::WIN32);
extern fn CFile _wfopen(WString, WString);
extern fn CFile _wfreopen(WString, WString, CFile);
extern fn CInt _write(Fd fd, void* buffer, CUInt count);
extern fn CInt _wremove(WString);

// Aliases to simplify libc use
macro Tm* localtime_r(Time_t* timer, Tm* buf) => _localtime64_s(buf, timer);
macro CInt setjmp(JmpBuf* buffer) => _setjmp($$frameaddress(), buffer);
macro Tm* gmtime_r(Time_t* timer, Tm* buf) => _gmtime64_s(buf, timer);
macro isz read(Fd fd, void* buffer, usz buffer_size) => _read(fd, buffer, buffer_size);
macro isz write(Fd fd, void* buffer, usz count) => _write(fd, buffer, count);


