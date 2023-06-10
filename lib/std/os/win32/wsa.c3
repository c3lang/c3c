module std::os::win32 @if(WIN32_LIBC);

def WSAError = distinct int;

extern fn WSAError win32_WSAGetLastError() @extern("WSAGetLastError");
extern fn void win32_WSASetLastError(WSAError error) @extern("WSASetLastError");

module std::os::win32::wsa @if(env::WIN32);

const WSAError INVALID_HANDLE = 6;
const WSAError NOT_ENOUGHT_MEMORY = 8;
const WSAError INVALID_PARAMETER = 87;
const WSAError OPERATION_ABORTED = 995;
const WSAError IO_INCOMPLETE = 996;
const WSAError IO_PENDING = 997;
const WSAError EINTR = 10004;
const WSAError EBADF = 10009;
const WSAError ACCESS = 10013;
const WSAError EFAULT = 10014;
const WSAError EINVAL = 10022;
const WSAError EMFILE = 10024;
const WSAError EWOULDBLOCK = 10035;
const WSAError EINPROGRESS = 10036;
const WSAError EALREADY = 10037;
const WSAError ENOTSOCK = 10038;
const WSAError EDESTADDRREQ = 10039;
const WSAError EMSGSIZE = 10040;
const WSAError EPROTOTYPE = 10041;
const WSAError ENOPROTOOPT = 10042;
const WSAError EPROTONOSUPPORT = 10043;
const WSAError ESOCKTNOSUPPORT = 10044;