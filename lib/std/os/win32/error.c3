module std::os::win32::wsa;

$if (env::OS_TYPE == OsType.WIN32):
extern fn int get_last_error() @extname("WSAGetLastError");
extern fn void set_last_error(int error) @extname("WSASetLastError");

const int INVALID_HANDLE = 6;
const int NOT_ENOUGHT_MEMORY = 8;
const int INVALID_PARAMETER = 87;
const int OPERATION_ABORTED = 995;
const int IO_INCOMPLETE = 996;
const int IO_PENDING = 997;
const int EINTR = 10004;
const int EBADF = 10009;
const int ACCESS = 10013;
const int EFAULT = 10014;
const int EINVAL = 10022;
const int EMFILE = 10024;
const int EWOULDBLOCK = 10035;
const int EINPROGRESS = 10036;
const int EALREADY = 10037;
const int ENOTSOCK = 10038;
const int EDESTADDRREQ = 10039;
const int EMSGSIZE = 10040;
const int EPROTOTYPE = 10041;
const int ENOPROTOOPT = 10042;
const int EPROTONOSUPPORT = 10043;
const int ESOCKTNOSUPPORT = 10044;
$endif;