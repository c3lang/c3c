module std::os::win32 @if(env::WIN32);

// See https://github.com/wine-mirror/wine/blob/master/include/winsock2.h

typedef WSAError = int;

struct Win32_pollfd
{
	Win32_SOCKET fd;
	Win32_SHORT events;
	Win32_SHORT revents;
}
alias Win32_WSAPOLLFD = Win32_pollfd;
alias Win32_PWSAPOLLFD = Win32_WSAPOLLFD*;
alias Win32_LPWSAPOLLFD = Win32_WSAPOLLFD*;

struct Win32_InAddr 
{
	union
	{
		struct s_un_b
		{
			Win32_UCHAR s_b1;
			Win32_UCHAR s_b2;
			Win32_UCHAR s_b3;
			Win32_UCHAR s_b4;
		}
		struct s_un_w
		{
			Win32_USHORT s_w1;
			Win32_USHORT s_w2;
		}
		Win32_ULONG s_addr;
	}
}

struct Win32_SOCKADDR_IN 
{
	Win32_SHORT sin_family;
	Win32_USHORT sin_port;
	Win32_InAddr sin_addr;
	Win32_CHAR[8]* sin_zero;
}

const usz _SS_PAD1SIZE = 6;
const usz _SS_PAD2SIZE = 112;

struct Win32_SOCKADDR_STORAGE 
{
	Win32_USHORT              ss_family;
	Win32_CHAR[_SS_PAD1SIZE]* __ss_pad1;
	Win32_INT64               __ss_align;
	Win32_CHAR[_SS_PAD2SIZE]* __ss_pad2;
}

alias Win32_WSAOVERLAPPED = Win32_OVERLAPPED;
alias Win32_LPWSAOVERLAPPED = Win32_WSAOVERLAPPED*;

alias Win32_LPWSAOVERLAPPED_COMPLETION_ROUTINE = fn void (
	Win32_DWORD dwError,
	Win32_DWORD cbTransferred,
	Win32_LPWSAOVERLAPPED
	lpOverlapped,
	Win32_DWORD dwFlags
);

alias Win32_LPFN_WSARECV = fn CInt(
	Win32_SOCKET socket,
	Win32_LPWSABUF buffers,
	Win32_DWORD buffer_count,
	Win32_LPDWORD bytes,
	Win32_LPDWORD flags,
	Win32_LPWSAOVERLAPPED overlapped,
	Win32_LPWSAOVERLAPPED_COMPLETION_ROUTINE completion_routine
);

alias Win32_LPFN_WSARECVFROM = fn CInt(
	Win32_SOCKET socket,
	Win32_LPWSABUF buffers,
	Win32_DWORD buffer_count,
	Win32_LPDWORD bytes,
	Win32_LPDWORD flags,
	Win32_SOCKADDR* addr,
	Win32_LPINT addr_len,
	Win32_LPWSAOVERLAPPED overlapped,
	Win32_LPWSAOVERLAPPED_COMPLETION_ROUTINE completion_routine
);

alias Win32_LPFn_CONNECTEX = fn bool(
	Win32_SOCKET,
	Win32_SOCKADDR*,
	Win32_INT,
	Win32_PVOID,
	Win32_DWORD,
	Win32_LPDWORD,
	void*
);

alias Win32_LPFn_ACCEPTEX = fn bool(
	Win32_SOCKET,
	Win32_SOCKET,
	Win32_PVOID,
	Win32_DWORD,
	Win32_DWORD,
	Win32_DWORD,
	Win32_LPDWORD,
	void*
);

const Win32_SHORT POLLERR    = 0x0001;
const Win32_SHORT POLLHUP    = 0x0002;
const Win32_SHORT POLLNVAL   = 0x0004;
const Win32_SHORT POLLWRNORM = 0x0010;
const Win32_SHORT POLLWRBAND = 0x0020;
const Win32_SHORT POLLRDNORM = 0x0100;
const Win32_SHORT POLLRDBAND = 0x0200;
const Win32_SHORT POLLPRI    = 0x0400;
const Win32_SHORT POLLIN     = POLLRDNORM | POLLRDBAND;
const Win32_SHORT POLLOUT    = POLLWRNORM;

const SD_RECEIVE = 0x00;
const SD_SEND    = 0x01;
const SD_BOTH    = 0x02;

extern fn CInt wsaPoll(Win32_LPWSAPOLLFD fdArray, Win32_ULONG fds, Win32_INT timeout) @extern("WSAPoll");
extern fn WSAError wsaGetLastError() @extern("WSAGetLastError");
extern fn void wsaSetLastError(WSAError error) @extern("WSASetLastError");
extern fn CInt wsaStartup(Win32_WORD, void*) @extern("WSAStartup");
extern fn CInt wsaCleanup() @extern("WSACleanup");

const int FIONBIO = -2147195266;
const int FIONREAD = 1074030207;
const int SIOCATMARK = 1074033415;

module std::os::win32::wsa @if(env::WIN32);

const WSAError NO_ERROR                     = 0;
const WSAError INVALID_HANDLE               = 6;
const WSAError NOT_ENOUGH_MEMORY            = 8;
const WSAError INVALID_PARAMETER            = 87;
const WSAError OPERATION_ABORTED            = 995;
const WSAError IO_INCOMPLETE                = 996;
const WSAError IO_PENDING                   = 997;
const WSAError EINTR                        = 10004;
const WSAError EBADF                        = 10009;
const WSAError EACCESS                      = 10013;
const WSAError EFAULT                       = 10014;
const WSAError EINVAL                       = 10022;
const WSAError EMFILE                       = 10024;
const WSAError EWOULDBLOCK                  = 10035;
const WSAError EINPROGRESS                  = 10036;
const WSAError EALREADY                     = 10037;
const WSAError ENOTSOCK                     = 10038;
const WSAError EDESTADDRREQ                 = 10039;
const WSAError EMSGSIZE                     = 10040;
const WSAError EPROTOTYPE                   = 10041;
const WSAError ENOPROTOOPT                  = 10042;
const WSAError EPROTONOSUPPORT              = 10043;
const WSAError ESOCKTNOSUPPORT              = 10044;
const WSAError EOPNOTSUPP                   = 10045;
const WSAError EPFNOSUPPORT                 = 10046;
const WSAError EAFNOSUPPORT                 = 10047;
const WSAError EADDRINUSE                   = 10048;
const WSAError EADDRNOTAVAIL                = 10049;
const WSAError ENETDOWN                     = 10050;
const WSAError ENETUNREACH                  = 10051;
const WSAError ENETRESET                    = 10052;
const WSAError ECONNABORTED                 = 10053;
const WSAError ECONNRESET                   = 10054;
const WSAError ENOBUFS                      = 10055;
const WSAError EISCONN                      = 10056;
const WSAError ENOTCONN                     = 10057;
const WSAError ESHUTDOWN                    = 10058;
const WSAError ETOOMANYREFS                 = 10059;
const WSAError ETIMEDOUT                    = 10060;
const WSAError ECONNREFUSED                 = 10061;
const WSAError ELOOP                        = 10062;
const WSAError ENAMETOOLONG                 = 10063;
const WSAError EHOSTDOWN                    = 10064;
const WSAError EHOSTUNREACH                 = 10065;
const WSAError ENOTEMPTY                    = 10066;
const WSAError EPROCLIM                     = 10067;
const WSAError EUSERS                       = 10068;
const WSAError EDQUOT                       = 10069;
const WSAError ESTALE                       = 10070;
const WSAError EREMOTE                      = 10071;
const WSAError SYSNOTREADY                  = 10091;
const WSAError VERNOTSUPPORTED              = 10092;
const WSAError NOTINITIALISED               = 10093;
const WSAError EDISCON                      = 10101;
const WSAError ENOMORE                      = 10102;
const WSAError ECANCELLED                   = 10103;
const WSAError EINVALIDPROCTABLE            = 10104;
const WSAError EINVALIDPROVIDER             = 10105;
const WSAError EPROVIDERFAILEDINIT          = 10106;
const WSAError SYSCALLFAILURE               = 10107;
const WSAError SERVICE_NOT_FOUND            = 10108;
const WSAError TYPE_NOT_FOUND               = 10109;
const WSAError E_NO_MORE                    = 10110;
const WSAError E_CANCELLED                  = 10111;
const WSAError REFUSED                      = 10112;
const WSAError HOST_NOT_FOUND               = 11001;
const WSAError TRY_AGAIN                    = 11002;
const WSAError NO_RECOVERY                  = 11003;
const WSAError NO_DATA                      = 11004;
const WSAError QOS_RECEIVERS                = 11005;
const WSAError QOS_SENDERS                  = 11006;
const WSAError QOS_NO_SENDERS               = 11007;
const WSAError QOS_NO_RECEIVERS             = 11008;
const WSAError QOS_REQUEST_CONFIRMED        = 11009;
const WSAError QOS_ADMISSION_FAILURE        = 11010;
const WSAError QOS_POLICY_FAILURE           = 11011;
const WSAError QOS_BAD_STYLE                = 11012;
const WSAError QOS_BAD_OBJECT               = 11013;
const WSAError QOS_TRAFFIC_CTRL_ERROR       = 11014;
const WSAError QOS_GENERIC_ERROR            = 11015;
const WSAError QOS_ESERVICETYPE             = 11016;
const WSAError QOS_EFLOWSPEC                = 11017;
const WSAError QOS_EPROVSPECBUF             = 11018;
const WSAError QOS_EFILTERSTYLE             = 11019;
const WSAError QOS_EFILTERTYPE              = 11020;
const WSAError QOS_EFILTERCOUNT             = 11021;
const WSAError QOS_EOBJLENGTH               = 11022;
const WSAError QOS_EFLOWCOUNT               = 11023;
const WSAError QOS_EUNKOWNPSOBJ             = 11024;
const WSAError QOS_EPOLICYOBJ               = 11025;
const WSAError QOS_EFLOWDESC                = 11026;
const WSAError QOS_EPSFLOWSPEC              = 11027;
const WSAError QOS_EPSFILTERSPEC            = 11028;
const WSAError QOS_ESDMODEOBJ               = 11029;
const WSAError QOS_ESHAPERATEOBJ            = 11030;
const WSAError QOS_RESERVED_PETYPE          = 11031;

