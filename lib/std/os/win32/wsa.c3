module std::os::win32 @if(env::WIN32) @link("ws2_32");

// See https://github.com/wine-mirror/wine/blob/master/include/winsock2.h

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

extern fn CInt wsaPoll(Win32_LPWSAPOLLFD fdArray, Win32_ULONG fds, Win32_INT timeout) @cname("WSAPoll");
extern fn WSAError wsaGetLastError() @cname("WSAGetLastError");
extern fn void wsaSetLastError(WSAError error) @cname("WSASetLastError");
extern fn CInt wsaStartup(Win32_WORD, void*) @cname("WSAStartup");
extern fn CInt wsaCleanup() @cname("WSACleanup");

const int FIONBIO = -2147195266;
const int FIONREAD = 1074030207;
const int SIOCATMARK = 1074033415;

module std::os::win32::wsa @if(env::WIN32);

constdef WSAError : int
{
	NO_ERROR                     = 0,
	INVALID_HANDLE               = 6,
	NOT_ENOUGH_MEMORY            = 8,
	INVALID_PARAMETER            = 87,
	OPERATION_ABORTED            = 995,
	IO_INCOMPLETE                = 996,
	IO_PENDING                   = 997,
	EINTR                        = 10004,
	EBADF                        = 10009,
	EACCESS                      = 10013,
	EFAULT                       = 10014,
	EINVAL                       = 10022,
	EMFILE                       = 10024,
	EWOULDBLOCK                  = 10035,
	EINPROGRESS                  = 10036,
	EALREADY                     = 10037,
	ENOTSOCK                     = 10038,
	EDESTADDRREQ                 = 10039,
	EMSGSIZE                     = 10040,
	EPROTOTYPE                   = 10041,
	ENOPROTOOPT                  = 10042,
	EPROTONOSUPPORT              = 10043,
	ESOCKTNOSUPPORT              = 10044,
	EOPNOTSUPP                   = 10045,
	EPFNOSUPPORT                 = 10046,
	EAFNOSUPPORT                 = 10047,
	EADDRINUSE                   = 10048,
	EADDRNOTAVAIL                = 10049,
	ENETDOWN                     = 10050,
	ENETUNREACH                  = 10051,
	ENETRESET                    = 10052,
	ECONNABORTED                 = 10053,
	ECONNRESET                   = 10054,
	ENOBUFS                      = 10055,
	EISCONN                      = 10056,
	ENOTCONN                     = 10057,
	ESHUTDOWN                    = 10058,
	ETOOMANYREFS                 = 10059,
	ETIMEDOUT                    = 10060,
	ECONNREFUSED                 = 10061,
	ELOOP                        = 10062,
	ENAMETOOLONG                 = 10063,
	EHOSTDOWN                    = 10064,
	EHOSTUNREACH                 = 10065,
	ENOTEMPTY                    = 10066,
	EPROCLIM                     = 10067,
	EUSERS                       = 10068,
	EDQUOT                       = 10069,
	ESTALE                       = 10070,
	EREMOTE                      = 10071,
	SYSNOTREADY                  = 10091,
	VERNOTSUPPORTED              = 10092,
	NOTINITIALISED               = 10093,
	EDISCON                      = 10101,
	ENOMORE                      = 10102,
	ECANCELLED                   = 10103,
	EINVALIDPROCTABLE            = 10104,
	EINVALIDPROVIDER             = 10105,
	EPROVIDERFAILEDINIT          = 10106,
	SYSCALLFAILURE               = 10107,
	SERVICE_NOT_FOUND            = 10108,
	TYPE_NOT_FOUND               = 10109,
	E_NO_MORE                    = 10110,
	E_CANCELLED                  = 10111,
	REFUSED                      = 10112,
	HOST_NOT_FOUND               = 11001,
	TRY_AGAIN                    = 11002,
	NO_RECOVERY                  = 11003,
	NO_DATA                      = 11004,
	QOS_RECEIVERS                = 11005,
	QOS_SENDERS                  = 11006,
	QOS_NO_SENDERS               = 11007,
	QOS_NO_RECEIVERS             = 11008,
	QOS_REQUEST_CONFIRMED        = 11009,
	QOS_ADMISSION_FAILURE        = 11010,
	QOS_POLICY_FAILURE           = 11011,
	QOS_BAD_STYLE                = 11012,
	QOS_BAD_OBJECT               = 11013,
	QOS_TRAFFIC_CTRL_ERROR       = 11014,
	QOS_GENERIC_ERROR            = 11015,
	QOS_ESERVICETYPE             = 11016,
	QOS_EFLOWSPEC                = 11017,
	QOS_EPROVSPECBUF             = 11018,
	QOS_EFILTERSTYLE             = 11019,
	QOS_EFILTERTYPE              = 11020,
	QOS_EFILTERCOUNT             = 11021,
	QOS_EOBJLENGTH               = 11022,
	QOS_EFLOWCOUNT               = 11023,
	QOS_EUNKOWNPSOBJ             = 11024,
	QOS_EPOLICYOBJ               = 11025,
	QOS_EFLOWDESC                = 11026,
	QOS_EPSFLOWSPEC              = 11027,
	QOS_EPSFILTERSPEC            = 11028,
	QOS_ESDMODEOBJ               = 11029,
	QOS_ESHAPERATEOBJ            = 11030,
	QOS_RESERVED_PETYPE          = 11031,
}
