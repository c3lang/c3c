module std::net::os;
import libc;

$if (env::OS_TYPE == OsType.LINUX):

struct AddrInfo
{
	int ai_flags;
	int ai_family;
	int ai_socktype;
	int ai_protocol;
	usz ai_addrlen;
    void* ai_addr;
	char* ai_canonname;
	AddrInfo* ai_next;
}

const int PLATFORM_AF_UNIX = 1;
const int PLATFORM_AF_INET = 2;
const int PLATFORM_AF_AX25 = 3;
const int PLATFORM_AF_IPX = 4;
const int PLATFORM_AF_APPLETALK = 5;
const int PLATFORM_AF_NETROM = 6;
const int PLATFORM_AF_BRIDGE = 7;
const int PLATFORM_AF_AAL5 = 8;
const int PLATFORM_AF_X25 = 9;
const int PLATFORM_AF_INET6 = 10;

const PLATFORM_O_NONBLOCK = 0o4000;

$endif;