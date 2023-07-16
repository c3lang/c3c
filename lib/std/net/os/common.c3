module std::net::os;
const bool SUPPORTS_INET = env::LIBC && (env::WIN32 || env::DARWIN || env::LINUX);

def Socklen_t = CUInt @if(!env::WIN32);
def Socklen_t = usz @if(env::WIN32);

def SockAddrPtr = distinct void*;

struct AddrInfo
{
	CInt ai_flags;
	CInt ai_family;
	CInt ai_socktype;
	CInt ai_protocol;
	Socklen_t ai_addrlen;
	struct @if(env::WIN32 || env::DARWIN)
	{
		ZString ai_canonname;
        SockAddrPtr ai_addr;
	}
	struct @if(env::LINUX)
	{
        SockAddrPtr ai_addr;
		ZString ai_canonname;
	}
	AddrInfo* ai_next;
}

const PLATFORM_O_NONBLOCK @if(!$defined(PLATFORM_O_NONBLOCK)) = 0;

const int PLATFORM_AF_UNIX = 1;
const int PLATFORM_AF_INET = 2;
const int PLATFORM_AF_INET6 @if(env::WIN32) = 23;
const int PLATFORM_AF_INET6 @if(env::DARWIN) = 30;
const int PLATFORM_AF_INET6 @if(env::LINUX) = 10;

const AI_PASSIVE = 0x1;
const AI_CANONNAME = 0x2;
const AI_NUMERICHOST = 0x4;

const int AF_UNSPEC = 0;
const int AF_INET @if(SUPPORTS_INET) = PLATFORM_AF_INET;
const int AF_APPLETALK @if(SUPPORTS_INET) = PLATFORM_AF_APPLETALK;
const int AF_IPX @if(SUPPORTS_INET) = PLATFORM_AF_IPX;
const int AF_INET6 @if(SUPPORTS_INET) = PLATFORM_AF_INET6;

const O_NONBLOCK = PLATFORM_O_NONBLOCK;

extern fn CInt getaddrinfo(ZString nodename, ZString servname, AddrInfo* hints, AddrInfo** res) @if(SUPPORTS_INET);
extern fn void freeaddrinfo(AddrInfo* res) @if(SUPPORTS_INET);
extern fn CInt setsockopt(NativeSocket socket, CInt level, CInt optname, void* optval, Socklen_t optlen) @if(SUPPORTS_INET);
