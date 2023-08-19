module std::net::os;
const bool SUPPORTS_INET = env::LIBC && (env::WIN32 || env::DARWIN || env::LINUX);

def AIFamily = distinct CInt;
def AIProtocol = distinct CInt;
def AISockType = distinct CInt;
def AIFlags = distinct CInt;

def Socklen_t = CUInt @if(!env::WIN32);
def Socklen_t = usz @if(env::WIN32);

def SockAddrPtr = distinct void*;

struct AddrInfo
{
	AIFlags ai_flags;
	AIFamily ai_family;
	AISockType ai_socktype;
	AIProtocol ai_protocol;
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

const AISockType SOCK_STREAM                    = 1; // Stream
const AISockType SOCK_DGRAM                     = 2; // Datagram
const AISockType SOCK_RAW                       = 3; // Raw
const AISockType SOCK_RDM                       = 4; // Reliably delivered
const AISockType SOCK_SEQPACKET                 = 5; // Sequential packet

const AIFlags AI_PASSIVE                        = 0x1;
const AIFlags AI_CANONNAME                      = 0x2;
const AIFlags AI_NUMERICHOST                    = 0x4;

const AIFamily AF_UNSPEC                        = 0;
const AIFamily AF_UNIX                          = 1;
const AIFamily AF_INET                          = 2;
const AIFamily AF_INET6                         = PLATFORM_AF_INET6;
const AIFamily AF_IPX                           = PLATFORM_AF_IPX;
const AIFamily AF_APPLETALK                     = PLATFORM_AF_APPLETALK;

const O_NONBLOCK = PLATFORM_O_NONBLOCK;

extern fn CInt getaddrinfo(ZString nodename, ZString servname, AddrInfo* hints, AddrInfo** res) @if(SUPPORTS_INET);
extern fn void freeaddrinfo(AddrInfo* res) @if(SUPPORTS_INET);
extern fn CInt setsockopt(NativeSocket socket, CInt level, CInt optname, void* optval, Socklen_t optlen) @if(SUPPORTS_INET);
extern fn CInt getsockopt(NativeSocket socket, CInt level, CInt optname, void* optval, Socklen_t optlen) @if(SUPPORTS_INET);

module std::net::os @if(!env::LIBC || !(env::WIN32 || env::DARWIN || env::LINUX));

const AIFamily PLATFORM_AF_INET6        = 0;
const AIFamily PLATFORM_AF_IPX          = 0;
const AIFamily PLATFORM_AF_APPLETALK    = 0;
