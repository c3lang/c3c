module std::net::os;

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

const bool SUPPORTS_INET = env::WIN32 || env::DARWIN || env::LINUX;

const PLATFORM_O_NONBLOCK @if(!$defined(PLATFORM_O_NONBLOCK)) = 0;

const AI_PASSIVE = 0x1;
const AI_CANONNAME = 0x2;
const AI_NUMERICHOST = 0x4;

const int AF_UNSPEC = 0;
const int AF_INET @if(SUPPORTS_INET) = PLATFORM_AF_INET;
const int AF_APPLETALK @if(SUPPORTS_INET) = PLATFORM_AF_APPLETALK;
const int AF_IPX @if(SUPPORTS_INET) = PLATFORM_AF_IPX;
const int AF_INET6 @if(SUPPORTS_INET) = PLATFORM_AF_INET6;

const O_NONBLOCK = PLATFORM_O_NONBLOCK;



