module std::net::os;

$if env::os_is_win32():

const int PLATFORM_AF_INET = 1;
const int PLATFORM_AF_IPX = 6;
const int PLATFORM_AF_APPLETALK = 16;
const int PLATFORM_AF_NETBIOS = 17;
const int PLATFORM_AF_INET6 = 23;
const int PLATFORM_AF_IRDA = 26;
const int PLATFORM_AF_BTH = 32;

struct AddrInfo
{
	int ai_flags;
	int ai_family;
	int ai_socktype;
	int ai_protocol;
	usz ai_addrlen;
	char* ai_canonname;
    void* ai_addr;
    AddrInfo* ai_next;
}

def NativeSocket = distinct uptr;

extern fn int wsa_startup(int, void*) @extern("WSAStartup");
extern fn int ioctlsocket(NativeSocket, long cmd, ulong *argp);
extern fn int closesocket(NativeSocket);

macro NativeSocket.close(NativeSocket this) => closesocket(this);

$endif