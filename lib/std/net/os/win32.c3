module std::net::os @if(env::WIN32);

const int PLATFORM_AF_IPX = 6;
const int PLATFORM_AF_APPLETALK = 16;
const int PLATFORM_AF_NETBIOS = 17;
const int PLATFORM_AF_IRDA = 26;
const int PLATFORM_AF_BTH = 32;

def NativeSocket = distinct uptr;

extern fn int wsa_startup(int, void*) @extern("WSAStartup");
extern fn int ioctlsocket(NativeSocket, long cmd, ulong *argp);
extern fn int closesocket(NativeSocket);
extern fn NativeSocket socket(int af, int type, int protocol);
extern fn int connect(NativeSocket, SockAddrPtr address, Socklen_t address_len);
extern fn int bind(NativeSocket, SockAddrPtr address, Socklen_t address_len);
extern fn int listen(NativeSocket, int backlog);
extern fn NativeSocket accept(NativeSocket, SockAddrPtr address, Socklen_t* address_len);

macro NativeSocket.close(self)
{
    if (int err = closesocket(self))
    {
        if (err == WSAENOTSOCK) return NetError.INVALID_SOCKET?;
		return NetError.GENERAL_ERROR?;
    }
}

const int WSAENOTSOCK = 10038;

// https://github.com/wine-mirror/wine/blob/master/include/winsock.h
const int SOL_SOCKET = 0xffff;
const int SO_REUSEADDR = 0x0004;
const int SO_KEEPALIVE = 0x0008;
const int SO_BROADCAST = 0x0200;