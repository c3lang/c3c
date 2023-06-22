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

macro NativeSocket.close(NativeSocket this) => closesocket(this);
