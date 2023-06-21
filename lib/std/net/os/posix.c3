module std::net::os @if(env::POSIX && SUPPORTS_INET);
import libc;

const int F_GETFL = 3;
const int F_SETFL = 4;

def NativeSocket = distinct inline Fd;

extern fn NativeSocket socket(int af, int type, int protocol) @extern("socket");
extern fn int getaddrinfo(ZString nodename, ZString servname, AddrInfo* hints, AddrInfo** res);
extern fn void freeaddrinfo(AddrInfo* addr);
extern fn int connect(NativeSocket, void*, usz);
extern fn int fcntl(NativeSocket socket, int cmd, ...) @extern("fcntl");

macro void! NativeSocket.close(NativeSocket this)
{
	if (libc::close(this))
	{
		if (libc::errno() == errno::EBADF) return NetError.INVALID_SOCKET?;
		return NetError.GENERAL_ERROR?;
	}
}

macro void! NativeSocket.set_non_blocking(NativeSocket this)
{
	int flags = fcntl(this, F_GETFL, 0);
    if (fcntl(this, F_SETFL, flags | O_NONBLOCK) == -1)
	{
		if (libc::errno() == errno::EBADF) return NetError.INVALID_SOCKET?;
		return NetError.GENERAL_ERROR?;
	}
}

macro bool NativeSocket.is_non_blocking(NativeSocket this)
{
	return fcntl(this, F_GETFL, 0) & O_NONBLOCK == O_NONBLOCK;
}