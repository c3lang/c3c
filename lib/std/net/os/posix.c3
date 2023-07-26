module std::net::os @if(env::POSIX && SUPPORTS_INET);
import libc;

const int F_GETFL = 3;
const int F_SETFL = 4;

def NativeSocket = distinct inline Fd;

extern fn CInt connect(NativeSocket socket, SockAddrPtr address, Socklen_t address_len);
extern fn NativeSocket socket(CInt af, CInt type, CInt protocol) @extern("socket");
extern fn int fcntl(NativeSocket socket, int cmd, ...) @extern("fcntl");
extern fn CInt bind(NativeSocket socket, SockAddrPtr address, Socklen_t address_len) @extern("bind");
extern fn CInt listen(NativeSocket socket, CInt backlog) @extern("listen");
extern fn NativeSocket accept(NativeSocket socket, SockAddrPtr address, Socklen_t* address_len) @extern("accept");

macro void! NativeSocket.close(self)
{
	if (libc::close(self))
	{
		if (libc::errno() == errno::EBADF) return NetError.INVALID_SOCKET?;
		return NetError.GENERAL_ERROR?;
	}
}

macro void! NativeSocket.set_non_blocking(self)
{
	int flags = fcntl(self, F_GETFL, 0);
	if (fcntl(self, F_SETFL, flags | O_NONBLOCK) == -1)
	{
		if (libc::errno() == errno::EBADF) return NetError.INVALID_SOCKET?;
		return NetError.GENERAL_ERROR?;
	}
}

macro bool NativeSocket.is_non_blocking(self)
{
	return fcntl(self, F_GETFL, 0) & O_NONBLOCK == O_NONBLOCK;
}
