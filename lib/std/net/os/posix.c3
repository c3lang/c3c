module std::net::os @if(env::POSIX && SUPPORTS_INET);
import std::io, libc;

const int F_GETFL = 3;
const int F_SETFL = 4;

typedef NativeSocket = inline Fd;

struct Posix_pollfd
{
	CInt fd;
	CUShort events;
	CUShort revents;
}

alias Posix_nfds_t = CUInt;

extern fn CInt connect(NativeSocket socket, SockAddrPtr address, Socklen_t address_len);
extern fn NativeSocket socket(AIFamily af, AISockType type, AIProtocol ip_protocol);
extern fn int fcntl(NativeSocket socket, int cmd, ...);
extern fn CInt bind(NativeSocket socket, SockAddrPtr address, Socklen_t address_len);
extern fn CInt listen(NativeSocket socket, CInt backlog);
extern fn NativeSocket accept(NativeSocket socket, SockAddrPtr address, Socklen_t* address_len);
extern fn CInt poll(Posix_pollfd* fds, Posix_nfds_t nfds, CInt timeout);
extern fn CInt socketpair(AIFamily domain, AISockType type, CInt protocol, NativeSocket[2]* sv);

const CUShort POLLIN      = 0x0001;
const CUShort POLLPRI     = 0x0002;
const CUShort POLLOUT     = 0x0004;
const CUShort POLLERR     = 0x0008;
const CUShort POLLHUP     = 0x0010;
const CUShort POLLNVAL    = 0x0020;

fn fault convert_error(Errno error)
{
	switch (error)
	{
		case errno::EACCES: return io::NO_PERMISSION;
		case errno::EADDRINUSE: return net::ADDRESS_IN_USE;
		case errno::EALREADY: return net::CONNECTION_ALREADY_IN_PROGRESS;
		case errno::EBADF: return net::BAD_SOCKET_DESCRIPTOR;
		case errno::ECONNREFUSED: return net::CONNECTION_REFUSED;
		case errno::ECONNRESET: return net::CONNECTION_RESET;
		case errno::EISCONN: return net::ALREADY_CONNECTED;
		case errno::ENETUNREACH: return net::NETWORK_UNREACHABLE;
		case errno::ENOTSOCK: return net::NOT_A_SOCKET;
		case errno::EINTR: return io::INTERRUPTED;
		case errno::EWOULDBLOCK: return io::WOULD_BLOCK;
		case errno::EOPNOTSUPP: return net::OPERATION_NOT_SUPPORTED_ON_SOCKET;
		case errno::ETIMEDOUT: return net::CONNECTION_TIMED_OUT;
		default: return io::GENERAL_ERROR;
	}
}

fn fault socket_error()
{
	return convert_error(libc::errno());
}

macro bool NativeSocket.is_valid(self)
{
	return (iptr)self >= 0;
}

macro void? NativeSocket.close(self)
{
	if (libc::close(self))
	{
		if (libc::errno() == errno::EBADF) return net::INVALID_SOCKET?;
		return net::GENERAL_ERROR?;
	}
}

macro void? NativeSocket.set_non_blocking(self, bool non_blocking)
{
	int flags = fcntl(self, F_GETFL, 0);
	if (non_blocking)
	{
		if (flags & O_NONBLOCK) return;
		flags |= O_NONBLOCK;
	}
	else
	{
		if (!(flags & O_NONBLOCK)) return;
		flags &= ~(int)O_NONBLOCK;
	}
	if (fcntl(self, F_SETFL, flags) == -1)
	{
		if (libc::errno() == errno::EBADF) return net::INVALID_SOCKET?;
		return net::GENERAL_ERROR?;
	}
}

macro bool NativeSocket.is_non_blocking(self)
{
	return fcntl(self, F_GETFL, 0) & O_NONBLOCK != 0;
}
