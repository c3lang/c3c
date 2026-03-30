module std::net::os @if(env::WIN32);
import std::os, std::io, libc, std::thread;
import std::core::mem;
import std::os::win32;

const AIFamily PLATFORM_AF_IPX          = (AIFamily)6;
const AIFamily PLATFORM_AF_APPLETALK    = (AIFamily)16;
const AIFamily PLATFORM_AF_NETBIOS      = (AIFamily)17;
const AIFamily PLATFORM_AF_INET6        = (AIFamily)23;
const AIFamily PLATFORM_AF_IRDA         = (AIFamily)26;
const AIFamily PLATFORM_AF_BTH          = (AIFamily)32;

const int FIONREAD = 1074030207;
const int FIONBIO = -2147195266;
const int FIOASYNC = -2147195267;

typedef NativeSocket = inline Win32_SOCKET;

extern fn CInt ioctlsocket(NativeSocket, CLong cmd, CULong *argp);
extern fn WSAError closesocket(NativeSocket);
extern fn NativeSocket socket(AIFamily af, AISockType type, AIProtocol ip_protocol);
extern fn int connect(NativeSocket, SockAddrPtr address, Socklen_t address_len);
extern fn int bind(NativeSocket, SockAddrPtr address, Socklen_t address_len);
extern fn int listen(NativeSocket, int backlog);
extern fn NativeSocket accept(NativeSocket, SockAddrPtr address, Socklen_t* address_len);
extern fn CInt getsockname(NativeSocket socket, SockAddrPtr address, Socklen_t* address_len);

char[408] wsa_data @local;
int wsa_init @local;

macro void? start_wsa()
{
	if (mem::compare_exchange(&wsa_init, 0, 1) == 0)
	{
		Win32_WORD version = 0x0202;
	    CInt wsa_error = win32::wsaStartup(version, &wsa_data);
		if (wsa_error > 0)
		{
			mem::@atomic_store(wsa_init, 0);
			return os::socket_error()~;
		}
	}
}

fn void close_wsa()
{
	if (mem::compare_exchange(&wsa_init, 1, 0) == 1)
	{
		win32::wsaCleanup();
		mem::@atomic_store(wsa_init, 0);
	}
}

macro bool NativeSocket.is_valid(self)
{
	return self != (NativeSocket)(uptr)-1;
}

fn void? NativeSocket.set_non_blocking(self, bool non_blocking)
{
	if (ioctlsocket(self, win32::FIONBIO, &&(CULong)non_blocking))
	{
		return socket_error()~;
	}
}

macro void? NativeSocket.close(self)
{
	WSAError error = closesocket(self);
	if (error) return convert_error(error)~;
}

// https://github.com/wine-mirror/wine/blob/master/include/winsock.h
const int SOL_SOCKET              = 0xffff;
const int SO_DEBUG                = 0x0001;
const int SO_ACCEPTCONN           = 0x0002;
const int SO_REUSEADDR            = 0x0004;
const int SO_KEEPALIVE            = 0x0008;
const int SO_DONTROUTE            = 0x0010;
const int SO_BROADCAST            = 0x0020;
const int SO_USELOOPBACK          = 0x0040;
const int SO_LINGER               = 0x0080;
const int SO_OOBINLINE            = 0x0100;
const int SO_SNDBUF               = 0x1001;
const int SO_RCVBUF               = 0x1002;
const int SO_SNDLOWAT             = 0x1003;
const int SO_RCVLOWAT             = 0x1004;
const int SO_SNDTIMEO             = 0x1005;
const int SO_RCVTIMEO             = 0x1006;
const int SO_ERROR                = 0x1007;
const int SO_TYPE                 = 0x1008;

fn fault convert_error(WSAError error)
{
	switch (error)
	{
		case NOTINITIALISED: return net::SOCKETS_NOT_INITIALIZED;
		case ENETDOWN: return net::NETWORK_UNREACHABLE;
		case INVALID_HANDLE: return net::BAD_SOCKET_DESCRIPTOR;
		case EACCESS: return io::NO_PERMISSION;
		case EINPROGRESS: return net::STILL_PROCESSING_CALLBACK;
		case EADDRINUSE: return net::ADDRESS_IN_USE;
		case EALREADY: return net::CONNECTION_ALREADY_IN_PROGRESS;
		case EBADF: return net::BAD_SOCKET_DESCRIPTOR;
		case EINTR: return io::INTERRUPTED;
		case EWOULDBLOCK: return io::WOULD_BLOCK;
		case ECONNREFUSED: return net::CONNECTION_REFUSED;
		case EISCONN: return net::ALREADY_CONNECTED;
		case ENETUNREACH: return net::NETWORK_UNREACHABLE;
		case ENOTSOCK: return net::NOT_A_SOCKET;
		case EOPNOTSUPP: return net::OPERATION_NOT_SUPPORTED_ON_SOCKET;
		case ETIMEDOUT: return net::CONNECTION_TIMED_OUT;
		case ECONNRESET: return net::CONNECTION_RESET;
		default: return io::GENERAL_ERROR;
	}
}

fn fault socket_error()
{
	return convert_error(win32::wsaGetLastError());
}

const CUShort POLLIN      = win32::POLLIN;
const CUShort POLLPRI     = win32::POLLPRI;
const CUShort POLLOUT     = win32::POLLOUT;
const CUShort POLLERR     = win32::POLLERR;
const CUShort POLLHUP     = win32::POLLHUP;
const CUShort POLLNVAL    = win32::POLLNVAL;
const CUShort POLLRDNORM  = win32::POLLRDNORM;
const CUShort POLLRDBAND  = win32::POLLRDBAND;
const CUShort POLLWRNORM  = win32::POLLWRNORM;
const CUShort POLLWRBAND  = win32::POLLWRBAND;

const int MSG_PEEK = 0x0002;