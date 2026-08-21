module std::net @feat(WIN32, POSIX);
import std::io, std::os, std::time, libc;

struct Socket (InStream, OutStream)
{
	NativeSocket sock;
	Socklen_t addrlen;
	// TODO proper way to get the size of sockaddr_storage
	// https://learn.microsoft.com/en-us/previous-versions/windows/desktop/legacy/ms740504(v=vs.85)
	char[128] addr_storage;
}

<*
 Loop over sockets. Caller must close unused sockets

 @deprecated "use @loop_over_addresses"
*>
macro void @loop_over_ai(AddrInfo* ai; @body(NativeSocket fd, AddrInfo* ai))
{
	while (ai)
	{
		NativeSocket sockfd = os::socket(ai.ai_family, ai.ai_socktype, ai.ai_protocol);
		if (sockfd.is_valid())
		{
			@body(sockfd, ai);
		}
		ai = ai.ai_next;
	}
}

<*
 Loop over sockets. Caller should set *retain_socket = true, when creating a new socket.
*>
macro void @loop_over_addresses(AddrInfo* ai; @body(NativeSocket fd, AddrInfo* ai, bool* retain_socket))
{
	while (ai)
	{
		NativeSocket sockfd = os::socket(ai.ai_family, ai.ai_socktype, ai.ai_protocol);
		bool retain_socket = false;
		defer if (!retain_socket) (void)sockfd.close();
		if (sockfd.is_valid())
		{
			@body(sockfd, ai, &retain_socket);
		}
		ai = ai.ai_next;
	}
}

const Duration POLL_FOREVER = (Duration)-1;

constset PollSubscribe : ushort
{
	ANY_READ     = os::POLLIN,
	PRIO_READ    = os::POLLPRI,
	OOB_READ     = os::POLLRDBAND,
	READ         = os::POLLRDNORM,
	ANY_WRITE    = os::POLLOUT,
	OOB_WRITE    = os::POLLWRBAND,
	WRITE        = os::POLLWRNORM,
}

constset PollEvent : ushort
{
	READ_PRIO  = os::POLLPRI,
	READ_OOB   = os::POLLRDBAND,
	READ       = os::POLLRDNORM,
	WRITE_OOB  = os::POLLWRBAND,
	WRITE      = os::POLLWRNORM,
	DISCONNECT = os::POLLHUP,
	ERROR      = os::POLLERR,
	INVALID    = os::POLLNVAL,
}

struct Poll
{
	NativeSocket socket;
	PollSubscribe events;
	PollEvent revents;
}

<*
 @param [inout] polls
 @param timeout : "duration to poll (clamped to Cint::max ms), or POLL_FOREVER."
*>
fn ulong? poll(Poll[] polls, Duration timeout)
{
	return poll_ms(polls, timeout == POLL_FOREVER ? -1 : timeout.to_ms()) @inline;
}

<*
 @param [inout] polls
 @param timeout_ms : "duration to poll in ms or -1. Clamped to Cint::max"
*>
fn ulong? poll_ms(Poll[] polls, long timeout_ms)
{
	if (timeout_ms > CInt::max) timeout_ms = CInt::max;
	$if env::WIN32:
		CInt result = win32::wsaPoll((Win32_LPWSAPOLLFD)polls.ptr, (Win32_ULONG)polls.len, (CInt)timeout_ms);
	$else
		CInt result = os::poll((Posix_pollfd*)polls.ptr, (Posix_nfds_t)polls.len, (CInt)timeout_ms);
	$endif
	return result < 0 ? os::socket_error()~ : (ulong)result;
}

macro Socket new_socket(fd, ai)
{
	Socket sock = { .sock = fd, .addrlen = ai.ai_addrlen };
	assert(sock.addr_storage.len >= ai.ai_addrlen, "storage %d < addrlen %d", sock.addr_storage.len, ai.ai_addrlen);
	mem::copy(&sock.addr_storage, (void*)ai.ai_addr, (sz)ai.ai_addrlen);
	return sock;
}

enum SocketOption : char (CInt value)
{
	REUSEADDR                  { os::SO_REUSEADDR },
	REUSEPORT @feat(!WIN32) { os::SO_REUSEPORT },
	KEEPALIVE                  { os::SO_KEEPALIVE },
	BROADCAST                  { os::SO_BROADCAST },
	OOBINLINE                  { os::SO_OOBINLINE },
	DONTROUTE                  { os::SO_DONTROUTE },
}

fn bool? Socket.get_broadcast(&self) => self.get_option(BROADCAST);
fn bool? Socket.get_keepalive(&self) => self.get_option(KEEPALIVE);
fn bool? Socket.get_reuseaddr(&self) => self.get_option(REUSEADDR);
fn bool? Socket.get_dontroute(&self) => self.get_option(DONTROUTE);
fn bool? Socket.get_oobinline(&self) => self.get_option(OOBINLINE);

fn void? Socket.set_broadcast(&self, bool value) => self.set_option(BROADCAST, value);
fn void? Socket.set_keepalive(&self, bool value) => self.set_option(KEEPALIVE, value);
fn void? Socket.set_reuseaddr(&self, bool value) => self.set_option(REUSEADDR, value);
fn void? Socket.set_dontroute(&self, bool value) => self.set_option(DONTROUTE, value);
fn void? Socket.set_oobinline(&self, bool value) => self.set_option(OOBINLINE, value);

fn void? Socket.set_option(&self, SocketOption option, bool value)
{
	CInt flag = (CInt)value;
	int errcode = os::setsockopt(self.sock, os::SOL_SOCKET, option.value, &flag, CInt::size);
	if (errcode != 0) return SOCKOPT_FAILED~;
}

fn bool? Socket.get_option(&self, SocketOption option)
{
	CInt flag;
	Socklen_t socklen = CInt::size;
	int errcode = os::getsockopt(self.sock, os::SOL_SOCKET, option.value, &flag, &socklen);
	if (errcode != 0) return SOCKOPT_FAILED~;
	return (bool)flag;
}

fn sz? Socket.read(&self, char[] bytes) @dynamic
{
$if env::WIN32:
	sz n = libc::recv(self.sock, bytes.ptr, (int)bytes.len, 0);
$else
	sz n = libc::recv(self.sock, bytes.ptr, (usz)bytes.len, 0);
$endif
	if (n < 0) return os::socket_error()~;
	return (sz)n;
}

fn char? Socket.read_byte(&self) @dynamic => io::read_byte_using_read(self);

fn sz? Socket.write(&self, char[] bytes) @dynamic
{
$if env::WIN32:
	sz n = libc::send(self.sock, bytes.ptr, (int)bytes.len, 0);
$else
	sz n = libc::send(self.sock, bytes.ptr, (usz)bytes.len, 0);
$endif
	if (n < 0) return os::socket_error()~;
	return (sz)n;
}

fn void? Socket.write_byte(&self, char byte) @dynamic => io::write_byte_using_write(self, byte);

fn void? Socket.destroy(&self) @dynamic
{
	self.close()!;
}
fn void? Socket.close(&self) @inline @dynamic
{
	self.sock.close()!;
}

fn sz? Socket.peek(&self, char[] bytes) @dynamic
{
  $if env::WIN32:
	  sz n = libc::recv(self.sock, bytes.ptr, (int)bytes.len, os::MSG_PEEK);
  $else
	  sz n = libc::recv(self.sock, bytes.ptr, (usz)bytes.len, os::MSG_PEEK);
  $endif
	if (n < 0) return os::socket_error()~;
	return (sz)n;
}

enum SocketShutdownHow : (CInt native_value)
{
	RECEIVE { env::WIN32 ??? libc::SD_RECEIVE : libc::SHUT_RD },
	SEND    { env::WIN32 ??? libc::SD_SEND : libc::SHUT_WR    },
	BOTH    { env::WIN32 ??? libc::SD_BOTH : libc::SHUT_RDWR  },
}

fn void? Socket.shutdown(&self, SocketShutdownHow how)
{
	if (libc::shutdown(self.sock, how.native_value) < 0)
	{
		return os::socket_error()~;
	}
}

<*
 Returns the InetAddress of the remote peer connected to this socket.
*>
fn InetAddress? Socket.peer_address(&self)
{
	return decode_sockaddr(_getpeername(self));
}

<*
 Returns the port number of the remote peer connected to this socket.
*>
fn ushort? Socket.peer_port(&self)
{
	return decode_port(_getpeername(self));
}

<*
 Returns the local InetAddress bound to this socket.
*>
fn InetAddress? Socket.local_address(&self)
{
	return decode_sockaddr(_getsockname(self));
}

<*
 Returns the local port bound to this socket.
*>
fn ushort? Socket.local_port(&self)
{
	return decode_port(_getsockname(self));
}

macro char[]? _getsockname(Socket* s) @local
{
	char[128] storage @noinit;
	Socklen_t len = storage.len;
	if (os::getsockname(s.sock, (SockAddrPtr)&storage, &len) < 0)
	{
		return os::socket_error()~;
	}
	return storage[:len];
}

macro char[]? _getpeername(Socket* s) @local
{
	char[128] storage @noinit;
	Socklen_t len = storage.len;
	if (os::getpeername(s.sock, (SockAddrPtr)&storage, &len) < 0)
	{
		return os::socket_error()~;
	}
	return storage[:len];
}

fn InetAddress? decode_sockaddr(char[] storage) @private
{
	if (storage.len < 8) return IP_NOT_DECODABLE~;

	$if env::DARWIN || env::BSD_FAMILY:
		AIFamily family = (AIFamily)storage[1];
	$else
		AIFamily family = (AIFamily)*(ushort*)storage.ptr;
	$endif

	switch (family)
	{
		case os::AF_INET:
			return { .ipv4 = { .a = storage[4], .b = storage[5], .c = storage[6], .d = storage[7] } };

		case os::AF_INET6:
			if (storage.len < 24) return IP_NOT_DECODABLE~;
			InetAddress addr = { .is_ipv6 = true };
			mem::copy(&addr.ipv6, &storage[8], 16);
			return addr;

		default:
			return IP_NOT_DECODABLE~;
	}
}

fn ushort? decode_port(char[] storage) @private
{
	if (storage.len < 4) return IP_NOT_DECODABLE~;
	return (ushort)storage[2] << 8 | storage[3];
}
