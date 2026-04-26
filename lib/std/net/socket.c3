module std::net @if(os::SUPPORTS_INET);
import std::io, std::os, std::time, libc;

struct Socket (InStream, OutStream)
{
	NativeSocket sock;
	Socklen_t addrlen;
	// TODO proper way to get the size of sockaddr_storage
	// https://learn.microsoft.com/en-us/previous-versions/windows/desktop/legacy/ms740504(v=vs.85)
	char[128] addr_storage;
}

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

const Duration POLL_FOREVER = (Duration)-1;

constdef PollSubscribe : ushort
{
	ANY_READ     = os::POLLIN,
	PRIO_READ    = os::POLLPRI,
	OOB_READ     = os::POLLRDBAND,
	READ         = os::POLLRDNORM,
	ANY_WRITE    = os::POLLOUT,
	OOB_WRITE    = os::POLLWRBAND,
	WRITE        = os::POLLWRNORM,
}

constdef PollEvent : ushort
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
	REUSEPORT @if(!env::WIN32) { os::SO_REUSEPORT },
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
