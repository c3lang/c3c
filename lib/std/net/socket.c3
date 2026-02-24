module std::net @if(os::SUPPORTS_INET);
import std::io, std::os, std::time, libc;

struct Socket (InStream, OutStream)
{
	NativeSocket sock;
	Socklen_t ai_addrlen;
	// TODO proper way to get the size of sockaddr_storage
	// https://learn.microsoft.com/en-us/previous-versions/windows/desktop/legacy/ms740504(v=vs.85)
	char[128] ai_addr_storage;
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

constdef PollSubscribe
{
	ANY_READ     = os::POLLIN,
	PRIO_READ    = os::POLLPRI,
	OOB_READ     = os::POLLRDBAND,
	READ         = os::POLLRDNORM,
	ANY_WRITE    = os::POLLOUT,
	OOB_WRITE    = os::POLLWRBAND,
	WRITE        = os::POLLWRNORM,
}

const PollSubscribe SUBSCRIBE_ANY_READ     = (PollSubscribe)os::POLLIN;
const PollSubscribe SUBSCRIBE_PRIO_READ    = (PollSubscribe)os::POLLPRI;
const PollSubscribe SUBSCRIBE_OOB_READ     = (PollSubscribe)os::POLLRDBAND;
const PollSubscribe SUBSCRIBE_READ         = (PollSubscribe)os::POLLRDNORM;
const PollSubscribe SUBSCRIBE_ANY_WRITE    = (PollSubscribe)os::POLLOUT;
const PollSubscribe SUBSCRIBE_OOB_WRITE    = (PollSubscribe)os::POLLWRBAND;
const PollSubscribe SUBSCRIBE_WRITE        = (PollSubscribe)os::POLLWRNORM;

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

const PollEvent POLL_EVENT_READ_PRIO  = (PollEvent)os::POLLPRI;
const PollEvent POLL_EVENT_READ_OOB   = (PollEvent)os::POLLRDBAND;
const PollEvent POLL_EVENT_READ       = (PollEvent)os::POLLRDNORM;
const PollEvent POLL_EVENT_WRITE_OOB  = (PollEvent)os::POLLWRBAND;
const PollEvent POLL_EVENT_WRITE      = (PollEvent)os::POLLWRNORM;
const PollEvent POLL_EVENT_DISCONNECT = (PollEvent)os::POLLHUP;
const PollEvent POLL_EVENT_ERROR      = (PollEvent)os::POLLERR;
const PollEvent POLL_EVENT_INVALID    = (PollEvent)os::POLLNVAL;

alias PollSubscribes @deprecated("Use PollSubscribe") = PollSubscribe;
alias PollEvents @deprecated("Use PollEvent") = PollEvent;

struct Poll
{
	NativeSocket socket;
	PollSubscribe events;
	PollEvent revents;
}

<*
 @param [inout] polls
 @param timeout : "duration to poll (clamped to CInt.max ms), or POLL_FOREVER."
*>
fn ulong? poll(Poll[] polls, Duration timeout)
{
	return poll_ms(polls, timeout == POLL_FOREVER ? -1 : timeout.to_ms()) @inline;
}

<*
 @param [inout] polls
 @param timeout_ms : "duration to poll in ms or -1. Clamped to CInt.max"
*>
fn ulong? poll_ms(Poll[] polls, long timeout_ms)
{
	if (timeout_ms > CInt.max) timeout_ms = CInt.max;
	$if env::WIN32:
		CInt result = win32::wsaPoll((Win32_LPWSAPOLLFD)polls.ptr, (Win32_ULONG)polls.len, (CInt)timeout_ms);
	$else
		CInt result = os::poll((Posix_pollfd*)polls.ptr, (Posix_nfds_t)polls.len, (CInt)timeout_ms);
	$endif
	return result < 0 ? os::socket_error()~ : (ulong)result;
}

macro Socket new_socket(fd, ai)
{
	Socket sock = { .sock = fd, .ai_addrlen = ai.ai_addrlen };
	assert(sock.ai_addr_storage.len >= ai.ai_addrlen, "storage %d < addrlen %d", sock.ai_addr_storage.len, ai.ai_addrlen);
	mem::copy(&sock.ai_addr_storage, (void*)ai.ai_addr, ai.ai_addrlen);
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
	int errcode = os::setsockopt(self.sock, os::SOL_SOCKET, option.value, &flag, CInt.sizeof);
	if (errcode != 0) return SOCKOPT_FAILED~;
}

fn bool? Socket.get_option(&self, SocketOption option)
{
	CInt flag;
	Socklen_t socklen = CInt.sizeof;
	int errcode = os::getsockopt(self.sock, os::SOL_SOCKET, option.value, &flag, &socklen);
	if (errcode != 0) return SOCKOPT_FAILED~;
	return (bool)flag;
}

fn usz? Socket.read(&self, char[] bytes) @dynamic
{
$if env::WIN32:
	isz n = libc::recv(self.sock, bytes.ptr, (int)bytes.len, 0);
$else
	isz n = libc::recv(self.sock, bytes.ptr, bytes.len, 0);
$endif
	if (n < 0) return os::socket_error()~;
	return (usz)n;
}

fn char? Socket.read_byte(&self) @dynamic => io::read_byte_using_read(self);

fn usz? Socket.write(&self, char[] bytes) @dynamic
{
$if env::WIN32:
	isz n = libc::send(self.sock, bytes.ptr, (int)bytes.len, 0);
$else
	isz n = libc::send(self.sock, bytes.ptr, bytes.len, 0);
$endif
	if (n < 0) return os::socket_error()~;
	return (usz)n;
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

fn usz? Socket.peek(&self, char[] bytes) @dynamic
{
  $if env::WIN32:
	  isz n = libc::recv(self.sock, bytes.ptr, (int)bytes.len, os::MSG_PEEK);
  $else
	  isz n = libc::recv(self.sock, bytes.ptr, bytes.len, os::MSG_PEEK);
  $endif
	if (n < 0) return os::socket_error()~;
	return (usz)n;
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
