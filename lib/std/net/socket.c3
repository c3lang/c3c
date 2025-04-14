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

const Duration POLL_FOREVER = -1;

typedef PollSubscribes = ushort;
typedef PollEvents = ushort;

const PollSubscribes SUBSCRIBE_ANY_READ     = os::POLLIN;
const PollSubscribes SUBSCRIBE_PRIO_READ    = os::POLLPRI;
const PollSubscribes SUBSCRIBE_OOB_READ     = os::POLLRDBAND;
const PollSubscribes SUBSCRIBE_READ         = os::POLLRDNORM;
const PollSubscribes SUBSCRIBE_ANY_WRITE    = os::POLLOUT;
const PollSubscribes SUBSCRIBE_OOB_WRITE    = os::POLLWRBAND;
const PollSubscribes SUBSCRIBE_WRITE        = os::POLLWRNORM;

const PollEvents POLL_EVENT_READ_PRIO  = os::POLLPRI;
const PollEvents POLL_EVENT_READ_OOB   = os::POLLRDBAND;
const PollEvents POLL_EVENT_READ       = os::POLLRDNORM;
const PollEvents POLL_EVENT_WRITE_OOB  = os::POLLWRBAND;
const PollEvents POLL_EVENT_WRITE      = os::POLLWRNORM;
const PollEvents POLL_EVENT_DISCONNECT = os::POLLHUP;
const PollEvents POLL_EVENT_ERROR      = os::POLLERR;
const PollEvents POLL_EVENT_INVALID    = os::POLLNVAL;

struct Poll
{
	NativeSocket socket;
	PollSubscribes events;
	PollEvents revents;
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
	return result < 0 ? os::socket_error()? : (ulong)result;
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
	REUSEADDR                  = os::SO_REUSEADDR,
	REUSEPORT @if(!env::WIN32) = os::SO_REUSEPORT,
	KEEPALIVE                  = os::SO_KEEPALIVE,
	BROADCAST                  = os::SO_BROADCAST,
	OOBINLINE                  = os::SO_OOBINLINE,
	DONTROUTE                  = os::SO_DONTROUTE,
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
	if (errcode != 0) return SOCKOPT_FAILED?;
}

fn bool? Socket.get_option(&self, SocketOption option)
{
	CInt flag;
	int errcode = os::setsockopt(self.sock, os::SOL_SOCKET, option.value, &flag, CInt.sizeof);
	if (errcode != 0) return SOCKOPT_FAILED?;
	return (bool)flag;
}

fn usz? Socket.read(&self, char[] bytes) @dynamic
{
$if env::WIN32:
	isz n = libc::recv(self.sock, bytes.ptr, (int)bytes.len, 0);
$else
	isz n = libc::recv(self.sock, bytes.ptr, bytes.len, 0);
$endif
	if (n < 0) return os::socket_error()?;
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
	if (n < 0) return os::socket_error()?;
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
	if (n < 0) return os::socket_error()?;
	return (usz)n;
}

enum SocketShutdownHow : (inline CInt native_value)
{
	RECEIVE = @select(env::WIN32, libc::SD_RECEIVE, libc::SHUT_RD),
	SEND = @select(env::WIN32, libc::SD_SEND, libc::SHUT_WR),
	BOTH = @select(env::WIN32, libc::SD_BOTH, libc::SHUT_RDWR),
}

fn void? Socket.shutdown(&self, SocketShutdownHow how)
{
	if (libc::shutdown(self.sock, how) < 0)
	{
		return os::socket_error()?;
	}
}
