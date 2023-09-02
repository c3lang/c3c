module std::net @if(os::SUPPORTS_INET);
import std::io;
import libc;

struct Socket
{
	inline Stream stream;
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
		if (sockfd > 0)
		{
			@body(sockfd, ai);
		}
		ai = ai.ai_next;
	}
}

macro Socket new_socket(fd, ai)
{
	Socket sock = { .stream.fns = &SOCKETSTREAM_INTERFACE, .sock = fd, .ai_addrlen = ai.ai_addrlen };
	assert(sock.ai_addr_storage.len >= ai.ai_addrlen, "storage %d < addrlen %d", sock.ai_addr_storage.len, ai.ai_addrlen);
	mem::copy(&sock.ai_addr_storage, (void*)ai.ai_addr, ai.ai_addrlen);
	return sock;
}

enum SocketOption : char (CInt value)
{
	REUSEADDR (os::SO_REUSEADDR),
	REUSEPORT (os::SO_REUSEPORT) @if(!env::WIN32),
	KEEPALIVE (os::SO_KEEPALIVE),
	BROADCAST (os::SO_BROADCAST),
	OOBINLINE (os::SO_OOBINLINE),
	DONTROUTE (os::SO_DONTROUTE),
}

const StreamInterface SOCKETSTREAM_INTERFACE = {
	.read_fn = (ReadStreamFn)&Socket.read,
	.write_fn = (WriteStreamFn)&Socket.write,
	.close_fn = (CloseStreamFn)&Socket.close,
};

fn bool! Socket.get_broadcast(&self) => self.get_option(BROADCAST);
fn bool! Socket.get_keepalive(&self) => self.get_option(KEEPALIVE);
fn bool! Socket.get_reuseaddr(&self) => self.get_option(REUSEADDR);
fn bool! Socket.get_dontroute(&self) => self.get_option(DONTROUTE);
fn bool! Socket.get_oobinline(&self) => self.get_option(OOBINLINE);

fn void! Socket.set_broadcast(&self, bool value) => self.set_option(BROADCAST, value);
fn void! Socket.set_keepalive(&self, bool value) => self.set_option(KEEPALIVE, value);
fn void! Socket.set_reuseaddr(&self, bool value) => self.set_option(REUSEADDR, value);
fn void! Socket.set_dontroute(&self, bool value) => self.set_option(DONTROUTE, value);
fn void! Socket.set_oobinline(&self, bool value) => self.set_option(OOBINLINE, value);

fn void! Socket.set_option(&self, SocketOption option, bool value)
{
	CInt flag = (CInt)value;
	int errcode = os::setsockopt(self.sock, os::SOL_SOCKET, option.value, &flag, CInt.sizeof);
	if (errcode != 0) return NetError.SOCKOPT_FAILED?;
}

fn bool! Socket.get_option(&self, SocketOption option)
{
	CInt flag;
	int errcode = os::setsockopt(self.sock, os::SOL_SOCKET, option.value, &flag, CInt.sizeof);
	if (errcode != 0) return NetError.SOCKOPT_FAILED?;
	return (bool)flag;
}

fn usz! Socket.read(&self, char[] bytes)
{
	isz n = libc::read((Fd)self.sock, bytes.ptr, bytes.len);
	if (n < 0) return NetError.READ_FAILED?;
	return (usz)n;
}

fn usz! Socket.write(&self, char[] bytes)
{
	isz n = libc::write((Fd)self.sock, bytes.ptr, bytes.len);
	if (n < 0) return NetError.WRITE_FAILED?;
	return (usz)n;
}

fn void! Socket.close(&self) @inline
{
	self.sock.close()!;
}
