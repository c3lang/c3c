module std::net @if(os::SUPPORTS_INET);
import std::io;
import libc;

enum Network : char (AIFamily domain, AISockType type)
{
	TCP  (os::AF_INET, os::SOCK_STREAM),
	TCP6 (os::AF_INET6, os::SOCK_STREAM),
	UDP  (os::AF_INET, os::SOCK_DGRAM),
	UDP6 (os::AF_INET6, os::SOCK_DGRAM),
}



struct Socket
{
	NativeSocket sock;
	Socklen_t ai_addrlen;
	// TODO proper way to get the size of sockaddr_storage
	// https://learn.microsoft.com/en-us/previous-versions/windows/desktop/legacy/ms740504(v=vs.85)
	char[128] ai_addr_storage;
}

fn Socket! _connect(AddrInfo* addrinfo, SocketOption[] options)
{
	@loop_over_ai(addrinfo; NativeSocket sockfd, AddrInfo* ai)
	{
		apply_sockoptions(sockfd, options)!;
		int errcode = os::connect(sockfd, ai.ai_addr, ai.ai_addrlen);
		// Keep the first successful connection.
		if (!errcode) return network_socket(sockfd, ai);
	};
	return os::socket_error()?;
}


fn Socket! Network.connect(&self, String host, String port, SocketOption... options)
{
	@network_loop_over_ai(self, host, port; NativeSocket sockfd, AddrInfo* ai)
	{
		apply_sockoptions(sockfd, options)!;
		int errcode = os::connect(sockfd, ai.ai_addr, ai.ai_addrlen);
		// Keep the first successful connection.
		if (errcode == 0) return network_socket(sockfd, ai);
	}!;
	return os::socket_error()?;
}

fn Socket! Network.listen(&self, String host, String port, int backlog, SocketOption... options)
{
	@network_loop_over_ai(self, host, port; NativeSocket sockfd, AddrInfo* ai)
	{
		apply_sockoptions(sockfd, options)!;
		int errcode = os::bind(sockfd, ai.ai_addr, ai.ai_addrlen);
		if (errcode == 0)
		{
			errcode = os::listen(sockfd, backlog);
			// Keep the first successful connection.
			if (errcode == 0) return network_socket(sockfd, ai);
		}
	}!;
	return os::socket_error()?;
}

fn AddrInfo*! Network.addrinfo(&self, String host, String port) @private
{
	ZString zhost = host.zstr_tcopy();
	ZString zport = port.zstr_tcopy();
	AddrInfo hints = { .ai_family = self.domain, .ai_socktype = self.type };
	AddrInfo* ai;
	int errcode = os::getaddrinfo(zhost, zport, &hints, &ai);
	if (errcode != 0) return NetError.ADDRINFO_FAILED?;
	return ai;
}

macro apply_sockoptions(sockfd, options)
{
	Socket sock = { .sock = sockfd };
	foreach (o : options) sock.set_option(o, true)!;
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

macro @network_loop_over_ai(network, host, port; @body(fd, ai)) @private
{
	AddrInfo* ai = network.addrinfo(host, port)!;
	AddrInfo* first = ai;
	defer os::freeaddrinfo(first);
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

macro Socket network_socket(fd, ai)
{
	Socket sock = { .sock = fd, .ai_addrlen = ai.ai_addrlen };
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



fn Stream Socket.as_stream(&self)
{
	return { .fns = &socketstream_interface, .data = self };
}

StreamInterface socketstream_interface = {
	.read_fn = fn(s, char[] bytes) => ((Socket*)s.data).read(bytes) @inline,
	.write_fn = fn(s, char[] bytes) => ((Socket*)s.data).write(bytes) @inline,
	.close_fn = fn(s) => ((Socket*)s.data).close(),
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

macro void! @set_option_value(NativeSocket sock, CInt option, value)
{
	var val = value;
	if (os::setsockopt(sock, os::SOL_SOCKET, option, &val, $sizeof(val))) return NetError.SOCKOPT_FAILED?;
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

struct Listener
{
	Socket socket;
}

fn void! Listener.init(&self, Network network, String host, String port, int backlog = 10, SocketOption... options)
{
	*self = { .socket = network.listen(host, port, backlog, ...options)! };
}

fn Socket! Listener.accept(&self)
{
	Socket sock = self.socket;
	sock.sock = os::accept(sock.sock, (SockAddrPtr)&sock.ai_addr_storage, &sock.ai_addrlen);
	if (sock.sock < 0) return NetError.ACCEPT_FAILED?;
	return sock;
}

fn void! Listener.close(&self) @inline
{
	self.socket.close()!;
}