module std::net @if(os::SUPPORTS_INET);
import std::io;
import libc;

enum Network : char (int domain, int type)
{
	TCP  (os::AF_INET, SOCK_STREAM),
	TCP6 (os::AF_INET6, SOCK_STREAM),
	UDP  (os::AF_INET, SOCK_DGRAM),
	UDP6 (os::AF_INET6, SOCK_DGRAM),
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
	switch (libc::errno())
	{
		case errno::EACCES: return IoError.NO_PERMISSION?;
		case errno::EADDRINUSE: return NetError.ADDRESS_IN_USE?;
		case errno::EALREADY: return NetError.CONNECTION_ALREADY_IN_PROGRESS?;
		case errno::EBADF: return NetError.BAD_SOCKET_DESCRIPTOR?;
		case errno::ECONNREFUSED: return NetError.CONNECTION_REFUSED?;
		case errno::EISCONN: return NetError.ALREADY_CONNECTED?;
		case errno::ENETUNREACH: return NetError.NETWORK_UNREACHABLE?;
		case errno::ENOTSOCK: return NetError.NOT_A_SOCKET?;
		case errno::EOPNOTSUPP: return NetError.OPERATION_NOT_SUPPORTED_ON_SOCKET?;
		case errno::ETIMEDOUT: return NetError.CONNECTION_TIMED_OUT?;
	}
return	 NetError.CONNECT_FAILED?;
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
	switch (libc::errno())
	{
		case errno::EADDRINUSE: return NetError.ADDRESS_IN_USE?;
		case errno::EBADF: return NetError.BAD_SOCKET_DESCRIPTOR?;
		case errno::ENOTSOCK: return NetError.NOT_A_SOCKET?;
		case errno::EOPNOTSUPP: return NetError.OPERATION_NOT_SUPPORTED_ON_SOCKET?;
	}
	return NetError.LISTEN_FAILED?;
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

macro apply_sockoptions(sockfd, options) @private
{
	Socket sock = { .sock = sockfd };
	foreach (o : options) sock.set_option(o)!;
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

macro Socket network_socket(fd, ai) @private
{
	Socket sock = { .sock = fd, .ai_addrlen = ai.ai_addrlen };
	assert(sock.ai_addr_storage.len >= ai.ai_addrlen, "storage %d < addrlen %d", sock.ai_addr_storage.len, ai.ai_addrlen);
	mem::copy(&sock.ai_addr_storage, (void*)ai.ai_addr, ai.ai_addrlen);
	return sock;
}

enum SocketOption : char (CInt value) @if(!env::WIN32)
{
	REUSEADDR (os::SO_REUSEADDR),
	REUSEPORT (os::SO_REUSEPORT),
	KEEPALIVE (os::SO_KEEPALIVE),
	BROADCAST (os::SO_BROADCAST),
}
enum SocketOption : char (CInt value) @if(env::WIN32)
{
	REUSEADDR (os::SO_REUSEADDR),
	KEEPALIVE (os::SO_KEEPALIVE),
	BROADCAST (os::SO_BROADCAST),
}

struct Socket
{
	NativeSocket sock;
	Socklen_t ai_addrlen;
	// TODO proper way to get the size of sockaddr_storage
	// https://learn.microsoft.com/en-us/previous-versions/windows/desktop/legacy/ms740504(v=vs.85)
	char[128] ai_addr_storage;
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

fn void! Socket.set_option(&self, SocketOption option)
{
	CInt flag = 1;
	int errcode = os::setsockopt(self.sock, os::SOL_SOCKET, option.value, &flag, CInt.sizeof);
	if (errcode != 0) return NetError.SOCKOPT_FAILED?;
}

fn void! Socket.unset_option(&self, SocketOption option)
{
	CInt flag = 0;
	int errcode = os::setsockopt(self.sock, os::SOL_SOCKET, option.value, &flag, CInt.sizeof);
	if (errcode != 0) return NetError.SOCKOPT_FAILED?;
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