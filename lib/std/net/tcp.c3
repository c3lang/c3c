module std::net::tcp @if(os::SUPPORTS_INET);
import std::net @public;
import std::time, libc;
import std::os::win32;

typedef TcpSocket = inline Socket;
typedef TcpServerSocket = inline Socket;

fn TcpSocket? connect(String host, uint port, Duration timeout = time::DURATION_ZERO, SocketOption... options, IpProtocol ip_protocol = UNSPECIFIED)
{
	AddrInfo* ai =  net::addrinfo(host, port, ip_protocol.ai_family, os::SOCK_STREAM)!;
	defer os::freeaddrinfo(ai);
	if (timeout > time::DURATION_ZERO)
	{
		return (TcpSocket)net::connect_with_timeout_from_addrinfo(ai, options, timeout)!;
	}
	return connect_to(ai, ...options);
}

fn TcpSocket? connect_async(String host, uint port, SocketOption... options, IpProtocol ip_protocol = UNSPECIFIED)
{
	AddrInfo* ai = net::addrinfo(host, port, ip_protocol.ai_family, os::SOCK_STREAM)!;
	defer os::freeaddrinfo(ai);
	return connect_async_to(ai, ...options);
}

fn TcpSocket? connect_to(AddrInfo* ai, SocketOption... options)
{
	return (TcpSocket)net::connect_from_addrinfo(ai, options);
}

fn TcpSocket? connect_async_to(AddrInfo* ai, SocketOption... options)
{
	return (TcpSocket)net::connect_async_from_addrinfo(ai, options);
}

fn TcpServerSocket? listen(String host, uint port, uint backlog, SocketOption... options, IpProtocol ip_protocol = UNSPECIFIED)
{
	AddrInfo* ai = net::addrinfo(host, port, ip_protocol.ai_family, os::SOCK_STREAM)!;
	defer os::freeaddrinfo(ai);
	return listen_to(ai, backlog, ...options);
}

fn TcpSocket? accept(TcpServerSocket* server_socket)
{
	TcpSocket socket;
	socket.ai_addrlen = socket.ai_addr_storage.len;
	socket.sock = os::accept(server_socket.sock, (SockAddrPtr)&socket.ai_addr_storage, &socket.ai_addrlen);
	if (!socket.sock.is_valid()) return net::ACCEPT_FAILED?;
	return socket;
}

fn TcpServerSocket? listen_to(AddrInfo* ai, uint backlog, SocketOption... options)
{
	net::@loop_over_ai(ai; NativeSocket sockfd, AddrInfo* ai_candidate)
	{
		net::apply_sockoptions(sockfd, options)!;
		bool err = os::bind(sockfd, ai_candidate.ai_addr, ai_candidate.ai_addrlen) || os::listen(sockfd, backlog);
		if (!err) return (TcpServerSocket)net::new_socket(sockfd, ai_candidate);
	};
	return os::socket_error()?;
}

struct TcpSocketPair
{
    TcpSocket send;
    TcpSocket recv;
}

fn TcpSocketPair*? TcpSocketPair.init(&self)
{
	$if env::WIN32:
		Win32_WORD version = 0x0202;
		char[408] wsa_data;
		CInt wsa_error = win32::wsaStartup(version, &wsa_data);
		if (wsa_error > 0) return os::socket_error()?;

		TcpServerSocket listen_sock = tcp::listen("127.0.0.1", 0, 0)!;

		TcpSocket listen_sock_info;
		listen_sock_info.ai_addrlen = listen_sock.ai_addr_storage.len;

		int sock_result = os::getsockname(listen_sock.sock, (SockAddrPtr) &listen_sock_info.ai_addr_storage,  &listen_sock_info.ai_addrlen);
		if (sock_result < 0) return os::socket_error()?;

		char[] listen_port_bytes = listen_sock_info.ai_addr_storage[2:2];
		char msb = listen_port_bytes[0];
		char lsb = listen_port_bytes[1];
		int listen_port = (msb << 8) | lsb;

		TcpSocket tcp_send_sock = tcp::connect_async("127.0.0.1", listen_port)!;
		TcpSocket tcp_recv_sock = tcp::accept(&listen_sock)!;

        listen_sock.close()!;
	$else
		NativeSocket[2] sockets;
		isz sockpair_result = os::socketpair(os::AF_UNIX, os::SOCK_STREAM, 0, &sockets);
		if (sockpair_result < 0) return os::socket_error()?;

		Socket send_sock = { .sock = sockets[0] };
		TcpSocket tcp_send_sock = (TcpSocket) send_sock;

		Socket recv_sock = { .sock = sockets[1] };
		TcpSocket tcp_recv_sock = (TcpSocket) recv_sock;
	$endif

    *self = { .send = tcp_send_sock,  .recv = tcp_recv_sock };
    return self;
}

fn void? TcpSocketPair.free(&self)
{
    self.send.close()!;
    self.recv.close()!;

    $if env::WIN32:
        win32::wsaCleanup();
    $endif
}
