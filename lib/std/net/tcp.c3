module std::net::tcp @if(os::SUPPORTS_INET);
import std::net @public;
import std::time, libc;
import std::os::win32;
import std::core::env;
import std::net::os;



typedef TcpSocket = inline Socket;
typedef TcpServerSocket = inline Socket;

fn TcpSocket? connect(String host, int port, Duration timeout = time::DURATION_ZERO, SocketOption... options, IpProtocol ip_protocol = UNSPECIFIED)
{
	AddrInfo* ai =  net::addrinfo(host, port, ip_protocol.ai_family, os::SOCK_STREAM)!;
	defer os::freeaddrinfo(ai);
	if (timeout > time::DURATION_ZERO)
	{
		return (TcpSocket)net::connect_with_timeout_from_addrinfo(ai, options, timeout)!;
	}
	return connect_to(ai, ...options);
}

fn TcpSocket? connect_async(String host, int port, SocketOption... options, IpProtocol ip_protocol = UNSPECIFIED)
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

fn TcpServerSocket? listen(String host, int port, int backlog, SocketOption... options, IpProtocol ip_protocol = UNSPECIFIED)
{
	AddrInfo* ai = net::addrinfo(host, port, ip_protocol.ai_family, os::SOCK_STREAM)!;
	defer os::freeaddrinfo(ai);
	return listen_to(ai, backlog, ...options);
}

fn TcpSocket? accept(TcpServerSocket* server_socket)
{
	TcpSocket socket;
	socket.addrlen = socket.addr_storage.len;
	$if env::WIN32:
		os::start_wsa()!;
	$endif
	socket.sock = os::accept(server_socket.sock, (SockAddrPtr)&socket.addr_storage, &socket.addrlen);
	if (!socket.sock.is_valid()) return net::ACCEPT_FAILED~;
	return socket;
}

fn TcpServerSocket? listen_to(AddrInfo* ai, int backlog, SocketOption... options)
{
	$if env::WIN32:
		os::start_wsa()!;
	$endif
	net::@loop_over_ai(ai; NativeSocket sockfd, AddrInfo* ai_candidate)
	{
		net::apply_sockoptions(sockfd, options)!;
		bool err = os::bind(sockfd, ai_candidate.ai_addr, ai_candidate.ai_addrlen) || os::listen(sockfd, backlog);
		if (!err) return (TcpServerSocket)net::new_socket(sockfd, ai_candidate);
	};
	return os::socket_error()~;
}

struct TcpSocketPair
{
	TcpSocket send;
	TcpSocket recv;
}

fn TcpSocketPair*? TcpSocketPair.init(&self)
{
	$if env::WIN32:
		os::start_wsa()!;

		TcpServerSocket listen_sock = tcp::listen("127.0.0.1", 0, 0)!;

		TcpSocket listen_sock_info;
		listen_sock_info.addrlen = listen_sock.addr_storage.len;

		int sock_result = os::getsockname(listen_sock.sock, (SockAddrPtr) &listen_sock_info.addr_storage,  &listen_sock_info.addrlen);
		if (sock_result < 0) return os::socket_error()~;

		char[] listen_port_bytes = listen_sock_info.addr_storage[2:2];
		char msb = listen_port_bytes[0];
		char lsb = listen_port_bytes[1];
		int listen_port = ((int)msb << 8) | lsb;

		defer (void)listen_sock.close();
		TcpSocket tcp_send_sock = tcp::connect_async("127.0.0.1", listen_port)!;
		TcpSocket tcp_recv_sock = tcp::accept(&listen_sock)!;

	$else
		NativeSocket[2] sockets;
		sz sockpair_result = os::socketpair(os::AF_UNIX, os::SOCK_STREAM, (AIProtocol)0, &sockets);
		if (sockpair_result < 0) return os::socket_error()~;

		Socket send_sock = { .sock = sockets[0] };
		TcpSocket tcp_send_sock = (TcpSocket) send_sock;

		Socket recv_sock = { .sock = sockets[1] };
		TcpSocket tcp_recv_sock = (TcpSocket) recv_sock;
	$endif

	*self = { .send = tcp_send_sock,  .recv = tcp_recv_sock };
	return self;
}

fn void? TcpSocketPair.destroy(&self)
{
	{
		defer catch (void)self.recv.close();
		self.send.close()!;
	}
	self.recv.close()!;
}
