module std::net::udp @if(os::SUPPORTS_INET);
import std::net @public;

typedef UdpSocket = inline Socket;

fn UdpSocket? connect(String host, uint port, SocketOption... options, IpProtocol ip_protocol = UNSPECIFIED)
{
	AddrInfo* ai =  net::addrinfo(host, port, ip_protocol.ai_family, os::SOCK_DGRAM)!;
	defer os::freeaddrinfo(ai);
	return connect_to(ai, ...options);
}

fn UdpSocket? connect_to(AddrInfo* ai, SocketOption... options)
{
	return (UdpSocket)net::connect_from_addrinfo(ai, options);
}

fn UdpSocket? connect_async(String host, uint port, SocketOption... options, IpProtocol ip_protocol = UNSPECIFIED)
{
	AddrInfo* ai =  net::addrinfo(host, port, ip_protocol.ai_family, os::SOCK_DGRAM)!;
	defer os::freeaddrinfo(ai);
	return connect_async_to(ai, ...options);
}

fn UdpSocket? connect_async_to(AddrInfo* ai, SocketOption... options)
{
	return (UdpSocket)net::connect_async_from_addrinfo(ai, options);
}
