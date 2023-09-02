module std::net @if(os::SUPPORTS_INET);
import libc;
macro apply_sockoptions(sockfd, options) @private
{
	Socket sock = { .sock = sockfd };
	foreach (o : options) sock.set_option(o, true)!;
}

fn Socket! connect_from_addrinfo(AddrInfo* addrinfo, SocketOption[] options) @private
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

fn bool last_error_is_delayed_connect()
{
	$switch
		$case env::WIN32:
		switch (win32_WSAGetLastError())
		{
			case wsa::EWOULDBLOCK:
			case wsa::EINPROGRESS: return true;
			default: return false;
		}
    	$default:
    	Errno err = libc::errno();
    	return err == errno::EINPROGRESS || err == errno::EAGAIN || err == errno::EWOULDBLOCK;
	$endswitch
}
fn Socket! connect_async_from_addrinfo(AddrInfo* addrinfo, SocketOption[] options) @private
{
	@loop_over_ai(addrinfo; NativeSocket sockfd, AddrInfo* ai)
	{
		apply_sockoptions(sockfd, options)!;
		sockfd.set_non_blocking(true)!;
		int errcode = os::connect(sockfd, ai.ai_addr, ai.ai_addrlen);
		if (!errcode || last_error_is_delayed_connect())
		{
			// Keep the first successful connection.
			return network_socket(sockfd, ai);
		}
	};
	return os::socket_error()?;

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
