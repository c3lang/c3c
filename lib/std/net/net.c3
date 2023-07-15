module std::net;
import std::net::os;

fault NetError
{
	INVALID_URL,
	URL_TOO_LONG,
	INVALID_SOCKET,
	GENERAL_ERROR,
	INVALID_IP_STRING,
    ADDRINFO_FAILED,
	CONNECT_FAILED,
	LISTEN_FAILED,
	ACCEPT_FAILED,
    WRITE_FAILED,
    READ_FAILED,
}

fn uint! ipv4toint(String s)
{
	uint out;
	int element;
	int current = -1;
	foreach (c : s)
	{
		if (c == '.')
		{
			if (current < 0) return NetError.INVALID_IP_STRING?;
			out = out << 8 + current;
			current = -1;
			element++;
			continue;
		}
		if (element > 3 || c < '0' || c > '9') return NetError.INVALID_IP_STRING?;
		if (current < 0)
		{
			current = c - '0';
			continue;
		}
		current = current * 10 + c - '0';
	}
	if (element != 3 || current < 0) return NetError.INVALID_IP_STRING?;
	out = out << 8 + current;
	return out;
}

fn String! inttoipv4(uint val, Allocator* using = mem::heap())
{
	char[3 * 4 + 3 + 1] buffer;
	String res = (String)io::bprintf(&buffer, "%d.%d.%d.%d", val >> 24, (val >> 16) & 0xFF, (val >> 8) & 0xFF, val & 0xFF)!;
	return res.copy(using);
}

enum Network : char (int domain, int type)
{
    TCP  (os::AF_INET, SOCK_STREAM),
    TCP6 (os::AF_INET6, SOCK_STREAM),
    UDP  (os::AF_INET, SOCK_DGRAM),
    UDP6 (os::AF_INET6, SOCK_DGRAM),
}

fn Socket! Network.connect(&self, String host, String port)
{
    @network_loop_over_ai(self, host, port; NativeSocket sockfd, AddrInfo* ai)
    {
        int errcode = os::connect(sockfd, ai.ai_addr, ai.ai_addrlen);
        // Keep the first successful connection.
        if (errcode == 0) return network_socket(sockfd, ai);
    }!;
    return NetError.CONNECT_FAILED?;
}

fn Socket! Network.listen(&self, String host, String port, int backlog)
{
    @network_loop_over_ai(self, host, port; NativeSocket sockfd, AddrInfo* ai)
    {
        int errcode = os::bind(sockfd, ai.ai_addr, ai.ai_addrlen);
        if (errcode == 0)
        {
            errcode = os::listen(sockfd, backlog);
            // Keep the first successful connection.
            if (errcode == 0) return network_socket(sockfd, ai);
        }
    }!;
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