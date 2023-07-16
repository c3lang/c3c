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
    SOCKOPT_FAILED,
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