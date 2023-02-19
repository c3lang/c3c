module std::net;


struct InetAddress
{
	bool is_ipv6;
	union
	{
		bitstruct ipv6 : char[16] @bigendian
		{
			ushort a : 0..15;
			ushort b : 16..31;
			ushort c : 32..47;
			ushort d : 48..63;
			ushort e : 64..79;
			ushort f : 80..95;
			ushort g : 96..111;
			ushort h : 112..127;
		}
		bitstruct ip6 : char[16] @bigendian
		{
			uint128 val : 0..127;
		}

		bitstruct ipv4 : char[16] @bigendian
		{
			char a : 96..103;
			char b : 104..111;
			char c : 112..119;
			char d : 120..127;
		}
		bitstruct ip4 : char[16] @bigendian
		{
			uint val : 96..127;
		}
	}
}

fn InetAddress! ipv4_from_str(String s)
{
	InetAddress addr;
	int element;
    int current = -1;
    foreach (c : s)
    {
		if (c == '.')
        {
            if (current < 0) return NetError.INVALID_IP_STRING!;
            if (current > 255) return NetError.INVALID_IP_STRING!;
            switch (element)
            {
                case 0: addr.ipv4.a = (char)current;
                case 1: addr.ipv4.b = (char)current;
                case 2: addr.ipv4.c = (char)current;
                default: return NetError.INVALID_IP_STRING!;
            }
            current = -1;
            element++;
            continue;
        }
        if (element > 3 || c < '0' || c > '9') return NetError.INVALID_IP_STRING!;
        if (current < 0)
        {
            current = c - '0';
            continue;
        }
        current = current * 10 + c - '0';
    }
    if (element != 3 || current < 0 || current > 255) return NetError.INVALID_IP_STRING!;
    addr.ipv4.d = (char)current;
    return addr;
}

fn bool InetAddress.is_loopback(InetAddress* addr)
{
	if (addr.is_ipv6)
	{
		return addr.ip6.val == 1;
	}
	return addr.ipv4.a == 127;
}

fn bool InetAddress.is_any_local(InetAddress* addr)
{
	if (addr.is_ipv6)
	{
		return addr.ip6.val == 0;
	}
	return addr.ip4.val == 0;
}

fn bool InetAddress.is_link_local(InetAddress* addr)
{
	if (addr.is_ipv6)
	{
		return addr.ipv6.a == 0xfa && (addr.ipv6.b & 0xc0) == 0x80;
	}
	return addr.ipv4.a == 169 && addr.ipv4.b == 254;
}

fn bool InetAddress.is_site_local(InetAddress* addr)
{
	if (addr.is_ipv6)
	{
		return addr.ipv6.a == 0xfe && (addr.ipv6.b & 0xc0) == 0xc0;
	}
	// 10/8 172.16/12 192.168/16
	switch
	{
		case addr.ipv4.a == 10:
		case addr.ipv4.a == 172 && (addr.ipv4.b & 0xF0) == 16:
		case addr.ipv4.a == 192 && addr.ipv4.b == 168:
			return true;
		default:
		    return false;
	}
}

fn bool InetAddress.is_multicast(InetAddress* addr)
{
	if (addr.is_ipv6)
	{
		return addr.ipv6.a == 0xff;
	}
	return addr.ip4.val & 0xf0000000 == 0xe0000000;
}

fn bool InetAddress.is_multicast_global(InetAddress* addr)
{
	if (addr.is_ipv6)
	{
		return addr.ipv6.a == 0xff && (addr.ipv6.b & 0x0f) == 0x0e;
	}
	return addr.ipv4.a >= 224 && addr.ipv4.a <= 238
		&& !(addr.ipv4.a == 224 && addr.ipv4.b == 0 && addr.ipv4.c == 0);
}

fn bool InetAddress.is_multicast_node_local(InetAddress* addr)
{
	if (!addr.is_ipv6) return false;
	return addr.ipv6.a == 0xff && addr.ipv6.b & 0x0f == 0x01;
}

fn bool InetAddress.is_multicast_site_local(InetAddress* addr)
{
	if (addr.is_ipv6)
	{
		return addr.ipv6.a == 0xff && addr.ipv6.b & 0x0f == 0x05;
	}
	return addr.ipv4.a == 239 && addr.ipv4.b == 255;
}

fn bool InetAddress.is_multicast_org_local(InetAddress* addr)
{
	if (addr.is_ipv6)
	{
		return addr.ipv6.a == 0xff && addr.ipv6.b & 0x0f == 0x08;
	}
	return addr.ipv4.a == 239 && addr.ipv4.b >= 192 && addr.ipv4.b <= 195;
}

fn bool InetAddress.is_multicast_link_local(InetAddress* addr)
{
	if (addr.is_ipv6)
	{
		return addr.ipv6.a == 0xff && (addr.ipv6.b & 0x0f) == 0x02;
	}
	return addr.ipv4.a == 224 && addr.ipv4.b == 0 && addr.ipv4.c == 0;
}
