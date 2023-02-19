module inetaddrtest;
import std::net;

fn void test_ipv4() @test
{
	InetAddress foo = { .ipv4 = { 127, 0, 0, 0 } };
	assert(foo.ip4.val == 2130706432);
	assert(foo.is_loopback());

}

fn void! test_ipv4_parse() @test
{
	InetAddress a = net::ipv4_from_str("127.0.0.1")?;
	assert(a.ipv4.a == 127 && a.ipv4.b == 0 && a.ipv4.c == 0 && a.ipv4.d == 1);
	a = net::ipv4_from_str("255.254.253.255")?;
	assert(a.ipv4.a == 255 && a.ipv4.b == 254 && a.ipv4.c == 253 && a.ipv4.d == 255);
	assert(catch(net::ipv4_from_str(".1.1.1.1")));
	assert(catch(net::ipv4_from_str("1..1.1")));
	assert(catch(net::ipv4_from_str("1..1.1.1")));
	assert(catch(net::ipv4_from_str("1.1.1.256")));
	assert(catch(net::ipv4_from_str("256.1.1.1")));
}

fn void test_ipv6() @test
{
	InetAddress foo = { .ipv6 = { 0x2001, 0x4860, 0x4860, 0, 0, 0, 0, 0x8888  } };
	assert(foo.ip6.val == 42541956123769884636017138956568135816);
}
