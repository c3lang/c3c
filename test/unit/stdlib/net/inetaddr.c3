module inetaddrtest @test;
import std::net;

fn void test_ipv4()
{
	InetAddress foo = { .ipv4 = { 127, 0, 0, 0 } };
	assert(foo.ip4.val == 2130706432);
	assert(foo.is_loopback());

}

fn void test_ipv4_to_string()
{
	InetAddress a = net::ipv4_from_str("127.0.0.1")!!;
	assert(a.to_new_string() == "127.0.0.1");
}

fn void test_ipv6_to_string()
{
	InetAddress a = net::ipv6_from_str("2001:db8::2:1")!!;
	a.to_new_string();
	assert(a.to_new_string() == "2001:0db8:0000:0000:0000:0000:0002:0001");
	assert(net::ipv6_from_str("2001:db8::1").to_new_string()!! == "2001:0db8:0000:0000:0000:0000:0000:0001");
	assert(net::ipv6_from_str("::1").to_new_string()!! == "0000:0000:0000:0000:0000:0000:0000:0001");
	assert(net::ipv6_from_str("2001::1").to_new_string()!! == "2001:0000:0000:0000:0000:0000:0000:0001");
	assert(net::ipv6_from_str("2001:db8:1234::").to_new_string()!! == "2001:0db8:1234:0000:0000:0000:0000:0000");
	assert(net::ipv6_from_str("2001::").to_new_string()!! == "2001:0000:0000:0000:0000:0000:0000:0000");
	assert(net::ipv6_from_str("::").to_new_string()!! == "0000:0000:0000:0000:0000:0000:0000:0000");
}

fn void test_ipv4_parse()
{
	InetAddress a = net::ipv4_from_str("127.0.0.1")!!;
	assert(a.ipv4.a == 127 && a.ipv4.b == 0 && a.ipv4.c == 0 && a.ipv4.d == 1);
	a = net::ipv4_from_str("255.254.253.255")!!;
	assert(a.ipv4.a == 255 && a.ipv4.b == 254 && a.ipv4.c == 253 && a.ipv4.d == 255);
	assert(@catch(net::ipv4_from_str(".1.1.1.1")));
	assert(@catch(net::ipv4_from_str("1..1.1")));
	assert(@catch(net::ipv4_from_str("1..1.1.1")));
	assert(@catch(net::ipv4_from_str("1.1.1.256")));
	assert(@catch(net::ipv4_from_str("256.1.1.1")));
}

fn void test_ipv6()
{
	InetAddress foo = { .ipv6 = { 0x2001, 0x4860, 0x4860, 0, 0, 0, 0, 0x8888  } };
	assert(foo.ip6.val == 42541956123769884636017138956568135816);
}
