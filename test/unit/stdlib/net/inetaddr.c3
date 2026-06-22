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
	assert(a.to_tstring() == "127.0.0.1");
}

fn void test_ipv6_to_string()
{
	InetAddress a = net::ipv6_from_str("2001:db8::2:1")!!;
	free(a.to_string(mem));

	assert(a.to_tstring() == "2001:0db8:0000:0000:0000:0000:0002:0001");
	assert(net::ipv6_from_str("2001:db8::1").to_tstring()!! == "2001:0db8:0000:0000:0000:0000:0000:0001");
	assert(net::ipv6_from_str("::1").to_tstring()!! == "0000:0000:0000:0000:0000:0000:0000:0001");
	assert(net::ipv6_from_str("2001::1").to_tstring()!! == "2001:0000:0000:0000:0000:0000:0000:0001");
	assert(net::ipv6_from_str("2001:db8:1234::").to_tstring()!! == "2001:0db8:1234:0000:0000:0000:0000:0000");
	assert(net::ipv6_from_str("2001::").to_tstring()!! == "2001:0000:0000:0000:0000:0000:0000:0000");
	assert(net::ipv6_from_str("::").to_tstring()!! == "0000:0000:0000:0000:0000:0000:0000:0000");
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

fn void test_ipv6_parse()
{
	assert(net::ipv6_from_str("a::b:c:d:e:f:0")!!.to_tstring() == "000a:0000:000b:000c:000d:000e:000f:0000");
	assert(net::ipv6_from_str("1:2:3:4:5:6:7::")!!.to_tstring() == "0001:0002:0003:0004:0005:0006:0007:0000");
	assert(net::ipv6_from_str("::2:3:4:5:6:7:8")!!.to_tstring() == "0000:0002:0003:0004:0005:0006:0007:0008");
	assert(net::ipv6_from_str("1:2:3:4:5:6::8")!!.to_tstring() == "0001:0002:0003:0004:0005:0006:0000:0008");
	assert(net::ipv6_from_str("1:2:3:4:5:6:7:8")!!.to_tstring() == "0001:0002:0003:0004:0005:0006:0007:0008");

	assert(@catch(net::ipv6_from_str("1:2:3:4:5:6:7:8:9")));
	assert(@catch(net::ipv6_from_str("1:2:3:4:5:6:7")));
	assert(@catch(net::ipv6_from_str("1:2:3:4:5:6:7:8::")));
	assert(@catch(net::ipv6_from_str("1::2:3:4:5:6:7:8")));
	assert(@catch(net::ipv6_from_str("1::2::3")));
	assert(@catch(net::ipv6_from_str("12345::1")));
	assert(@catch(net::ipv6_from_str("xyz::1")));
}

fn void test_is_loopback()
{
	assert(net::ipv4_from_str("127.0.0.1")!!.is_loopback());
	assert(net::ipv4_from_str("127.255.255.254")!!.is_loopback());
	assert(!net::ipv4_from_str("128.0.0.1")!!.is_loopback());
	assert(!net::ipv4_from_str("10.0.0.1")!!.is_loopback());

	assert(net::ipv6_from_str("::1")!!.is_loopback());
	assert(!net::ipv6_from_str("::2")!!.is_loopback());
	assert(!net::ipv6_from_str("::")!!.is_loopback());
	assert(!net::ipv6_from_str("fe80::1")!!.is_loopback());
}

fn void test_is_any_local()
{
	assert(net::ipv4_from_str("0.0.0.0")!!.is_any_local());
	assert(!net::ipv4_from_str("0.0.0.1")!!.is_any_local());
	assert(!net::ipv4_from_str("127.0.0.1")!!.is_any_local());

	assert(net::ipv6_from_str("::")!!.is_any_local());
	assert(!net::ipv6_from_str("::1")!!.is_any_local());
}

fn void test_is_link_local()
{
	assert(net::ipv4_from_str("169.254.0.0")!!.is_link_local());
	assert(net::ipv4_from_str("169.254.1.1")!!.is_link_local());
	assert(net::ipv4_from_str("169.254.255.255")!!.is_link_local());
	assert(!net::ipv4_from_str("169.253.0.0")!!.is_link_local());
	assert(!net::ipv4_from_str("169.255.0.0")!!.is_link_local());
	assert(!net::ipv4_from_str("170.254.0.0")!!.is_link_local());

	assert(net::ipv6_from_str("fe80::1")!!.is_link_local());
	assert(net::ipv6_from_str("fe80::")!!.is_link_local());
	assert(net::ipv6_from_str("fea0::1")!!.is_link_local());
	assert(net::ipv6_from_str("febf::1")!!.is_link_local());
	assert(!net::ipv6_from_str("fec0::1")!!.is_link_local());
	assert(!net::ipv6_from_str("fe7f::1")!!.is_link_local());
	assert(!net::ipv6_from_str("2001:db8::1")!!.is_link_local());
}

fn void test_is_site_local()
{
	assert(net::ipv4_from_str("10.0.0.0")!!.is_site_local());
	assert(net::ipv4_from_str("10.255.255.255")!!.is_site_local());
	assert(net::ipv4_from_str("172.16.0.0")!!.is_site_local());
	assert(net::ipv4_from_str("172.31.255.255")!!.is_site_local());
	assert(net::ipv4_from_str("192.168.0.0")!!.is_site_local());
	assert(net::ipv4_from_str("192.168.1.1")!!.is_site_local());
	assert(!net::ipv4_from_str("11.0.0.0")!!.is_site_local());
	assert(!net::ipv4_from_str("172.15.0.0")!!.is_site_local());
	assert(!net::ipv4_from_str("172.32.0.0")!!.is_site_local());
	assert(!net::ipv4_from_str("192.167.1.1")!!.is_site_local());
	assert(!net::ipv4_from_str("192.169.1.1")!!.is_site_local());

	assert(net::ipv6_from_str("fec0::1")!!.is_site_local());
	assert(net::ipv6_from_str("fef0::1")!!.is_site_local());
	assert(net::ipv6_from_str("feff::1")!!.is_site_local());
	assert(!net::ipv6_from_str("febf::1")!!.is_site_local());
	assert(!net::ipv6_from_str("ff00::1")!!.is_site_local());
	assert(!net::ipv6_from_str("2001:db8::1")!!.is_site_local());
}

fn void test_is_multicast()
{
	assert(net::ipv4_from_str("224.0.0.1")!!.is_multicast());
	assert(net::ipv4_from_str("239.255.255.255")!!.is_multicast());
	assert(!net::ipv4_from_str("223.255.255.255")!!.is_multicast());
	assert(!net::ipv4_from_str("240.0.0.0")!!.is_multicast());

	assert(net::ipv6_from_str("ff00::")!!.is_multicast());
	assert(net::ipv6_from_str("ff02::1")!!.is_multicast());
	assert(net::ipv6_from_str("ff05::1:3")!!.is_multicast());
	assert(net::ipv6_from_str("ffff::1")!!.is_multicast());
	assert(!net::ipv6_from_str("fe80::1")!!.is_multicast());
	assert(!net::ipv6_from_str("2001:db8::1")!!.is_multicast());
}

fn void test_is_multicast_global()
{
	assert(net::ipv4_from_str("224.0.1.1")!!.is_multicast_global());
	assert(net::ipv4_from_str("232.0.0.1")!!.is_multicast_global());
	assert(net::ipv4_from_str("238.255.255.255")!!.is_multicast_global());
	assert(!net::ipv4_from_str("224.0.0.1")!!.is_multicast_global());
	assert(!net::ipv4_from_str("239.0.0.1")!!.is_multicast_global());
	assert(!net::ipv4_from_str("223.0.0.1")!!.is_multicast_global());

	assert(net::ipv6_from_str("ff0e::1")!!.is_multicast_global());
	assert(net::ipv6_from_str("ff1e::1")!!.is_multicast_global());
	assert(!net::ipv6_from_str("ff02::1")!!.is_multicast_global());
	assert(!net::ipv6_from_str("ff05::1")!!.is_multicast_global());
	assert(!net::ipv6_from_str("fe80::1")!!.is_multicast_global());
}

fn void test_is_multicast_node_local()
{
	assert(!net::ipv4_from_str("224.0.0.1")!!.is_multicast_node_local());

	assert(net::ipv6_from_str("ff01::1")!!.is_multicast_node_local());
	assert(net::ipv6_from_str("ff11::1")!!.is_multicast_node_local());
	assert(!net::ipv6_from_str("ff02::1")!!.is_multicast_node_local());
	assert(!net::ipv6_from_str("ff05::1")!!.is_multicast_node_local());
	assert(!net::ipv6_from_str("fe80::1")!!.is_multicast_node_local());
}

fn void test_is_multicast_link_local()
{
	assert(net::ipv4_from_str("224.0.0.1")!!.is_multicast_link_local());
	assert(net::ipv4_from_str("224.0.0.255")!!.is_multicast_link_local());
	assert(!net::ipv4_from_str("224.0.1.0")!!.is_multicast_link_local());
	assert(!net::ipv4_from_str("224.1.0.0")!!.is_multicast_link_local());

	assert(net::ipv6_from_str("ff02::1")!!.is_multicast_link_local());
	assert(net::ipv6_from_str("ff12::1")!!.is_multicast_link_local());
	assert(!net::ipv6_from_str("ff01::1")!!.is_multicast_link_local());
	assert(!net::ipv6_from_str("ff05::1")!!.is_multicast_link_local());
	assert(!net::ipv6_from_str("fe80::1")!!.is_multicast_link_local());
}

fn void test_is_multicast_site_local()
{
	assert(net::ipv4_from_str("239.255.0.0")!!.is_multicast_site_local());
	assert(net::ipv4_from_str("239.255.255.255")!!.is_multicast_site_local());
	assert(!net::ipv4_from_str("239.254.0.0")!!.is_multicast_site_local());
	assert(!net::ipv4_from_str("238.255.0.0")!!.is_multicast_site_local());

	assert(net::ipv6_from_str("ff05::1")!!.is_multicast_site_local());
	assert(net::ipv6_from_str("ff15::1")!!.is_multicast_site_local());
	assert(!net::ipv6_from_str("ff02::1")!!.is_multicast_site_local());
	assert(!net::ipv6_from_str("ff08::1")!!.is_multicast_site_local());
}

fn void test_is_multicast_org_local()
{
	assert(net::ipv4_from_str("239.192.0.0")!!.is_multicast_org_local());
	assert(net::ipv4_from_str("239.195.255.255")!!.is_multicast_org_local());
	assert(!net::ipv4_from_str("239.191.0.0")!!.is_multicast_org_local());
	assert(!net::ipv4_from_str("239.196.0.0")!!.is_multicast_org_local());

	assert(net::ipv6_from_str("ff08::1")!!.is_multicast_org_local());
	assert(net::ipv6_from_str("ff18::1")!!.is_multicast_org_local());
	assert(!net::ipv6_from_str("ff05::1")!!.is_multicast_org_local());
	assert(!net::ipv6_from_str("ff0e::1")!!.is_multicast_org_local());
}
