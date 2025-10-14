module tcpsockpairtest @test;
import std::net;

fn void test_tcp_sock_pair()
{
	tcp::TcpSocketPair sockets;
	sockets.init()!!;
	defer sockets.free()!!;

	String expected = "hello, world!";
	sockets.send.write(expected)!!;

	char[100] recv_buf;
	sockets.recv.read(&recv_buf)!!;

	String result = (String) recv_buf[0:expected.len];

	assert(result.trim() == expected);
}
