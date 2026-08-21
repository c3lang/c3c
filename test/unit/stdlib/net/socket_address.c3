module socketaddresstest @test;
import std::net;

fn void test_socket_local_and_peer_address() @feat(!EMSCRIPTEN)
{
	TcpServerSocket server = tcp::listen("127.0.0.1", 0, 4, REUSEADDR)!!;
	defer (void)server.close();

	InetAddress server_local_addr = server.local_address()!!;
	test::@check(server_local_addr.is_loopback(), "server local address should be loopback");
	test::eq(server_local_addr.to_tstring(), "127.0.0.1");

	ushort server_port = server.local_port()!!;
	test::gt(server_port, 0);

	TcpSocket client = tcp::connect("127.0.0.1", server_port)!!;
	defer (void)client.close();

	TcpSocket accepted = tcp::accept(&server)!!;
	defer (void)accepted.close();

	InetAddress client_peer_addr = client.peer_address()!!;
	test::@check(client_peer_addr.is_loopback());
	test::eq(client_peer_addr.to_tstring(), "127.0.0.1");
	test::eq(client.peer_port()!!, server_port);

	InetAddress client_local_addr = client.local_address()!!;
	test::@check(client_local_addr.is_loopback());
	test::eq(client_local_addr.to_tstring(), "127.0.0.1");

	ushort client_local_port = client.local_port()!!;
	test::gt(client_local_port, 0);

	InetAddress accepted_peer_addr = accepted.peer_address()!!;
	test::@check(accepted_peer_addr.is_loopback());
	test::eq(accepted_peer_addr.to_tstring(), "127.0.0.1");
	test::eq(accepted.peer_port()!!, client_local_port);

	InetAddress accepted_local_addr = accepted.local_address()!!;
	test::@check(accepted_local_addr.is_loopback());
	test::eq(accepted_local_addr.to_tstring(), "127.0.0.1");
	test::eq(accepted.local_port()!!, server_port);
}

fn void test_socket_unconnected_peer_address_error() @feat(!EMSCRIPTEN)
{
	TcpServerSocket server = tcp::listen("127.0.0.1", 0, 1)!!;
	defer (void)server.close();

	test::@error(server.peer_address());
}

fn void test_socket_unconnected_peer_port_error() @feat(!EMSCRIPTEN)
{
	TcpServerSocket server = tcp::listen("127.0.0.1", 0, 1)!!;
	defer (void)server.close();

	test::@error(server.peer_port());
}