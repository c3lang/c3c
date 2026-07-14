module tcpconntimeouttest @test;
import std::net, std::time;

// Regression tests: a connect with a non-zero timeout used to fail with
// io::GENERAL_ERROR for every connection. The non-blocking completion was
// tested against POLLWRNORM instead of the subscribed POLLOUT, and a
// synchronous failure (a refused loopback connection on the BSDs) lost its
// errno before it was read. The real result now comes through
// getsockopt(SO_ERROR), or is captured as soon as the connect fails.

fn void test_connect_timeout_refused() @if(env::POSIX && !env::EMSCRIPTEN)
{
	// Nothing listens on port 1, so the connect must be refused with the real
	// error -- not masked as io::GENERAL_ERROR.
	if (catch err = tcp::connect("127.0.0.1", 1, time::SEC))
	{
		assert(err == net::CONNECTION_REFUSED);
		return;
	}
	assert(false, "connect to a closed port should be refused");
}

fn void test_connect_timeout_connects() @if(env::POSIX && !env::EMSCRIPTEN)
{
	tcp::TcpServerSocket server = tcp::listen("127.0.0.1", 47281, 4, net::SocketOption.REUSEADDR)!!;
	defer (void)server.close();

	// A timed connect to a listening socket must return a working, connected
	// socket -- verified with a round trip.
	tcp::TcpSocket client = tcp::connect("127.0.0.1", 47281, 5 * time::SEC)!!;
	defer (void)client.close();

	tcp::TcpSocket accepted = tcp::accept(&server)!!;
	defer (void)accepted.close();

	String expected = "ping";
	accepted.write(expected)!!;

	char[32] buf;
	client.read(&buf)!!;
	assert((String)buf[:expected.len] == expected);
}
