module std::net;
import std::io;
import libc;

const int SOCK_STREAM = 1;      // Stream
const int SOCK_DGRAM = 2;       // Datagram
const int SOCK_RAW = 3;         // Raw
const int SOCK_RDM = 4;         // Reliably delivered
const int SOCK_SEQPACKET = 5;   // Sequential packet

enum SocketOption : char (CInt value) @if(!env::WIN32)
{
    REUSEADDR (os::SO_REUSEADDR),
    REUSEPORT (os::SO_REUSEPORT),
    KEEPALIVE (os::SO_KEEPALIVE),
    BROADCAST (os::SO_BROADCAST),
}
enum SocketOption : char (CInt value) @if(env::WIN32)
{
    REUSEADDR (os::SO_REUSEADDR),
    KEEPALIVE (os::SO_KEEPALIVE),
    BROADCAST (os::SO_BROADCAST),
}

struct Socket
{
    NativeSocket sock;
    Socklen_t ai_addrlen;
    // TODO proper way to get the size of sockaddr_storage
    // https://learn.microsoft.com/en-us/previous-versions/windows/desktop/legacy/ms740504(v=vs.85)
    char[128] ai_addr_storage;
}

fn Stream Socket.as_stream(&self)
{
    return { .fns = &socketstream_interface, .data = self };
}

StreamInterface socketstream_interface = {
    .read_fn = fn(s, char[] bytes) => ((Socket*)s.data).read(bytes) @inline,
    .write_fn = fn(s, char[] bytes) => ((Socket*)s.data).write(bytes) @inline,
    .close_fn = fn(s) => ((Socket*)s.data).close(),
};

fn void! Socket.set_option(&self, SocketOption option)
{
    CInt flag = 1;
    int errcode = os::setsockopt(self.sock, os::SOL_SOCKET, option.value, &flag, CInt.sizeof);
    if (errcode != 0) return NetError.SOCKOPT_FAILED?;
}

fn void! Socket.unset_option(&self, SocketOption option)
{
    CInt flag = 0;
    int errcode = os::setsockopt(self.sock, os::SOL_SOCKET, option.value, &flag, CInt.sizeof);
    if (errcode != 0) return NetError.SOCKOPT_FAILED?;
}

fn usz! Socket.read(&self, char[] bytes)
{
    isz n = libc::read(self.sock, bytes.ptr, bytes.len);
    if (n < 0) return NetError.READ_FAILED?;
    return (usz)n;
}

fn usz! Socket.write(&self, char[] bytes)
{
    isz n = libc::write(self.sock, bytes.ptr, bytes.len);
    if (n < 0) return NetError.WRITE_FAILED?;
    return (usz)n;
}

fn void! Socket.close(&self) @inline
{
    self.sock.close()!;
}

struct Listener
{
    Socket socket;
}

fn void! Listener.init(&self, Network network, String host, String port, int backlog = 10, SocketOption... options)
{
    *self = { .socket = network.listen(host, port, backlog, ...options)! };
}

fn Socket! Listener.accept(&self)
{
    Socket sock = self.socket;
    sock.sock = os::accept(sock.sock, (SockAddrPtr)&sock.ai_addr_storage, &sock.ai_addrlen);
    if (sock.sock < 0) return NetError.ACCEPT_FAILED?;
    return sock;
}

fn void! Listener.close(&self) @inline
{
    self.socket.close()!;
}