module std::net::os @if(env::EMSCRIPTEN);

const AIFamily PLATFORM_AF_INET6        = (AIFamily)10;
const AIFamily PLATFORM_AF_IPX          = (AIFamily)4;
const AIFamily PLATFORM_AF_APPLETALK    = (AIFamily)5;

const int SOL_SOCKET = 1;
const int SO_REUSEADDR = 2;
const int SO_TYPE = 3;
const int SO_ERROR = 4;
const int SO_DONTROUTE = 5;
const int SO_BROADCAST = 6;
const int SO_SNDBUF = 7;
const int SO_RCVBUF = 8;
const int SO_KEEPALIVE = 9;
const int SO_OOBINLINE = 10;
const int SO_REUSEPORT = 15;

const CUShort POLLRDNORM = 0x0040;
const CUShort POLLRDBAND = 0x0080;
const CUShort POLLWRNORM = 0x0100;
const CUShort POLLWRBAND = 0x0200;

const CInt MSG_PEEK = 0x0002;
const int PLATFORM_O_NONBLOCK = 0o4000;
