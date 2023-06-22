module std::net::os @if(env::LINUX);
import libc;

const int PLATFORM_AF_AX25 = 3;
const int PLATFORM_AF_IPX = 4;
const int PLATFORM_AF_APPLETALK = 5;
const int PLATFORM_AF_NETROM = 6;
const int PLATFORM_AF_BRIDGE = 7;
const int PLATFORM_AF_AAL5 = 8;
const int PLATFORM_AF_X25 = 9;

const PLATFORM_O_NONBLOCK = 0o4000;