module std::net::os;

$if ($defined(PLATFORM_AF_INET)):

const AI_PASSIVE = 0x1;
const AI_CANONNAME = 0x2;
const AI_NUMERICHOST = 0x4;

const int AF_UNSPEC = 0;
const int AF_INET = PLATFORM_AF_INET;
const int AF_APPLETALK = PLATFORM_AF_APPLETALK;
const int AF_IPX = PLATFORM_AF_IPX;
const int AF_INET6 = PLATFORM_AF_INET6;

const O_NONBLOCK = PLATFORM_O_NONBLOCK;


$endif;