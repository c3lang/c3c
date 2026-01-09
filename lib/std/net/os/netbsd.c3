module std::net::os @if(env::NETBSD);
import libc;

const AIFamily PLATFORM_AF_UNSPEC          = 0;                 // unspecified
const AIFamily PLATFORM_AF_LOCAL           = 1;                 // local to host
const AIFamily PLATFORM_AF_UNIX            = PLATFORM_AF_LOCAL; // backward compatibility
const AIFamily PLATFORM_AF_INET            = 2;                 // internetwork: UDP, TCP, etc.
const AIFamily PLATFORM_AF_IMPLINK         = 3;                 // arpanet imp addresses
const AIFamily PLATFORM_AF_PUP             = 4;                 // pup protocols: e.g. BSP
const AIFamily PLATFORM_AF_CHAOS           = 5;                 // mit CHAOS protocols
const AIFamily PLATFORM_AF_NS              = 6;                 // XEROX NS protocols
const AIFamily PLATFORM_AF_ISO             = 7;                 // ISO protocols
const AIFamily PLATFORM_AF_OSI             = PLATFORM_AF_ISO;
const AIFamily PLATFORM_AF_ECMA            = 8;                 // european computer manufacturers
const AIFamily PLATFORM_AF_DATAKIT         = 9;                 // datakit protocols
const AIFamily PLATFORM_AF_CCITT           = 10;                // CCITT protocols, X.25 etc
const AIFamily PLATFORM_AF_SNA             = 11;                // IBM SNA
const AIFamily PLATFORM_AF_DECNET          = 12;                // DECnet
const AIFamily PLATFORM_AF_DLI             = 13;                // DEC Direct data link interface
const AIFamily PLATFORM_AF_LAT             = 14;                // LAT
const AIFamily PLATFORM_AF_HYLINK          = 15;                // NSC Hyperchannel
const AIFamily PLATFORM_AF_APPLETALK       = 16;                // Apple Talk
const AIFamily PLATFORM_AF_OROUTE          = 17;                // Internal Routing Protocol
const AIFamily PLATFORM_AF_LINK            = 18;                // Link layer interface
const AIFamily PLATFORM_PSEUDO_AF_XTP      = 19;                // eXpress Transfer Protocol (no AF)
const AIFamily PLATFORM_AF_COIP            = 20;                // connection-oriented IP, aka ST II
const AIFamily PLATFORM_AF_CNT             = 21;                // Computer Network Technology
const AIFamily PLATFORM_PSEUDO_AF_RTIP     = 22;                // Help Identify RTIP packets
const AIFamily PLATFORM_AF_IPX             = 23;                // Novell Internet Protocol
const AIFamily PLATFORM_AF_INET6           = 24;                // IP version 6
const AIFamily PLATFORM_PSEUDO_AF_PIP      = 25;                // Help Identify PIP packets
const AIFamily PLATFORM_AF_ISDN            = 26;                // Integrated Services Digital Networ
const AIFamily PLATFORM_AF_E164            = PLATFORM_AF_ISDN;  // CCITT E.164 recommendation
const AIFamily PLATFORM_AF_NATM            = 27;                // native ATM access
const AIFamily PLATFORM_AF_ARP             = 28;                // (rev.) addr. res. prot. (RFC 826)
const AIFamily PLATFORM_PSEUDO_AF_KEY      = 29;                // Internal key management protocol
const AIFamily PLATFORM_PSEUDO_AF_HDRCMPLT = 30;                /* Used by BPF to not rewrite hdrs
                                                                 * in interface output routine */
const AIFamily PLATFORM_AF_BLUETOOTH       = 31;                // Bluetooth: HCI, SCO, L2CAP, RFCOMM
const AIFamily PLATFORM_AF_IEEE80211       = 32;                // IEEE80211
const AIFamily PLATFORM_AF_MPLS            = 33;                // MultiProtocol Label Switching
const AIFamily PLATFORM_AF_ROUTE           = 34;                // Internal Routing Protocol
const AIFamily PLATFORM_AF_CAN             = 35;
const AIFamily PLATFORM_AF_ETHER           = 36;
const AIFamily PLATFORM_AF_MAX             = 37;

const int SOL_SOCKET = 0xFFFF;

const int SO_DEBUG        = 0x0001;    // turn on debugging info recording
const int SO_ACCEPTCONN   = 0x0002;    // socket has had listen()
const int SO_REUSEADDR    = 0x0004;    // allow local address reuse
const int SO_KEEPALIVE    = 0x0008;    // keep connections alive
const int SO_DONTROUTE    = 0x0010;    // just use interface addresses
const int SO_BROADCAST    = 0x0020;    // permit sending of broadcast msgs
const int SO_USELOOPBACK  = 0x0040;    // bypass hardware when possible
const int SO_LINGER       = 0x0080;    // linger on close if data present
const int SO_OOBINLINE    = 0x0100;    // leave received OOB data in line
const int SO_REUSEPORT    = 0x0200;    // allow local address & port reuse
const int SO_NOSIGPIPE    = 0x0800;    // no SIGPIPE from EPIPE
const int SO_ACCEPTFILTER = 0x1000;    // there is an accept filter
const int SO_TIMESTAMP    = 0x2000;    // timestamp received dgram traffic
const int SO_RERROR       = 0x4000;    // Keep track of receive errors

// additional
const int SO_SNDBUF     = 0x1001;      // send buffer size
const int SO_RCVBUF     = 0x1002;      // receive buffer size
const int SO_SNDLOWAT   = 0x1003;      // send low-water mark
const int SO_RCVLOWAT   = 0x1004;      // receive low-water mark
const int SO_ERROR      = 0x1007;      // get error status and clear
const int SO_TYPE       = 0x1008;      // get socket type
const int SO_OVERFLOWED = 0x1009;      // datagrams: return packets dropped

const int SO_NOHEADER   = 0x100a;      /* user supplies no header to kernel;
                                        * kernel removes header and supplies
                                        * payload
                                        */
const int SO_SNDTIMEO   = 0x100b;       // send timeout
const int SO_RCVTIMEO   = 0x100c;       // receive timeout

// POLLIN through POLLNVAL are predefined by lib/std/net/os/posix.c3
const CUShort POLLRDNORM = 0x0040;
const CUShort POLLWRNORM = POLLOUT;
const CUShort POLLRDBAND = 0x0080;
const CUShort POLLWRBAND = 0x0100;

const CInt MSG_OOB          = 0x0001;    // process out-of-band data
const CInt MSG_PEEK         = 0x0002;    // peek at incoming message
const CInt MSG_DONTROUTE    = 0x0004;    // send without using routing tables
const CInt MSG_EOR          = 0x0008;    // data completes record
const CInt MSG_TRUNC        = 0x0010;    // data discarded before delivery
const CInt MSG_CTRUNC       = 0x0020;    // control data lost before delivery
const CInt MSG_WAITALL      = 0x0040;    // wait for full request or error
const CInt MSG_DONTWAIT     = 0x0080;    // this message should be nonblocking
const CInt MSG_BCAST        = 0x0100;    // this message was rcvd using link-level brdcst
const CInt MSG_MCAST        = 0x0200;    // this message was rcvd using link-level mcast
const CInt MSG_NOSIGNAL     = 0x0400;    // do not generate SIGPIPE on EOF
const CInt MSG_CMSG_CLOEXEC = 0x0800;    // close on exec receiving fd
const CInt MSG_NBIO         = 0x1000;    // use non-blocking I/O
const CInt MSG_WAITFORONE   = 0x2000;    // recvmmsg() wait for one message
const CInt MSG_NOTIFICATION = 0x4000;    // SCTP notification

// socket creation options
const SOCK_CLOEXEC      = 0x10000000;    // set close on exec on socket
const SOCK_NONBLOCK     = 0x20000000;    // set non blocking i/o socket
const SOCK_NOSIGPIPE    = 0x40000000;    // don't send sigpipe
const SOCK_FLAGS_MASK   = 0xf0000000;    // flags mask

const PLATFORM_O_NONBLOCK = SOCK_NONBLOCK;
