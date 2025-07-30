module std::net::os @if(env::OPENBSD);
import libc;

// for most of this, see OpenBSD /usr/include/sys/socket.h
const AIFlags AI_EXT                    = 0x8;
const AIFlags AI_NUMERICSERV            = 0x10;
const AIFlags AI_FQDN                   = 0x20;
const AIFlags AI_ADDRCONFIG             = 0x40;

const AIFamily PLATFORM_AF_LOCAL                = AF_UNIX;         // draft POSIX compatibility
const AIFamily PLATFORM_AF_IMPLINK              =  3;              // arpanet imp addresses
const AIFamily PLATFORM_AF_PUP                  =  4;              // pup protocols: e.g. BSP
const AIFamily PLATFORM_AF_CHAOS                =  5;              // mit CHAOS protocols
const AIFamily PLATFORM_AF_NS                   =  6;              // XEROX NS protocols
const AIFamily PLATFORM_AF_ISO                  =  7;              // ISO protocols
const AIFamily PLATFORM_AF_OSI                  = PLATFORM_AF_ISO;
const AIFamily PLATFORM_AF_ECMA                 =  8;              // european computer manufacturers
const AIFamily PLATFORM_AF_DATAKIT              =  9;              // datakit protocols
const AIFamily PLATFORM_AF_CCITT                = 10;              // CCITT protocols, X.25 etc
const AIFamily PLATFORM_AF_SNA                  = 11;              // IBM SNA
const AIFamily PLATFORM_AF_DECNET               = 12;              // DECnet
const AIFamily PLATFORM_AF_DLI                  = 13;              // DEC Direct data link interface
const AIFamily PLATFORM_AF_LAT                  = 14;              // LAT
const AIFamily PLATFORM_AF_HYLINK               = 15;              // NSC Hyperchannel
const AIFamily PLATFORM_AF_APPLETALK            = 16;              // Apple Talk
const AIFamily PLATFORM_AF_ROUTE                = 17;              // Internal Routing Protocol
const AIFamily PLATFORM_AF_LINK                 = 18;              // Link layer interface
const AIFamily PLATFORM_PSEUDO_AF_XTP           = 19;              // eXpress Transfer Protocol (no AF)
const AIFamily PLATFORM_AF_COIP                 = 20;              // connection-oriented IP, aka ST II
const AIFamily PLATFORM_AF_CNT                  = 21;              // Computer Network Technology
const AIFamily PLATFORM_PSEUDO_AF_RTIP          = 22;              // Help Identify RTIP packets
const AIFamily PLATFORM_AF_IPX                  = 23;              // Novell Internet Protocol
const AIFamily PLATFORM_AF_INET6                = 24;              // IPv6
const AIFamily PLATFORM_PSEUDO_AF_PIP           = 25;              // Help Identify PIP packets
const AIFamily PLATFORM_AF_ISDN                 = 26;              // Integrated Services Digital Network*/
const AIFamily PLATFORM_AF_E164                 = PLATFORM_AF_ISDN;         // CCITT E.164 recommendation
const AIFamily PLATFORM_AF_NATM                 = 27;              // native ATM access
const AIFamily PLATFORM_AF_ENCAP                = 28;
const AIFamily PLATFORM_AF_SIP                  = 29;              // Simple Internet Protocol
const AIFamily PLATFORM_AF_KEY                  = 30;
const AIFamily PLATFORM_PSEUDO_AF_HDRCMPLT      = 31;           // Used by BPF to not rewrite headers in interface output routine
const AIFamily PLATFORM_AF_BLUETOOTH            = 32;              // Bluetooth
const AIFamily PLATFORM_AF_MPLS                 = 33;              // MPLS
const AIFamily PLATFORM_PSEUDO_AF_PFLOW         = 34;              // pflow
const AIFamily PLATFORM_PSEUDO_AF_PIPEX         = 35;              // PIPEX
const AIFamily PLATFORM_AF_FRAME                = 36;              // frame (Ethernet) sockets
const AIFamily PLATFORM_AF_MAX                  = 37;

// see: https://github.com/openbsd/src/blob/1ca4dc792538d17b4c2bd71550f68070505fd7b9/sys/sys/socket.h#L157
const int SOL_SOCKET = 0xFFFF;

const int SO_DEBUG        = 0x0001;   // turn on debugging info recording
const int SO_ACCEPTCONN   = 0x0002;   // socket has had listen()
const int SO_REUSEADDR    = 0x0004;   // allow local address reuse
const int SO_KEEPALIVE    = 0x0008;   // keep connections alive
const int SO_DONTROUTE    = 0x0010;   // just use interface addresses
const int SO_BROADCAST    = 0x0020;   // permit sending of broadcast msgs
const int SO_USELOOPBACK  = 0x0040;   // bypass hardware when possible
const int SO_LINGER       = 0x0080;   // linger on close if data present
const int SO_OOBINLINE    = 0x0100;   // leave received OOB data in line
const int SO_REUSEPORT    = 0x0200;   // allow local address & port reuse
const int SO_TIMESTAMP    = 0x0800;   // timestamp received dgram traffic
const int SO_BINDANY      = 0x1000;   // allow bind to any address
const int SO_ZEROIZE      = 0x2000;   // zero out all mbufs sent over socket

// additional
const int SO_SNDBUF       = 0x1001;   // send buffer size
const int SO_RCVBUF       = 0x1002;   // receive buffer size
const int SO_SNDLOWAT     = 0x1003;   // send low-water mark
const int SO_RCVLOWAT     = 0x1004;   // receive low-water mark
const int SO_SNDTIMEO     = 0x1005;   // send timeout
const int SO_RCVTIMEO     = 0x1006;   // receive timeout
const int SO_ERROR        = 0x1007;   // get error status and clear
const int SO_TYPE         = 0x1008;   // get socket type
const int SO_NETPROC      = 0x1020;   // multiplex; network processing
const int SO_RTABLE       = 0x1021;   // routing table to be used
const int SO_PEERCRED     = 0x1022;   // get connect-time credentials
const int SO_SPLICE       = 0x1023;   // splice data to other socket
const int SO_DOMAIN       = 0x1024;   // get socket domain
const int SO_PROTOCOL     = 0x1025;   // get socket protocol

// POLLIN through POLLNVAL are predefined by lib/std/net/os/posix.c3
const CUShort POLLRDNORM            = 0x0040;
const CUShort POLLNORM              = POLLRDNORM;
const CUShort POLLWRNORM            = POLLOUT;
const CUShort POLLRDBAND            = 0x0080;
const CUShort POLLWRBAND            = 0x0100;

const CInt MSG_OOB                 = 0x1;    // process out-of-band data
const CInt MSG_PEEK                = 0x2;    // peek at incoming message
const CInt MSG_DONTROUTE           = 0x4;    // send without using routing tables
const CInt MSG_EOR                 = 0x8;    // data completes record
const CInt MSG_TRUNC               = 0x10;   // data discarded before delivery
const CInt MSG_CTRUNC              = 0x20;   // control data lost before delivery
const CInt MSG_WAITALL             = 0x40;   // wait for full request or error
const CInt MSG_DONTWAIT            = 0x80;   // this message should be nonblocking
const CInt MSG_BCAST               = 0x100;  // this message rec'd as broadcast
const CInt MSG_MCAST               = 0x200;  // this message rec'd as multicast
const CInt MSG_NOSIGNAL            = 0x400;  // do not send SIGPIPE
const CInt MSG_CMSG_CLOEXEC        = 0x800;  // set FD_CLOEXEC on received fds
const CInt MSG_WAITFORONE          = 0x1000; // nonblocking but wait for one msg

// socket creation options
const SOCK_CLOEXEC                  = 0x8000; // set FD_CLOEXEC
const SOCK_NONBLOCK                 = 0x4000; // set O_NONBLOCK
const SOCK_NONBLOCK_INHERIT         = 0x2000; // inherit O_NONBLOCK from listener
const SOCK_DNS                      = 0x1000; // set SS_DNS

const PLATFORM_O_NONBLOCK = SOCK_NONBLOCK;
