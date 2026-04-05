module std::net::os @if(env::FREEBSD);
import libc;

// all of this, see FreeBSD /usr/include/sys/socket.h


// Creation flags, OR'ed into socket() and socketpair() type argument.
const SOCK_CLOEXEC		= 0x10000000;
const SOCK_NONBLOCK		= 0x20000000;
const SOCK_CLOFORK		= 0x40000000;


//  Option flags per-socket.

const int SO_DEBUG			= 0x00000001;	// turn on debugging info recording 
const int SO_ACCEPTCONN		= 0x00000002;	// socket has had listen() 
const int SO_REUSEADDR		= 0x00000004;	// allow local address reuse 
const int SO_KEEPALIVE		= 0x00000008;	// keep connections alive 
const int SO_DONTROUTE		= 0x00000010;	// just use interface addresses 
const int SO_BROADCAST		= 0x00000020;	// permit sending of broadcast msgs 
const int SO_USELOOPBACK	= 0x00000040;	// bypass hardware when possible 
const int SO_LINGER			= 0x00000080;	// linger on close if data present 
const int SO_OOBINLINE		= 0x00000100;	// leave received OOB data in line 
const int SO_REUSEPORT		= 0x00000200;	// allow local address & port reuse 
const int SO_TIMESTAMP		= 0x00000400;	// timestamp received dgram traffic 
const int SO_NOSIGPIPE		= 0x00000800;	// no SIGPIPE from EPIPE 
const int SO_ACCEPTFILTER	= 0x00001000;	// there is an accept filter 
const int SO_BINTIME		= 0x00002000;	// timestamp received dgram traffic 
const int SO_NO_OFFLOAD		= 0x00004000;	// socket cannot be offloaded 
const int SO_NO_DDP			= 0x00008000;	// disable direct data placement 
const int SO_REUSEPORT_LB	= 0x00010000;	// reuse with load balancing 
const int SO_RERROR			= 0x00020000;	// keep track of receive errors 



// Additional options, not kept in so_options.

const int SO_SNDBUF				= 0x1001;		// send buffer size 
const int SO_RCVBUF				= 0x1002;		// receive buffer size 
const int SO_SNDLOWAT			= 0x1003;		// send low-water mark 
const int SO_RCVLOWAT			= 0x1004;		// receive low-water mark 
const int SO_SNDTIMEO			= 0x1005;		// send timeout 
const int SO_RCVTIMEO			= 0x1006;		// receive timeout 
const int SO_ERROR				= 0x1007;		// get error status and clear 
const int SO_TYPE				= 0x1008;		// get socket type 
const int SO_LABEL				= 0x1009;		// socket's MAC label 
const int SO_PEERLABEL			= 0x1010;		// socket's peer's MAC label
const int SO_LISTENQLIMIT		= 0x1011;		// socket's backlog limit 
const int SO_LISTENQLEN			= 0x1012;		// socket's complete queue lenth
const int SO_LISTENINCQLEN		= 0x1013;		// socket's incomplete queue lenth
const int SO_FIB				= 0x1014;		// get or set socket FIB 
const int SO_SETFIB				= SO_FIB;		// backward compat alias 
const int SO_USER_COOKIE		= 0x1015;		// user cookie (dummynet etc.)
const int SO_PROTOCOL			= 0x1016;		// get socket protocol (Linux name)
const int SO_PROTOTYPE			= SO_PROTOCOL;	// alias for SO_PROTOCOL (SunOS name)
const int SO_TS_CLOCK			= 0x1017;		// clock type used for SO_TIMESTAMP
const int SO_MAX_PACING_RATE	= 0x1018;		// socket's max TX pacing rate (Linux name)
const int SO_DOMAIN				= 0x1019;		// get socket domain 
const int SO_SPLICE				= 0x1023;		// splice data to other socket 

const SO_TS_REALTIME_MICRO		= 0;	// microsecond resolution, realtime 
const SO_TS_BINTIME				= 1;	// sub-nanosecond resolution, realtime 
const SO_TS_REALTIME			= 2;	// nanosecond resolution, realtime 
const SO_TS_MONOTONIC			= 3;	// nanosecond resolution, monotonic 
const SO_TS_DEFAULT				= SO_TS_REALTIME_MICRO;
const SO_TS_CLOCK_MAX			= SO_TS_MONOTONIC;

const int SOL_SOCKET			= 0xFFFF;	// options for socket level


// Address families.

const AIFamily PLATFORM_AF_UNSPEC			= (AIFamily)0;		// unspecified
const AIFamily PLATFORM_AF_UNIX				= (AIFamily)1;		// standardized name for AF_LOCAL
const AIFamily PLATFORM_AF_LOCAL			= (AIFamily)PLATFORM_AF_UNIX;	// local to host (pipes, portals)
const AIFamily PLATFORM_AF_INET				= (AIFamily)2;		// internetwork: UDP, TCP, etc.
const AIFamily PLATFORM_AF_IMPLINK			= (AIFamily)3;		// arpanet imp addresses
const AIFamily PLATFORM_AF_PUP				= (AIFamily)4;		// pup protocols: e.g. BSP
const AIFamily PLATFORM_AF_CHAOS			= (AIFamily)5;		// mit CHAOS protocols
const AIFamily PLATFORM_AF_NETBIOS			= (AIFamily)6;		// SMB protocols
const AIFamily PLATFORM_AF_ISO				= (AIFamily)7;		// ISO protocols
const AIFamily PLATFORM_AF_OSI				= (AIFamily)PLATFORM_AF_ISO;
const AIFamily PLATFORM_AF_ECMA				= (AIFamily)8;		// European computer manufacturers
const AIFamily PLATFORM_AF_DATAKIT			= (AIFamily)9;		// datakit protocols
const AIFamily PLATFORM_AF_CCITT			= (AIFamily)10;		// CCITT protocols, X.25 etc
const AIFamily PLATFORM_AF_SNA				= (AIFamily)11;		// IBM SNA
const AIFamily PLATFORM_AF_DECNET			= (AIFamily)12;		// DECnet
const AIFamily PLATFORM_AF_DLI				= (AIFamily)13;		// DEC Direct data link interface
const AIFamily PLATFORM_AF_LAT				= (AIFamily)14;		// LAT
const AIFamily PLATFORM_AF_HYLINK			= (AIFamily)15;		// NSC Hyperchannel
const AIFamily PLATFORM_AF_APPLETALK		= (AIFamily)16;		// Apple Talk
const AIFamily PLATFORM_AF_ROUTE			= (AIFamily)17;		// Internal Routing Protocol
const AIFamily PLATFORM_AF_LINK				= (AIFamily)18;		// Link layer interface
const AIFamily PLATFORM_PSEUDO_AF_XTP		= (AIFamily)19;		// eXpress Transfer Protocol (no AF)
const AIFamily PLATFORM_AF_COIP				= (AIFamily)20;		// connection-oriented IP, aka ST II
const AIFamily PLATFORM_AF_CNT				= (AIFamily)21;		// Computer Network Technology
const AIFamily PLATFORM_PSEUDO_AF_RTIP		= (AIFamily)22;		// Help Identify RTIP packets
const AIFamily PLATFORM_AF_IPX				= (AIFamily)23;		// Novell Internet Protocol
const AIFamily PLATFORM_AF_SIP				= (AIFamily)24;		// Simple Internet Protocol
const AIFamily PLATFORM_PSEUDO_AF_PIP		= (AIFamily)25;		// Help Identify PIP packets
const AIFamily PLATFORM_AF_ISDN				= (AIFamily)26;		// Integrated Services Digital Network
const AIFamily PLATFORM_AF_E164				= (AIFamily)PLATFORM_AF_ISDN;	// CCITT E.164 recommendation
const AIFamily PLATFORM_PSEUDO_AF_KEY		= (AIFamily)27;		// Internal key-management function
const AIFamily PLATFORM_AF_INET6			= (AIFamily)28;		// IPv6
const AIFamily PLATFORM_AF_NATM				= (AIFamily)29;		// native ATM access
const AIFamily PLATFORM_AF_ATM				= (AIFamily)30;		// ATM
const AIFamily PLATFORM_PSEUDO_AF_HDRCMPLT  = (AIFamily)31;		// Used by BPF to not rewrite headers in interface output routine
const AIFamily PLATFORM_AF_NETGRAPH			= (AIFamily)32;		// Netgraph sockets
const AIFamily PLATFORM_AF_SLOW				= (AIFamily)33;		// 802.3ad slow protocol
const AIFamily PLATFORM_AF_SCLUSTER			= (AIFamily)34;		// Sitara cluster protocol
const AIFamily PLATFORM_AF_ARP				= (AIFamily)35;
const AIFamily PLATFORM_AF_BLUETOOTH		= (AIFamily)36;		// Bluetooth sockets
const AIFamily PLATFORM_AF_IEEE80211		= (AIFamily)37;		// IEEE 802.11 protocol
const AIFamily PLATFORM_AF_NETLINK			= (AIFamily)38;		// Netlink protocol
const AIFamily PLATFORM_AF_INET_SDP			= (AIFamily)40;		// OFED Socket Direct Protocol ipv4
const AIFamily PLATFORM_AF_INET6_SDP		= (AIFamily)42;		// OFED Socket Direct Protocol ipv6
const AIFamily PLATFORM_AF_HYPERV			= (AIFamily)43;		// HyperV sockets
const AIFamily PLATFORM_AF_DIVERT			= (AIFamily)44;		// divert(4)
const AIFamily PLATFORM_AF_IPFWLOG			= (AIFamily)46;
const AIFamily PLATFORM_AF_MAX				= (AIFamily)46;

const int SOCK_MAXADDRLEN = 255;	// longest possible addresses


//Protocol families, same as address families for now.

const PF_UNSPEC		= PLATFORM_AF_UNSPEC;
const PF_LOCAL		= PLATFORM_AF_LOCAL;
const PF_UNIX		= PLATFORM_AF_LOCAL;	// backward compatibility 
const PF_INET		= PLATFORM_AF_INET;
const PF_IMPLINK	= PLATFORM_AF_IMPLINK;
const PF_PUP		= PLATFORM_AF_PUP;
const PF_CHAOS		= PLATFORM_AF_CHAOS;
const PF_NETBIOS	= PLATFORM_AF_NETBIOS;
const PF_ISO		= PLATFORM_AF_ISO;
const PF_OSI		= PLATFORM_AF_ISO;
const PF_ECMA		= PLATFORM_AF_ECMA;
const PF_DATAKIT	= PLATFORM_AF_DATAKIT;
const PF_CCITT		= PLATFORM_AF_CCITT;
const PF_SNA		= PLATFORM_AF_SNA;
const PF_DECNET		= PLATFORM_AF_DECNET;
const PF_DLI		= PLATFORM_AF_DLI;
const PF_LAT		= PLATFORM_AF_LAT;
const PF_HYLINK		= PLATFORM_AF_HYLINK;
const PF_APPLETALK	= PLATFORM_AF_APPLETALK;
const PF_ROUTE		= PLATFORM_AF_ROUTE;
const PF_LINK		= PLATFORM_AF_LINK;
const PF_XTP		= PLATFORM_PSEUDO_AF_XTP;	// really just proto family, no AF
const PF_COIP		= PLATFORM_AF_COIP;
const PF_CNT		= PLATFORM_AF_CNT;
const PF_SIP		= PLATFORM_AF_SIP;
const PF_IPX		= PLATFORM_AF_IPX;
const PF_RTIP		= PLATFORM_PSEUDO_AF_RTIP;	// same format as AF_INET
const PF_PIP		= PLATFORM_PSEUDO_AF_PIP;
const PF_ISDN		= PLATFORM_AF_ISDN;
const PF_KEY		= PLATFORM_PSEUDO_AF_KEY;
const PF_INET6		= PLATFORM_AF_INET6;
const PF_NATM		= PLATFORM_AF_NATM;
const PF_ATM		= PLATFORM_AF_ATM;
const PF_NETGRAPH	= PLATFORM_AF_NETGRAPH;
const PF_SLOW		= PLATFORM_AF_SLOW;
const PF_SCLUSTER	= PLATFORM_AF_SCLUSTER;
const PF_ARP		= PLATFORM_AF_ARP;
const PF_BLUETOOTH	= PLATFORM_AF_BLUETOOTH;
const PF_IEEE80211	= PLATFORM_AF_IEEE80211;
const PF_NETLINK	= PLATFORM_AF_NETLINK;
const PF_INET_SDP	= PLATFORM_AF_INET_SDP;
const PF_INET6_SDP	= PLATFORM_AF_INET6_SDP;
const PF_DIVERT		= PLATFORM_AF_DIVERT;
const PF_IPFWLOG	= PLATFORM_AF_IPFWLOG;
const PF_MAX		= PLATFORM_AF_MAX;

// TODO: pickup from socket.h at #; also look at pool.h for what need to be implemented

//Maximum queue length specifiable by listen.
const int SOMAXCONN = 128;

// POLLIN through POLLNVAL are predefined by lib/std/net/os/posix.c3
const CUShort POLLRDNORM	= 0x0040;		//  non-OOB/URG data available 
const CUShort POLLWRNORM	= POLLOUT;		//  no write type differentiation 
const CUShort POLLRDBAND	= 0x0080;		//  OOB/Urgent readable data 
const CUShort POLLWRBAND	= 0x0100;		//  OOB/Urgent data can be written 
const CUShort POLLINIGNEOF	= 0x2000;		//  like POLLIN, except ignore EOF 
const CUShort POLLRDHUP		= 0x4000;		//  half shut down 



const CInt MSG_OOB				= 0x00000001;	// process out-of-band data 
const CInt MSG_PEEK				= 0x00000002;	// peek at incoming message 
const CInt MSG_DONTROUTE		= 0x00000004;	// send without using routing tables 
const CInt MSG_EOR				= 0x00000008;	// data completes record 
const CInt MSG_TRUNC			= 0x00000010;	// data discarded before delivery 
const CInt MSG_CTRUNC			= 0x00000020;	// control data lost before delivery 
const CInt MSG_WAITALL			= 0x00000040;	// wait for full request or error 
const CInt MSG_DONTWAIT			= 0x00000080;	// this message should be nonblocking 
const CInt MSG_EOF				= 0x00000100;	// data completes connection 
const CInt MSG_NOTIFICATION		= 0x00002000;	// SCTP notification 
const CInt MSG_NBIO				= 0x00004000;	// FIONBIO mode, used by fifofs 
const CInt MSG_COMPAT			= 0x00008000;	// used in sendit() 
const CInt MSG_NOSIGNAL			= 0x00020000;	// do not generate SIGPIPE on EOF 
const CInt MSG_CMSG_CLOEXEC		= 0x00040000;	// make received fds close-on-exec 
const CInt MSG_WAITFORONE		= 0x00080000;	// for recvmmsg() 
const CInt MSG_CMSG_CLOFORK		= 0x00400000;	// make received fds close-on-fork 


const PLATFORM_O_NONBLOCK = SOCK_NONBLOCK;
