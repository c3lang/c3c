module std::net;
import std::io;
import libc;

const int SOCK_STREAM = 1;      // Stream
const int SOCK_DGRAM = 2;       // Datagram
const int SOCK_RAW = 3;         // Raw
const int SOCK_RDM = 4;         // Reliably delivered
const int SOCK_SEQPACKET = 5;   // Sequential packet