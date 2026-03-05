module libc @if(env::DARWIN ||| env::BSD_FAMILY);

const CInt SIGIOT = 6;
const CInt SIGEMT = 7;
const CInt SIGIO = 23;
const CInt SIGPROF = 27;
const CInt SIGINFO = 29;
const CInt SIGTHR = 32;
