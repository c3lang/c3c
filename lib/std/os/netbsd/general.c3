module std::os::netbsd @if(env::NETBSD);

const CTL_UNSPEC      = 0;   // unused
const CTL_KERN        = 1;   // "high kernel": proc, limits
const CTL_VM          = 2;   // virtual memory
const CTL_VFS         = 3;   // file system, mount type is next
const CTL_NET         = 4;   // network, see socket.h
const CTL_DEBUG       = 5;   // debugging parameters
const CTL_HW          = 6;   // generic CPU/io
const CTL_MACHDEP     = 7;   // machine dependent
const CTL_USER        = 8;   // user-level
const CTL_DDB         = 9;   // in-kernel debugger
const CTL_PROC        = 10;  // per-proc attr
const CTL_VENDOR      = 11;  // vendor-specific data
const CTL_EMUL        = 12;  // emulation-specific data
const CTL_SECURITY    = 13;  // security

const HW_MACHINE      = 1;   // string: machine class
const HW_MODEL        = 2;   // string: specific machine model
const HW_NCPU         = 3;   // int: number of cpus
const HW_BYTEORDER    = 4;   // int: machine byte order
const HW_PHYSMEM      = 5;   // int: total memory (bytes)
const HW_USERMEM      = 6;   // int: non-kernel memory (bytes)
const HW_PAGESIZE     = 7;   // int: software page size
const HW_DISKNAMES    = 8;   // string: disk drive names
const HW_IOSTATS      = 9;   // struct: iostats[]
const HW_MACHINE_ARCH = 10;  // string: machine architecture
const HW_ALIGNBYTES   = 11;  // int: ALIGNBYTES for the kernel
const HW_CNMAGIC      = 12;  // string: console magic sequence(s)
const HW_PHYSMEM64    = 13;  // quad: total memory (bytes)
const HW_USERMEM64    = 14;  // quad: non-kernel memory (bytes)
const HW_IOSTATNAMES  = 15;  // string: iostat names
const HW_NCPUONLINE   = 16;  // number CPUs online
