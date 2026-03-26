module std::os::freebsd @if(env::FREEBSD);



// Top-level identifiers <sys/sysctl.h>

const CTL_SYSCTL	= 0;		// "magic" numbers 
const CTL_KERN		= 1;		// "high kernel": proc, limits 
const CTL_VM		= 2;		// virtual memory 
const CTL_VFS		= 3;		// filesystem, mount type is next 
const CTL_NET		= 4;		// network, see socket.h 
const CTL_DEBUG		= 5;		// debugging parameters 
const CTL_HW		= 6;		// generic cpu/io 
const CTL_MACHDEP	= 7;		// machine dependent 
const CTL_USER		= 8;		// user-level 
const CTL_P1003_1B	= 9;		// POSIX 1003.1B 





// CTL_HW identifiers

const HW_MACHINE		 = 1;		// string: machine class 
const HW_MODEL			 = 2;		// string: specific machine model 
const HW_NCPU			 = 3;		// int: number of cpus 
const HW_BYTEORDER		 = 4;		// int: machine byte order 
const HW_PHYSMEM		 = 5;		// int: total memory 
const HW_USERMEM		 = 6;		// int: non-kernel memory 
const HW_PAGESIZE		 = 7;		// int: software page size 
const HW_DISKNAMES		 = 8;		// strings: disk drive names 
const HW_DISKSTATS		 = 9;		// struct: diskstats[] 
const HW_FLOATINGPT		 = 10;		// int: has hw floating point? 
const HW_MACHINE_ARCH	 = 11;		// string: machine architecture 
const HW_REALMEM		 = 12;		// int: 'real' memory 



