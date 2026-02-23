module std::os::linux @if(env::LINUX);
import libc;

// https://github.com/bminor/glibc/blob/master/sysdeps/unix/sysv/linux/sys/epoll.h
const uint EPOLLIN = EpollEvents.EPOLLIN;
const uint EPOLLPRI = EpollEvents.EPOLLPRI;
const uint EPOLLOUT = EpollEvents.EPOLLOUT;
const uint EPOLLRDNORM = EpollEvents.EPOLLRDNORM;
const uint EPOLLRDBAND = EpollEvents.EPOLLRDBAND;
const uint EPOLLWRNORM = EpollEvents.EPOLLWRNORM;
const uint EPOLLWRBAND = EpollEvents.EPOLLWRBAND;
const uint EPOLLMSG = EpollEvents.EPOLLMSG;
const uint EPOLLERR = EpollEvents.EPOLLERR;
const uint EPOLLHUP = EpollEvents.EPOLLHUP;
const uint EPOLLRDHUP = EpollEvents.EPOLLRDHUP;
const uint EPOLLEXCLUSIVE = EpollEvents.EPOLLEXCLUSIVE;
const uint EPOLLWAKEUP = EpollEvents.EPOLLWAKEUP;
const uint EPOLLONESHOT = EpollEvents.EPOLLONESHOT;
const uint EPOLLET = EpollEvents.EPOLLET;

constdef EpollEvents : inline uint
{
	EPOLLIN        = 0x001,
	EPOLLPRI       = 0x002,
	EPOLLOUT       = 0x004,
	EPOLLRDNORM    = 0x040,
	EPOLLRDBAND    = 0x080,
	EPOLLWRNORM    = 0x100,
	EPOLLWRBAND    = 0x200,
	EPOLLMSG       = 0x400,
	EPOLLERR       = 0x008,
	EPOLLHUP       = 0x010,
	EPOLLRDHUP     = 0x2000,
	EPOLLEXCLUSIVE = 1u << 28,
	EPOLLWAKEUP    = 1u << 29,
	EPOLLONESHOT   = 1u << 30,
	EPOLLET        = 1u << 31
}

/* Valid opcodes ( "op" parameter ) to issue to epoll_ctl().  */
const uint EPOLL_CTL_ADD = 1; /* Add a file descriptor to the interface.  */
const uint EPOLL_CTL_DEL = 2; /* Remove a file descriptor from the interface.  */
const uint EPOLL_CTL_MOD = 3; /* Change file descriptor epoll_event structure.  */

union EpollData 
{
	void*   ptr;
	int     fd;
	uint	u32;
	ulong   u64;
}

struct EpollEvent @packed 
{
	<* Epoll events *>
	uint	events;
	<* User data variable *>
	EpollData data;
}

struct EpollParams 
{
	uint	busy_poll_usecs;
	ushort  busy_poll_budget;
	char	prefer_busy_poll;
	
	<* pad the struct to a multiple of 64bits *>
	char	__pad;
}

// https://github.com/MatthiasWM/mosrun/blob/master/scratchpad.txt#L330-L348
/*
 * Ioctl's have the command encoded in the lower word,
 * and the size of any in or out parameters in the upper
 * word.  The high 2 bits of the upper word are used
 * to encode the in/out status of the parameter; for now
 * we restrict parameters to at most 256 bytes (disklabels are 216 bytes).
 */
const IOCPARM_MASK =	0xff;		/* parameters must be < 256 bytes */
const IOC_VOID =		0x20000000;	/* no parameters */
const IOC_OUT =			0x40000000;	/* copy out parameters */
const IOC_IN =			0x80000000;	/* copy in parameters */
const IOC_INOUT =		(IOC_IN|IOC_OUT);

macro ulong @ioctl_IO   ($x,$y)		 @const  => (IOC_VOID | (($x)<<8)|y);
macro ulong @ioctl_IOR  ($x,$y,$Type)   @const  => (IOC_OUT  |(($Type.sizeof&IOCPARM_MASK)<<16)|(($x)<<8)|$y);
macro ulong @ioctl_IOW  ($x,$y,$Type)   @const  => (IOC_IN   |(($Type.sizeof&IOCPARM_MASK)<<16)|(($x)<<8)|$y);
/* this should be _IORW, but stdio got there first */
macro ulong @ioctl_IOWR ($x,$y,$Type)   @const  => (IOC_INOUT|(($Type.sizeof&IOCPARM_MASK)<<16)|(($x)<<8)|$y);
macro ulong @ioctl_ION  ($x,$y,$n)	  @const  => (IOC_INOUT|((($n)&IOCPARM_MASK)<<16)|(($x)<<8)|y);

// https://github.com/bminor/glibc/blob/master/sysdeps/unix/sysv/linux/sys/epoll.h
const EPOLL_IOC_TYPE = 0x8A;
const EPIOCSPARAMS = @ioctl_IOW(EPOLL_IOC_TYPE, 0x01, EpollParams);
const EPIOCGPARAMS = @ioctl_IOR(EPOLL_IOC_TYPE, 0x02, EpollParams);

<* 
* Creates an epoll instance. Returns an fd for the new instance.
* The "size" parameter is a hint specifying the number of file
* descriptors to be associated with the new instance.  
* The fd returned by epoll_create() should be closed with close(). 
*>
extern fn int epoll_create(int);

<* 
* Same as epoll_create but with an FLAGS parameter.
* The unused SIZE parameter has been dropped.  
*>
extern fn int epoll_create1(int);

<* 
* Manipulate an epoll instance "epfd". Returns 0 in case of success,
* -1 in case of error ( the "errno" variable will contain the
* specific error code ) The "op" parameter is one of the EPOLL_CTL_*
* constants defined above. The "fd" parameter is the target of the
* operation. The "event" parameter describes which events the caller
* is interested in and any associated user data.  
*>
extern fn int epoll_ctl (int, int, int, EpollEvent*);

<* 
* Wait for events on an epoll instance "epfd". Returns the number of
* triggered events returned in "events" buffer. Or -1 in case of
* error with the "errno" variable set to the specific error code. The
* "events" parameter is a buffer that will contain triggered
* events. The "maxevents" is the maximum number of events to be
* returned ( usually size of "events" ). The "timeout" parameter
* specifies the maximum wait time in milliseconds (-1 == infinite).
*>
extern fn int epoll_wait (int, EpollEvent*, int, int);

<* 
* Same as epoll_wait, but the thread's signal mask is temporarily
* and atomically replaced with the one provided as parameter.
*>
extern fn int epoll_pwait (int, EpollEvent*, int, int, Sigset_t*);

<* 
* Same as epoll_pwait, but the timeout as a timespec.
*>
extern fn int epoll_pwait2 (int, EpollEvent*, int, TimeSpec*, Sigset_t*);
