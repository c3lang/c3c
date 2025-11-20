module std::os::darwin @if(env::DARWIN);

alias __Darwin_sigset_t = uint;
alias __Darwin_size_t = usz;

struct __Darwin_arm_exception_state64
{
	ulong __far;       /* Virtual Fault Address */
	uint  __esr;       /* Exception syndrome */
	uint  __exception; /* number of arm exception taken */
}

struct __Darwin_arm_thread_state64
{
	ulong[29] __x;   /* General purpose registers x0-x28 */
	ulong    __fp;   /* Frame pointer x29 */
	ulong    __lr;   /* Link register x30 */
	ulong    __sp;   /* Stack pointer x31 */
	ulong    __pc;   /* Program counter */
	ulong    __cpsr; /* Current program status register */
	ulong    __pad;  /* Same size for 32-bit or 64-bit clients */
}

struct __Darwin_arm_neon_state64
{
	uint128[32] __v;
	uint        __fpsr;
	uint        __fpcr;
}

struct __Darwin_mcontext64
{
	__Darwin_arm_exception_state64   __es;
	__Darwin_arm_thread_state64      __ss;
	__Darwin_arm_neon_state64        __ns;
}

struct __Darwin_sigaltstack
{
	void*           ss_sp;         /* signal stack base */
	__Darwin_size_t ss_size;        /* signal stack length */
	int             ss_flags;       /* SA_DISABLE and/or SA_ONSTACK */
}

