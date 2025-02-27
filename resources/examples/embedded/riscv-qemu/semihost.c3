module semihost;

// See: https://github.com/ARM-software/abi-aa/blob/main/semihosting/semihosting.rst#sys-exit-extended-0x20

extern fn int sys_semihost(int operation, SemihostParameters* parms);

struct SemihostParameters
{
	int field1;
	int field2;
}

const int SYS_EXIT_EXTENDED = 0x20;
const int ADP_STOPPED_APPLICATIONEXIT = 0x20026;

fn void exit(int status)
{
	SemihostParameters parms;
	parms.field1 = ADP_STOPPED_APPLICATIONEXIT;
	parms.field2 = status;
	sys_semihost(SYS_EXIT_EXTENDED, &parms);
}
