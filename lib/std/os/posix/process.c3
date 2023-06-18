module std::os::posix @if(POSIX_LIBC);

struct Posix_spawn_file_actions_t
{
	int __allocated;
	int __used;
	void* __actions;
	int[16] __pad;
}

struct Posix_spawnattr_t
{
	void*[42] opaque;
}

extern fn CInt posix_spawn_file_actions_init(Posix_spawn_file_actions_t *file_actions);
extern fn CInt posix_spawn_file_actions_destroy(Posix_spawn_file_actions_t *file_actions);
extern fn CInt posix_spawn_file_actions_addclose(Posix_spawn_file_actions_t *file_actions, CInt fd);
extern fn CInt posix_spawn_file_actions_adddup2(Posix_spawn_file_actions_t *file_actions, CInt fd, CInt newfd);

def spawn_file_actions_init = posix_spawn_file_actions_init;
def spawn_file_actions_destroy = posix_spawn_file_actions_destroy;
def spawn_file_actions_addclose = posix_spawn_file_actions_addclose;
def spawn_file_actions_adddup2 = posix_spawn_file_actions_adddup2;

extern fn CInt posix_spawnp(Pid_t* pid, char* file, Posix_spawn_file_actions_t* file_actions,
	Posix_spawnattr_t* attrp, ZString* argv, ZString* envp);
extern fn CInt posix_spawn(Pid_t* pid, char* file, Posix_spawn_file_actions_t* file_actions,
	Posix_spawnattr_t* attrp, ZString* argv, ZString* envp);
def spawnp = posix_spawnp;
def spawn = posix_spawn;

extern fn CInt kill(Pid_t pid, CInt sig);
extern fn Pid_t waitpid(Pid_t pid, CInt* stat_loc, int options);

macro CInt wEXITSTATUS(CInt status) => (status & 0xff00) >> 8;
macro CInt wTERMSIG(CInt status) => status & 0x7f;
macro CInt wSTOPSIG(CInt status) => wEXITSTATUS(status);
macro bool wIFEXITED(CInt status) => wTERMSIG(status) == 0;
macro bool wIFSIGNALED(CInt status) => ((ichar) ((status & 0x7f) + 1) >> 1) > 0;
macro bool wIFSTOPPED(CInt status) => (status & 0xff) == 0x7f;
macro bool wIFCONTINUED(CInt status) => status == __W_CONTINUED;
macro CInt wWCOREDUMP(CInt status) => status & __WCOREFLAG;
macro CInt w_EXITCODE(CInt ret, CInt sig) => (ret << 8) | sig;
macro CInt w_STOPCODE(CInt sig) => (sig << 8) | 0x7f;
const CInt __WCOREFLAG = 0x80;
const CInt __W_CONTINUED = 0xffff;
const CInt WNOHANG = 1;
const CInt WUNTRACES = 2;
