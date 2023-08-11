module std::os::win32 @if(env::WIN32);

const Win32_DWORD STARTF_USESTDHANDLES = 0x00000100;
const Win32_DWORD CREATE_NO_WINDOW = 0x08000000;
const Win32_DWORD CREATE_PROTECTED_PROCESS = 0x00040000;
const Win32_DWORD CREATE_UNICODE_ENVIRONMENT = 0x00000400;
const uint WAIT_OBJECT_0 = 0;
const uint WAIT_ABANDONED = 128;
const uint WAIT_IO_COMPLETION = 192;
const uint WAIT_FAILED = (uint)-1;
const Win32_DWORD HANDLE_FLAG_INHERIT = 1;
const Win32_DWORD HANDLE_FLAG_PROTECT_FROM_CLOSE = 2;
const uint INFINITE = (uint)-1;
const Win32_DWORD PIPE_ACCESS_DUPLEX = 0x00000003;
const Win32_DWORD PIPE_ACCESS_INBOUND = 0x00000001;
const Win32_DWORD PIPE_ACCESS_OUTBOUND = 0x00000002;
const Win32_DWORD FILE_FLAG_FIRST_PIPE_INSTANCE = 0x00080000;
const Win32_DWORD FILE_FLAG_WRITE_THROUGH = 0x80000000;
const Win32_DWORD FILE_FLAG_OVERLAPPED = 0x40000000;
const Win32_DWORD WRITE_DAC = 0x00040000;
const Win32_DWORD WRITE_OWNER = 0x00080000;
const Win32_DWORD ACCESS_SYSTEM_SECURITY = 0x01000000;
const Win32_DWORD PIPE_TYPE_BYTE = 0;
const Win32_DWORD PIPE_TYPE_MESSAGE = 4;
const Win32_DWORD PIPE_READMODE_BYTE = 0;
const Win32_DWORD PIPE_READMODE_MESSAGE = 2;
const Win32_DWORD PIPE_WAIT = 0;
const Win32_DWORD PIPE_NOWAIT = 1;
const Win32_DWORD PIPE_ACCEPT_REMOTE_CLIENTS = 0;
const Win32_DWORD PIPE_REJECT_REMOTE_CLIENTS = 8;


extern fn void initializeCriticalSection(Win32_CRITICAL_SECTION* section) @extern("InitializeCriticalSection");
extern fn void deleteCriticalSection(Win32_CRITICAL_SECTION* section) @extern("DeleteCriticalSection");
extern fn Win32_HANDLE createMutex(void*, bool, void*) @extern("CreateMutexA");
extern fn Win32_BOOL releaseMutex(Win32_HANDLE) @extern("ReleaseMutex");
extern fn void enterCriticalSection(Win32_CRITICAL_SECTION* section) @extern("EnterCriticalSection");
extern fn void leaveCriticalSection(Win32_CRITICAL_SECTION* section) @extern("LeaveCriticalSection");
extern fn Win32_BOOL tryEnterCriticalSection(Win32_CRITICAL_SECTION* section) @extern("TryEnterCriticalSection");
extern fn uint waitForSingleObject(Win32_HANDLE, uint milliseconds) @extern("WaitForSingleObject");
extern fn void sleep(uint ms) @extern("Sleep");
extern fn uint waitForMultipleObjects(uint count, Win32_HANDLE* handles, bool wait_all, uint ms) @extern("WaitForMultipleObjects");
extern fn Win32_BOOL resetEvent(Win32_HANDLE event) @extern("ResetEvent");
extern fn Win32_BOOL setEvent(Win32_HANDLE handle) @extern("SetEvent");
extern fn long interlockedCompareExchange(int* dest, int exchange, int comperand) @extern("InterlockedCompareExchange");
extern fn uint sleepEx(uint ms, bool alertable) @extern("SleepEx");
extern fn Win32_HANDLE createThread(void* attributes, usz stack, ThreadFn func, void* arg, uint flags, uint* thread_id) @extern("CreateThread");
extern fn Win32_BOOL getExitCodeThread(Win32_HANDLE handle, uint* exit_code) @extern("GetExitCodeThread");
extern fn Win32_BOOL getExitCodeProcess(Win32_HANDLE hProcess, Win32_LPDWORD lpExitCode) @extern("GetExitCodeProcess");
extern fn uint getThreadId(Win32_HANDLE) @extern("GetThreadId");
extern fn void exitThread(uint res) @noreturn @extern("ExitThread");
extern fn Win32_HANDLE getCurrentThread() @extern("GetCurrentThread");
extern fn Win32_BOOL terminateProcess(Win32_HANDLE hProcess, Win32_UINT uExitCode) @extern("TerminateProcess");
extern fn Win32_DWORD getCurrentProcessId() @extern("GetCurrentProcessId");
extern fn Win32_DWORD getCurrentThreadId() @extern("GetCurrentThreadId");
extern fn Win32_BOOL setHandleInformation(Win32_HANDLE hObject, Win32_DWORD dwMask, Win32_DWORD dwFlags) @extern("SetHandleInformation");
extern fn Win32_HANDLE createEventA(
	Win32_LPSECURITY_ATTRIBUTES lpEventAttributes,
	Win32_BOOL                  bManualReset,
	Win32_BOOL                  bInitialState,
	Win32_LPCSTR                lpName
) @extern("CreateEventA");
extern fn Win32_BOOL createProcessW(
	Win32_LPCWSTR lpApplicationName,
	Win32_LPWSTR lpCommandLine,
	Win32_LPSECURITY_ATTRIBUTES lpProcessAttributes,
	Win32_LPSECURITY_ATTRIBUTES lpThreadAttributes,
	Win32_BOOL bInheritHandles,
	Win32_DWORD dwCreationFlags,
	Win32_LPVOID lpEnvironment,
	Win32_LPCWSTR lpCurrentDirectory,
	Win32_LPSTARTUPINFOW lpStartupInfo,
	Win32_LPPROCESS_INFORMATION lpProcessInformation
) @extern("CreateProcessW");
extern fn Win32_HANDLE createNamedPipeA(
	Win32_LPCSTR lpName, Win32_DWORD dwOpenMode, Win32_DWORD dwPipeMode,
	Win32_DWORD nMaxInstances, Win32_DWORD nOutBufferSize, Win32_DWORD nInBufferSize,
	Win32_DWORD nDefaultTimeOut, Win32_LPSECURITY_ATTRIBUTES lpSecurityAttributes
) @extern("CreateNamedPipeA");
extern fn Win32_BOOL getOverlappedResult(
	Win32_HANDLE       hFile,
	Win32_LPOVERLAPPED lpOverlapped,
	Win32_LPDWORD      lpNumberOfBytesTransferred,
	Win32_BOOL         bWait
) @extern("GetOverlappedResult");

