module std::os::win32 @if(env::WIN32);

struct Win32_pollfd
{
	Win32_SOCKET fd;
	Win32_SHORT events;
	Win32_SHORT revents;
}
def Win32_WSAPOLLFD = Win32_pollfd;
def Win32_PWSAPOLLFD = Win32_WSAPOLLFD*;
def Win32_LPWSAPOLLFD = Win32_WSAPOLLFD*;

struct Win32_InAddr {
  union {
    struct s_un_b {
      Win32_UCHAR s_b1;
      Win32_UCHAR s_b2;
      Win32_UCHAR s_b3;
      Win32_UCHAR s_b4;
    }
    struct s_un_w {
      Win32_USHORT s_w1;
      Win32_USHORT s_w2;
    }
    Win32_ULONG s_addr;
  }
}

struct Win32_SOCKADDR_IN 
{
    Win32_SHORT sin_family;
    Win32_USHORT sin_port;
    Win32_InAddr sin_addr;
    Win32_CHAR[8]* sin_zero;
}

const usz _SS_PAD1SIZE = 6;
const usz _SS_PAD2SIZE = 112;

struct Win32_SOCKADDR_STORAGE 
{
    Win32_USHORT  ss_family;
    Win32_CHAR[_SS_PAD1SIZE]*      __ss_pad1;
    Win32_INT64 __ss_align;
    Win32_CHAR[_SS_PAD2SIZE]*      __ss_pad2;
}

def Win32_WSAOVERLAPPED = Win32_OVERLAPPED;
def Win32_LPWSAOVERLAPPED = Win32_WSAOVERLAPPED*;

def Win32_LPWSAOVERLAPPED_COMPLETION_ROUTINE = fn void (Win32_DWORD dwError, Win32_DWORD cbTransferred, Win32_LPWSAOVERLAPPED lpOverlapped, Win32_DWORD dwFlags);

def Win32_LPFN_WSARECV = fn CInt
            (Win32_SOCKET socket,
             Win32_LPWSABUF buffers,
             Win32_DWORD buffer_count,
             Win32_LPDWORD bytes,
             Win32_LPDWORD flags,
             Win32_LPWSAOVERLAPPED overlapped,
             Win32_LPWSAOVERLAPPED_COMPLETION_ROUTINE completion_routine);

def Win32_LPFN_WSARECVFROM = fn CInt
            (Win32_SOCKET socket,
             Win32_LPWSABUF buffers,
             Win32_DWORD buffer_count,
             Win32_LPDWORD bytes,
             Win32_LPDWORD flags,
             Win32_SOCKADDR* addr,
             Win32_LPINT addr_len,
             Win32_LPWSAOVERLAPPED overlapped,
             Win32_LPWSAOVERLAPPED_COMPLETION_ROUTINE completion_routine);

def Win32_LPFn_CONNECTEX = fn bool
                          (Win32_SOCKET,
                           Win32_SOCKADDR*,
                           Win32_INT,
                           Win32_PVOID,
                           Win32_DWORD,
                           Win32_LPDWORD,
                           void*);

def Win32_LPFn_ACCEPTEX = fn bool
                          (Win32_SOCKET,
                           Win32_SOCKET,
                           Win32_PVOID,
                           Win32_DWORD,
                           Win32_DWORD,
                           Win32_DWORD,
                           Win32_LPDWORD,
                           void*);
                           