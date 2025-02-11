module std::os::win32 @if(env::WIN32);

struct Win32_addrinfo {
  Win32_INT ai_flags;
  Win32_INT ai_family;
  Win32_INT ai_socktype;
  Win32_INT ai_protocol;
  Win32_SIZE_T ai_addrlen;
  Win32_CHAR* ai_canonname;
  Win32_SOCKADDR* ai_addr;
  Win32_ADDRINFO* ai_next;
}

def Win32_ADDRINFO = Win32_addrinfo;
def Win32_ADDRINFOA = Win32_ADDRINFO;
def Win32_PADDRINFOA = Win32_ADDRINFO*;

struct Win32_addrinfoW {
  Win32_INT ai_flags;
  Win32_INT ai_family;
  Win32_INT ai_socktype;
  Win32_INT ai_protocol;
  Win32_SIZE_T ai_addrlen;
  Win32_PWSTR ai_canonname;
  Win32_SOCKADDR *ai_addr;
  Win32_addrinfo *ai_next;
}

def Win32_ADDRINFOW = Win32_addrinfoW;
def Win32_PADDRINFOW = Win32_addrinfoW*;