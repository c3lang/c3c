module std::thread::os @if (!env::POSIX && !env::WIN32);

def NativeMutex = distinct int;
def NativeConditionVariable = distinct int;
def NativeOnceFlag = distinct int;
def NativeThread = distinct int;