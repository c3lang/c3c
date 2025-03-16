module std::thread::os @if (!env::POSIX && !env::WIN32);

typedef NativeMutex = int;
typedef NativeConditionVariable = int;
typedef NativeOnceFlag = int;
typedef NativeThread = int;