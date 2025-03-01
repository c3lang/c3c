module std::thread::os @if (!env::POSIX && !env::WIN32);

distinct NativeMutex = int;
distinct NativeConditionVariable = int;
distinct NativeOnceFlag = int;
distinct NativeThread = int;