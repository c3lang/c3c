module std::crypto::random;
import std::io::file;
import std::os::win32;

faultdef SECURE_RANDOM_UNAVAILABLE;

alias RtlGenRandom = fn CInt(void *, ulong);
RtlGenRandom rtlgenrandom @if(env::WIN32) @local;

fn void crypto_init_win32_random() @if(env::WIN32) @init
{
    Win32_HMODULE advapi32_dll = win32::loadLibraryA("advapi32.dll");
    rtlgenrandom = win32::getProcAddress(advapi32_dll, "SystemFunction036");
}

<*
 Returns cryptographically secure random bytes. Fails if it is unavailable.
 @param buffer
 @return? SECURE_RANDOM_UNAVAILABLE
*>
fn void? crypto_secure_random(char[] buffer)
{
    $switch:
        $case env::ANDROID:
        $case env::BSD_FAMILY:
        $case env::DARWIN:
        $case env::LINUX:
            File? random_file = file::open("/dev/random", "r");

            if (catch excuse = random_file)
            {
                random_file = file::open("/dev/urandom", "r");
            }

            if (catch excuse = random_file)
            {
                return SECURE_RANDOM_UNAVAILABLE~;
            }

            defer (void) random_file.close();

            usz? read_amount = random_file.read(buffer);
            if (catch excuse = read_amount)
            {
                return SECURE_RANDOM_UNAVAILABLE~;
            }

            if (read_amount != buffer.len)
            {
                return SECURE_RANDOM_UNAVAILABLE~;
            }

            return;

        $case env::WIN32:
            if (rtlgenrandom == (void *) 0)
            {
                return SECURE_RANDOM_UNAVAILABLE~;
            }

            if (!rtlgenrandom(buffer.ptr, buffer.len))
            {
                return SECURE_RANDOM_UNAVAILABLE~;
            }

            return;

        $default:
            return SECURE_RANDOM_UNAVAILABLE~;
    $endswitch
}
