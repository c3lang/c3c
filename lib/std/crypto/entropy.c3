// Copyright (c) 2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// Based on an initial version by @laura240406 on GitHub. See `c3c` PR #2812.
//
module std::crypto::entropy;
import libc;

faultdef SECURE_RANDOM_UNAVAILABLE;

<*
 Returns cryptographically secure random bytes. Fails if it is unavailable.

 @param [out] buffer : "The buffer to populate with random bytes."
 @param timeout : "Set to `true` to disallow blocking when this function cannot fully fill the provided `buffer`."

 @return? SECURE_RANDOM_UNAVAILABLE : "The delegated RNG wasn't available or failed to fully populate the output buffer."
 @return? TIMEOUT : "When `timeout` is `true`, the secure RNG failed to populate the buffer in a reasonable time."
*>
fn void? fill(char[] buffer, bool timeout = false)
{
	if (!buffer.len) return;
	$switch:
		$case env::LINUX:
		$case env::ANDROID:
		$case env::FREEBSD:
		$case env::NETBSD:
		$case env::EMSCRIPTEN:
			$if (@in(env::ARCH_TYPE, ARM, ARMB, AARCH64, X86, X86_64)):
				sz result = (sz)$$syscall(entropy_syscall_id(), (uptr)buffer.ptr, (uptr)buffer.len, 0);
				if (result < 0 && result != Errno.EINTR) return SECURE_RANDOM_UNAVAILABLE~;   /* it's possible for signal handlers to interrupt this call */
				for (sz prevent_block = 0; result < buffer.len && (!timeout || prevent_block < 1_000); prevent_block++)
				{
					sz more = (sz)$$syscall(entropy_syscall_id(), (uptr)(&buffer[result]), (uptr)(buffer.len - result), 0);
					if (more < 0)
					{
						if (more != Errno.EINTR) return SECURE_RANDOM_UNAVAILABLE~;
						continue;
					}
					result += more;
				}
				if (result < buffer.len) return TIMEOUT~;
			$else
				return unix_fallback(buffer, timeout);
			$endif
		$case env::OPENBSD:
		$case env::DARWIN:
			arc4random_buf((void*)buffer.ptr, buffer.len);
			return;
		$case env::WIN32:
			win32_init();
			if (rtlgenrandom == null || !rtlgenrandom(buffer.ptr, (ulong)buffer.len)) return SECURE_RANDOM_UNAVAILABLE~;
		$default:
			return SECURE_RANDOM_UNAVAILABLE~;
	$endswitch
}

<*
 Create a new byte array and fill it with random bytes from a secure, high-entropy source.

 @param [&inout] allocator : "The allocator to use when creating the byte array."
 @param length : "The length of the resultant byte array."
 @param timeout : "Set to `true` to disallow blocking when this function cannot fully fill the provided `buffer`."

 @return? SECURE_RANDOM_UNAVAILABLE : "The delegated RNG wasn't available or failed to fully populate the output buffer."
 @return? TIMEOUT : "When `timeout` is `true`, the secure RNG failed to populate the buffer in a reasonable time."
 @return "The created and populated slice of random bytes."
*>
fn char[]? make(Allocator allocator, sz length, bool timeout = false)
{
	if (!length) return {};
	char[] res = alloc::alloc_array(allocator, char, length)[:length];
	fill(res, timeout)!;
	return res;
}


module std::crypto::entropy @private @if (env::WIN32);
import std::os::win32, std::thread;

OnceFlag run_once;
RtlGenRandom rtlgenrandom;

alias RtlGenRandom = fn CInt(void*, ulong);

fn void _crypto_init_win32_random()
{
	Win32_HMODULE advapi32_dll = win32::loadLibraryA("advapi32.dll");
	rtlgenrandom = win32::getProcAddress(advapi32_dll, "SystemFunction036");
}

macro void win32_init() => run_once.call(&_crypto_init_win32_random);


// ============================================================================
module std::crypto::entropy @private;
import std::io::file;


<*
 For relevant systems, get the syscall ID associated with `getrandom` for the platform/architecture.

 See: https://www.chromium.org/chromium-os/developer-library/reference/linux-constants/syscalls/#cross-arch-numbers
*>
macro sz entropy_syscall_id() @const
{
	$switch:
		$case env::LINUX:
		$case env::ANDROID:
			$switch:
				$case env::X86_64: return 318;
				$case env::ARCH_TYPE == ARM || env::ARCH_TYPE == ARMB: return 384;
				$case env::AARCH64: return 278;
				$case env::X86: return 355;
			$endswitch
		// $case env::OPENBSD: return 7;   // https://github.com/openbsd/src/blob/a944417af3ca44cd189efef88c2299690e2958fd/sys/sys/syscall.h#L29
		$case env::FREEBSD: return 563;   // https://github.com/freebsd/freebsd-src/blob/ff2c98b30b57b9763e2a6575f729bab676e6c025/sys/sys/syscall.h#L504
		$case env::NETBSD: return 91;   // https://github.com/NetBSD/src/blob/51f0c24a1280db0c2e0b5391594c7782d8fd07e8/sys/sys/syscall.h#L275
		$default: $error "No entropy-providing syscall identifier exists for the current environment.";
	$endswitch
}

<*
 A generic, UNIX-style fallback method that simply tries to access /dev/[u]random as an entropy source.

 @param buffer : "The buffer to populate with random bytes."
 @param timeout : "Set to `true` to disallow blocking when this function cannot fully fill the provided `buffer`."
*>
fn void? unix_fallback(char[] buffer, bool timeout = false)
{
	File rnd_file = file::open("/dev/urandom", "r") ?? file::open("/dev/random", "r") ?? SECURE_RANDOM_UNAVAILABLE~!;
	defer (void)rnd_file.close();
	sz count;
	for (sz prevent_block = 0; count < buffer.len && (!timeout || prevent_block < 1_000); prevent_block++)
	{
		count += rnd_file.read(buffer) ?? SECURE_RANDOM_UNAVAILABLE~!;
	}
	if (count < buffer.len) return TIMEOUT~;
}

<*
 Import for OpenBSD which fills an existing buffer with high-entropy data.
 See: https://man.openbsd.org/arc4random.3
*>
extern fn void arc4random_buf(void* buf, sz nbytes) @if (env::OPENBSD || env::DARWIN);

/*
 NOTE: `getentropy` for emscripten might be a better option
 https://github.com/emscripten-core/emscripten/pull/12240
 https://github.com/emscripten-core/emscripten/blob/main/system/lib/libc/musl/src/misc/getentropy.c
*/

// ============================================================================
