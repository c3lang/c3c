// Copyright (c) 2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// Based on an initial version by @laura240406 on GitHub. See `c3c` PR #2812.
//
module std::crypto::entropy @private @if (env::WIN32);

import std::os::win32, std::thread;


OnceFlag win32_init;

alias RtlGenRandom = fn CInt(void*, ulong);
RtlGenRandom rtlgenrandom;

fn void _crypto_init_win32_random()
{
	Win32_HMODULE advapi32_dll = win32::loadLibraryA("advapi32.dll");
	rtlgenrandom = win32::getProcAddress(advapi32_dll, "SystemFunction036");
}

macro void win32_init() => win32_init.call(&_crypto_init_win32_random);


module std::crypto::entropy;

import libc, std::io::file;//, std::thread;

faultdef SECURE_RANDOM_UNAVAILABLE;


<*
 For relevant systems, get the syscall ID associated with `getrandom` for the platform/architecture.

 See: https://www.chromium.org/chromium-os/developer-library/reference/linux-constants/syscalls/#cross-arch-numbers
*>
macro usz _entropy_syscall_id() @local @const
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
		$case env::OPENBSD: return 7;   // https://github.com/openbsd/src/blob/a944417af3ca44cd189efef88c2299690e2958fd/sys/sys/syscall.h#L29
		$case env::FREEBSD: return 563;   // https://github.com/freebsd/freebsd-src/blob/ff2c98b30b57b9763e2a6575f729bab676e6c025/sys/sys/syscall.h#L504
		$case env::NETBSD: return 91;   // https://github.com/NetBSD/src/blob/51f0c24a1280db0c2e0b5391594c7782d8fd07e8/sys/sys/syscall.h#L275
		$default: unreachable("No entropy-providing syscall identifier exists for the current environment.");
	$endswitch
}

fn void? _fallback_method(char[] buffer, bool timeout = false) @local
{
	File rnd_file = file::open("/dev/random", "r") ?? file::open("/dev/urandom", "r") ?? SECURE_RANDOM_UNAVAILABLE~!;
	defer (void)rnd_file.close();
	usz count;
	for (usz prevent_block = 0; count < buffer.len && (!timeout || prevent_block < 1_000); prevent_block++)
	{
		count += rnd_file.read(buffer) ?? SECURE_RANDOM_UNAVAILABLE~!;
	}
	if (count < buffer.len) return TIMEOUT~;
}


<*
 Returns cryptographically secure random bytes. Fails if it is unavailable.

 @param [out] buffer : "The buffer to populate with random bytes."
 @param timeout : "Set to `true` to disallow blocking when this function cannot fully fill the provided `buffer`."

 @return? SECURE_RANDOM_UNAVAILABLE : "The delegated RNG wasn't available or failed to fully populate the output buffer."
 @return? TIMEOUT : "When `timeout` is `true`, the secure RNG failed to populate the buffer in a reasonable time."
*>
fn void? fill(char[] buffer, bool timeout = false)
{
	$switch:
		$case env::LINUX:
		$case env::ANDROID:
		$case env::FREEBSD:
		$case env::NETBSD:
			$if (@in(env::ARCH_TYPE, ARM, ARMB, AARCH64, X86, X86_64)):
				isz result = $$syscall(_entropy_syscall_id(), (uptr)buffer.ptr, buffer.len, 0);
				if (result < 0 && result != errno::EINTR) return SECURE_RANDOM_UNAVAILABLE~;   /* it's possible for signal handlers to interrupt this call */
				for (usz prevent_block = 0; result < buffer.len && (!timeout || prevent_block < 1_000); prevent_block++)
				{
					isz more = $$syscall(_entropy_syscall_id(), (uptr)((isz)buffer.ptr + result), buffer.len, 0);
					if (more < 0 && more != errno::EINTR) return SECURE_RANDOM_UNAVAILABLE~;
					result += more;
				}
				if (result < buffer.len) return TIMEOUT~;
			$else
				return _fallback_method(buffer, timeout);
			$endif

		$case env::OPENBSD:
			// https://man.openbsd.org/getentropy
			if (buffer.len <= 256)
			{
				isz result = $$syscall(_entropy_syscall_id(), buffer.ptr, buffer.len);
				if (result < 0) return SECURE_RANDOM_UNAVAILABLE~;
			}
			else
			{
				return _fallback_method(buffer, timeout);
			}

		$case env::DARWIN:
			// TODO: would be nice to use something similar to https://github.com/briansmith/ring/pull/398 here...
			return _fallback_method(buffer, timeout);

		$case env::WIN32:
			win32_init();
			if (rtlgenrandom == null || !rtlgenrandom(buffer.ptr, buffer.len)) return SECURE_RANDOM_UNAVAILABLE~;

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
fn char[]? make(Allocator allocator, usz length, bool timeout = false)
{
	if (!length) return {};
	char[] res = allocator::alloc_array(allocator, char, length)[:length];
	fill(res, timeout)!;
	return res;
}
