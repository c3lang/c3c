module std::os;
import libc;

typedef DylibHandle @constinit = void*;

excuse
{
	DYLIB_NOT_FOUND,
	SYMBOL_NOT_FOUND,
}
const DLOPEN_SUPPORTED = $feat(WIN32) || $feat(POSIX);

<*
 @require [handle] !!handle : "Handle must be initialized"
 @require [name] name != null : "The name may not be empty"

 @return "The symbol if found"
*>
fn void* dlsym(DylibHandle handle, ZString name)
{
	$switch:
		$case $feat(WIN32):
			return win32::getProcAddress((Win32_HMODULE)handle, (Win32_LPCSTR)name);
		$case $feat(POSIX):
			return libc::dlsym((void*)handle, name);
		$default:
			unreachable();
	$endswitch
}

<*
 @require [handle] !!handle : "Handle must be initialized"
 @require [name] name != null : "The name may not be empty"

 @return "The symbol if found"
 @ensure return != null
*>
fn void*? dlsym_try(DylibHandle handle, ZString name)
{
	return dlsym(handle, name) @inline ?: SYMBOL_NOT_FOUND~;
}

bitstruct DylibOpenOptions : char
{
	bool resolve_now;     // POSIX: RTLD_NOW instead of RTLD_LAZY. Win32: n/a, always eager.
	bool global_symbols;  // POSIX: RTLD_GLOBAL instead of RTLD_LOCAL. Win32: n/a, no global namespace.
	bool safe_search;     // Win32: LOAD_LIBRARY_SEARCH_DEFAULT_DIRS. POSIX: n/a.
	bool system_only;     // Win32: LOAD_LIBRARY_SEARCH_SYSTEM32. POSIX: n/a.
}

fn DylibHandle dlopen(ZString name, DylibOpenOptions opts = {})
{
	$switch:
		$case $feat(WIN32):
			Win32_DWORD flags;
			if (opts.system_only) flags |= win32::LOAD_LIBRARY_SEARCH_SYSTEM32;
			if (opts.safe_search) flags |= win32::LOAD_LIBRARY_SEARCH_DEFAULT_DIRS;
			return (DylibHandle)(flags ? win32::loadLibraryExA((Win32_LPCSTR)name, null, flags) : win32::loadLibraryA((Win32_LPCSTR)name));
		$case $feat(POSIX):
			int mode = (opts.resolve_now ? libc::RTLD_NOW : libc::RTLD_LAZY)
				| (opts.global_symbols ? libc::RTLD_GLOBAL : libc::RTLD_LOCAL);
			return (DylibHandle)libc::dlopen(name, mode);
		$default:
			return null;
	$endswitch
}
<*
 @ensure !!return
*>
fn DylibHandle? dlopen_try(ZString name, DylibOpenOptions opts = {})
{
	DylibHandle handle = dlopen(name, opts);
	if (!handle) return DYLIB_NOT_FOUND~;
	return handle;
}

fn void DylibHandle.close(&self)
{
	DylibHandle h = *self;
	if (!h) return;
	$switch:
		$case $feat(WIN32):
			win32::freeLibrary((Win32_HMODULE)h);
		$case $feat(POSIX):
			libc::dlclose((void*)h);
		$default:
			// Do nothing
	$endswitch
	*self = {};
}

fn DylibHandle? dlopen_any(ZString... names, DylibOpenOptions opts = {})
{
	foreach (name : names)
	{
		DylibHandle proc = dlopen(name, opts);
		if (proc) return proc;
	}
	return DYLIB_NOT_FOUND~;
}

<*
 Exit the process with a given exit code. This will typically call 'exit' in LibC
*>
fn void exit(int result) @weak @noreturn
{
	$if env::LIBC:
		libc::exit(result);
	$else
		$$trap();
	$endif
}

<*
 Exit the process with a given exit code. This will typically call '_Exit' in LibC
 usually bypassing '@finalizer' functions.
*>
fn void fastexit(int result) @weak @noreturn
{
	$if env::LIBC:
		libc::_exit(result);
	$else
		$$trap();
	$endif
}

