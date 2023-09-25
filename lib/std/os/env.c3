// Copyright (c) 2021-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::os::env;
import libc;

/**
 * @param [in] name
 * @require name.len > 0
 * @return! SearchResult.MISSING
 **/
fn String! get_var(String name, Allocator* using = mem::heap())
{
	@pool(using)
	{
	$switch
	$case env::LIBC && !env::WIN32:
		ZString val = libc::getenv(name.zstr_tcopy());
		return val ? val.copy(using) : SearchResult.MISSING?;
	$case env::WIN32:
		// https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-getenvironmentvariable
		const usz BUFSIZE = 1024;
		WString buff = (WString)tcalloc(BUFSIZE * 2 + 2);
		WString wstr = name.to_wstring(mem::temp())!;
		usz len = win32::getEnvironmentVariableW(wstr, buff, BUFSIZE);
		if (len == 0) return SearchResult.MISSING?;
		if (len > BUFSIZE)
		{
			buff = (WString)tmalloc(len * 2 + 2);
			win32::getEnvironmentVariableW(wstr, buff, (Win32_DWORD)len);
		}
		return string::from_wstring(buff, using);
	$default:
		return "";
	$endswitch
	};
}


/**
 * @param [in] name
 * @param [in] value
 * @require name.len > 0
 **/
fn bool set_var(String name, String value, bool overwrite = true)
{
	@pool()
	{
	$switch
	$case env::WIN32:
		WString wname = name.to_wstring(mem::temp())!!;
		if (!overwrite)
		{
			Char16[8] buff;
			if (win32::getEnvironmentVariableW(wname, &buff, 8) > 0) return true;
		}
		// https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-setenvironmentvariable
		return (win32::setEnvironmentVariableW(wname, value.to_wstring(mem::temp())) ?? 1) == 0;
	$case env::LIBC && !env::WIN32:
		return libc::setenv(name.zstr_tcopy(), value.zstr_copy(), (int)overwrite) == 0;
	$default:
		return false;
	$endswitch
	};

}

/**
 * Returns the current user's home directory.
 **/
fn String! get_home_dir(Allocator* using = mem::heap())
{
	String home;
	$if !env::WIN32:
		home = "HOME";
	$else
		home = "USERPROFILE";
	$endif
	return get_var(home, using);
}

/**
 * Returns the current user's config directory.
 **/
fn Path! get_config_dir(Allocator* using = mem::heap())
{
	@pool(using)
	{
	$if env::WIN32:
		return path::new(get_var("AppData", .using = mem::temp()), .using = using);
	$else
		$if env::DARWIN:
			String s = get_var("HOME", .using = mem::temp())!;
			const DIR = "Library/Application Support";
		$else
			String s = get_var("XDG_CONFIG_HOME", .using = mem::temp()) ?? get_var("HOME", .using = mem::temp())!;
			const DIR = ".config";
		$endif
		return path::new(s, .using = mem::temp()).append(DIR, .using = using);
	$endif
	};
}


/**
 * @param [in] name
 * @require name.len > 0
 **/
fn bool clear_var(String name)
{
	@pool()
	{
	$switch
	$case env::WIN32:
		WString wname = name.to_wstring(mem::temp())!!;
		return win32::setEnvironmentVariableW(wname, null) == 0;
	$case env::LIBC && !env::WIN32:
		return libc::unsetenv(name.zstr_tcopy()) == 0;
	$default:
		return false;
	$endswitch
	};
}

fn String! executable_path(Allocator *using = mem::heap())
{
	$if env::DARWIN:
		return darwin::executable_path();
	$else
		return "<Unsupported>";
	$endif
}