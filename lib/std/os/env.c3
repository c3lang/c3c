// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::os::env;
import libc;

/**
 * @param [in] name
 * @require name.len > 0
 **/
fn String! get_var(String name)
{
	$if env::LIBC && !env::WIN32:
	@pool()
	{
		ZString val = libc::getenv(name.zstr_tcopy());
		return val ? val.str_view() : SearchResult.MISSING?;
	};
	$else
	return "";
	$endif
}

/**
 * @param [in] name
 * @param [in] value
 * @require name.len > 0
 **/
fn void set_var(String name, String value, bool overwrite = true)
{
	$if env::LIBC && !env::WIN32:
	@pool()
	{
		if (libc::setenv(name.zstr_tcopy(), value.zstr_copy(), (int)overwrite))
		{
			unreachable();
		}
	};
	$endif
}

/**
 * @param [in] name
 * @require name.len > 0
 **/
fn void clear_var(String name)
{
	$if env::LIBC && !env::WIN32:
	@pool()
	{
		if (libc::unsetenv(name.zstr_tcopy()))
		{
			unreachable();
		}
	};
	$endif
}

fn String! executable_path(Allocator *using = mem::heap())
{
	$if env::DARWIN:
		return darwin::executable_path();
	$else
		return "<Unsupported>";
	$endif
}