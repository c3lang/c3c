module std::io::path;

const PathEnv DEFAULT_PATH_ENV = env::os_is_win32() ? PathEnv.WIN32 : PathEnv.POSIX;
const char PREFERRED_SEPARATOR_WIN32 = '\\';
const char PREFERRED_SEPARATOR_POSIX = '/';
const char PREFERRED_SEPARATOR = env::os_is_win32() ? PREFERRED_SEPARATOR_WIN32 : PREFERRED_SEPARATOR_POSIX;

fault PathResult
{
	INVALID_PATH,
	NO_PARENT,
}

struct Path
{
	String path_string;
	PathEnv env;
}

enum PathEnv
{
	WIN32,
	POSIX
}


fn Path! getcwd(Allocator* using = mem::heap())
{
	@allocating_pool(using; bool is_temp)
	{
		return new(os::getcwd(using));
	};
}

fn Path! tgetcwd()
{
	return getcwd(mem::temp()) @inline;
}

fn bool is_dir(String path)
{
	return os::native_is_dir(path);
}

macro bool is_separator(char c, PathEnv path_env = DEFAULT_PATH_ENV)
{
	return c == '/' || (c == '\\' && path_env == PathEnv.WIN32);
}

macro bool is_posix_separator(char c)
{
	return c == '/' || c == '\\';
}

macro bool is_win32_separator(char c)
{
	return c == '/' || c == '\\';
}

fn Path! new(String path, Allocator* using = mem::heap(), PathEnv path_env = DEFAULT_PATH_ENV)
{
	return { normalize(path.copy(using), path_env), path_env };
}

fn Path! new_windows(String path, Allocator* using = mem::heap())
{
	return new(path, using, WIN32);
}

fn Path! new_posix(String path, Allocator* using = mem::heap())
{
	return new(path, using, POSIX);
}

fn bool Path.equals(Path p1, Path p2)
{
	return p1.env == p2.env && p1.path_string == p2.path_string;
}

/**
 * Append the string to the current path.
 *
 * @param [in] path
 * @param [in] filename
 * @ensure return.path_string.len >= path.path_string.len
 **/
fn Path! Path.append(Path path, String filename, Allocator* using = mem::heap())
{
	if (!path.path_string.len) return new(filename, using, path.env)?;
	assert(!is_separator(path.path_string[^1], path.env));

	// Handle temp nested temp allocations.
	@allocating_pool(using; bool is_temp)
	{
		DString dstr = dstring::tnew_with_capacity(path.path_string.len + 1 + filename.len);
		dstr.append(path.path_string);
		dstr.append(PREFERRED_SEPARATOR);
		dstr.append(filename);
		return { normalize(is_temp ? dstr.str() : dstr.copy_str(using), path.env), path.env };
	};
}

fn Path! temp_directory(Allocator* using = mem::heap())
{
	return os::native_temp_directory(using);
}

fn String Path.volume_name(Path path)
{
	usz len = volume_name_len(path.as_str(), path.env)!!;
	if (!len) return "";
	return path.path_string[:len];
}

fn usz! volume_name_len(String path, PathEnv path_env) @local
{
	usz len = path.len;
	if (len < 2 || path_env != PathEnv.WIN32) return 0;
	switch (path[0])
    {
    	case '\\':
    	    // "\\" paths.. must be longer than 2
    	    if (len == 2) return 0;
			int count = 1;
			while (count < len && path[count] == '\\') count++;
			// Not 2 => folded paths
			if (count != 2) return 0;
			// Check that we have a name followed by '/'
			for (usz i = 2; i < len; i++)
    		{
    		    char c = path[i];
    			if (is_win32_separator(c)) return i;
    			if (is_reserved_win32_path_char(c)) return PathResult.INVALID_PATH!;
    		}
    		return PathResult.INVALID_PATH!;
    	case 'A'..'Z':
    	case 'a'..'z':
    	    return path[1] == ':' ? 2 : 0;
    	default:
    	    return 0;
    }
}

fn Path! Path.parent(Path path)
{
	if (path.path_string.len == 1 && is_separator(path.path_string[0], path.env)) return PathResult.NO_PARENT!;
	foreach_r(i, c : path.path_string)
	{
		if (is_separator(c, path.env))
		{
			return { path.path_string[:i], path.env };
		}
	}
	return PathResult.NO_PARENT!;
}

fn String! normalize(String path_str, PathEnv path_env = DEFAULT_PATH_ENV)
{
	if (!path_str.len) return path_str;
    usz path_start = volume_name_len(path_str, path_env)?;
	usz path_len = path_str.len;
	if (path_start == path_len) return path_str;
	char path_separator = path_env == PathEnv.WIN32 ? PREFERRED_SEPARATOR_WIN32 : PREFERRED_SEPARATOR_POSIX;
	usz len = path_start;
	bool has_root = is_separator(path_str[path_start], path_env);
	if (has_root)
	{
		path_str[len++] = path_separator;
		path_start++;
	}
	// It is safe to write it as true, since we already dealt with /foo.
	// This allows us to avoid checking whether it is the start of the path.
	bool previous_was_separator = true;

	for (usz i = path_start; i < path_len; i++)
	{
		char c = path_str[i];
		// Fold foo///bar into foo/bar
		if (is_separator(c, path_env))
		{
			// Fold //
			if (previous_was_separator) continue;

			// New /, so mark and rewrite
			path_str.ptr[len++] = path_separator;
			previous_was_separator = true;
			continue;
		}

		// The rest are names of the path elements, so check that the
		// characters are valid.
		if (is_reserved_path_char(c, path_env)) return PathResult.INVALID_PATH!;

		// If we have '.' after a separator
		if (c == '.' && previous_was_separator)
		{
			// Get the number of dots until next separator, expecting 1 or 2
			bool is_last = i == path_len - 1;
			int dots = 1;
			if (!is_last && path_str[i + 1] == '.')
			{
				dots = 2;
				is_last = i == path_len - 2;
				if (!is_last && !is_separator(path_str[i + 2], path_env))
				{
					dots = 0;
				}
			}
			switch (dots)
			{
				case 1:
					// /./abc -> skip to /./abc
					//  ^                  ^
					i++;
					continue;
				case 2:
					// We're walking back, doing so when already at the root is invalid.
					if (len == path_start) return PathResult.INVALID_PATH!;
					// Step back, now looking at '/' abc/def/. -> abc/def/
					len--;
					// Step back until finding a separator or the start.
					while (len > path_start && !is_separator(path_str[len - 1], path_env))
					{
						len--;
					}
					// Reading, we go from /../abc to /../abc
					//                      ^            ^
 					i += 2;
					continue;
				default:
					break;

			}
		}
		if (i != len) path_str[len] = c;
		previous_was_separator = false;
		len++;
	}
	if (len > path_start + 1 && is_separator(path_str[len - 1], path_env)) len--;
	path_str.ptr[len] = 0;
	return path_str[:len];
}

fn String Path.root_directory(Path path)
{
	String path_str = path.as_str();
	usz len = path_str.len;
	if (!len) return "";
	if (path.env == PathEnv.WIN32)
	{
		usz root_len = volume_name_len(path_str, path.env)!!;
		if (root_len == len || !is_win32_separator(path_str[root_len])) return "";
		return path_str[root_len..root_len];
	}
	if (!is_posix_separator(path_str[0])) return "";
	for (usz i = 1; i < len; i++)
	{
		if (is_posix_separator(path_str[i]))
		{
			return path_str[:i];
		}
	}
	return path_str;
}

fn usz! Path.file_size(Path path)
{
	return os::native_file_size(path.as_str());
}

fn String Path.as_str(Path path)
{
	return path.path_string;
}

fn bool Path.file_or_dir_exists(Path path)
{
	return os::native_file_or_dir_exists(path.as_str());
}

fn bool Path.is_dir(Path path)
{
	return os::native_is_dir(path.as_str());
}

fn bool Path.has_suffix(Path path, String str)
{
	return path.as_str().ends_with(str);
}

fn bool Path.is_file(Path path)
{
	return os::native_is_file(path.as_str());
}


fn void Path.free(Path path)
{
	free(path.path_string.ptr);
}


const bool[256] RESERVED_PATH_CHAR_POSIX = {
	[0] = true,
	['/'] = true,
};
const bool[256] RESERVED_PATH_CHAR_WIN32 = {
	[0..31] = true,
	['>'] = true,
	['<'] = true,
	[':'] = true,
	['\"'] = true,
	['/'] = true,
	['\\'] = true,
	['|'] = true,
	['?'] = true,
	['*'] = true,
};

macro bool is_reserved_win32_path_char(char c)
{
	return RESERVED_PATH_CHAR_WIN32[c];
}

macro bool is_reserved_path_char(char c, PathEnv path_env = DEFAULT_PATH_ENV)
{
	return path_env == PathEnv.WIN32
		? RESERVED_PATH_CHAR_WIN32[c]
		: RESERVED_PATH_CHAR_POSIX[c];
}

