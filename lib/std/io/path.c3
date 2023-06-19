module std::io::path;
import std::collections::list;

const PathEnv DEFAULT_PATH_ENV = env::os_is_win32() ? PathEnv.WIN32 : PathEnv.POSIX;
const char PREFERRED_SEPARATOR_WIN32 = '\\';
const char PREFERRED_SEPARATOR_POSIX = '/';
const char PREFERRED_SEPARATOR = env::os_is_win32() ? PREFERRED_SEPARATOR_WIN32 : PREFERRED_SEPARATOR_POSIX;

def PathList = List<Path>;

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
	@stack_mem(256; Allocator* mem)
	{
		return new(os::getcwd(mem), using);
	};
}

fn bool is_dir(Path path) => os::native_is_dir(path.as_str());
fn bool is_file(Path path) => os::native_is_file(path.as_str());
fn usz! file_size(Path path) => os::native_file_size(path.as_str());
fn bool exists(Path path) => os::native_file_or_dir_exists(path.as_str());
fn Path! tgetcwd() => getcwd(mem::temp()) @inline;
fn void! chdir(Path path) => os::native_chdir(path) @inline;
fn Path! temp_directory(Allocator* using = mem::heap()) => os::native_temp_directory(using);

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

fn PathList! ls(Path dir, bool no_dirs = false, bool no_symlinks = false, String mask = "", Allocator* using = mem::heap())
{
	return os::native_ls(dir, no_dirs, no_symlinks, mask, using);
}

enum MkdirPermissions
{
	NORMAL,
	USER_ONLY,
	USER_AND_ADMIN
}

fn bool! mkdir(Path path, bool recursive = false, MkdirPermissions permissions = NORMAL)
{
	if (!path.path_string.len) return PathResult.INVALID_PATH?;
	if (is_dir(path)) return false;
	if (exists(path)) return IoError.FILE_NOT_DIR?;

	if (recursive)
	{
		if (try parent = path.parent()) mkdir(parent, true, permissions)!;
	}
	if (!is_dir(path.parent()) ?? false) return IoError.CANNOT_READ_DIR?;

	return os::native_mkdir(path, permissions);
}

fn bool! rmdir(Path path)
{
	if (!path.path_string.len) return PathResult.INVALID_PATH?;
	return os::native_rmdir(path);
}

fn void! rmtree(Path path)
{
	if (!path.path_string.len) return PathResult.INVALID_PATH?;
	$if $defined(os::native_rmtree):
		return os::native_rmtree(path);
	$else
		assert(false, "rmtree is not available");
	$endif
}

fn Path! new(String path, Allocator* using = mem::heap(), PathEnv path_env = DEFAULT_PATH_ENV)
{
	return { normalize(path.copy(using), path_env), path_env };
}

fn Path! new_win32_zutf16(Char16* path, Allocator* using = mem::heap())
{
	@stack_mem(480; Allocator* mem)
	{
		return path::new(string::from_zutf16(path, .using = mem)!, .using = using);
	};
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
	if (!path.path_string.len) return new(filename, using, path.env)!;
	assert(!is_separator(path.path_string[^1], path.env));

	@stack_mem(256; Allocator* mem)
	{
		DString dstr = dstring::new_with_capacity(path.path_string.len + 1 + filename.len, .using = mem);
		dstr.append(path.path_string);
		dstr.append(PREFERRED_SEPARATOR);
		dstr.append(filename);
		return { normalize(dstr.copy_str(using), path.env), path.env };
	};
}

fn Path! Path.tappend(Path path, String filename) => path.append(filename, mem::temp());

fn usz Path.start_of_base_name(Path path) @local
{
	String path_str = path.path_string;
	if (!path_str.len) return 0;
	if (path.env == PathEnv.WIN32)
	{
		return path_str.rindex_of(`\`) + 1 ?? volume_name_len(path_str, path.env)!!;
	}
	return path_str.rindex_of("/") + 1 ?? 0;
}

fn String Path.basename(Path path)
{
	usz basename_start = path.start_of_base_name();
	String path_str = path.path_string;
	if (basename_start == path_str.len) return "";
	return path_str[basename_start..];
}

fn String Path.dirname(Path path)
{
	usz basename_start = path.start_of_base_name();
	String path_str = path.path_string;
	if (basename_start == 0) return "";
	usz start = volume_name_len(path_str, path.env)!!;
	if (basename_start <= start + 1) return path_str[:basename_start];
	return path_str[:basename_start - 1];
}

fn String! Path.extension(Path path)
{
	String basename = path.basename();
	usz index = basename.rindex_of(".")!;
	// Plain ".foo" does not have an
	if (index == 0) return SearchResult.MISSING?;
	if (index == basename.len) return "";
	return basename[index + 1..];
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
    			if (is_reserved_win32_path_char(c)) return PathResult.INVALID_PATH?;
    		}
    		return PathResult.INVALID_PATH?;
    	case 'A'..'Z':
    	case 'a'..'z':
    	    return path[1] == ':' ? 2 : 0;
    	default:
    	    return 0;
    }
}

fn Path! Path.parent(Path path)
{
	if (path.path_string.len == 1 && is_separator(path.path_string[0], path.env)) return PathResult.NO_PARENT?;
	foreach_r(i, c : path.path_string)
	{
		if (is_separator(c, path.env))
		{
			return { path.path_string[:i], path.env };
		}
	}
	return PathResult.NO_PARENT?;
}

fn String! normalize(String path_str, PathEnv path_env = DEFAULT_PATH_ENV)
{
	if (!path_str.len) return path_str;
    usz path_start = volume_name_len(path_str, path_env)!;
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
		if (is_reserved_path_char(c, path_env)) return PathResult.INVALID_PATH?;

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
					// This is an error: /a/../..
					if (len == path_start && has_root) return PathResult.INVALID_PATH?;

					// If this .. at the start, or after ../? If so, we just copy ..
					if (len == path_start ||
						(len - path_start >= 3 && path_str[len - 1] == path_separator
						&& path_str[len - 3] == '.' && path_str[len - 3] == '.' &&
						(len - 3 == 0 || path_str[len - 4] == path_separator)))
					{
						if (i != len)
						{
							path_str[len] = '.';
							path_str[len + 1] = '.';
						}
						len += 2;
						if (len < path_len) path_str[len++] = path_separator;
						i += 2;
						continue;
					}
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

fn ZString Path.as_zstr(Path path) => (ZString)path.path_string.ptr;

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


fn String Path.as_str(Path path)
{
	return path.path_string;
}


fn bool Path.has_suffix(Path path, String str)
{
	return path.as_str().ends_with(str);
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

