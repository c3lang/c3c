module std::io::path;
import std::collections::list, std::io::os;
import std::os::win32;

const PathEnv DEFAULT_ENV = env::WIN32 ? PathEnv.WIN32 : PathEnv.POSIX;
const char PREFERRED_SEPARATOR_WIN32 = '\\';
const char PREFERRED_SEPARATOR_POSIX = '/';
const char PREFERRED_SEPARATOR = env::WIN32 ? PREFERRED_SEPARATOR_WIN32 : PREFERRED_SEPARATOR_POSIX;

alias PathList = List { Path };

faultdef INVALID_PATH, NO_PARENT;

alias Path = PathImp;

struct PathImp (Printable)
{
	String path_string;
	PathEnv env;
	Allocator allocator;
}

enum PathEnv
{
	WIN32,
	POSIX
}

fn Path? cwd(Allocator allocator)
{
	@pool()
	{
		return new(allocator, os::getcwd(tmem));
	};
}

fn bool is_dir(Path path) => os::native_is_dir(path.str_view());
fn bool is_file(Path path) => os::native_is_file(path.str_view());
fn usz? file_size(Path path) => os::native_file_size(path.str_view());
fn bool exists(Path path) => os::native_file_or_dir_exists(path.str_view());
fn Path? tcwd() => cwd(tmem) @inline;

<*
 @require @is_pathlike(path) : "Expected a Path or String to chdir"
*>
macro void? chdir(path)
{
	$if @typeis(path, String):
		@pool()
		{
			return os::native_chdir(temp(path));
		};
	$else
		return os::native_chdir(path) @inline;
	$endif
}

fn Path? temp_directory(Allocator allocator) => os::native_temp_directory(allocator);

fn void? delete(Path path) => os::native_remove(path.str_view()) @inline;

macro bool @is_pathlike(#path) => @typeis(#path, String) || @typeis(#path, Path);

macro bool is_separator(char c, PathEnv path_env = DEFAULT_ENV)
{
	return c == '/' || (c == '\\' && path_env == PathEnv.WIN32);
}

macro bool is_posix_separator(char c) => c == '/';
macro bool is_win32_separator(char c) =>  c == '/' || c == '\\';

fn PathList? ls(Allocator allocator, Path dir, bool no_dirs = false, bool no_symlinks = false, String mask = "")
{
	$if $defined(os::native_ls):
		return os::native_ls(dir, no_dirs, no_symlinks, mask, allocator);
	$else
		return io::UNSUPPORTED_OPERATION?;
	$endif
}

enum MkdirPermissions
{
	NORMAL,
	USER_ONLY,
	USER_AND_ADMIN
}

<*
 Create a directory on a given path, optionally recursive.

 @param path : `The path to create`
 @require @is_pathlike(path) : "Expected a Path or String to chdir"
 @param recursive : `If directories in between should be created if they're missing, defaults to false`
 @param permissions : `The permissions to set on the directory`
*>
macro bool? mkdir(Path path, bool recursive = false, MkdirPermissions permissions = NORMAL)
{
	$if @typeis(path, String):
		@pool() { return _mkdir(temp(path), recursive, permissions); };
	$else
		return _mkdir(path, recursive, permissions);
	$endif
}


<*
 Tries to delete directory, which must be empty.

 @param path : `The path to delete`
 @require @is_pathlike(path) : "Expected a Path or String to chdir"
 @return `true if there was a directory to delete, false otherwise`
 @return? INVALID_PATH : `if the path was invalid`
*>
macro bool? rmdir(path)
{
	$if @typeis(path, String):
		@pool() { return _rmdir(temp(path)); };
	$else
		return _mkdir(path);
	$endif
}

<*
 Like [rmdir] but deletes a directory even if it contains items.
*>
fn void? rmtree(Path path)
{
	if (!path.path_string.len) return INVALID_PATH?;
	$if $defined(os::native_rmtree):
		return os::native_rmtree(path);
	$else
		return io::UNSUPPORTED_OPERATION?;
	$endif
}

<*
 Creates a new path.

 @return? INVALID_PATH : `if the path was invalid`
*>
fn Path? new(Allocator allocator, String path, PathEnv path_env = DEFAULT_ENV)
{
	return { normalize(path.copy(allocator), path_env), path_env, allocator };
}

<*
 Creates a new path using the temp allocator.

 @return? INVALID_PATH : `if the path was invalid`
*>
fn Path? temp(String path, PathEnv path_env = DEFAULT_ENV)
{
	return new(tmem, path, path_env);
}

fn Path? from_win32_wstring(Allocator allocator, WString path) => @pool()
{
	return path::new(allocator, string::tfrom_wstring(path)!);
}

fn Path? for_windows(Allocator allocator, String path)
{
	return new(allocator, path, WIN32);
}

fn Path? for_posix(Allocator allocator, String path)
{
	return new(allocator, path, POSIX);
}

fn bool Path.equals(self, Path p2)
{
	return self.env == p2.env && self.path_string == p2.path_string;
}

<*
 Append the string to the current path.

 @param [in] filename
*>
fn Path? Path.append(self, Allocator allocator, String filename)
{
	if (!self.path_string.len) return new(allocator, filename, self.env)!;
	assert(!is_separator(self.path_string[^1], self.env));

	@pool()
	{
		DString dstr = dstring::temp_with_capacity(self.path_string.len + 1 + filename.len);
		dstr.append(self.path_string);
		dstr.append(PREFERRED_SEPARATOR);
		dstr.append(filename);
		return new(allocator, dstr.str_view(), self.env);
	};
}

fn Path? Path.tappend(self, String filename) => self.append(tmem, filename);

fn usz? start_of_base_name(String str, PathEnv path_env) @local
{
	if (!str.len) return 0;
	usz? start_slash = str.rindex_of_char('/');
	if (path_env != PathEnv.WIN32) return start_slash + 1 ?? 0;
	if (try index = str.rindex_of_char('\\'))
	{
		if (try start_slash && start_slash > index) return start_slash + 1;
		// c:\ style path, we're done!
		if (str[0] != '\\') return index + 1;
		// Handle \\server\foo
		// Find the \ before "foo"
		usz last_index = 2 + str[2..].index_of_char('\\')!;
		// If they don't match, we're done
		if (last_index > index) return INVALID_PATH?;
		if (last_index != index) return index + 1;
		// Otherwise just default to the volume length.
	}
	return volume_name_len(str, path_env)!!;
}


fn bool? String.is_absolute_path(self) => @pool()
{
	return temp(self).is_absolute();
}

fn bool? Path.is_absolute(self)
{
	String path_str = self.str_view();
	if (!path_str.len) return false;
	usz path_start = volume_name_len(path_str, self.env)!;
	if (path_start > 0 && path_str[0] == '\\') return true;
	return path_start < path_str.len && is_separator(path_str[path_start], self.env);
}


fn Path? String.to_absolute_path(self, Allocator allocator) => @pool()
{
	return temp(self).absolute(allocator);
}

<*
 @require self.env == DEFAULT_ENV : "This method is only available on native paths"
*>
fn Path? Path.absolute(self, Allocator allocator)
{
	String path_str = self.str_view();
	if (!path_str.len) return INVALID_PATH?;
	if (self.is_absolute()!) return new(allocator, path_str, self.env);
	if (path_str == ".")
	{
		@pool()
		{
			String cwd = os::getcwd(tmem)!;
			return new(allocator, cwd, self.env);
		};
	}
	$if DEFAULT_ENV == WIN32:
		@pool()
		{
			const usz BUFFER_LEN = 4096;
			WString buffer = (WString)mem::talloc_array(Char16, BUFFER_LEN);
			buffer = win32::_wfullpath(buffer, path_str.to_temp_wstring()!, BUFFER_LEN);
			if (!buffer) return INVALID_PATH?;
			return { string::from_wstring(allocator, buffer), WIN32, allocator };
		};
	$else
		String cwd = os::getcwd(tmem)!;
		return (Path){ cwd, self.env, tmem }.append(allocator, path_str)!;
	$endif
}

fn String? String.file_basename(self, Allocator allocator) => @pool()
{
	return temp(self).basename().copy(allocator);
}

fn String? String.file_tbasename(self) => self.file_basename(tmem);

fn String Path.basename(self)
{
	usz basename_start = start_of_base_name(self.path_string, self.env)!!;
	String path_str = self.path_string;
	if (basename_start == path_str.len) return "";
	return path_str[basename_start..];
}

fn String? String.path_tdirname(self) => self.path_dirname(tmem);

fn String? String.path_dirname(self, Allocator allocator) => @pool()
{
	return temp(self).dirname().copy(allocator);
}

fn String Path.dirname(self)
{
	String path_str = self.path_string;
	usz basename_start = start_of_base_name(path_str, self.env)!!;
	if (basename_start == 0) return ".";
	usz start = volume_name_len(path_str, self.env)!!;
	if (basename_start <= start + 1)
	{
		if (self.env == WIN32 && basename_start > start && path_str[0..1] == `\\`)
		{
			return path_str[:basename_start - 1];
		}
		return path_str[:basename_start];
	}
	return path_str[:basename_start - 1];
}


<*
 Test if the path has the given extension, so given the path /foo/bar.c3
 this would be true matching the extension "c3"

 @param [in] extension : `The extension name (not including the leading '.')`
 @require extension.len > 0 : `The extension cannot be empty`
 @return `true if the extension matches`
*>
fn bool Path.has_extension(self, String extension)
{
	String basename = self.basename();
	if (basename.len <= extension.len) return false;
	if (basename[^extension.len + 1] != '.') return false;
	return basename[^extension.len..] == extension;
}

fn String? Path.extension(self)
{
	String basename = self.basename();
	usz index = basename.rindex_of(".")!;
	// Plain ".foo" does not have an extension
	if (index == 0 || index == basename.len) return "";
	return basename[index + 1..];
}

fn String Path.volume_name(self)
{
	usz len = volume_name_len(self.str_view(), self.env)!!;
	if (!len) return "";
	return self.path_string[:len];
}

fn Path? String.to_path(self, Allocator allocator)
{
	return new(allocator, self);
}

fn Path? String.to_tpath(self)
{
	return new(tmem, self);
}

fn usz? volume_name_len(String path, PathEnv path_env) @local
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
			// Check that we have a name followed by '\'
			isz base_found = 0;
			for (usz i = 2; i < len; i++)
			{
				char c = path[i];
				if (is_win32_separator(c))
				{
					if (base_found) return i;
					base_found = i;
					continue;
				}
				if (is_reserved_win32_path_char(c)) return INVALID_PATH?;
			}
			if (base_found > 0 && base_found + 1 < len) return len;
			return INVALID_PATH?;
		case 'A'..'Z':
		case 'a'..'z':
			return path[1] == ':' ? 2 : 0;
		default:
			return 0;
	}
}

<*
 Get the path of the parent. This does not allocate, but returns a slice
 of the path itself.

 @return `The parent of the path as a non-allocated path`
 @return? NO_PARENT : `if this path does not have a parent`
*>
fn Path? Path.parent(self)
{
	if (self.path_string.len == 1 && is_separator(self.path_string[0], self.env)) return NO_PARENT?;
	foreach_r(i, c : self.path_string)
	{
		if (is_separator(c, self.env))
		{
			return { self.path_string[:i], self.env, null };
		}
	}
	return NO_PARENT?;
}

fn String? normalize(String path_str, PathEnv path_env = DEFAULT_ENV)
{
	if (!path_str.len) return path_str;
	usz path_start = volume_name_len(path_str, path_env)!;
	if (path_start > 0 && path_env == PathEnv.WIN32)
	{
		for (usz i = 0; i < path_start; i++) if (path_str[i] == '/') path_str[i] = '\\';
	}
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
		if (is_reserved_path_char(c, path_env)) return INVALID_PATH?;

		// If we have '.' after a separator
		if (c == '.' && previous_was_separator)
		{
			// Get the number of dots until next separator, expecting 1 or 2
			bool is_last = i == path_len - 1;
			int dots = 1;
			if (!is_last)
			{
				char next = path_str[i + 1];
				switch
				{
					case next == '.':
						dots = 2;
						is_last = i == path_len - 2;
						if (!is_last && !is_separator(path_str[i + 2], path_env))
						{
							dots = 0;
						}
					case !is_separator(next, path_env):
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
					if (len == path_start && has_root) return INVALID_PATH?;

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
	if (path_str.len > len) path_str.ptr[len] = 0;
	// Empty path after normalization -> "."
	if (!len)
	{
		path_str[0] = '.';
		return path_str[:1];
	}
	return path_str[:len];
}

fn ZString Path.as_zstr(self) => (ZString)self.path_string.ptr;

fn String Path.root_directory(self)
{
	String path_str = self.str_view();
	usz len = path_str.len;
	if (!len) return "";
	if (path_str == ".") return ".";
	if (self.env == PathEnv.WIN32)
	{
		usz root_len = volume_name_len(path_str, self.env)!!;
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

alias PathWalker = fn bool? (Path, bool is_dir, void*);

<*
 Walk the path recursively. PathWalker is run on every file and
 directory found. Return true to abort the walk.
 @require self.env == DEFAULT_ENV : "This method is only available on native paths"
*>
fn bool? Path.walk(self, PathWalker w, void* data)
{
	const PATH_MAX = 512;
	@stack_mem(PATH_MAX; Allocator allocator)
	{
		Path abs = self.absolute(allocator)!;
		PathList files = ls(allocator, abs)!;
		foreach (f : files)
		{
			if (f.str_view() == "." || f.str_view() == "..") continue;
			f = abs.append(allocator, f.str_view())!;
			bool is_directory = is_dir(f);
			if (w(f, is_directory, data)!) return true;
			if (is_directory && f.walk(w, data)!) return true;
		}
	};
	return false;
}

alias TraverseCallback = fn bool? (Path, bool is_dir, any data);

<*
 Walk the path recursively. TraverseCallback is run for every file and
 directory found. Return true to abort the walk.
 @require path.env == DEFAULT_ENV : "This method is only available on native paths"
*>
fn bool? traverse(Path path, TraverseCallback callback, any data)
{
	const PATH_MAX = 512;
	@stack_mem(PATH_MAX; Allocator allocator)
	{
		Path abs = path.absolute(allocator)!;
		PathList files = ls(allocator, abs)!;
		foreach (f : files)
		{
			if (f.str_view() == "." || f.str_view() == "..") continue;
			@stack_mem(128; Allocator smem)
			{
				f = abs.append(smem, f.str_view())!;
				bool is_directory = is_dir(f);
				if (callback(f, is_directory, data)!) return true;
				if (is_directory && traverse(f, callback, data)!) return true;
			};
		}
	};
	return false;
}

fn String Path.str_view(self) @inline
{
	return self.path_string;
}


fn bool Path.has_suffix(self, String str)
{
	return self.str_view().ends_with(str);
}

<*
 @require self.allocator != null : "This Path should never be freed"
*>
fn void Path.free(self)
{
	allocator::free(self.allocator, self.path_string.ptr);
}

fn usz? Path.to_format(&self, Formatter* formatter) @dynamic
{
	return formatter.print(self.str_view());
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

macro bool is_reserved_path_char(char c, PathEnv path_env = DEFAULT_ENV)
{
	return path_env == PathEnv.WIN32
		? RESERVED_PATH_CHAR_WIN32[c]
		: RESERVED_PATH_CHAR_POSIX[c];
}
fn bool? _mkdir(Path path, bool recursive = false, MkdirPermissions permissions = NORMAL) @private
{
	if (!path.path_string.len) return INVALID_PATH?;
	if (is_dir(path)) return false;
	if (exists(path)) return io::FILE_NOT_DIR?;

	if (recursive)
	{
		if (try parent = path.parent()) mkdir(parent, true, permissions)!;
	}
	if (!is_dir(path.parent()) ?? false) return io::CANNOT_READ_DIR?;

	return os::native_mkdir(path, permissions);
}

fn bool? _rmdir(Path path) @private
{
	if (!path.path_string.len) return INVALID_PATH?;
	return os::native_rmdir(path);
}
