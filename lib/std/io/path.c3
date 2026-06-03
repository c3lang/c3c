module std::io::path;
import std::collections::list, std::io::os;
import std::os::win32;

alias Path = env::WIN32 ? PathWin : PathPosix;
const PathEnv DEFAULT_ENV = env::WIN32 ? PathEnv.WIN32 : PathEnv.POSIX;

typedef PathCommon = inline String;
typedef PathWin = inline PathCommon;
typedef PathPosix = inline PathCommon;

constdef PathSeparator : inline char
{
	WIN32 = '\\',
	POSIX = '/'
}
const PathSeparator PREFERRED_SEPARATOR = env::WIN32 ? WIN32 : POSIX;

alias PathList = List { Path };

faultdef INVALID_PATH, NO_PARENT;

fn Path? cwd(Allocator allocator)
{
	@pool()
	{
		return new(allocator, os::getcwd(tmem));
	};
}

enum PathEnv : (PathSeparator separator)
{
	WIN32 { WIN32 },
	POSIX { POSIX }
}

fn bool is_dir(Path path) => os::native_is_dir(path);
fn bool is_file(Path path) => os::native_is_file(path);
fn long? file_size(Path path) => os::native_file_size(path);
fn bool exists(Path path) => os::native_file_or_dir_exists(path);
fn Path? tcwd() => cwd(tmem) @inline;

<*
 @require @is_pathlike(path) : "Expected a Path or String to chdir"
*>
macro void? chdir(path)
{
	$if $Typeof(path) == String:
		@pool()
		{
			return os::native_chdir(tnew(path));
		};
	$else
		return os::native_chdir(path) @inline;
	$endif
}

fn Path? temp_directory(Allocator allocator) => os::native_temp_directory(allocator);
fn Path? home_directory(Allocator allocator) => os::native_home_directory(allocator);
fn Path? desktop_directory(Allocator allocator) => os::native_user_directory(allocator, DESKTOP);
fn Path? videos_directory(Allocator allocator) => os::native_user_directory(allocator, VIDEOS);
fn Path? music_directory(Allocator allocator) => os::native_user_directory(allocator, MUSIC);
fn Path? documents_directory(Allocator allocator) => os::native_user_directory(allocator, DOCUMENTS);
fn Path? screenshots_directory(Allocator allocator) => os::native_user_directory(allocator, SCREENSHOTS);
fn Path? saved_games_directory(Allocator allocator) => os::native_user_directory(allocator, SAVED_GAMES);
fn Path? downloads_directory(Allocator allocator) => os::native_user_directory(allocator, DOWNLOADS);
fn Path? pictures_directory(Allocator allocator) => os::native_user_directory(allocator, PICTURES);
fn Path? templates_directory(Allocator allocator) => os::native_user_directory(allocator, TEMPLATES);
fn Path? public_share_directory(Allocator allocator) => os::native_user_directory(allocator, PUBLIC_SHARE);

fn void? delete(PathCommon path) => os::native_remove(path) @inline;

macro bool @is_pathlike(#path) @const => $Typeof(#path) == String ||| $defined(PathCommon c = #path);

macro bool is_separator(char c, PathEnv path_env = DEFAULT_ENV)
{
	return c == '/' || (c == '\\' && path_env == WIN32);
}

macro bool is_posix_separator(char c) => c == '/';
macro bool is_win32_separator(char c) =>  c == '/' || c == '\\';

<*
 List directory contents.

 @param allocator : `The allocator for the returned list and paths`
 @param dir : `The directory to list`
 @param no_dirs : `Exclude directories from the result`
 @param no_symlinks : `Exclude symbolic links from the result, where supported by the platform`
 @param mask : `Optional pattern. Supports one '*' wildcard, for example "*.c3" or "foo*bar"`
 @param recursive : `If true, recursively include entries in subdirectories`
 @param relative : `If true, return paths relative to dir. Defaults to true`
 @param rebase : `If non-empty, prepend this path to each returned entry`
*>
fn PathList? ls(Allocator allocator, Path dir, bool no_dirs = false, bool no_symlinks = false, String mask = "", bool recursive = false, bool relative = true, Path rebase = (Path)"")
{
	PathList list;
	list.init(allocator);
	_ls_collect(allocator, &list, dir, (Path)"", no_dirs, no_symlinks, mask, recursive, relative, rebase)!;
	return list;
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
macro bool? mkdir(path, bool recursive = false, MkdirPermissions permissions = NORMAL)
{
	$if $Typeof(path) == String:
		@pool() { return _mkdir(tnew(path), recursive, permissions); };
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
	$if $Typeof(path) == String:
		@pool() { return _rmdir(temp(path)); };
	$else
		return _rmdir(path);
	$endif
}

<*
 Like [rmdir] but deletes a directory even if it contains items.
*>
fn void? rmtree(Path path)
{
	if (!path.len) return INVALID_PATH~;
	$if $defined(os::native_rmtree):
		return os::native_rmtree(path);
	$else
		return io::UNSUPPORTED_OPERATION~;
	$endif
}

<*
 Creates a new path.

 @return? INVALID_PATH : `if the path was invalid`
*>
fn Path? new(Allocator allocator, String path)
{
	return (Path)normalize(path.copy(allocator), DEFAULT_ENV);
}

<*
 Creates a new path using the temp allocator.

 @return? INVALID_PATH : `if the path was invalid`
*>
fn Path? tnew(String path) => new(tmem, path);

fn Path? from_wstring(Allocator allocator, WString path) => @pool()
{
	return path::new(allocator, string::tfrom_wstring(path)!);
}

fn PathWin? for_windows(Allocator allocator, String path) => (PathWin)normalize(path.copy(allocator), WIN32);

fn PathPosix? for_posix(Allocator allocator, String path) => (PathPosix)normalize(path.copy(allocator), POSIX);


<*
 Append one or more strings to the current path.

 @param [in] parts
 @param extension : "The optional extension to append, automatically inserting a '.' if necessary."
*>
fn PathWin? PathWin.append(self, Allocator allocator, String... parts, String extension = "") => @pool()
{
	return _append_path(self, allocator, parts, extension, WIN32);
}

<*
 Append one or more strings to the current path.

 @param [in] parts
 @param extension : "The optional extension to append, automatically inserting a '.' if necessary."
*>
fn PathPosix? PathPosix.append(self, Allocator allocator, String... parts, String extension = "") => @pool()
{
	return _append_path(self, allocator, parts, extension, POSIX);
}

fn PathPosix? PathPosix.tappend(self, String... parts, String extension = "") => self.append(tmem, ...parts, extension: extension);
fn PathWin? PathWin.tappend(self, String... parts, String extension = "") => self.append(tmem, ...parts, extension: extension);

fn bool? String.is_absolute_path(self) => @pool()
{
	return tnew(self).is_absolute();
}

fn bool? PathPosix.is_absolute(self)
{
	return self.len > 0 && self[0] == PathSeparator.POSIX;
}

fn bool? PathWin.is_absolute(self)
{
	if (!self.len) return false;
	sz path_start = volume_name_len_win(self)!;
	if (path_start > 0 && self[0] == '\\') return true;
	return path_start < self.len && is_separator(self[path_start], WIN32);
}


fn Path? String.to_absolute_path(self, Allocator allocator) => @pool()
{
	return tnew(self).absolute(allocator);
}

fn Path? Path.absolute(self, Allocator allocator)
{
	if (!self.len) return INVALID_PATH~;
	if (self.is_absolute()!) return self.copy(allocator);
	if (self == ".")
	{
		return (Path)os::getcwd(allocator);
	}
	$if Path::typeid == PathWin:
		@pool()
		{
			const sz BUFFER_LEN = 4096;
			WString buffer = (WString)mem::talloc_array(Char16, BUFFER_LEN);
			buffer = win32::_wfullpath(buffer, self.to_temp_wstring()!, BUFFER_LEN);
			if (!buffer) return INVALID_PATH~;
			return (Path)string::from_wstring(allocator, buffer);
		};
	$else
		Path cwd = (Path)os::getcwd(tmem)!;
		return cwd.append(allocator, self);
	$endif
}

fn String? String.file_basename(self, Allocator allocator) => @pool()
{
	return tnew(self).basename().copy(allocator);
}

fn String? String.file_tbasename(self) => self.file_basename(tmem);

fn String PathWin.basename(self) => _basename(self);
fn String PathPosix.basename(self) => _basename(self);

fn String? String.path_tdirname(self) => self.path_dirname(tmem);

fn String? String.path_dirname(self, Allocator allocator) => @pool()
{
	return tnew(self).dirname().copy(allocator);
}

fn String PathPosix.dirname(self)
{
	sz basename_start = start_of_base_name(self, POSIX)!!;
	if (basename_start == 0) return ".";
	if (basename_start <= 1)
	{
		return self[:basename_start];
	}
	return self[:basename_start - 1];
}

fn String PathWin.dirname(self)
{
	sz basename_start = start_of_base_name(self, WIN32)!!;
	if (basename_start == 0) return ".";
	sz start = volume_name_len_win(self)!!;
	if (basename_start <= start + 1)
	{
		if (basename_start > start && self[0..1] == `\\`)
		{
			return self[:basename_start - 1];
		}
		return self[:basename_start];
	}
	return self[:basename_start - 1];
}


<*
 Test if the path has the given extension, so given the path /foo/bar.c3
 this would be true matching the extension "c3"

 @param [in] extension : `The extension name (not including the leading '.')`
 @require extension.len > 0 : `The extension cannot be empty`
 @return `true if the extension matches`
*>
fn bool PathPosix.has_extension(self, String extension) => _has_extension(self, extension);

<*
 Test if the path has the given extension, so given the path C:\foo\bar.c3
 this would be true matching the extension "c3"

 @param [in] extension : `The extension name (not including the leading '.')`
 @require extension.len > 0 : `The extension cannot be empty`
 @return `true if the extension matches`
*>
fn bool PathWin.has_extension(self, String extension) => _has_extension(self, extension);

fn String? PathWin.extension(self) => _extension(self);
fn String? PathPosix.extension(self) => _extension(self);

fn String PathPosix.volume_name(self) => "";
fn String PathWin.volume_name(self)
{
	sz len = volume_name_len_win(self)!!;
	if (!len) return "";
	return self[:len];
}

fn Path? String.to_path(self, Allocator allocator) => new(allocator, self);
fn Path? String.to_tpath(self) => new(tmem, self);


<*
 Get the path of the parent. This does not allocate, but returns a slice
 of the path itself.

 @return `The parent of the path as a non-allocated path`
 @return? NO_PARENT : `if this path does not have a parent`
*>
fn PathWin? PathWin.parent(self) => _parent(self);

<*
 Get the path of the parent. This does not allocate, but returns a slice
 of the path itself.

 @return `The parent of the path as a non-allocated path`
 @return? NO_PARENT : `if this path does not have a parent`
*>
fn PathPosix? PathPosix.parent(self) => _parent(self);


fn String? normalize(String path_str, PathEnv path_env = DEFAULT_ENV)
{
	if (!path_str.len) return path_str;
	sz path_start = 0;
	if (path_env == WIN32)
	{
		path_start = volume_name_len_win(path_str)!;
		if (path_start > 0)
		{
			for (sz i = 0; i < path_start; i++) if (path_str[i] == '/') path_str[i] = '\\';
		}
	}
	sz path_len = path_str.len;
	if (path_start == path_len) return path_str;
	PathSeparator path_separator = path_env.separator;
	sz len = path_start;
	bool has_root = is_separator(path_str[path_start], path_env);
	if (has_root)
	{
		path_str[len++] = path_separator;
		path_start++;
	}
	// It is safe to write it as true, since we already dealt with /foo.
	// This allows us to avoid checking whether it is the start of the path.
	bool previous_was_separator = true;

	for (sz i = path_start; i < path_len; i++)
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
		if (is_reserved_path_char(c, path_env)) return INVALID_PATH~;

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
					if (len == path_start && has_root) return INVALID_PATH~;

					// If this .. at the start, or after ../? If so, we just copy ..
					if (len == path_start ||
						(len - path_start >= 3 && path_str[len - 1] == path_separator
						&& path_str[len - 3] == '.' && path_str[len - 2] == '.' &&
						(len - 3 == 0 || path_str[len - 4] == path_separator)))
					{
						if (i != len) path_str[len:2] = ".."[:2];
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

fn String PathWin.root_directory(self) => _root_directory(self, WIN32);
fn String PathPosix.root_directory(self) => _root_directory(self, POSIX);

alias PathWalker = fn bool? (Path, bool is_dir, void*);

<*
 Walk the path recursively. PathWalker is run on every file and
 directory found. Return true to abort the walk.
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
			if (f == "." || f == "..") continue;
			f = abs.append(allocator, f)!;
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
			if (f == "." || f == "..") continue;
			@stack_mem(128; Allocator smem)
			{
				f = abs.append(smem, f)!;
				bool is_directory = is_dir(f);
				if (callback(f, is_directory, data)!) return true;
				if (is_directory && traverse(f, callback, data)!) return true;
			};
		}
	};
	return false;
}

fn bool PathCommon.has_suffix(self, String str) => self.ends_with(str);

fn PathWin PathWin.copy(self, Allocator allocator) => (PathWin)((String)self).copy(allocator);
fn PathWin PathWin.tcopy(self) => self.copy(tmem) @inline;
fn PathPosix PathPosix.copy(self, Allocator allocator) => (PathPosix)((String)self).copy(allocator);
fn PathPosix PathPosix.tcopy(self) => self.copy(tmem) @inline;

fn void PathCommon.free(self, Allocator allocator) => alloc::free(allocator, self.ptr);

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

macro bool is_reserved_win32_path_char(char c) => RESERVED_PATH_CHAR_WIN32[c];

macro bool is_reserved_path_char(char c, PathEnv path_env = DEFAULT_ENV)
{
	return path_env == PathEnv.WIN32
		? RESERVED_PATH_CHAR_WIN32[c]
		: RESERVED_PATH_CHAR_POSIX[c];
}


// --- Non public

fn bool? _mkdir(Path path, bool recursive = false, MkdirPermissions permissions = NORMAL) @private
{
	if (!path.len) return INVALID_PATH~;
	if (is_dir(path)) return false;
	if (exists(path)) return io::FILE_NOT_DIR~;

	if (recursive)
	{
		if (try parent = path.parent()) mkdir(parent, true, permissions)!;
	}
	if (!is_dir(path.parent()) ?? false) return io::CANNOT_READ_DIR~;

	return os::native_mkdir(path, permissions);
}

fn bool? _rmdir(Path path) @private
{
	if (!path) return INVALID_PATH~;
	return os::native_rmdir(path);
}

fn void? _ls_collect(Allocator allocator, PathList* result, Path base, Path rel_dir, bool no_dirs, bool no_symlinks, String mask, bool recursive, bool relative, Path rebase) @private
{
	$if $defined(os::native_ls):
		@pool()
		{
			Path scan_dir = rel_dir ? base.tappend(rel_dir)! : base;
			PathList entries = os::native_ls(scan_dir, false, no_symlinks, "", tmem)!;
			foreach (entry : entries)
			{
				Path rel = rel_dir ? rel_dir.tappend(entry)! : entry;
				Path full = scan_dir.tappend(entry)!;
				bool is_directory = is_dir(full);
				if (!(is_directory && no_dirs) && _path_match(rel, entry, mask))
				{
					Path item = relative ? rel : full;
					if (rebase) item = rebase.tappend(item)!;
					result.push(item.copy(allocator));
				}
				if (recursive && is_directory) _ls_collect(allocator, result, base, rel, no_dirs, no_symlinks, mask, recursive, relative, rebase)!;
			}
		};
	$else
		return io::UNSUPPORTED_OPERATION~;
	$endif
}

fn bool _path_match(Path rel, Path name, String mask) @private
{
	if (!mask) return true;
	if (try star = mask.index_of_char('*'))
	{
		String prefix = mask[:star];
		String suffix = mask[star + 1..];
		return rel.len >= prefix.len + suffix.len && rel.starts_with(prefix) && rel.ends_with(suffix)
			|| name.len >= prefix.len + suffix.len && name.starts_with(prefix) && name.ends_with(suffix);
	}
	return rel == mask || name == mask;
}

macro _append_path(self, Allocator allocator, String[] parts, String extension, PathEnv path_env) @local => @pool()
{
	sz capacity = self.len + parts.len;
	if (extension) capacity += 1 + extension.len;
	foreach (part : parts) if (part) capacity += part.len;
	@stack_mem(2048; smem)
	{
		DString dstr = dstring::new_with_capacity(smem, capacity);
		dstr.append(self);
		foreach (part : parts)
		{
			if (!part) continue;
			if (dstr.len() && !is_separator(dstr[^1], path_env)) dstr.append(path_env.separator);
			dstr.append(part);
		}
		if (extension)
		{
			if (extension[0] != '.') dstr.append('.');
			dstr.append(extension);
		}
		return ($Typeof(self))normalize(dstr.str_view(), path_env).copy(allocator);
	};

}

fn sz? start_of_base_name(String str, PathEnv path_env) @local
{
	if (!str.len) return 0;
	sz? start_slash = str.rindex_of_char('/');
	if (path_env != WIN32) return (start_slash + 1) ?? 0;
	if (try index = str.rindex_of_char('\\'))
	{
		if (try start_slash && start_slash > index) return start_slash + 1;
		// c:\ style path, we're done!
		if (str[0] != '\\') return index + 1;
		// Handle \\server\foo
		// Find the \ before "foo"
		sz last_index = 2 + str[2..].index_of_char('\\')!;
		// If they don't match, we're done
		if (last_index > index) return INVALID_PATH~;
		if (last_index != index) return index + 1;
		// Otherwise just default to the volume length.
	}
	if (path_env == WIN32) return volume_name_len_win(str)!!;
	return 0;
}

fn sz? volume_name_len_win(String path) @local
{
	sz len = path.len;
	if (len < 2) return 0;
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
			sz base_found = 0;
			for (sz i = 2; i < len; i++)
			{
				char c = path[i];
				if (is_win32_separator(c))
				{
					if (base_found) return i;
					base_found = i;
					continue;
				}
				if (is_reserved_win32_path_char(c)) return INVALID_PATH~;
			}
			if (base_found > 0 && base_found + 1 < len) return len;
			return INVALID_PATH~;
		case 'A'..'Z':
		case 'a'..'z':
			return path[1] == ':' ? 2 : 0;
		default:
			return 0;
	}
}

macro String _basename(self) @local
{
	sz basename_start = start_of_base_name(self, $Typeof(self)::typeid == PathPosix::typeid ? POSIX : WIN32)!!;
	if (basename_start == self.len) return "";
	return self[basename_start..];
}

macro bool _has_extension(self, String extension) @local
{
	String basename = self.basename();
	if (basename.len <= extension.len) return false;
	if (basename[^extension.len + 1] != '.') return false;
	return basename[^extension.len..] == extension;
}

macro String? _extension(self) @local
{
	String basename = self.basename();
	sz index = basename.rindex_of(".")!;
	// Plain ".foo" does not have an extension
	if (index == 0 || index == basename.len) return "";
	return basename[index + 1..];
}

macro _parent(self) @local
{
	PathEnv env = $Typeof(self)::typeid == PathPosix::typeid ? POSIX : WIN32;
	if (self.len == 1 && is_separator(self[0], env)) return NO_PARENT~;
	foreach_r(i, c : self)
	{
		if (is_separator(c, env))
		{
			if (i == 0) return self[..0];
			$if $Typeof(self)::typeid == PathWin::typeid:
				if (i > 1)
				{
					if (try volume_len = volume_name_len_win(self))
					{
						// Handle C:\foo
						if (volume_len == i)
						{
							if (i + 1 == self.len) return NO_PARENT~;
							return self[:i + 1];
						}
					}
				}
			$endif
			return self[:i];
		}
	}
	return NO_PARENT~;
}

fn String _root_directory(String path_str, PathEnv env) @local @inline
{
	sz len = path_str.len;
	if (!len) return "";
	if (path_str == ".") return ".";
	if (env == WIN32)
	{
		sz root_len = volume_name_len_win(path_str)!!;
		if (root_len == len || !is_win32_separator(path_str[root_len])) return "";
		return path_str[root_len..root_len];
	}
	if (!is_posix_separator(path_str[0])) return "";
	for (sz i = 1; i < len; i++)
	{
		if (is_posix_separator(path_str[i]))
		{
			return path_str[:i];
		}
	}
	return path_str;
}
