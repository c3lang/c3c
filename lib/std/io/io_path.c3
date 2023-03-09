module std::io::path;

const PREFERRED_SEPARATOR = env::os_is_win32() ? '\\' : '/';

fault PathResult
{
	INVALID_PATH
}

typedef Path = distinct String;



fn Path! getcwd(Allocator* using = mem::heap())
{
	return (Path)os::getcwd(using);
}

fn Path! tgetcwd()
{
	return getcwd(mem::temp()) @inline;
}



fn bool is_dir(String path)
{
	return os::native_is_dir(path);
}

macro bool is_separator(char c)
{
	$if (env::os_is_win32()):
		return c == '/' || c == '\\';
	$else:
		return c == '/';
	$endif;
}



fn Path! new(String path, Allocator* using = mem::heap())
{
	Path copy = (Path)path.copy(using);
	copy.normalize()?;
	return (Path)copy;
}

/**
 * Append the string to the current path.
 *
 * @param [in] path
 * @param [in] filename
 * @ensure return.len >= filename.len + path.len
 **/
fn Path! Path.append(Path path, String filename, Allocator* using = mem::heap())
{
	if (!path.len) return new(filename, using)?;
	if (is_separator(path[^1]))
	{
		return (Path)path.as_str().concat(filename, using);
	}
	// Handle temp nested temp allocations.
	TempAllocator* temp = mem::temp();
	usz mark = temp.used;
	defer if (using != temp) temp.reset(mark);
	DString dstr = dstring::new_with_capacity(path.len + 1 + filename.len, .using = temp);
	dstr.append(path.as_str());
	dstr.append(PREFERRED_SEPARATOR);
	dstr.append(filename);
	Path p = using == temp ? (Path)dstr.str() : (Path)dstr.copy_str(using);
	p.normalize()?;
	return p;
}

fn Path! temp_directory(Allocator* using = mem::heap())
{
	return os::native_temp_directory(using);
}

fn Path Path.root_name(Path path)
{
	usz len = root_name_len(path.as_str())!!;
	if (!len) return (Path)"";
	return path[:len];
}


fn usz! root_name_len(String path) @local
{
	usz len = path.len;
	if (!len) return 0;
	$if (env::os_is_win32()):
		switch (path[0])
		{
			case '\\':
				if (len < 2 || path[1] != '\\') break;
				if (len == 2 || is_separator(path[2])) return PathResult.INVALID_PATH!;
				for (usz i = 2; i < len; i++)
				{
					char c = path[i];
					if (is_separator(c)) return i;
					if (is_reserved_path_char(c)) return PathResult.INVALID_PATH!;
				}
				return len;
			case 'A'..'Z':
			case 'a'..'z':
				if (len < 2 || path[1] != ':') break;
				if (len < 3 || !is_separator(path[2])) return PathResult.INVALID_PATH!;
				return 2;
		}
	$endif;
	return 0;
}

fn Path! Path.parent(Path path)
{
	foreach_r(i, c : path)
	{
		if (is_separator(c) && i != path.len - 1)
		{
			return (Path)path[:i];
		}
	}
	return PathResult.INVALID_PATH!;
}

fn void! Path.normalize(Path* path)
{
	String path_str = path.as_str();
	if (!path_str.len) return;
    usz path_start = root_name_len(path_str)?;
	usz len = path_start;
	bool previous_was_separator = false;
	usz path_len = path.len;
	for (usz i = path_start; i < path_len; i++)
	{
		char c = path_str[i];
		// Fold foo///bar into foo/bar
		if (is_separator(c))
		{
			if (previous_was_separator)
			{
				continue;
			}
			path_str.ptr[len++] = PREFERRED_SEPARATOR;
			previous_was_separator = true;
			continue;
		}

		// If we get . we have different things that might happen:
		if (c == '.' && i < path_len - 1)
		{
			// Is this ./ or /./ ?
			if ((previous_was_separator || i == path_start) && is_separator(path_str[i + 1]))
			{
				// Then we skip this
				i += 2;
				continue;
			}
			// Is this /../ in that case we must walk back and erase(!)
			if (i < path_len - 2 && previous_was_separator && path_str[i + 1] == '.' && is_separator(path_str[i + 2]))
			{
				assert(len > path_start);
				len--;
				while (len > path_start && !is_separator(path_str[len - 1]))
				{
					len--;
				}
				i += 2;
				continue;
			}
		}
		if (i != len) path_str[len] = c;
		previous_was_separator = false;
		len++;
	}
	path_str.ptr[len] = 0;
	*path = (Path)path_str[:len];
}

fn Path Path.root_directory(Path path)
{
	String path_str = path.as_str();
	usz len = path_str.len;
	if (!len) return (Path)"";
	$if (env::os_is_win32()):
		usz root_len = root_name_len(path_str)!!;
		if (root_len == len || !is_separator(path_str[root_len])) return (Path)"";
		return (Path)path_str[root_len..root_len];
	$else:
		if (!is_separator(path_str[0])) return (Path)"";
		for (usz i = 1; i < len; i++)
		{
			if (is_separator(path_str[i]))
			{
				return (Path)path_str[:i];
			}
		}
		return path;
	$endif;
}

fn usz! Path.file_size(Path path)
{
	return os::native_file_size(path.as_str());
}

fn String Path.as_str(Path path)
{
	return (String)path;
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
	free(path.ptr);
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

macro bool is_reserved_path_char(char c)
{
$if (env::os_is_win32()):
	return RESERVED_PATH_CHAR_WIN32[c];
$else:
	return RESERVED_PATH_CHAR_POSIX[c];
$endif;
}

