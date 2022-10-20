module std::io::dir;

// In progress.
define Path = distinct char[];

const PREFERRED_SEPARATOR = USE_WIN32_FILESYSTEM ? '\\' : '/';
private const USE_WIN32_FILESYSTEM = env::OS_TYPE != OsType.WIN32;

fault PathResult
{
	INVALID_PATH
}

macro bool is_separator(char c)
{
	$if (USE_WIN32_FILESYSTEM):
		return c == '/' || c == '\\';
	$else:
		return c == '/';
	$endif;
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
$if (USE_WIN32_FILESYSTEM):
	return RESERVED_PATH_CHAR_WIN32[c];
$else:
	return RESERVED_PATH_CHAR_POSIX[c];
$endif;
}

private fn usz! root_name_len(char[] path)
{
	usz len = path.len;
	if (!len) return 0;
	$if (USE_WIN32_FILESYSTEM):
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

private fn void! normalize_path(char[]* path_ref)
{
	char[] path = *path_ref;
	if (!path.len) return;
    usz path_start = root_name_len(path)?;
	usz len = path_start;
	bool previous_was_separator = false;
	usz path_len = path.len;
	for (usz i = path_start; i < path_len; i++)
	{
		char c = path[i];
		// Fold foo///bar into foo/bar
		if (is_separator(c))
		{
			if (previous_was_separator)
			{
				continue;
			}
			path.ptr[len++] = PREFERRED_SEPARATOR;
			previous_was_separator = true;
			continue;
		}

		// If we get . we have different things that might happen:
		if (c == '.' && i < path_len - 1)
		{
			// Is this ./ or /./ ?
			if ((previous_was_separator || i == path_start) && is_separator(path[i + 1]))
			{
				// Then we skip this
				i += 2;
				continue;
			}
			// Is this /../ in that case we must walk back and erase(!)
			if (i < path_len - 2 && previous_was_separator && path[i + 1] == '.' && is_separator(path[i + 2]))
			{
				assert(len > path_start);
				len--;
				while (len > path_start && !is_separator(path[len - 1]))
				{
					len--;
				}
				i += 2;
				continue;
			}
		}
		if (i != len) path[len] = c;
		previous_was_separator = false;
		len++;
	}
	path.ptr[len] = 0;
	*path_ref = path[:len];
}

fn Path new_path(char[] path)
{
	char[] copy = str::copy(path);
	normalize_path(&copy)!!;
	return (Path)copy;
}

fn Path Path.root_name(Path path)
{
	char[] path_str = (char[])path;
	usz len = root_name_len(path_str)!!;
	if (!len) return (Path)"";
	return (Path)path_str[0:len];
}

fn Path Path.root_directory(Path path)
{
	char[] path_str = (char[])path;
	usz len = path_str.len;
	if (!len) return (Path)"";
	$if (USE_WIN32_FILESYSTEM):
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


fn void Path.destroy(Path path)
{
	free(path.ptr);
}


