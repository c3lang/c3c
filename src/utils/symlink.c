#ifdef _WIN32

#include <windows.h>

#include "lib.h"

static void normalize(char *path)
{
	for (;*path != 0; path++)
	{
		if (*path == '/') *path = '\\';
	}
}

static char *getdir(char *path)
{
	*strrchr(path, '\\') = 0;
	return path;
}

/* Windows doesn't have symlinks. so here it's just copy */
void win_symlink(char *target, char *linkpath)
{
	normalize(target);
	normalize(linkpath);

	char *dest = linkpath;

	if (target[0] != '.') {
		dest = str_dup(linkpath);
		getdir(dest);
	}

	dest = file_append_path(dest, target);
	bool isdir = false;

	DWORD attrs = GetFileAttributesA(dest);
	if (attrs != INVALID_FILE_ATTRIBUTES && (attrs & FILE_ATTRIBUTE_DIRECTORY)) {
		isdir = true;
	}

	if (isdir) {
		file_copy_dir(dest, linkpath);
	} else {
		file_copy_file(dest, linkpath, true);
	}
}

#endif