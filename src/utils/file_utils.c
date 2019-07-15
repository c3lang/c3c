// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "file_utils.h"
#include <stdio.h>
#include <stdlib.h>

const char* expand_path(const char* path)
{
	if (path[0] == '~' && path[1] == '/')
	{
		// Ignore leak.
		char *ret = NULL;
		char *home = getenv("HOME");
		if (!home || asprintf(&ret, "%s%s", home, &path[1]) == -1) return &path[2];
		return ret;
	}
	return path;
}