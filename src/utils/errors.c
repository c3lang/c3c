// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "errors.h"
#include <stdarg.h>

void eprintf(const char *format, ...)
{
	va_list arglist;
	va_start(arglist, format);
	vfprintf(stderr, format, arglist);
	va_end(arglist);
}

void error_exit(const char *format, ...)
{
	va_list arglist;
	va_start(arglist, format);
	vfprintf(stderr, format, arglist);
	fprintf(stderr, "\n");
	va_end(arglist);
	exit(EXIT_FAILURE);
}
