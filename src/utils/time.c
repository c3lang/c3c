// Copyright (c) 2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.
#include "lib.h"
#include <time.h>

const char *time_get(void)
{
	time_t rawtime;
	struct tm *timeinfo;
	time(&rawtime);
	timeinfo = localtime(&rawtime);
	scratch_buffer_clear();
	scratch_buffer_printf("%02d:%02d:%02d", timeinfo->tm_hour, timeinfo->tm_min, timeinfo->tm_sec);
	return scratch_buffer_copy();
}

const char *date_get(void)
{
	time_t rawtime;
	struct tm *timeinfo;
	time(&rawtime);
	timeinfo = localtime(&rawtime);
	scratch_buffer_clear();
	scratch_buffer_printf("%02d-%02d-%02d", timeinfo->tm_year + 1900, timeinfo->tm_mon, timeinfo->tm_mday);
	return scratch_buffer_copy();
}