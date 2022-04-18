// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "benchmark.h"
#include <time.h>
#include <stdio.h>
static clock_t begin = 0;
void bench_begin(void)
{
	begin = clock();
}
double bench_mark(void)
{
	return (clock() - begin) / (double)CLOCKS_PER_SEC;
}
