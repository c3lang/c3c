#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.


#include <stdint.h>
#include <time.h>

#if USE_PTHREAD
typedef struct timespec BenchTime;
#else
typedef clock_t BenchTime;
#endif

void bench_begin(void);
double bench_mark(void);
BenchTime benchstart(void);
double benchmark(BenchTime start);
