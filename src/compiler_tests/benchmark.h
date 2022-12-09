#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.


#include <stdint.h>

void bench_begin(void);
double bench_mark(void);
uint64_t benchstart(void);
double benchmark(uint64_t start);
