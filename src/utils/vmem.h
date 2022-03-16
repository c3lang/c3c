#pragma once

// Copyright (c) 2020 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#include "common.h"

typedef struct
{
    void *ptr;
    size_t allocated;
    size_t size;
#if PLATFORM_WINDOWS
    size_t committed;
#endif
} Vmem;

void vmem_init(Vmem *vmem, size_t size_in_mb);
void *vmem_alloc(Vmem *vmem, size_t alloc);
void vmem_free(Vmem *vmem);
