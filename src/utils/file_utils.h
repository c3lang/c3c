#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "common.h"

const char* expand_path(const char* path);

char *read_file(const char *path, size_t *return_size);

int filename_to_module(const char *path, char buffer[MAX_IDENTIFIER_LENGTH + 1]);
