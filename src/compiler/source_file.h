#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.


#include "compiler_common.h"

File *source_file_load(const char *filename, bool *already_loaded);
File *source_file_from_position(SourceLoc loc);
