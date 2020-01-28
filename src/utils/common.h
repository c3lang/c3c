#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.


#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include "errors.h"
#include <stdbool.h>

#define MAX_IDENTIFIER_LENGTH 31
#define PROJECT_TOML "project.toml"
#ifndef __unused
#define __unused
#endif