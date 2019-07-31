#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "tokens.h"
#include "utils/common.h"

typedef uint32_t SourceLoc;
#define INVALID_LOC UINT32_MAX

typedef struct
{
	SourceLoc loc;
	uint32_t length;
} SourceRange;


typedef struct
{
	const char* start;
	SourceRange span;
	TokenType type : 8;
	union
	{
		const char *string;
	};
} Token;

typedef struct
{
	const char *contents;
	const char *name;
	const char *full_path;
	SourceLoc start_id;
	SourceLoc end_id;
} File;

#define TOKEN_MAX_LENGTH 0xFFFF

