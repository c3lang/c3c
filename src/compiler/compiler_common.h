#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <stdint.h>

typedef uint32_t SourceLoc;

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
	SourceLoc start;
	SourceLoc end;
} File;

#define TOKEN_MAX_LENGTH 0xFFFF
#define MAX_IDENTIFIER_LENGTH 31
