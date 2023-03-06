// Copyright (c) 2021-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

module std::core::mem::array;

/**
 * @require usz.max / elements > $Type.sizeof
 **/
macro alloc($Type, usz elements) @deprecated => malloc($Type, elements);

/**
 * @require usz.max / elements > $Type.sizeof
 **/
macro talloc($Type, usz elements) @deprecated => tmalloc($Type, elements);

/**
 * @require (usz.max / elements > $Type.sizeof)
 **/
macro make($Type, usz elements, Allocator* allocator = mem::heap()) @deprecated
{
	return calloc($Type, elements, .using = allocator);
}

/**
 * @require (usz.max / elements > $Type.sizeof)
 **/
macro tmake($Type, usz elements) @deprecated
{
	return tcalloc($Type, elements);
}
