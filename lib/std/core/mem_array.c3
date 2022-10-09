// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::mem::array;

/**
 * @require usz.max / elements > $Type.sizeof
 **/
macro alloc($Type, usz elements)
{
    $Type* ptr = malloc($Type.sizeof * elements);
    return ptr[:elements];
}

/**
 * @require usz.max / elements > $Type.sizeof
 **/
macro talloc($Type, usz elements)
{
    $Type* ptr = tmalloc($Type.sizeof * elements, $Type[1].alignof);
    return ptr[:elements];
}

/**
 * @require (usz.max / elements > $Type.sizeof)
 **/
macro make($Type, usz elements, Allocator* allocator = mem::current_allocator())
{
    $Type* ptr = allocator.calloc($Type.sizeof * elements)!!;
    return ptr[:elements];
}

/**
 * @require (usz.max / elements > $Type.sizeof)
 **/
macro tmake($Type, usz elements)
{
    $Type* ptr = tcalloc($Type.sizeof * elements, $Type[1].alignof);
    return ptr[:elements];
}
