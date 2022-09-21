// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::mem::array;

/**
 * @require usize.max / elements > $Type.sizeof
 **/
macro alloc($Type, usize elements)
{
    $Type* ptr = malloc($Type.sizeof * elements);
    return ptr[:elements];
}

/**
 * @require usize.max / elements > $Type.sizeof
 **/
macro talloc($Type, usize elements)
{
    $Type* ptr = tmalloc($Type.sizeof * elements, $alignof($Type[1]));
    return ptr[:elements];
}

/**
 * @require (usize.max / elements > $Type.sizeof)
 **/
macro make($Type, usize elements)
{
    $Type* ptr = calloc($Type.sizeof * elements);
    return ptr[:elements];
}

/**
 * @require (usize.max / elements > $Type.sizeof)
 **/
macro tmake($Type, usize elements)
{
    $Type* ptr = tcalloc($Type.sizeof * elements, $alignof($Type[1]));
    return ptr[:elements];
}
