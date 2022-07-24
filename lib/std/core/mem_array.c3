// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::mem::array;

/**
 * @require usize.typeid.max / elements > $Type.sizeof
 **/
macro alloc($Type, usize elements)
{
    $Type* ptr = mem::alloc($Type.sizeof * elements, $alignof($Type));
    return ptr[:elements];
}

/**
 * @require (usize.typeid.max / elements > $Type.sizeof)
 **/
macro make($Type, usize elements)
{
    $Type* ptr = mem::calloc($sizeof($Type) * elements, $alignof($Type));
    return ptr[:elements];
}
