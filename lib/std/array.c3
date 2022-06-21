// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::array;

/**
 * @require elements > 0
 **/
macro alloc($Type, usize elements)
{
    $Type* ptr = mem::alloc($Type.sizeof * elements, $alignof($Type));
    return ptr[0..(elements - 1)];
}

/**
 * @require elements > 0
 **/
macro calloc($Type, usize elements)
{
	assert($Type.max / elements < $Type.sizeof);
    $Type* ptr = mem::calloc($sizeof($Type) * elements, $alignof($Type));
    return ptr[0..(elements - 1)];
}
