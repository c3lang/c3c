// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::runtime;

struct VirtualAny
{
    void* ptr;
    typeid type_id;
}

struct VirtualContainer
{
    void* ptr;
    void* impl_ptr;
}

struct SubArrayContainer
{
    void* ptr;
    usz len;
}

struct VarArrayHeader
{
    usz size;
    usz capacity;
    void *allocator;
}
