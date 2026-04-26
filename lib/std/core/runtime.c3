// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::runtime;
import libc, std::time, std::io, std::sort;

struct ReflectedParam (Printable) @if(!$defined(ReflectedParam))
{
	String name;
	typeid type;
}

struct AnyRaw
{
	void* ptr;
	typeid type;
}

struct SliceRaw
{
	void* ptr;
	sz len;
}

macro @enum_lookup($Type, $name, value)
{
	$foreach $val : $Type::values:
		if ($val.$name == value) return $val;
	$endforeach
	return NOT_FOUND~;
}


module std::core::runtime @if(env::FREESTANDING_WASM);

extern fn void __wasm_call_ctors();
fn void wasm_initialize() @cname("_initialize") @wasm
{
	// The linker synthesizes this to call constructors.
	__wasm_call_ctors();
}

module std::core::runtime @if(env::EMSCRIPTEN);

extern fn void __wasm_call_ctors();
fn void wasm_emscripten_initialize() @cname("_initialize") @wasm
{
	__wasm_call_ctors();
}

fn CInt atexit(void* func) @weak @cname("atexit")
{
	return 0;
}