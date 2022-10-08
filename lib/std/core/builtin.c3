// Copyright (c) 2021-2022 Christoffer Lerno and contributors. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::builtin;
import libc;
import std::hash;

fault IteratorResult
{
	NO_MORE_ELEMENT
}

fault SearchResult
{
	MISSING
}

fault VarCastResult
{
	TYPE_MISMATCH
}

/**
 * Stores a variable on the stack, then restores it at the end of the
 * macro scope.
 *
 * @param variable `the variable to store and restore`
 **/
macro void @scope(&variable; @body) @builtin
{
	$typeof(variable) temp = variable;
	defer variable = temp;
	@body();
}

macro void @swap(&a, &b) @builtin
{
	$typeof(a) temp = a;
    a = b;
    b = temp;
}

/**
 * Convert a variant type to a type, returning an failure if there is a type mismatch.
 *
 * @param v `the variant to convert to the given type.`
 * @param $Type `the type to convert to`
 * @return `The variant.ptr converted to its type.`
 **/
macro varcast(variant v, $Type) @builtin
{
	if (v.type != $Type.typeid) return VarCastResult.TYPE_MISMATCH!;
	return ($Type*)v.ptr;
}

struct CallstackElement
{
	CallstackElement* prev;
	char* function;
	char* file;
	uint line;
}

fn void panic(char* message, char *file, char *function, uint line) @builtin
{
	CallstackElement* stack = $$stacktrace();
	$if ($defined(libc::stderr) && $defined(libc::fprintf)):

	if (stack) stack = stack.prev;
	if (stack)
	{
		libc::fprintf(libc::stderr(), "\nERROR: '%s'\n", message);
	}
	else
	{
		libc::fprintf(libc::stderr(), "\nERROR: '%s', function %s (%s:%d)\n", message, function, file, line);
	}
	while (stack)
	{
		libc::fprintf(libc::stderr(), "  at function %s (%s:%u)\n", stack.function, stack.file, stack.line);
		if (stack == stack.prev) break;
		stack = stack.prev;
	}

	$endif;

	$$trap();
}

macro void unreachable($string = "Unreachable statement reached.") @builtin @noreturn
{
	panic($string, $$FILE, $$FUNC, $$LINE);
	$$unreachable();
}

macro bitcast(expr, $Type) @builtin
{
    var $size = (usize)($sizeof(expr));
    $assert($size == $Type.sizeof, "Cannot bitcast between types of different size.");
    $Type x = void;
    mem::copy(&x, &expr, $size, $Type.alignof, $alignof(expr));
    return x;
}

/**
 * @require $Type.kindof == TypeKind.ENUM `Only enums may be used`
 **/
macro enum_by_name($Type, char[] enum_name) @builtin
{
	typeid x = $Type.typeid;
	foreach (i, name : x.names)
	{
		if (str::compare(name, enum_name)) return ($Type)i;
	}
	return SearchResult.MISSING!;
}

/**
 * Locality for prefetch, levels 0 - 3, corresponding
 * to "extremely local" to "no locality"
 **/
enum PrefetchLocality
{
	NO_LOCALITY,
	FAR,
	NEAR,
	VERY_NEAR,
}

/**
 * Prefetch a pointer.

 * @param [in] ptr `Pointer to prefetch`
 * @param $locality `Locality ranging from none to extremely local`
 * @param $write `Prefetch for write, otherwise prefetch for read.`
 **/
macro prefetch(void* ptr, PrefetchLocality $locality = VERY_NEAR, bool $write = false) @builtin
{
	$$prefetch(ptr, $write ? 1 : 0, $locality.ordinal);
}

macro bool @castable(#expr, $To) @builtin
{
	return $checks(($To)#expr);
}

macro bool @convertible(#expr, $To) @builtin
{
	return $checks($To x = #expr);
}

macro uint int.hash(int i) = i;
macro uint uint.hash(uint i) = i;
macro uint short.hash(short s) = s;
macro uint ushort.hash(ushort s) = s;
macro uint char.hash(char c) = c;
macro uint ichar.hash(ichar c) = c;
macro uint long.hash(long i) = (uint)((i >> 32) ^ i);
macro uint ulong.hash(ulong i) = (uint)((i >> 32) ^ i);
macro uint bool.hash(bool b) = (uint)b;
macro uint typeid.hash(typeid t) = (uint)(((uptr)t >> 32) ^ (uptr)t);
macro uint char[].hash(char[] c) = (uint)fnv32a::encode(c);