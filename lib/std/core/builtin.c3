// Copyright (c) 2021-2022 Christoffer Lerno and contributors. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::builtin;
import libc;
import std::hash;

/**
 * Use `IteratorResult` when reading the end of an iterator, or accessing a result out of bounds.
 **/
fault IteratorResult
{
	NO_MORE_ELEMENT
}

/**
 * Use `SearchResult` when trying to return a value from some collection but the element is missing.
 **/
fault SearchResult
{
	MISSING
}

/**
 * Use `CastResult` when an attempt at conversion fails.
 **/
fault CastResult
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
	var temp = *variable;
	defer *variable = temp;
	@body();
}

/**
 * Swap two variables
 * @require $assignable(*b, $typeof(*a)) && $assignable(*a, $typeof(*b))
 **/
macro void @swap(&a, &b) @builtin
{
	var temp = *a;
	*a = *b;
	*b = temp;
}

/**
 * Convert an `any` type to a type, returning an failure if there is a type mismatch.
 *
 * @param v `the any to convert to the given type.`
 * @param $Type `the type to convert to`
 * @return `The any.ptr converted to its type.`
 * @ensure @typeis(return, $Type*)
 * @return! CastResult.TYPE_MISMATCH
 **/
macro anycast(any* v, $Type) @builtin
{
	if (v.type != $Type.typeid) return CastResult.TYPE_MISMATCH?;
	return ($Type*)v.ptr;
}

enum CallLocation : int(String name)
{
	UNKNOWN("???"),
	FUNCTION("function"),
	METHOD("method"),
	MACRO("macro"),
	LAMBDA("lambda"),
	TEST("test"),
}

struct CallstackElement
{
	CallstackElement* prev;
	String function;
	String file;
	CallLocation location;
	uint line;
}

fn bool print_backtrace(String message, int backtraces_to_ignore) @if(env::NATIVE_STACKTRACE)
{
	@pool()
	{
		void*[256] buffer;
		void*[] backtraces = backtrace::capture_current(&buffer);
		backtraces_to_ignore++;
		BacktraceList! backtrace = backtrace::symbolize_backtrace(backtraces, mem::temp());
		if (catch backtrace) return false;
		if (backtrace.len() <= backtraces_to_ignore) return false;
		io::eprint("\nERROR: '");
        io::eprint(message);
        io::eprintn("'");
		foreach (i, &trace : backtrace)
        {
            if (i < backtraces_to_ignore) continue;
            if (trace.is_unknown())
            {
				io::eprintn("  in ???");
				continue;
			}
			if (trace.has_file())
            {
                io::eprintfn("  in %s (%s:%d) [%s]", trace.function, trace.file, trace.line, trace.object_file);
                continue;
            }
            io::eprintfn("  in %s (source unavailable) [%s]", trace.function, trace.object_file);
		}
		return true;
	};
}
fn void default_panic(String message, String file, String function, uint line) @if(env::NATIVE_STACKTRACE)
{
	$if $defined(io::stderr):
	if (!print_backtrace(message, 2))
	{
		io::eprintfn("\nERROR: '%s', in %s (%s:%d)", message, function, file, line);
		return;
	}
	$endif
	$$trap();

}
fn void default_panic(String message, String file, String function, uint line) @if(!env::NATIVE_STACKTRACE)
{
	CallstackElement* stack = $$stacktrace();
	$if $defined(io::stderr):
	if (stack) stack = stack.prev;
	if (stack)
	{
		io::eprint("\nERROR: '");
		io::eprint(message);
		io::eprintn("'");
	}
	else
	{
		io::eprint("\nERROR: '");
		io::eprint(message);
		io::eprintfn("', in %s (%s:%d)", function, file, line);
	}
	while (stack)
	{
		io::eprintfn("  in %s %s (%s:%d)", stack.location.name, stack.function, stack.file, stack.line);
		if (stack == stack.prev) break;
		stack = stack.prev;
	}
	$endif
	$$trap();
}

def PanicFn = fn void(String message, String file, String function, uint line);

PanicFn panic = &default_panic;

fn void panicf(String fmt, String file, String function, uint line, args...)
{
	@stack_mem(512; Allocator* allocator)
	{
		DString s;
		s.init_new(.allocator = allocator);
		s.appendf(fmt, ...args);
		panic(s.str_view(), file, function, line);
	};
}

/**
 * Marks the path as unreachable. This will panic in safe mode, and in fast will simply be assumed
 * never happens.
 * @param [in] string "The panic message or format string"
 **/
macro void unreachable(String string = "Unreachable statement reached.", ...) @builtin @noreturn
{
	panicf(string, $$FILE, $$FUNC, $$LINE, $vasplat());
	$$unreachable();
}

/**
 * Marks the path as unsupported, this is similar to unreachable.
 * @param [in] string "The error message"
 **/
macro void unsupported(String string = "Unsupported function invoked") @builtin @noreturn
{
	panicf(string, $$FILE, $$FUNC, $$LINE, $vasplat());
	$$unreachable();
}

macro any_make(void* ptr, typeid type) @builtin
{
	return $$any_make(ptr, type);
}

macro any.retype_to(&self, typeid type)
{
	return $$any_make(self.ptr, type);
}

macro any.as_inner(&self)
{
	return $$any_make(self.ptr, self.type.inner);
}

/**
 * @param expr "the expression to cast"
 * @param $Type "the type to cast to"
 *
 * @require $sizeof(expr) == $Type.sizeof  "Cannot bitcast between types of different size."
 * @ensure @typeis(return, $Type)
 **/
macro bitcast(expr, $Type) @builtin
{
	$if $Type.alignof <= $alignof(expr):
		return *($Type*)&expr;
	$else
		$Type x @noinit;
		$$memcpy(&x, &expr, $sizeof(expr), false, $Type.alignof, $alignof(expr));
		return x;
	$endif
}

/**
 * @param $Type `The type of the enum`
 * @param [in] enum_name `The name of the enum to search for`
 * @require $Type.kindof == ENUM `Only enums may be used`
 * @ensure @typeis(return, $Type)
 * @return! SearchResult.MISSING
 **/
macro enum_by_name($Type, String enum_name) @builtin
{
	typeid x = $Type.typeid;
	foreach (i, name : x.names)
	{
		if (name == enum_name) return ($Type)i;
	}
	return SearchResult.MISSING?;
}

/**
 * Mark an expression as likely to be true
 *
 * @param #value "expression to be marked likely"
 * @param $probability "in the range 0 - 1"
 * @require $probability >= 0 && $probability <= 1.0
 **/
macro bool @likely(bool #value, $probability = 1.0) @builtin
{
    $switch
    $case env::BUILTIN_EXPECT_IS_DISABLED:
	return #value;
    $case $probability == 1.0:
	return $$expect(#value, true);
    $default:
	return $$expect_with_probability(#value, true, $probability);
    $endswitch
}

/**
 * Mark an expression as unlikely to be true
 *
 * @param #value "expression to be marked unlikely"
 * @param $probability "in the range 0 - 1"
 * @require $probability >= 0 && $probability <= 1.0
 **/
macro bool @unlikely(bool #value, $probability = 1.0) @builtin
{
    $switch
    $case env::BUILTIN_EXPECT_IS_DISABLED:
	return #value;
    $case $probability == 1.0:
	return $$expect(#value, false);
    $default:
	return $$expect_with_probability(#value, false, $probability);
    $endswitch
}

/**
 * @require values::@is_int(#value) || values::@is_bool(#value)
 * @require $assignable(expected, $typeof(#value))
 * @require $probability >= 0 && $probability <= 1.0
 **/
macro @expect(#value, expected, $probability = 1.0) @builtin
{
    $switch
    $case env::BUILTIN_EXPECT_IS_DISABLED:
	return #value == expected;
    $case $probability == 1.0:
	return $$expect(#value, ($typeof(#value))expected);
    $default:
	return $$expect_with_probability(#value, expected, $probability);
    $endswitch
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
macro @prefetch(void* ptr, PrefetchLocality $locality = VERY_NEAR, bool $write = false) @builtin
{
	$if !env::BUILTIN_PREFETCH_IS_DISABLED:
		$$prefetch(ptr, $write ? 1 : 0, $locality.ordinal);
	$endif
}

macro swizzle(v, ...) @builtin
{
	return $$swizzle(v, $vasplat());
}

macro swizzle2(v, v2, ...) @builtin
{
	return $$swizzle2(v, v2, $vasplat());
}

macro anyfault @catch(#expr) @builtin
{
	if (catch f = #expr) return f;
	return anyfault {};
}

macro bool @ok(#expr) @builtin
{
	if (catch #expr) return false;
	return true;
}

macro char[] @as_char_view(&value) @builtin
{
	return ((char*)value)[:$sizeof(*value)];
}

macro uint int.hash(int i) => i;
macro uint uint.hash(uint i) => i;
macro uint short.hash(short s) => s;
macro uint ushort.hash(ushort s) => s;
macro uint char.hash(char c) => c;
macro uint ichar.hash(ichar c) => c;
macro uint long.hash(long i) => (uint)((i >> 32) ^ i);
macro uint ulong.hash(ulong i) => (uint)((i >> 32) ^ i);
macro uint int128.hash(int128 i) => (uint)((i >> 96) ^ (i >> 64) ^ (i >> 32) ^ i);
macro uint uint128.hash(uint128 i) => (uint)((i >> 96) ^ (i >> 64) ^ (i >> 32) ^ i);
macro uint bool.hash(bool b) => (uint)b;
macro uint typeid.hash(typeid t) => ((ulong)(uptr)t).hash();
macro uint String.hash(String c) => (uint)fnv32a::encode(c);
macro uint char[].hash(char[] c) => (uint)fnv32a::encode(c);
macro uint void*.hash(void* ptr) => ((ulong)(uptr)ptr).hash();

module std::core::builtin @if((env::LINUX || env::DARWIN) && env::COMPILER_SAFE_MODE && env::DEBUG_SYMBOLS);
import libc;

fn void sig_panic(String message)
{
	default_panic(message, "???", "???", 0);
}

SignalFunction old_bus_error;
SignalFunction old_segmentation_fault;

fn void sig_bus_error(CInt i)
{
	$if !env::NATIVE_STACKTRACE:
		sig_panic("Illegal memory access.");
	$else
		$if $defined(io::stderr):
			if (!print_backtrace("Illegal memory access.", 1))
			{
				io::eprintn("\nERROR: 'Illegal memory access'.");
			}
		$endif
	$endif
	$$trap();
}

fn void sig_segmentation_fault(CInt i)
{
	$if !env::NATIVE_STACKTRACE:
		sig_panic("Out of bounds memory access.");
	$else
		$if $defined(io::stderr):
			if (!print_backtrace("Out of bounds memory access.", 1))
			{
				io::eprintn("\nERROR: Memory error without backtrace, possible stack overflow.");
			}
		$endif
	$endif
	$$trap();
}

fn void install_signal_handler(CInt signal, SignalFunction func) @local
{
	SignalFunction old = libc::signal(signal, func);
	// Restore
	if ((iptr)old > 1024) libc::signal(signal, old);
}

// Clean this up
fn void install_signal_handlers() @init(101) @local
{
	install_signal_handler(libc::SIGBUS, &sig_bus_error);
	install_signal_handler(libc::SIGSEGV, &sig_segmentation_fault);
}
