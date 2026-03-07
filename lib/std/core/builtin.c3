// Copyright (c) 2021-2024 Christoffer Lerno and contributors. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::builtin;
import libc, std::hash, std::io, std::os::backtrace;


<*
 EMPTY_MACRO_SLOT is a value used for implementing optional arguments for macros in an efficient
 way. It relies on the fact that distinct types are not implicitly convertible.

 You can use `@is_empty_macro_slot()` and `@is_valid_macro_slot()` to figure out whether
 the argument has been used or not.

 An example:

```c3
macro foo(a, #b = EMPTY_MACRO_SLOT)
{
	$if @is_valid_macro_slot(#b):
		return invoke_foo2(a, #b);
	$else
		return invoke_foo1(a);
	$endif
}
*>
const EmptySlot EMPTY_MACRO_SLOT @builtin @deprecated("Use `#arg = ...` instead.") = null;

typedef EmptySlot @constinit = void*;
macro bool @is_empty_macro_slot(#arg) @const @builtin
		@deprecated("Use `#arg = ...` to define an optional macro slot, and `$defined(#arg)` to detect whether the argument is set.")
	=> $typeof(#arg) == EmptySlot;
macro bool @is_valid_macro_slot(#arg) @const @builtin
		@deprecated("Use `#arg = ...` to define an optional macro slot, and `$defined(#arg)` to detect whether the argument is set.")
	=> $typeof(#arg) != EmptySlot;

<*
 Returns a random value at compile time.

 @ensure return >= 0.0 && return < 1.0
 @return "A compile time random"
*>
macro @rnd() @const @builtin => $$rnd();

/*
 Use `NO_MORE_ELEMENT` when reading the end of an iterator, or accessing a result out of bounds.
*/
faultdef NO_MORE_ELEMENT @builtin;

/*
 Use `NOT_FOUND` when trying to return a value from some collection but the element is missing.
*/
faultdef NOT_FOUND @builtin;

/*
 Use `TYPE_MISMATCH` when an attempt at conversion fails.
*/
faultdef TYPE_MISMATCH @builtin;

/*
 Use `CAPACITY_EXCEEDED` when trying to add to a bounded list or similar.
*/
faultdef CAPACITY_EXCEEDED @builtin;
/*
 Use `NOT_IMPLEMENTED` when something is conditionally available.
*/
faultdef NOT_IMPLEMENTED @builtin;

alias VoidFn = fn void();

<*
 Stores a variable on the stack, then restores it at the end of the
 macro scope.

 @param #variable : `the variable to store and restore`
 @require $defined(#variable = #variable) : `Expected an actual variable`
*>
macro void @scope(#variable; @body) @builtin
{
	var temp = #variable;
	defer #variable = temp;
	@body();
}

<*
 Swap two variables
 @require $defined(#a = #b, #b = #a) : `The values must be mutually assignable`
*>
macro void @swap(#a, #b) @builtin
{
	var temp = #a;
	#a = #b;
	#b = temp;
}

macro usz bitsizeof($Type) @builtin @const => $Type.sizeof * 8u;

macro usz @bitsizeof(#expr) @builtin @const => $sizeof(#expr) * 8u;

<*
 Compile-time check for whether a set of constants contains a certain expression.

 @param #needle : "The expression whose value should be located."
*>
macro bool @in(#needle, ...) @builtin @const
{
    $for var $x = 0; $x < $vacount; $x++:
        $assert $defined(#needle == $vaconst[$x])
            : "Index %s: types '%s' (needle) and '%s' are not equatable", $x, $typeof(#needle), $typeof($vaconst[$x]);
        $if #needle == $vaconst[$x]: return true; $endif
    $endfor
    return false;
}

<*
 Convert an `any` type to a type, returning an failure if there is a type mismatch.

 @param v : `the any to convert to the given type.`
 @param $Type : `the type to convert to`
 @return `The any.ptr converted to its type.`
 @ensure $typeof(return) == $Type*
 @return? TYPE_MISMATCH
*>
macro anycast(any v, $Type) @builtin
{
	if (v.type != $Type.typeid) return TYPE_MISMATCH~;
	return ($Type*)v.ptr;
}

<*
  @return "The value in the pointer"
  @return? TYPE_MISMATCH
*>
macro any.to(self, $Type)
{
	if (self.type != $Type.typeid) return TYPE_MISMATCH~;
	return *($Type*)self.ptr;
}

<*
 @require self.type == $Type : "The 'any' contained an unexpected type."
 @return "The value in the pointer"
*>
macro any.as(self, $Type)
{
	return *($Type*)self.ptr;
}

macro bool @assignable_to(#foo, $Type) @const @builtin @deprecated("use '$defined($Type x = #foo)'") => $defined(*&&($Type){} = #foo);

macro @addr(#val) @builtin
{
	$if $defined(&#val):
		return &#val;
	$else
		return &&#val;
	$endif
}

macro typeid @typeid(#value) @const @builtin
{
	return $typeof(#value).typeid;
}

macro TypeKind @typekind(#value) @const @builtin @deprecated("Use `$kindof(#value)`.")
{
	return $kindof(#value);
}

macro bool @typeis(#value, $Type) @const @builtin @deprecated("Use `$typeof(#value) == $Type` instead.")
{
	return $typeof(#value).typeid == $Type.typeid;
}

fn int panic_print_backtrace_count(BacktraceList backtrace, int backtraces_to_ignore)
{
	bool inside_builtin_panic_skipped = true;
	for (int i = backtraces_to_ignore; i < backtrace.len(); i++) {
		switch (backtrace[i].function) {
			case "std.core.builtin.print_backtrace":
			case "std.core.builtin.default_panic":
			case "std.core.builtin.panicf":
				if (inside_builtin_panic_skipped) backtraces_to_ignore++;
			case "__GI___sigaction":
				backtraces_to_ignore = i + 1;
			default:
				inside_builtin_panic_skipped = false;
				break;
		}
	}
	return backtraces_to_ignore;
}

fn bool print_backtrace(String message, int backtraces_to_ignore, void *added_backtrace = null) @if (env::NATIVE_STACKTRACE) => @stack_mem(0x1100; Allocator smem)
{
	void*[256] buffer;
	void*[] backtraces = backtrace::capture_current(&buffer);
	if (added_backtrace)
	{
		backtraces[++backtraces_to_ignore] = added_backtrace;
	}
	@stack_mem(4096; Allocator mem)
	{
		BacktraceList? backtrace = backtrace::symbolize_backtrace(mem, backtraces);
		if (catch backtrace) return false;

		// Update ignore-count to also ignore the first traces from the panic functions
		backtraces_to_ignore = panic_print_backtrace_count(backtrace, backtraces_to_ignore);
		if (backtrace.len() <= backtraces_to_ignore) return false;

		io::eprint("\nERROR: '");
		io::eprint(message);
		io::eprintn("'");

		usz repetition_count = 0;
		foreach (i, &trace : backtrace)
		{
			if (i < backtraces_to_ignore) continue;
			if (trace.has_file() && trace.file.ends_with("/lib/std/core/private/main_stub.c3")) {
				break;
			}

			// Repeating backtraces (f.e. for max recursion depth) pretty print
			if (i > 0 && *trace == backtrace[i-1]) {
				if (i + 1 < backtrace.len() && *trace == backtrace[i+1]) {
					repetition_count += 1;
					continue;
				} else if (repetition_count > 0) {
					if (trace.is_unknown()) {
						io::eprintfn("  ... (%s repetitions of ???)", repetition_count, trace.function);
					} else {
						io::eprintfn("  ... (%s repetitions of %s)", repetition_count, trace.function);
					}
				}
			} else {
				repetition_count = 0;
			}

			String inline_suffix = trace.is_inline ? " [inline]" : "";
			switch
			{
				case trace.is_unknown():
					io::eprintfn("  in ???%s", inline_suffix);
				case trace.has_file():
					io::eprintfn("  in %s (%s:%d) [%s]%s", trace.function, trace.file, trace.line, trace.object_file, inline_suffix);
				default:
					io::eprintfn("  in %s (source unavailable) [%s]%s", trace.function, trace.object_file, inline_suffix);
			}
		}
	};
	return true;
}

fn void default_panic(String message, String file, String function, uint line) @if(env::NATIVE_STACKTRACE)
{
	in_panic = true;
	$if $defined(io::stderr) && env::PANIC_MSG:
		if (!print_backtrace(message, 2))
		{
			io::eprintfn("\nERROR: '%s', in %s (%s:%d)", message, function, file, line);
		}
	$endif
	$$trap();

}

macro void abort(String string = "Unrecoverable error reached", ...) @format(0) @builtin @noreturn
{
	panicf(string, $$FILE, $$FUNC, $$LINE, $vasplat);
	$$trap();
}

bool in_panic @private = false;

fn void default_panic(String message, String file, String function, uint line) @if (!env::NATIVE_STACKTRACE)
{
	$if $defined(io::stderr) && env::PANIC_MSG:
		if (in_panic)
		{
			io::eprintn("Panic inside of panic.");
			return;
		}
		in_panic = true;
		$if $defined(io::stderr):
		io::eprint("\nERROR: '");
		io::eprint(message);
		io::eprintfn("', in %s (%s:%d)", function, file, line);
		$endif
		in_panic = false;
	$endif
	$$trap();
}

alias PanicFn = fn void(String message, String file, String function, uint line);

PanicFn panic = &default_panic;

fn void panicf(String fmt, String file, String function, uint line, args...)
{
	$if $defined(io::stderr) && env::PANIC_MSG:
		if (in_panic)
		{
			io::eprint("Panic inside of panic: ");
			io::eprintn(fmt);
			return;
		}
		in_panic = true;
		@stack_mem(512; Allocator allocator)
		{
			DString s;
			s.init(allocator);
			s.appendf(fmt, ...args);
			in_panic = false;
			panic(s.str_view(), file, function, line);
		};
	$endif
}

<*
 Marks the path as unreachable. This will panic in safe mode, and in fast will simply be assumed
 never happens.
 @param [in] string : "The panic message or format string"
*>
macro void unreachable(String string = "Unreachable statement reached.", ...) @builtin @noreturn
{
	$if env::COMPILER_SAFE_MODE:
	panicf(string, $$FILE, $$FUNC, $$LINE, $vasplat);
	$else
	$$unreachable();
	$endif
}

<*
 Marks the path as unsupported, this is similar to unreachable.
 @param [in] string : "The error message"
*>
macro void unsupported(String string = "Unsupported function invoked") @builtin @noreturn
{
	panicf(string, $$FILE, $$FUNC, $$LINE, $vasplat);
	$$unreachable();
}

<*
 Unconditionally break into an attached debugger when reached.
*>
macro void breakpoint() @builtin
{
	$$breakpoint();
}

macro any_make(void* ptr, typeid type) @builtin
{
	return $$any_make(ptr, type);
}

macro any.retype_to(&self, typeid type)
{
	return $$any_make(self.ptr, type);
}

<*
 @require (bool)self.type : "A typed any was expected"
*>
macro any.as_inner(&self)
{
	return $$any_make(self.ptr, self.type.inner);
}

<*
 @param expr : "the expression to cast"
 @param $Type : "the type to cast to"

 @require $sizeof(expr) == $Type.sizeof  : "Cannot bitcast between types of different size."
 @ensure $typeof(return) == $Type*
*>
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

<*
 @param $Type : `The type of the enum`
 @param [in] enum_name : `The name of the enum to search for`
 @require $Type.kindof == ENUM : `Only enums may be used`
 @ensure $typeof(return) == $Type*
 @return? NOT_FOUND
*>
macro enum_by_name($Type, String enum_name) @builtin
{
	typeid x = $Type.typeid;
	foreach (i, name : x.names)
	{
		if (name == enum_name) return $Type.from_ordinal(i);
	}
	return NOT_FOUND~;
}

<*
 @param $Type : `The type of the enum`
 @require $Type.kindof == ENUM : `Only enums may be used`
 @require $defined($Type.#value) : `Expected '#value' to match an enum associated value`
 @require $defined($typeof(($Type){}.#value) v = value) : `Expected the value to match the type of the associated value`
 @ensure $typeof(return) == $Type*
 @return? NOT_FOUND
*>
macro @enum_from_value($Type, #value, value) @builtin @deprecated("Use Enum.lookup_field and Enum.lookup")
{
	foreach (e : $Type.values)
	{
		if (e.#value == value) return e;
	}
	return NOT_FOUND~;
}

<*
 Mark an expression as likely to be true

 @param #value : "expression to be marked likely"
 @param $probability : "in the range 0 - 1"
 @require $probability >= 0 && $probability <= 1.0
*>
macro bool @likely(bool #value, $probability = 1.0) @builtin
{
	$switch:
	$case env::BUILTIN_EXPECT_IS_DISABLED:
	return #value;
	$case $probability == 1.0:
	return $$expect(#value, true);
	$default:
	return $$expect_with_probability(#value, true, $probability);
	$endswitch
}

<*
 Mark an expression as unlikely to be true

 @param #value : "expression to be marked unlikely"
 @param $probability : "in the range 0 - 1"
 @require $probability >= 0 && $probability <= 1.0
*>
macro bool @unlikely(bool #value, $probability = 1.0) @builtin
{
	$switch:
	$case env::BUILTIN_EXPECT_IS_DISABLED:
	return #value;
	$case $probability == 1.0:
	return $$expect(#value, false);
	$default:
	return $$expect_with_probability(#value, false, $probability);
	$endswitch
}

<*
 @require values::@is_int(#value) || values::@is_bool(#value)
 @require $defined($typeof(#value) v = expected)
 @require $probability >= 0 && $probability <= 1.0
*>
macro @expect(#value, expected, $probability = 1.0) @builtin
{
	$switch:
	$case env::BUILTIN_EXPECT_IS_DISABLED:
	return #value == expected;
	$case $probability == 1.0:
	return $$expect(#value, ($typeof(#value))expected);
	$default:
	return $$expect_with_probability(#value, expected, $probability);
	$endswitch
}

<*
 Locality for prefetch, levels 0 - 3, corresponding
 to "extremely local" to "no locality"
*>
enum PrefetchLocality
{
	NO_LOCALITY,
	FAR,
	NEAR,
	VERY_NEAR,
}

<*
 Prefetch a pointer.

 @param [in] ptr : `Pointer to prefetch`
 @param $locality : `Locality ranging from none to extremely local`
 @param $write : `Prefetch for write, otherwise prefetch for read.`
*>
macro @prefetch(void* ptr, PrefetchLocality $locality = VERY_NEAR, bool $write = false) @builtin
{
	$if !env::BUILTIN_PREFETCH_IS_DISABLED:
		$$prefetch(ptr, $write ? 1 : 0, $locality.ordinal);
	$endif
}

<*
 Shuffle a vector by its index

 int[<4>] a = { 1, 2, 3, 4 };
 assert(swizzle(a, 0, 1, 1, 3) == (int[<4>]) { 1, 2, 2, 4 });
*>
macro swizzle(v, ...) @builtin
{
	return $$swizzle(v, $vasplat);
}


<*
 Shuffle two vectors by a common index from arranging the vectors sequentially in memory

 int[<4>] a = { 1, 2, 3, 4 };
 int[<4>] b = { 100, 1000, 10000, 100000 };
 assert(swizzle2(a, b, 0, 1, 4, 6, 2) == (int[<5>]) { 1, 2, 100, 10000, 3 });
*>
macro swizzle2(v, v2, ...) @builtin
{
	return $$swizzle2(v, v2, $vasplat);
}


<*
 Returns the count of leading zero bits from an integer at compile-time.

 @require types::is_int($typeof($value)) : "Input value must be an integer"
 @require $sizeof($value) * 8 <= 128 : "Input value must be 128 bits wide or lower"
*>
macro uint @clz($value) @builtin @const
{
	$if $value == 0:
		return $sizeof($value) * 8;   // it's all leading zeroes
	$endif

	usz $n = 0;
	uint128 $x = (uint128)$value;

	$if $x <= 0x0000_0000_0000_0000_FFFF_FFFF_FFFF_FFFF: $n += 64; $x <<= 64; $endif
	$if $x <= 0x0000_0000_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF: $n += 32; $x <<= 32; $endif
	$if $x <= 0x0000_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF: $n += 16; $x <<= 16; $endif
	$if $x <= 0x00FF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF: $n += 8;  $x <<= 8;  $endif
	$if $x <= 0x0FFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF: $n += 4;  $x <<= 4;  $endif
	$if $x <= 0x3FFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF: $n += 2;  $x <<= 2;  $endif
	$if $x <= 0x7FFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF: $n += 1;             $endif

	return $n % ($sizeof($value) * 8);   // mod by the bitsize of the input value to go back from uint128 -> it's-type
}

<*
 Return the excuse in the Optional if it is Empty, otherwise
 return a null fault.

 @require $kindof(#expr) == OPTIONAL : `@catch expects an Optional value`
*>
macro fault @catch(#expr) @builtin
{
	if (catch f = #expr) return f;
	return {};
}

<*
 Check if an Optional expression holds a value or is empty, returning true
 if it has a value.

 @require $kindof(#expr) == OPTIONAL : `@ok expects an Optional value`
*>
macro bool @ok(#expr) @builtin
{
	if (catch #expr) return false;
	return true;
}

<*
 Check if an Optional expression evaluates to a fault. If so, return it;
 else, assign the result to an expression.

 @require $defined(#v = #v) : "#v must be a variable"
 @require $defined(#expr!) : "Expected an optional expression"
 @require $defined(#v = #expr!!)  : `Type of #expr must be an optional of #v's type`
*>
macro void? @try(#v, #expr) @builtin @maydiscard
{
	var res = #expr;
	if (catch err = res) return err~;
	#v = res;
}

<*
 Check if an Optional expression evaluates to a fault. If so, return true if it is the
 expected fault, the optional if it is unexpected, or false if there was no fault and
 the assign happened.

 This can be used in like this:

  while (true)
  {
	char[] data;
	// Read until end of file
	if (@try_catch(data, load_line(), io::EOF)!) break;
	.. use data ..
  }

 In this example we read until we reach an EOF, which is expected. However, if we encounter some other
 fault, we rethrow is. Without this macro, the code is instead written like:

  while (true)
  {
	char[]? data;
	data = load_line();
	if (catch err = data)
	{
	  if (err = io::EOF) break;
	  return err?
	}
	.. use data ..
  }

 @require $defined(#v = #v) : "#v must be a variable"
 @require $defined(#expr!) : "Expected an optional expression"
 @require $defined(#v = #expr!!)  : `Type of #expr must be an optional of #v's type`
 @return "True if it was the expected fault, false if the variable was assigned, otherwise returns an optional."
*>
macro bool? @try_catch(#v, #expr, fault expected_fault) @builtin
{
	var res = #expr;
	if (catch err = res)
	{
		return err == expected_fault ? true : err~;
	}
	#v = res;
	return false;
}

<*
 @require $defined(&#value, (char*)&#value) : "This must be a value that can be viewed as a char array"
 *>
macro char[] @as_char_view(#value) @builtin
{
	return ((char*)&#value)[:$sizeof(#value)];
}

macro isz @str_find(String $string, String $needle) @builtin => $$str_find($string, $needle);
macro String @str_upper(String $str) @builtin => $$str_upper($str);
macro String @str_lower(String $str) @builtin => $$str_lower($str);
macro uint @str_hash(String $str) @builtin => $$str_hash($str);
macro String @str_pascalcase(String $str) @builtin => $$str_pascalcase($str);
macro String @str_snakecase(String $str) @builtin => $$str_snakecase($str);
macro String @str_camelcase(String $str) @builtin => @str_capitalize($$str_pascalcase($str));
macro String @str_constantcase(String $str) @builtin => @str_upper($$str_snakecase($str));
macro String @str_replace(String $str, String $pattern, String $replace, uint $limit = 0) @builtin => $$str_replace($str, $pattern, $replace, $limit);
macro String @str_capitalize(String $str) @builtin
{
	$switch $str.len:
		$case 0: return $str;
		$case 1: return $$str_upper($str);
		$default: return $$str_upper($str[0:1]) +++ $str[1..];
	$endswitch
}
macro String @str_uncapitalize(String $str) @builtin
{
	$switch $str.len:
		$case 0: return $str;
		$case 1: return $$str_lower($str);
		$default: return $$str_lower($str[0:1]) +++ $str[1..];
	$endswitch
}

macro @generic_hash_core(h, value)
{
	h ^= (uint)value; // insert lowest 32 bits
	h *= 0x96f59e5b; // diffuse them up
	h ^= h >> 16; // diffuse them down
	return h;
}

macro uint @generic_hash(value)
{
	uint h = @generic_hash_core((uint)0x3efd4391, value);
	$for var $cnt = 4; $cnt < $sizeof(value); $cnt += 4:
		value >>= 32; // reduce value
		h = @generic_hash_core(h, value);
	$endfor
	return h;
}


macro uint int128.hash(self)       => @generic_hash(self);
macro uint uint128.hash(self)      => @generic_hash(self);
macro uint long.hash(self)         => @generic_hash(self);
macro uint ulong.hash(self)        => @generic_hash(self);
macro uint int.hash(self)          => @generic_hash(self);
macro uint uint.hash(self)         => @generic_hash(self);
macro uint short.hash(self)        => @generic_hash(self);
macro uint ushort.hash(self)       => @generic_hash(self);
macro uint ichar.hash(self)        => @generic_hash(self);
macro uint char.hash(self)         => @generic_hash(self);
macro uint bool.hash(self)         => @generic_hash(self);

macro uint int128[*].hash(&self)   => hash_array(self);
macro uint uint128[*].hash(&self)  => hash_array(self);
macro uint long[*].hash(&self)     => hash_array(self);
macro uint ulong[*].hash(&self)    => hash_array(self);
macro uint int[*].hash(&self)      => hash_array(self);
macro uint uint[*].hash(&self)     => hash_array(self);
macro uint short[*].hash(&self)    => hash_array(self);
macro uint ushort[*].hash(&self)   => hash_array(self);
macro uint char[*].hash(&self)     => hash_array(self);
macro uint ichar[*].hash(&self)    => hash_array(self);
macro uint bool[*].hash(&self)     => hash_array(self);

macro uint int128[<*>].hash(self)  => hash_vec(self);
macro uint uint128[<*>].hash(self) => hash_vec(self);
macro uint long[<*>].hash(self)    => hash_vec(self);
macro uint ulong[<*>].hash(self)   => hash_vec(self);
macro uint int[<*>].hash(self)     => hash_vec(self);
macro uint uint[<*>].hash(self)    => hash_vec(self);
macro uint short[<*>].hash(self)   => hash_vec(self);
macro uint ushort[<*>].hash(self)  => hash_vec(self);
macro uint char[<*>].hash(self)    => hash_vec(self);
macro uint ichar[<*>].hash(self)   => hash_vec(self);
macro uint bool[<*>].hash(self)    => hash_vec(self);

macro uint typeid.hash(typeid t) => @generic_hash(((ulong)(uptr)t));
macro uint String.hash(String c) => (uint)a5hash::hash(c);
macro uint char[].hash(char[] c) => (uint)a5hash::hash(c);
macro uint ZString.hash(ZString c) => (uint)a5hash::hash(c.str_view());
macro uint void*.hash(void* ptr) => @generic_hash(((ulong)(uptr)ptr));

<*
 @require $kindof(array_ptr) == POINTER &&& $kindof(*array_ptr) == ARRAY
*>
macro uint hash_array(array_ptr) @local
{
	var $len = $sizeof(*array_ptr);

	$if $len > 16:
		return (uint)komi::hash(((char*)array_ptr)[:$len]);
	$else
		return (uint)wyhash2::hash(((char*)array_ptr)[:$len]);
	$endif
}

<*
 @require $kindof(vec) == VECTOR
*>
macro uint hash_vec(vec) @local
{
	var $len = $sizeof(vec);

	$if $len > 16:
		return (uint)komi::hash(((char*)&&vec)[:$len]);
	$else
		return (uint)wyhash2::hash(((char*)&&vec)[:$len]);
	$endif
}

const MAX_FRAMEADDRESS = 128;
<* @require n >= 0 *>
macro void* get_frameaddress(int n, $min = 0, $max = MAX_FRAMEADDRESS)
{
	$if $min == 0 && $max == MAX_FRAMEADDRESS:
		if (n > MAX_FRAMEADDRESS) return null;
	$endif

	$if $min == $max:
		return $$frameaddress($min);
	$else
		var $mid = ($min + $max) / 2;
		if (n <= $mid)
		{
			return get_frameaddress(n, $min, $mid);
		}
		else
		{
			return get_frameaddress(n, $mid + 1, $max);
		}
	$endif
}

<* @require n >= 0 *>
macro void* get_returnaddress(int n, $min = 0, $max = MAX_FRAMEADDRESS)
{
	$if $min == 0 && $max == MAX_FRAMEADDRESS:
		if (n > MAX_FRAMEADDRESS) return null;
	$endif

	$if $min == $max:
		return $$returnaddress($min);
	$else
		var $mid = ($min + $max) / 2;
		if (n <= $mid)
		{
			return get_returnaddress(n, $min, $mid);
		}
		else
		{
			return get_returnaddress(n, $mid + 1, $max);
		}
	$endif
}

fn String fault.short_name(self)
{
	String name = self.nameof;
	return name[name.index_of("::") + 2..]!!;
}


macro String fault.@short_name($self) @const
{
	var $name = $self.nameof;
	return $name[@str_find($name, "::") + 2..];
}

module std::core::builtin @if((env::LINUX || env::ANDROID || env::DARWIN) && env::COMPILER_SAFE_MODE && env::DEBUG_SYMBOLS);
import libc, std::io, std::os::posix;

fn void sig_panic(String message)
{
	default_panic(message, "???", "???", 0);
}

fn void sig_bus_error(CInt i, void* info, void* context)
{
	$if !env::NATIVE_STACKTRACE:
		sig_panic("Illegal memory access.");
	$else
		$if $defined(io::stderr):
			if (!print_backtrace("Illegal memory access.", 2, posix::stack_instruction(context)))
			{
				io::eprintn("\nERROR: 'Illegal memory access'.");
			}
		$endif
	$endif
	os::fastexit(128 + i);
}

fn void sig_segmentation_fault(CInt i, void* p1, void* context)
{
	$if !env::NATIVE_STACKTRACE:
		sig_panic("Out of bounds memory access.");
	$else
		$if $defined(io::stderr):
			if (!print_backtrace("Out of bounds memory access.", 2, posix::stack_instruction(context)))
			{
				io::eprintn("\nERROR: Memory error without backtrace, possible stack overflow.");
			}
		$endif
	$endif
	os::fastexit(128 + i);
}

fn void sig_illegal_instruction(CInt i, void* p1, void* context)
{
	if (in_panic) os::fastexit(128 + i);
	$if !env::NATIVE_STACKTRACE:
		sig_panic("Illegal instruction.");
	$else
		$if $defined(io::stderr):
			if (!print_backtrace("Illegal instruction.", 2, posix::stack_instruction(context)))
			{
				io::eprintn("\nERROR: Illegal instruction.");
			}
		$endif
	$endif
	os::fastexit(128 + i);
}

char[64 * 1024] sig_stack @local @if(env::BACKTRACE && env::LINUX);

// Clean this up
fn void install_signal_handlers() @init(101) @local @if(env::BACKTRACE)
{
	$if env::LINUX:
		Stack_t ss = {
			.ss_sp = &sig_stack,
			.ss_size = sig_stack.len
		};
		libc::sigaltstack(&ss, null);
	$endif

	posix::install_signal_handler(libc::SIGBUS, &sig_bus_error);
	posix::install_signal_handler(libc::SIGSEGV, &sig_segmentation_fault);
	posix::install_signal_handler(libc::SIGILL,  &sig_illegal_instruction);
}
