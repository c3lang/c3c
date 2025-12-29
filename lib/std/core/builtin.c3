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
const EmptySlot EMPTY_MACRO_SLOT @builtin = null;

typedef EmptySlot = void*;
macro bool @is_empty_macro_slot(#arg) @const @builtin => $typeof(#arg) == EmptySlot;
macro bool @is_valid_macro_slot(#arg) @const @builtin => $typeof(#arg) != EmptySlot;

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
	if (v.type != $Type.typeid) return TYPE_MISMATCH?;
	return ($Type*)v.ptr;
}

<*
  @return "The value in the pointer"
  @return? TYPE_MISMATCH
*>
macro any.to(self, $Type)
{
	if (self.type != $Type.typeid) return TYPE_MISMATCH?;
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


fn bool print_backtrace(String message, int backtraces_to_ignore, void *added_backtrace = null) @if (env::NATIVE_STACKTRACE) => @stack_mem(0x1100; Allocator smem)
{
	void*[256] buffer;
	void*[] backtraces = backtrace::capture_current(&buffer);
	if (added_backtrace)
	{
		backtraces[++backtraces_to_ignore] = added_backtrace;
	}
	@stack_mem(2048; Allocator mem)
	{
		BacktraceList? backtrace = backtrace::symbolize_backtrace(mem, backtraces);
		if (catch backtrace) return false;
		if (backtrace.len() <= backtraces_to_ignore) return false;
		io::eprint("\nERROR: '");
		io::eprint(message);
		io::eprintn("'");
		foreach (i, &trace : backtrace)
		{
			if (i < backtraces_to_ignore) continue;
			String inline_suffix = trace.is_inline ? " [inline]" : "";
			if (trace.is_unknown())
			{
				io::eprintfn("  in ???%s", inline_suffix);
				continue;
			}
			if (trace.has_file())
			{
				io::eprintfn("  in %s (%s:%d) [%s]%s", trace.function, trace.file, trace.line, trace.object_file, inline_suffix);
				continue;
			}
			io::eprintfn("  in %s (source unavailable) [%s]%s", trace.function, trace.object_file, inline_suffix);
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
	return NOT_FOUND?;
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
	return NOT_FOUND?;
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
	if (catch err = res) return err?;
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
		return err == expected_fault ? true : err?;
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
	var $len = $sizeof(vec.len * $typeof(vec).inner.sizeof);

	$if $len > 16:
		return (uint)komi::hash(((char*)&&vec)[:$len]);
	$else
		return (uint)wyhash2::hash(((char*)&&vec)[:$len]);
	$endif
}

const MAX_FRAMEADDRESS = 128;
<*
 @require n >= 0
*>
macro void* get_frameaddress(int n)
{
	if (n > MAX_FRAMEADDRESS) return null;
	switch (n)
	{
		case 0: return $$frameaddress(0);
		case 1: return $$frameaddress(1);
		case 2: return $$frameaddress(2);
		case 3: return $$frameaddress(3);
		case 4: return $$frameaddress(4);
		case 5: return $$frameaddress(5);
		case 6: return $$frameaddress(6);
		case 7: return $$frameaddress(7);
		case 8: return $$frameaddress(8);
		case 9: return $$frameaddress(9);
		case 10: return $$frameaddress(10);
		case 11: return $$frameaddress(11);
		case 12: return $$frameaddress(12);
		case 13: return $$frameaddress(13);
		case 14: return $$frameaddress(14);
		case 15: return $$frameaddress(15);
		case 16: return $$frameaddress(16);
		case 17: return $$frameaddress(17);
		case 18: return $$frameaddress(18);
		case 19: return $$frameaddress(19);
		case 20: return $$frameaddress(20);
		case 21: return $$frameaddress(21);
		case 22: return $$frameaddress(22);
		case 23: return $$frameaddress(23);
		case 24: return $$frameaddress(24);
		case 25: return $$frameaddress(25);
		case 26: return $$frameaddress(26);
		case 27: return $$frameaddress(27);
		case 28: return $$frameaddress(28);
		case 29: return $$frameaddress(29);
		case 30: return $$frameaddress(30);
		case 31: return $$frameaddress(31);
		case 32: return $$frameaddress(32);
		case 33: return $$frameaddress(33);
		case 34: return $$frameaddress(34);
		case 35: return $$frameaddress(35);
		case 36: return $$frameaddress(36);
		case 37: return $$frameaddress(37);
		case 38: return $$frameaddress(38);
		case 39: return $$frameaddress(39);
		case 40: return $$frameaddress(40);
		case 41: return $$frameaddress(41);
		case 42: return $$frameaddress(42);
		case 43: return $$frameaddress(43);
		case 44: return $$frameaddress(44);
		case 45: return $$frameaddress(45);
		case 46: return $$frameaddress(46);
		case 47: return $$frameaddress(47);
		case 48: return $$frameaddress(48);
		case 49: return $$frameaddress(49);
		case 50: return $$frameaddress(50);
		case 51: return $$frameaddress(51);
		case 52: return $$frameaddress(52);
		case 53: return $$frameaddress(53);
		case 54: return $$frameaddress(54);
		case 55: return $$frameaddress(55);
		case 56: return $$frameaddress(56);
		case 57: return $$frameaddress(57);
		case 58: return $$frameaddress(58);
		case 59: return $$frameaddress(59);
		case 60: return $$frameaddress(60);
		case 61: return $$frameaddress(61);
		case 62: return $$frameaddress(62);
		case 63: return $$frameaddress(63);
		case 64: return $$frameaddress(64);
		case 65: return $$frameaddress(65);
		case 66: return $$frameaddress(66);
		case 67: return $$frameaddress(67);
		case 68: return $$frameaddress(68);
		case 69: return $$frameaddress(69);
		case 70: return $$frameaddress(70);
		case 71: return $$frameaddress(71);
		case 72: return $$frameaddress(72);
		case 73: return $$frameaddress(73);
		case 74: return $$frameaddress(74);
		case 75: return $$frameaddress(75);
		case 76: return $$frameaddress(76);
		case 77: return $$frameaddress(77);
		case 78: return $$frameaddress(78);
		case 79: return $$frameaddress(79);
		case 80: return $$frameaddress(80);
		case 81: return $$frameaddress(81);
		case 82: return $$frameaddress(82);
		case 83: return $$frameaddress(83);
		case 84: return $$frameaddress(84);
		case 85: return $$frameaddress(85);
		case 86: return $$frameaddress(86);
		case 87: return $$frameaddress(87);
		case 88: return $$frameaddress(88);
		case 89: return $$frameaddress(89);
		case 90: return $$frameaddress(90);
		case 91: return $$frameaddress(91);
		case 92: return $$frameaddress(92);
		case 93: return $$frameaddress(93);
		case 94: return $$frameaddress(94);
		case 95: return $$frameaddress(95);
		case 96: return $$frameaddress(96);
		case 97: return $$frameaddress(97);
		case 98: return $$frameaddress(98);
		case 99: return $$frameaddress(99);
		case 100: return $$frameaddress(100);
		case 101: return $$frameaddress(101);
		case 102: return $$frameaddress(102);
		case 103: return $$frameaddress(103);
		case 104: return $$frameaddress(104);
		case 105: return $$frameaddress(105);
		case 106: return $$frameaddress(106);
		case 107: return $$frameaddress(107);
		case 108: return $$frameaddress(108);
		case 109: return $$frameaddress(109);
		case 110: return $$frameaddress(110);
		case 111: return $$frameaddress(111);
		case 112: return $$frameaddress(112);
		case 113: return $$frameaddress(113);
		case 114: return $$frameaddress(114);
		case 115: return $$frameaddress(115);
		case 116: return $$frameaddress(116);
		case 117: return $$frameaddress(117);
		case 118: return $$frameaddress(118);
		case 119: return $$frameaddress(119);
		case 120: return $$frameaddress(120);
		case 121: return $$frameaddress(121);
		case 122: return $$frameaddress(122);
		case 123: return $$frameaddress(123);
		case 124: return $$frameaddress(124);
		case 125: return $$frameaddress(125);
		case 126: return $$frameaddress(126);
		case 127: return $$frameaddress(127);
		case 128: return $$frameaddress(128);
		default: unreachable();
	}
}

<*
 @require n >= 0
*>
macro void* get_returnaddress(int n)
{
	if (n > MAX_FRAMEADDRESS) return null;
	switch (n)
	{
		case 0: return $$returnaddress(0);
		case 1: return $$returnaddress(1);
		case 2: return $$returnaddress(2);
		case 3: return $$returnaddress(3);
		case 4: return $$returnaddress(4);
		case 5: return $$returnaddress(5);
		case 6: return $$returnaddress(6);
		case 7: return $$returnaddress(7);
		case 8: return $$returnaddress(8);
		case 9: return $$returnaddress(9);
		case 10: return $$returnaddress(10);
		case 11: return $$returnaddress(11);
		case 12: return $$returnaddress(12);
		case 13: return $$returnaddress(13);
		case 14: return $$returnaddress(14);
		case 15: return $$returnaddress(15);
		case 16: return $$returnaddress(16);
		case 17: return $$returnaddress(17);
		case 18: return $$returnaddress(18);
		case 19: return $$returnaddress(19);
		case 20: return $$returnaddress(20);
		case 21: return $$returnaddress(21);
		case 22: return $$returnaddress(22);
		case 23: return $$returnaddress(23);
		case 24: return $$returnaddress(24);
		case 25: return $$returnaddress(25);
		case 26: return $$returnaddress(26);
		case 27: return $$returnaddress(27);
		case 28: return $$returnaddress(28);
		case 29: return $$returnaddress(29);
		case 30: return $$returnaddress(30);
		case 31: return $$returnaddress(31);
		case 32: return $$returnaddress(32);
		case 33: return $$returnaddress(33);
		case 34: return $$returnaddress(34);
		case 35: return $$returnaddress(35);
		case 36: return $$returnaddress(36);
		case 37: return $$returnaddress(37);
		case 38: return $$returnaddress(38);
		case 39: return $$returnaddress(39);
		case 40: return $$returnaddress(40);
		case 41: return $$returnaddress(41);
		case 42: return $$returnaddress(42);
		case 43: return $$returnaddress(43);
		case 44: return $$returnaddress(44);
		case 45: return $$returnaddress(45);
		case 46: return $$returnaddress(46);
		case 47: return $$returnaddress(47);
		case 48: return $$returnaddress(48);
		case 49: return $$returnaddress(49);
		case 50: return $$returnaddress(50);
		case 51: return $$returnaddress(51);
		case 52: return $$returnaddress(52);
		case 53: return $$returnaddress(53);
		case 54: return $$returnaddress(54);
		case 55: return $$returnaddress(55);
		case 56: return $$returnaddress(56);
		case 57: return $$returnaddress(57);
		case 58: return $$returnaddress(58);
		case 59: return $$returnaddress(59);
		case 60: return $$returnaddress(60);
		case 61: return $$returnaddress(61);
		case 62: return $$returnaddress(62);
		case 63: return $$returnaddress(63);
		case 64: return $$returnaddress(64);
		case 65: return $$returnaddress(65);
		case 66: return $$returnaddress(66);
		case 67: return $$returnaddress(67);
		case 68: return $$returnaddress(68);
		case 69: return $$returnaddress(69);
		case 70: return $$returnaddress(70);
		case 71: return $$returnaddress(71);
		case 72: return $$returnaddress(72);
		case 73: return $$returnaddress(73);
		case 74: return $$returnaddress(74);
		case 75: return $$returnaddress(75);
		case 76: return $$returnaddress(76);
		case 77: return $$returnaddress(77);
		case 78: return $$returnaddress(78);
		case 79: return $$returnaddress(79);
		case 80: return $$returnaddress(80);
		case 81: return $$returnaddress(81);
		case 82: return $$returnaddress(82);
		case 83: return $$returnaddress(83);
		case 84: return $$returnaddress(84);
		case 85: return $$returnaddress(85);
		case 86: return $$returnaddress(86);
		case 87: return $$returnaddress(87);
		case 88: return $$returnaddress(88);
		case 89: return $$returnaddress(89);
		case 90: return $$returnaddress(90);
		case 91: return $$returnaddress(91);
		case 92: return $$returnaddress(92);
		case 93: return $$returnaddress(93);
		case 94: return $$returnaddress(94);
		case 95: return $$returnaddress(95);
		case 96: return $$returnaddress(96);
		case 97: return $$returnaddress(97);
		case 98: return $$returnaddress(98);
		case 99: return $$returnaddress(99);
		case 100: return $$returnaddress(100);
		case 101: return $$returnaddress(101);
		case 102: return $$returnaddress(102);
		case 103: return $$returnaddress(103);
		case 104: return $$returnaddress(104);
		case 105: return $$returnaddress(105);
		case 106: return $$returnaddress(106);
		case 107: return $$returnaddress(107);
		case 108: return $$returnaddress(108);
		case 109: return $$returnaddress(109);
		case 110: return $$returnaddress(110);
		case 111: return $$returnaddress(111);
		case 112: return $$returnaddress(112);
		case 113: return $$returnaddress(113);
		case 114: return $$returnaddress(114);
		case 115: return $$returnaddress(115);
		case 116: return $$returnaddress(116);
		case 117: return $$returnaddress(117);
		case 118: return $$returnaddress(118);
		case 119: return $$returnaddress(119);
		case 120: return $$returnaddress(120);
		case 121: return $$returnaddress(121);
		case 122: return $$returnaddress(122);
		case 123: return $$returnaddress(123);
		case 124: return $$returnaddress(124);
		case 125: return $$returnaddress(125);
		case 126: return $$returnaddress(126);
		case 127: return $$returnaddress(127);
		case 128: return $$returnaddress(128);
		default: unreachable();
	}
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
