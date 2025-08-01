# C3C Release Notes

## 0.7.5 Change list

### Changes / improvements
- Support `alias foo = module std::io` module aliasing.

### Fixes
### Stdlib changes

## 0.7.4 Change list

### Changes / improvements
- Added const enums: `enum Foo : const`. Behaves like C enums but may be any type.
- Casting to / from an enum is now possible again. No need to use `.ordinal` and `.from_ordinal`.
- Inline associated enum values are deprecated, use `--use-old-enums` to re-enable them.
- `$typeof` may return a compile time type.
- Improved error messages on missing qualifier on enum value. #2260
- Add `--echo-prefix` to edit the prefix with `$echo` statements. Supports {FILE} and {LINE}
- Catch accidental `foo == BAR;` where `foo = BAR;` was most likely intended. #2274
- Improve error message when doing a rethrow in a function that doesn't return an optional.
- Add `--list-asm` to view all supported `asm` instructions.
- Formatting option "%h" now supports pointers.
- Improve error on unsigned implicit conversion to signed.
- Update error message for struct initialization #2286
- Add SipHash family of keyed PRFs. #2287 
- `$is_const` is deprecated in favour of `@is_const` based on `$defined`.
- Multiline contract comments #2113
- Removed the use of temp allocator in backtrace printing.
- `env::AUTHORS` and `env::AUTHOR_EMAILS` added.
- Suppress codegen of panic printing with when panic messages are set to "off".
- Implicit linking of libc math when libc math functions are used.
- Allow even smaller memory limits.
- Check unaligned array access.
- Add "@structlike" for typedefs.
- "poison" the current function early when a declaration can't be correctly resolved.
- Add komihash, a5hash, metrohash64, metrohash128, and wyhash2 variants with tests/benchmark. #2293
- '$assignable' is deprecated.
- Deprecate allocator::heap() and allocator::temp()
- Add `thread::fence` providing a thread fence.
- Place output in `out` by default for projects. Use temp folder for building at the command line.
- Allow absolute paths for `$embed`.
- Add `@try` and `@try_catch`.
- Assignment evaluation order now right->left, following C++17 and possibly C23.

### Fixes
- mkdir/rmdir would not work properly with substring paths on non-windows platforms.
- Hex string formatter check incorrectly rejected slices.
- Correctly reject interface methods `type` and `ptr`.
- Comparing a null ZString with a non-null ZString would crash.
- Switch case with const non-int / enum would be treated as ints and crash. #2263
- Missing bounds check on upper bound with const ranges `foo[1:3]`.
- Check up the hierarchy when considering if an interface cast is valid #2267.
- Fix issue with labelled break inside of a $switch.
- Non-const macros may not return untyped lists.
- `$for` ct-state not properly popped.
- Inline `r / complex` for complex numbers fixed.
- Const slice lengths were not always detected as constant.
- Const slice indexing was not bounds checked.
- Initialize pool correctly in print_backtrace.
- `--max-mem` now works correctly again.
- Casting a fault to a pointer would trigger an assert.
- Make `to_float` more tolerant to spaces.
- Fixes to thread local pointer handling.
- Fixes to JSON parsing and Object.
- Array indices are now using int64 internally.
- Bit shift operation fails with inline uint enum despite matching underlying type #2279.
- Fix to codegen when using a bitstruct constant defined using a cast with an operator #2248.
- Function pointers are now compile time constants. 
- Splat 8 arguments can sometimes cause incorrect behaviour in the compiler. #2283
- Correctly poison the analysis after a failed $assert or $error. #2284
- `$foo` variables could be assigned non-compile time values.
- `$foo[0] = ...` was incorrectly requiring that the assigned values were compile time constants.
- "Inlined at" would sometimes show the current location.
- Fixed bug splatting constants into constants.
- Resize bug when resizing memory down in ArenaAllocator, DynamicArenaAllocator, BackedArenaAllocator.
- Error message for missing arg incorrect for methods with zero args #2296.
- Fix stringify of $vaexpr #2301.
- Segfault when failing to cast subexpression to 'isz' in pointer subtraction #2305.
- Fix unexpected display of macro definition when passing a poisoned expression #2305.
- `@links` on macros would not be added to calling functions.
- Fix `Formatter.print` returning incorrect size.
- A distinct type based on an array would yield .len == 0
- Overloading addition with a pointer would not work.
- Copying const enums and regular enums incorrect #2313.
- Regression: Chaining an optional together with contracts could in some cases lose the optional.
- `char[*] b = *(char[*]*)&a;` would crash the compiler if `a` was a slice. #2320
- Implicitly cast const int expressions would sometimes not be detected as compile time const.
- Using @noreturn in a short body macro would not work properly #2326.
- Bug when reporting error in a macro return would crash the compiler #2326.
- Short body return expression would not have the correct span.
- Fix issue where recursively creating a dir would be incorrectly marked as a failure the first time.
- `@format` did not work correctly with macros #2341.
- Crash when parsing recursive type declaration #2345.
- Remove unnecessary "ret" in naked functions #2344.
- Lambdas now properly follow its attributes #2346.
- Not setting android-ndk resulted in a "set ndk-path" error.
- Lambda deduplication would be incorrect when generated at the global scope.
- Disallow accessing parameters in a naked function, as well as `return`, this fixes #1955.
- Assigning string literal to char[<*>] stores pointer rather than characters. #2357

### Stdlib changes
- Improve contract for readline. #2280
- Added Whirlpool hash.
- Added Ed25519.
- Added string::bformat.
- Virtual memory library.
- New virtual emory arena allocator.
- Added `WString.len`.
- Added `@addr` macro.
- Add `ConditionVariable.wait_until` and `ConditionVariable.wait_for`
- Added readline_to_stream that takes a stream.
- Added `Ref` and `RefCounted` experimental functionality.
- Added `Volatile` generic type.
- Added `UnalignedRef` generic type.
- Add String conversion functions snake_case -> PascalCase and vice versa.

## 0.7.3 Change list

### Changes / improvements
- `$typefrom` now also accepts a constant string, and so works like `$evaltype`.
- `$evaltype` is deprecated in favour of `$typefrom`.
- Literal rules have changed, this makes `-0xFF` now a signed integer.
- Implicitly convert from constant typeid to Type in `$Type` assignment, and `$assignable`.
- Make $Type parameters accept constant typeid values.
- Deprecate `foo.#bar`.
- Allow inference across `&&` #2172.
- Added support for custom file extensions in project.json targets.
- `$eval` now also works with `@foo`, `#foo`, `$Foo` and `$foo` parameters #2114.
- `@sprintf` macro (based on the `$$sprintf` builtin) allows compile time format strings #1874.
- Improve error reports when encountering a broken "if-catch".
- Add printf format to `$assert` and `$error` #2183.
- Make accepting arguments for `main` a bit more liberal, accepting `main(int argc, ZString* argv)`
- Make `$echo` and `@sprintf` correctly stringify compile time initializers and slices.
- Add `--sources` build option to add additional files to compile. #2097
- Support untyped second argument for operator overloading.
- The form-feed character '\f' is no longer valid white space.
- Show code that caused unreachable code #2207
- Allow generics over distinct types #2216.
- Support distrinct types as the base type of bitstructs. #2218
- Add hash::sha512 module to stdlib. #2227
- Compile time type assignment (eg `$Foo = int`) is no longer an expression.
- Add `@allow_deprecated` attribute to functions to selectively allow deprecated declarations #2223.
- Improve error message on pointer diff #2239.
- Compile-time comparison of constant vectors. #1575.
- $member.get supports bitstructs.
- $member.set for setting members without the *& trick.
- Initial support for #1925, does not affect C compilation yet, and doesn't try to link etc. Using "--emit-only"

### Fixes
- `-2147483648`, MIN literals work correctly.
- Splatting const slices would not be const. #2185
- Fixes to `$define` handling of binary ops.
- Fixes methodsof to pick up all sorts of extension methods. #2192
- `--lsp` sometimes does not emit end tag #2194.
- Improve Android termux detection.
- Update Android ABI.
- Fixes to `@format` checking #2199.
- Distinct versions of builtin types ignore @operator overloads #2204.
- @operator macro using untyped parameter causes compiler segfault #2200.
- Make `unreachable()` only panic in safe mode.
- `cflags` additions for targets was not handed properly. #2209
- `$echo` would suppress warning about unreachable code. #2205
- Correctly format '%c' when given a width. #2199
- Fix to `is_array_or_slice_of_char` #2214.
- Method on array slice caused segfault #2211.
- In some cases, the compiler would dereference a compile time null. #2215
- Incorrect codegen if a macro ends with unreachable and is assigned to something. #2210
- Fix error for named arguments-order with compile-time arguments #2212
- Bug in AST copying would make operator overloading like `+=` compile incorrectly #2217.
- `$defined(#expr)` broken with binary. #2219 
- Method ambiguity when importing parent module publicly in private submodule. #2208
- Linker errors when shadowing @local with public function #2198
- Bug when offsetting pointers of large structs using ++ and --.
- `x++` and `x--` works on pointer vectors #2222.
- `x += 1` and `x -= 1` works propertly on pointer vectors #2222.
- Fixes to `x += { 1, 1 }` for enum and pointer vectors #2222.
- Linking fails on operator method imported as `@public` #2224.
- Lambda C-style vaargs were not properly rejected, leading to crash #2229.
- Incorrect handling of constant null fault causing compiler crash #2232.
- Overload resolution fixes to inline typedef #2226.
- `math::overflow_*` wrappers incorrectly don't allow distinct integers #2221.
- Compiler segfault when using distinct type in attribute imported from other module #2234.
- Assert casting bitstruct to short/char #2237.
- @tag didn't work with members #2236.
- Assert comparing untyped lists #2240.
- Fix bugs relating to optional interface addr-of #2244.
- Compiler null pointer when building a static-lib with -o somedir/... #2246
- Segfault in the compiler when using a bitstruct constant defined using a cast with an operator #2248.
- Default assert() message drops parens #2249.

### Stdlib changes
- Deprecate `String.is_zstr` and `String.quick_zstr` #2188.
- Add comparison with `==` for ZString types.
- `is_array_or_slice_of_char` and `is_arrayptr_or_slice_of_char` are replaced by constant `@` variants.
- `@pool` now has an optional `reserve` parameter, some minor changes to the temp_allocator API
- io::struct_to_format now supports bitstructs.
- Add `String.escape`, `String.unescape` for escaping and unescaping a string.

## 0.7.2 Change list

### Changes / improvements
- Better default assert messages when no message is specified #2122
- Add `--run-dir`, to specify directory for running executable using `compile-run` and `run` #2121.
- Add `run-dir` to project.json.
- Add `quiet` to project.json.
- Deprecate uXX and iXX bit suffixes.
- Add experimental LL / ULL suffixes for int128 and uint128 literals.
- Allow the right hand side of `|||` and `&&&` be runtime values.
- Added `@rnd()` compile time random function (using the `$$rnd()` builtin). #2078
- Add `math::@ceil()` compile time ceil function. #2134
- Improve error message when using keywords as functions/macros/variables #2133.
- Deprecate `MyEnum.elements`.
- Deprecate `SomeFn.params`.
- Improve error message when encountering recursively defined structs. #2146
- Limit vector max size, default is 4096 bits, but may be increased using --max-vector-size.
- Allow the use of `has_tagof` on builtin types.
- `@jump` now included in `--list-attributes` #2155.
- Add `$$matrix_mul` and `$$matrix_transpose` builtins.
- Add `d` as floating point suffix for `double` types.
- Deprecate `f32`, `f64` and `f128` suffixes.
- Allow recursive generic modules.
- Add deprecation for `@param foo "abc"`.
- Add `--header-output` and `header-output` options for controlling header output folder.
- Generic faults is disallowed.
- Detect when a slice on the stack is accidentally returned from a function.

### Fixes
- Assert triggered when casting from `int[2]` to `uint[2]` #2115
- Assert when a macro with compile time value is discarded, e.g. `foo();` where `foo()` returns an untyped list. #2117
- Fix stringify for compound initializers #2120.
- Fix No index OOB check for `[:^n]` #2123.
- Fix regression in Time diff due to operator overloading #2124.
- attrdef with any invalid name causes compiler assert #2128.
- Correctly error on `@attrdef Foo = ;`.
- Contract on trying to use Object without initializing it.
- Variable aliases of aliases would not resolve correctly. #2131
- Variable aliases could not be assigned to.
- Some folding was missing in binary op compile time resolution #2135.
- Defining an enum like `ABC = { 1 2 }` was accidentally allowed.
- Using a non-const as the end range for a bitstruct would trigger an assert.
- Incorrect parsing of ad hoc generic types, like `Foo{int}****` #2140.
- $define did not correctly handle generic types #2140.
- Incorrect parsing of call attributes #2144.
- Error when using named argument on trailing macro body expansion #2139.
- Designated const initializers with `{}` would overwrite the parent field.
- Empty default case in @jump switch does not fallthrough #2147.
- `&&&` was accidentally available as a valid prefix operator.
- Missing error on default values for body with default arguments #2148.
- `--path` does not interact correctly with relative path arguments #2149.
- Add missing `@noreturn` to `os::exit`.
- Implicit casting from struct to interface failure for inheriting interfaces #2151.
- Distinct types could not be used with tagof #2152.
- `$$sat_mul` was missing.
- `for` with incorrect `var` declaration caused crash #2154.
- Check pointer/slice/etc on `[out]` and `&` params. #2156.
- Compiler didn't check foreach over flexible array member, and folding a flexible array member was allowed #2164.
- Too strict project view #2163.
- Bug using `#foo` arguments with `$defined` #2173
- Incorrect ensure on String.split.
- Removed the naive check for compile time modification, which fixes #1997 but regresses in detection.

### Stdlib changes
- Added `String.quick_ztr` and `String.is_zstr`
- std::ascii moved into std::core::ascii. Old _m variants are deprecated, as is uint methods.
- Add `String.tokenize_all` to replace the now deprecated `String.splitter`
- Add `String.count` to count the number of instances of a string.
- Add `String.replace` and `String.treplace` to replace substrings within a string.
- Add `Duration * Int` and `Clock - Clock` overload.
- Add `DateTime + Duration` overloads.
- Add `Maybe.equals` and respective `==` operator when the inner type is equatable.
- Add `inherit_stdio` option to `SubProcessOptions` to inherit parent's stdin, stdout, and stderr instead of creating pipes. #2012
- Remove superfluous `cleanup` parameter in `os::exit` and `os::fastexit`.
- Add `extern fn ioctl(CInt fd, ulong request, ...)` binding to libc;

## 0.7.1 Change list

### Changes / improvements
- Better errors on some common casting mistakes (pointer->slice, String->ZString, deref pointer->array) #2064.
- Better errors trying to convert an enum to an int and vice versa.
- Function `@require` checks are added to the caller in safe mode. #186
- Improved error message when narrowing isn't allowed.
- Operator overloading for `+ - * / % & | ^ << >> ~ == != += -= *= /= %= &= |= ^= <<= >>=`
- Add `@operator_r` and `@operator_s` attributes.
- More stdlib tests: `sincos`, `ArenaAllocator`, `Slice2d`.
- Make aliases able to use `@deprecated`.
- Refactored stdlib file organization.
- Allow `@if` on locals.
- String str = "" is now guaranteed to be null terminated. #2083
- Improved error messages on `Foo { 3, abc }` #2099.
- `Foo[1..2] = { .baz = 123 }` inference now works. #2095
- Deprecated old inference with slice copy. Copying must now ensure a slicing operator at the end of the right hand side: `foo[1..2] = bar[..]` rather than the old `foo[1..2] = bar`. The old behaviour can be mostly retained with `--use-old-slice-copy`).
- Added `Enum.lookup` and `Enum.lookup_field`.
- `c3c build` picks first target rather than the first executable #2105.
- New Win32 Mutex, ConditionVariable and OnceFlag implementation

### Fixes
- Trying to cast an enum to int and back caused the compiler to crash.
- Incorrect rounding at compile time going from double to int.
- Regression with invalid setup of the WASM temp allocator.
- Correctly detect multiple overloads of the same type.
- ABI bug on x64 Linux / MacOS when passing a union containing a struct of 3 floats. #2087
- Bug with slice acces as inline struct member #2088.
- `@if` now does implicit conversion to bool like `$if`. #2086
- Fix broken enum inline -> bool conversions #2094.
- `@if` was ignored on attrdef, regression 0.7 #2093.
- `@ensure` was not included when the function doesn't return a value #2098.
- Added missing `@clone_aligned` and add checks to `@tclone`
- Comparing a distinct type with an enum with an inline distinct type failed unexpectedly.
- The `%s` would not properly print function pointers.
- Compiler crash when passing an untyped list as an argument to `assert` #2108.
- `@ensure` should be allowed to read "out" variables. #2107
- Error message for casting generic to incompatible type does not work properly with nested generics #1953
- Fixed enum regression after 0.7.0 enum change.
- ConditionVariable now properly works on Win32

### Stdlib changes
- Hash functions for integer vectors and arrays.
- Prefer `math::I` and `math::I_F` for `math::IMAGINARY` and `math::IMAGINARYF` the latter is deprecated.
- Add `array::contains` to check for a value in an array or slice.

## 0.7.0 Change list

### Changes / improvements
- Removed `Foo { 1, 2 }` initializer.
- Changed `Foo(<int>)` to `Foo {int}`.
- Removed `{| |}` expression blocks.
- Removed macro `&ref` and `$varef` parameters.
- Removed `$vaexpr(0)` syntax in favour of `$vaexpr[0]`
- Enum does not cast to/from an integer (its ordinal).
- Removed use of `void!` for main, test and benchmark functions.
- Removed `$or`, `$and`, `$concat` compile time functions.
- Removed `@adhoc` attribute.
- Disallow inline use of nested generics (e.g. `List{List{int}}`.
- Remove `.allocator = allocator` syntax for functions.
- Remove `@operator(construct)`.
- Removal of "any-switch".
- Allow swizzling assign, eg. `abc.xz += { 5, 10 };`
- Added `$$wstr16` and `$$wstr32` builtins.
- `$foreach` "()" replaced by trailing ":" `$foreach ($x, $y : $foo)` -> `$foreach $x, $y : $foo:`
- `$for` "()" replaced by trailing ":" `$for (var $x = 0; $x < FOO; $x++)` -> `$for var $x = 0; $x < FOO; $x++:`
- `$switch` "()" replaced by trailing ":" `$switch ($Type)` -> `$switch $Type:`
- Empty `$switch` requires trailing ":" `$switch` -> `$switch:`
- Rename `@return!` to `@return?` and change syntax to require ":" after faults.
- Remove `if (catch foo) { case ... }` syntax.
- Remove `[?]` syntax.
- Change `int!` to `int?` syntax.
- New `fault` declaration using `faultdef`.
- Enum associated values can reference the calling enum.
- Improve error message on `foo ?? io::EOF` with missing '?' #2036
- Make `@public` import recursive. #2018
- Fault nameof prefixes the first last module path, for instance `std::io::EOF` is rendered as `io::EOF`.
- Rename `def` to `alias`.
- Change `distinct` -> `typedef`.
- Order of attribute declaration is changed for `alias`.
- Added `LANGUAGE_DEV_VERSION` env constant.
- Rename `anyfault` -> `fault`.
- `!!foo` now works same as as `! ! foo`.
- Temp allocator now supports more than 2 in-flight stacks.
- Printing stacktrace uses its own temp allocator.
- Allow inferred type on body parameters. E.g. `@stack_mem(1024; alloc) { ... };`
- Use `@pool_init()` to set up a temp pool on a thread. Only the main thread has implicit temp pool setup.
- `tmem` is now a variable.
- Compile test and benchmark functions when invoking `--lsp` #2058.
- Added `@format` attribute for compile time printf validation #2057.
- Formatter no longer implicitly converts enums to ordinals.

### Fixes
- Fix address sanitizer to work on MachO targets (e.g. MacOS).
- Post and pre-decrement operators switched places for vector elements #2010.
- Aliases were incorrectly considered compile time constants.
- FreeBSD libc stat definitions were incorrect.
- Atomic max was incorrect.
- `"+".to_float()` would panic.
- `import` can now both be @public and @norecurse.
- Crash when trying to convert a struct slice to a vector #2039.
- Crash resolving a method on `Foo[2]` when `Foo` is distinct #2042.
- Bug due to missing cast when doing `$i[$x] = $z`.
- Incorrectly allowed getting pointer to a macro #2049.
- &self not runtime null-checked in macro #1827.
- Bug when printing a boolean value as an integer using printf.
- Show error when a generic module contains a self-generic type.
- "Single module" was not enforced when creating a static library using as a project target.

### Stdlib changes
- `new_*` functions in general moved to version without `new_` prefix.
- `string::new_from_*` changed to `string::from_*`.
- `String.to_utf16_copy` and related changed to `String.to_utf16`.
- `String.to_utf16_tcopy` and related changed to `String.to_temp_utf16`
- `mem::temp_new` changed to `mem::tnew`.
- `mem::temp_alloc` and related changed to `mem::talloc`.
- `mem::temp_new_array` changed to `mem::temp_array`.
- Add `ONHEAP` variants for List/HashMap for initializing global maps on the heap.
- Remove Vec2 and other aliases from std::math. Replace `.length_sq()` with `sq_magnitude()`
- Change all hash functions to have a common `hash` function.
- `@wstring`, `@wstring32`, `@char32` and `@char16` compile time macros added.
- Updates to `Atomic` to handle distinct types and booleans.
- Added `math::iota`.
- `@pool` no longer takes an argument.
- `Allocator` interface removes `mark` and `reset`.
- DynamicArenaAllocator has changed init function.
- Added `BackedArenaAllocator` which is allocated to a fixed size, then allocates on the backing allocator and supports mark/reset.
- `AnyList` now also defaults to the temp allocator.
- `os::getcwd` and `os::get_home_dir` requires an explicit allocator.
- `file::load_new` and `file::load_path_new` removed.
- `os::exit` and `os::fastexit` added.

## 0.6.8 Change list

### Changes / improvements
- Increase precedence of `(Foo) { 1, 2 }`
- Add `--enable-new-generics` to enable `Foo{int}` generic syntax.
- `{| |}` expression blocks deprecated.
- c3c `--test-leak-report` flag for displaying full memory lead report if any
- Output into /.build/obj/<platform> by default.
- Output llvm/asm into llvm/<platform> and asm/<platform> by default.
- Add flag `--suppress-run`. For commands which may run executable after building, skip the run step. #1931
- Add `--build-env` for build environment information.
- Deprecation of `@operator(construct)`.

### Fixes
- Bug appearing when `??` was combined with boolean in some cases.
- Test runner --test-disable-sort didn't work, c3c was expecting --test-nosort
- Test runner with tracking allocator assertion at failed test #1963
- Test runner with tracking allocator didn't properly handle teardown_fn
- Correctly give an error if a character literal contains a line break.
- Implicitly unwrapped optional value in defer incorrectly copied #1982.
- Crash when trying to define a method macro that isn't `@construct` but has no arguments.
- Regression, `.gitkeep` files were generated incorrectly.
- Aliases are now correctly handled as if they were variables/functions in regards to namespacing and accept `@builtin`.
- Correctly handle in/out when interacting with inout.
- Don't delete .o files not produced by the compiler.
- Fix optional jumps in expression lists, #1942.
- Several fixes for .o files and -o output, improving handling and naming.
- Fix bug casting bool to int to other int #1995.
- `@if` declarations were missing from -P output #1973.
- Check exe and lib output so -o works with directories.
- Swizzling an inline vector in a struct would cause a crash.
- Fixed error and poor error message when using an invalid target name.

### Stdlib changes

## 0.6.7 Change list

### Changes / improvements
- Contracts @require/@ensure are no longer treated as conditionals, but must be explicitly bool.
- Add `win-debug` setting to be able to pick dwarf for output #1855.
- Error on switch case fallthough if there is more than one newline #1849.
- Added flags to `c3c project view` to filter displayed properties
- Compile time array assignment #1806.
- Allow `+++` to work on all types of arrays.
- Allow `(int[*]) { 1, 2 }` cast style initialization.
- Experimental change from `[*]` to `[?]`
- Warn on if-catch with just a `default` case.
- Compile time array inc/dec.
- Improve error message when using ',' in struct declarations. #1920
- Compile time array assign ops, e.g. `$c[1] += 3` #1890.
- Add `inline` to enums #1819.
- Cleaner error message when missing comma in struct initializer #1941.
- Distinct inline void causes unexpected error if used in slice #1946.
- Allow `fn int test() => @pool() { return 1; }` short function syntax usage #1906.
- Test runner will also check for leaks.
- Improve inference on `??` #1943.
- Detect unaligned loads #1951.
- `Thread` no longer allocates memory on posix.

### Fixes
- Fix issue requiring prefix on a generic interface declaration.
- Fix bug in SHA1 for longer blocks #1854.
- Fix lack of location for reporting lambdas with missing return statement #1857.
- Compiler allows a generic module to be declared with different parameters #1856.
- Fix issue with `@const` where the statement `$foo = 1;` was not considered constant.
- Const strings and bytes were not properly converted to compile time bools.
- Concatenating a const empty slice with another array caused a null pointer access.
- Fix `linux-crt` and `linux-crtbegin` not getting recognized as a project paramater
- Fix dues to crash when converting a const vector to another vector #1864.
- Filter `$exec` output from `\r`, which otherwise would cause a compiler assert #1867.
- Fixes to `"exec" use, including issue when compiling with MinGW.
- Correctly check jump table size and be generous when compiling it #1877.
- Fix bug where .min/.max would fail on a distinct int #1888.
- Fix issue where compile time declarations in expression list would not be handled properly.
- Issue where trailing body argument was allowed without type even though the definition specified it #1879.
- Fix issues with @jump on empty `default` or only `default` #1893 #1894
- Fixes miscompilation of nested `@jump` #1896.
- Fixed STB_WEAK errors when using consts in macros in the stdlib #1871.
- Missing error when placing a single statement for-body on a new row #1892.
- Fix bug where in dead code, only the first statement would be turned into a nop.
- Remove unused $inline argument to mem::copy.
- Defer is broken when placed before a $foreach #1912.
- Usage of @noreturn macro is type-checked as if it returns #1913.
- Bug when indexing into a constant array at compile time.
- Fixing various issues around shifts, like `z <<= { 1, 2 }`.
- `return (any)&foo` would not be reported as an escaping variable if `foo` was a pointer or slice.
- Incorrect error message when providing too many associated values for enum #1934.
- Allow function types to have a calling convention. #1938
- Issue with defer copying when triggered by break or continue #1936.
- Assert when using optional as init or inc part in a for loop #1942.
- Fix bigint hex parsing #1945.
- `bigint::from_int(0)` throws assertion #1944.
- `write` of qoi would leak memory.
- Issue when having an empty `Path` or just "."
- `set_env` would leak memory.
- Fix issue where aligned bitstructs did not store/load with the given alignment.
- Fix issue in GrowableBitSet with sanitizers.
- Fix issue in List with sanitizers.
- Circumvent Aarch64 miscompilations of atomics.
- Fixes to ByteBuffer allocation/free.
- Fix issue where compiling both for asm and object file would corrupt the obj file output.
- Fix `poll` and `POLL_FOREVER`.
- Missing end padding when including a packed struct #1966.
- Issue when scalar expanding a boolean from a conditional to a bool vector #1954.
- Fix issue when parsing bitstructs, preventing them from implementing interfaces.
- Regression `String! a; char* b = a.ptr;` would incorrectly be allowed.
- Fix issue where target was ignored for projects.
- Fix issue when dereferencing a constant string.
- Fix problem where a line break in a literal was allowed.

### Stdlib changes
- Added '%h' and '%H' for printing out binary data in hexadecimal using the formatter.
- Added weakly linked `__powidf2`
- Added channels for threads.
- New `std::core::test` module for unit testing machinery.
- New unit test default runner.
- Added weakly linked `fmodf`.
- Add `@select` to perform the equivalent of `a ? x : y` at compile time.
- `HashMap` is now `Printable`.
- Add `allocator::wrap` to create an arena allocator on the stack from bytes.

## 0.6.6 Change list

### Changes / improvements
- Split help into normal and "full" help, #1703
- Removed 'headers' command line option.
- Add `enum.from_ordinal` and `fault.from_ordinal`
- Deprecate cast-style conversion from integer <-> enum.
- Make deprecation an error in test mode.
- Add `--win-vs-dirs` to override VS detection dirs.
- Add `"name"` project property to override the name of the resulting binary. #1719
- Improved `add-project` to take arguments.
- Improve error reporting when using type names as the function argument #1750.
- Improve ordering of method registration to support adding methods to generic modules with method constraints #1746
- Support experimental `@operator(construct)` operator overload.
- Allow using 'var' to declare lambdas in functions.
- Add 'validation' setting and make dead code a warning.
- Allow compile time `$foreach` iteration over constant Strings and bytes.
- Improved error message when accessing `@private` from other modules #1769.
- Include `@name` when searching for possible matches to `name` in the error message. #1779
- Improve `@param` parse errors #1777
- Improved `#foo` resolution inside of the compiler.
- Deprecated '&' macro arguments.
- Deprecate `fn void! main() type main functions.
- Deprecate old `void!` @benchmark and @test functions.
- Allow test runners to take String[] arguments.
- Added `--lsp` output.
- Improve the error message when running out of memory.
- Allowed passing arguments to @test / @benchmark runners via `c3c test[benchmark] -- -o --opt1 <arg1>`
- Handle bytes and string literals the same way in terms of zero termination.
- Function comments are stored and displayed with -P.
- Prevent `#hash` arguments from taking code that modifies ct variables. #1794
- Make stringify to recursively enter `#hash` expressions #1834.

### Fixes
- Fix case trying to initialize a `char[*]*` from a String.
- Fix Map & HashMap `put_all_for_create` not copying all elements, causing `init_from_map` to create incomplete copy.
- Fix bug when a macro calling an extern function was called in another module also declaring and calling the same function. #1690
- `static-lib` and `dynamic-lib` options from the command line now produces headers.
- Fix bug outputting exported functions without predefined extname.
- Fix problem where crt1 was linked for dynamic libraries on Linux and BSD. #1710
- Fix CRT detection on Arch Linux.
- Fix lexer allowing a trailing underscore (_) with hex and binary literals.
- Fix `--list-operators` CLI command printing underscore (_) and hash (#).
- Fix bug in temp allocator when temp memory is exhausted and allocation needs overaligned mem. #1715
- Incorrectly handles distinct enums and pointers with '+=' and '-=' #1717.
- Prevent DString from being initialized with "".
- Fix bug in OnStackAllocator when freeing overallocated data. #1720
- Use `weak_odr` rather than `weak` on Windows which seems to prevent issues such as #1704.
- Use `weak` on dyn-symbols on Linux.
- Fix crash on project.json not having an empty set of targets.
- Miscompile when indexing an array with small unsigned types for enums.
- Change CBool to be 1 byte.
- `any_to_int` checks value to be int and no longer works with enum.
- Add check in formatter printing "%c".
- Fix bug where `!!` and `!` was not recognized to jump out of the current scope.
- Fix bug when including compile time parameters in trailing body more than once.
- Fix issue with compiling a constant struct containing a string array in a local context.
- Fix error where panic would not properly stop the program when stacktrace couldn't be printed #1751.
- Macros with default arguments to `&`, `#` and type parameters didn't work as expected. #1754.
- `net::poll()` with negative timeout behaved incorrectly.
- Return type inference bugs with macros #1757
- `$defined` in a global scope should accept testing normal macros.
- Assert on add to uninitialized ct variable #1765.
- Dynamic function lookup fails after changing type without dummy anycast #1761
- $vasplat was allowed inside of a function when passed as an argument to a function.
- Prohibit raw vaargs in regular functions with a function body.
- Assert on certain slice to slice casts. #1768.
- Fix vector float -> bool conversion.
- Fix `+a = 1` erronously being accepted.
- Fix not freeing a zero length String
- Macros with trailing bodys aren't allowed as the single statement after a while loop with no body #1772.
- Deref subscripts as needed for macro ref method arguments. #1789
- Change ordering to simplify adding methods to type in conditional modules.
- `#foo` style arguments were not type checked when given a type. #1790
- Bug when using +++ on value build a slice or array: the rhs cast was not done.
- Fix bug preventing compile time slices from being iterated over with `$foreach`.
- Fix bug with defer assignment in macro #1807.
- Fix regression with swizzle references for vectors #1810.
- Assert when partially initializing a constant struct containing a slice #1812.
- Assert concatenating constant slices #1805.
- Do not link "ld" on Linux with no libc.
- Fix bug when multiple `$else` clauses followed an `$if` #1824.
- Report the correct type as not having a method when access fails #1828.
- Prevent temp arena scribbling from causing an asan warning. #1825
- Fix bug where `&i[0] = null` was not detected to be an error #1833.

### Stdlib changes
- Increase BitWriter.write_bits limit up to 32 bits.
- Updates to `Slice2d`, like `get_xy` and others.
- Added `iter()` `value_iter()` and `key_iter()` to HashMap.
- Add "tokenizer" to String.
- Add "skip_empty" to split methods. Add split_to_buffer method.
- Add `@enum_from_value`.
- Updated hash function.
- Added URL parser.
- Added convenience functions to `Maybe`.
- Added `String.trim_left()` and `.trim_right()`.
- Deprecation of several `&` macros.
- Format functions for timedates.
- Add `@assert_leak()` to assert on memory leaks in the scope.
- Added `double.set_high_word()`, `double.set_low_word()`, and `float.set_word()`.

## 0.6.5 Change list

### Changes / improvements
- Allow splat in initializers.
- Init command will now add `test-sources` to `project.json` #1520
- `a++` may be discarded if `a` is optional and ++/-- works for overloaded operators.
- Improve support for Windows cross compilation on targets with case sensitive file systems.
- Add "sources" support to library `manifest.json`, defaults to root folder if unspecified.
- Add char_at method in DString and operators [], len, []= and &[].
- Add `-q` option, make `--run-once` implicitly `-q`.
- Add `-v`, `-vv` and `-vvv` options for increasing verbosity, replacing debug-log and debug-stats options.

### Fixes
- Fix bug where `a > 0 ? f() : g()` could cause a compiler crash if both returned `void!`.
- `@builtin` was not respected for generic modules #1617.
- Fix issue writing a single byte in the WriteBuffer
- A distinct inline pointer type can now participate in pointer arithmetics.
- Support &a[0] returning the distinct type when applying it to a distinct of a pointer.
- Fix error when calling `HashMap.remove` on uninitialized `HashMap`.
- Fix issue with resolved try-unwrap in defer.
- Fix issue with overloaded subscript and ++/-- and assign ops (e.g. `*=`)
- Fix issue with properties in different targets not being respected #1633.
- Indexing an Optional slice would crash in codegen #1636.
- SimpleHeapAllocator bug when splitting blocks allowed memory overrun.
- Not possible to alias or take reference for extension methods on non-user defined types. #1637
- Prevent methods from using names of properties or fields. #1638
- b64 / hex data strings can now be used with \` as well.
- Contracts on generic modules would evaluate too late, sometimes not catching the error until it already occurred elsewhere.
- Fix bug preventing optionals from being used in ranges or as indices.
- Crash compiling for arm64 when returning 16 byte and smaller structs by value not a power of 2 #1649.
- Enforce single module compilation for static libraries to make constructors run properly.
- Crash when using --no-obj without compile-only. #1653
- Do not produce expression locations for windows.
- Issue where multiple methods were accepted for the same type.
- Issue where a method was linked to a type alias instead of the underlying type.
- Fix Fnv1a encoding.
- Fix issue with accessing arrays in access-overloaded types, e.g. `list[1][2]` #1665.
- Cast removing arbitrary array indices and converting them to pointers should always be fine #1664
- Incorrect "no-libc" definition of `cos`, making it unavailable for wasm.
- Fix issue with the adjoint and inverse calculations for `Matrix2x2`.
- It was possible to create 0 length arrays using byte literals. #1678
- Crash when a constant null typeid is checked for properties. #1679

### Stdlib changes
- Add `io::MultiReader`, `io::MultiWriter`, and `io::TeeReader` structs.
- Updated Base32 API.
- Add `file::save`.
- Add `memcpy` / `memset` / `memcmp` to nolibc.
- Add `sort::quickselect` to find the k-th smallest element in an unordered list.
- Add `sort::is_sorted` to determine if a list is sorted.
- Implement RFC 3986 for url encoding and decoding.

## 0.6.4 Change list

### Changes / improvements
- Const vector -> const slice implicit conversion.
- Slicing arrays, slices and bytes at compile time #1466.
- Better error for `int a[4] = ...`. #1518
- Better error for `int Foo(int a)` declarations #1516
- Improve error message in the case of `MyInterface x = foo;` #1522
- Deprecate `@adhoc`, allow non-nested ad hoc generic types.
- Constant bytes <=> char[] conversion should work #1514.
- Infer now works across ternary.
- Interfaces now support .ptr and .type directly without casting to `any`.
- Switch to `<* *>` docs.
- Improve error messages on expressions like `var $type = int;` #1553.
- Disallow casting a `void*` to `any` or an interface, unless it is `null`.
- Defer resolution of declarations when looked up in `def` aliased #1559.
- Adding constants to the Json AST #1540
- Adding info to the globals inside Json AST #1541
- Null-check function pointer invocation #1573.
- `string::new_struct_to_str` and `io::struct_to_format` to dump struct data.
- `io::print` will now print structs.
- Improve error message when using `void` aliases as variable storage type.
- Add a target type: "prepare" which doesn't compile anything (but may run `exec`)
- Improve error message on incorrect inner struct/union name #1847.

### Fixes
- `Unsupported int[*] $x = { 1, 2, 3, 4 }` #1489.
- Unexpected compile error using a typed constant with `copysign` #1517
- Incorrect subscript resolution #1519.
- Segfault with passing a program with `-` using stdin.
- Using no module with `-` would reject the program.
- Unintended deref of pointers with methods caused regression with hash function.
- Fix broken sincos function.
- Bug when a continue is copied in a defer.
- Compiler error when any/interface initialized using {} #1533.
- Bug when defers and $if were combined in a macro, which would cause miscompilation.
- Fixes to the CSV reader.
- Crash returning struct or vector from function using ternary expression #1537.
- Improved error message on invalid subscript index type #1535.
- Improved error message when declaring a variable `void!`.
- Cannot use void as a generic parameter #1546
- Interfaces not correctly copied with generics #1545
- Memory leak in keys.new_list fixed.
- Standard library is now correctly weakly linked, fixing the use of C3 .so together with executable. #1549, #1107.
- Wrong error message for interface methods with body #1536.
- Empty expression block would crash compiler with debug on #1554.
- Improve infer conversions on constants, e.g. `ZString a = foo ? "a" : "b";` #1561
- Show error when declarations do not start with `fn` in interfaces. #1565
- `if (try foo)` was handled incorrectly inside a defer.
- `&self` argument not implicitly null checked. #1556.
- `(uptr)&((Foo*)null).a` incorrectly inserts a null check. #1544
- Incorrect error message when `$eval` is provided an invalid string. #1570
- `HashMap.copy_keys` did not properly copy keys which needed to be allocated #1569
- Named vector component access would not fold at compile time. #1574
- `$define` would occasionally not properly evaluate declarations it encountered.
- Fixes with error handling recursive `@tag` #1583.
- Sometimes generating introspection info would not be in the global scope causing a crash #1586.
- @tag on macros cannot be retrieved with tagof #1582
- Taking the $typeof of a wildcard optional returns `void!`.
- Fix bug with enums with jump tables #1840.
- Enum associated declarations accidentally allowed declaration in function style. #1841
- Quicksort and insertsort incorrectly allowing arrays and vectors by value. #1845.

### Stdlib changes
- Remove unintended print of `char[]` as String
- Add read/write to stream with big endian ints.
- Move accidently hidden "wrap_bytes".
- Added CBool #1530.
- Added encoding/base32 module.

## 0.6.3 Change list

### Changes / improvements
- Introduce `arg: x` named arguments instead of `.arg = x`, deprecate old style.
- Support splat for varargs #1352.
- Allow `var` in lambdas in macros.
- Support `int[*] { 1, 2, 3 }` expressions.
- Support inline struct designated init as if inline was anonymous.
- Introduce the `.paramsof` property.
- Support environment variable 'C3C_LIB' to find the standard library.
- Support environment variable 'C3C_CC' to find the default C compiler.
- Support casting bitstructs to bool.
- Allow user-defined attributes to have typed parameters.
- Add `.gitkeep` files to project subfolders.
- Add `env::COMPILER_BUILD_HASH` and `env::COMPILER_BUILD_DATE`
- Support linking .o files in compilation command. #1417
- Slicing constant strings at compile time works.
- Add `project fetch` subcommand to fetch missing project dependencies (general and target specific)
- Ability of `vendor-fetch` to download the dependencies in the first specified path `dependencies-search-path`
- Ability of `vendor-fetch` to register the fetched dependencies in the project file.
- Allow the "self" parameter to be $/# for macro methods.
- Support compile time slicing of untyped lists.
- Allow specifying an import module using `@wasm` #1305.
- Deprecated inline generic types outside of struct definitions and macros unless marked `@adhoc`.
- Improved method detection in earlier stages of checking.
- Allow `@norecurse` attribute for non-recursive imports #1480.
- wasm32 / wasm64 targets are use-libc=no by default.
- Add hash/sha256 module

### Fixes
- Issue where a lambda wasn't correctly registered as external. #1408
- Generic methods were incorrectly registered as functions, leading to naming collisions. #1402
- Deprecated tuple / triple types.
- Converting a slice to a vector/array would copy too little data.
- Crash when reading an empty 'manifest.json'.
- "optsize" did not work correctly in project.json.
- `l[0].a = 1` now supported for overloads due to better lvalue handling #1357.
- Asserts are retained regardless of optimization when running tests.
- Limit object filename lengths. #1415
- Fix regression for `$include`.
- Correct '.so' suffix on dynamic libraries on Linux.
- Fix bug where inline index access to array in a struct would crash the compiler.
- Asserts are now correctly included and traced in when running tests.
- Use atexit to fix finalizers on Windows #1361.
- Fix bugs in "trap-on-wrap" #1434.
- Bug with casting anyfault to error.
- Lambda / function type would accidentally be processed as a method.
- Fix error message when not finding a particular function.
- Crash invoking a `@body` argument with the wrong number of parameters.
- Fix reordering semantics in struct assignment.
- Regression when passing types as `#expr` arguments. #1461
- Temp allocator overwrites data when doing reset on extra allocated pages. #1462
- User defined attributes could not have more than 1 parameter due to bug.
- Folding a constant array of structs at compile time would cause an assert.
- Enum attributes would be overwritten by enum value attributes.
- LLVM issue with try when bool is combined #1467.
- Segfault using ternary with no assignment #1468.
- Inner types make some errors misleading #1471.
- Fix bug when passing a type as a compile time value.
- Fix bug due to enum associated values not being checked for liveness.
- Regression when compile time accessing a union field not last assigned to.
- Safer seed of rand() for WASM without libc.
- Bad error message aliasing an ident with a path. #1481.
- Error when slicing a struct with an inline array #1488.
- Improved error messages on `Foo a = foo { 1 };` #1496
- Bug in json decoder escape handling.
- Fix bug when reading zip manifest, that would not return a zero terminated string. #1490
- Fix thread tests.
- Detect recursion errors on non-recursive mutexes in safe mode.
- Foreach over distinct pointer failed to be caught as error #1506.
- Foreach over distinct iterable would ignore operator(len).
- Compiler crash when compiling c code in a library without --obj-out #1503.

### Stdlib changes
- Additional init functions for hashmap.
- `format` functions are now functions and work better with splat.
- Add support for the QOI format.
- Add `io::read_new_fully` for reading to the end of a stream.
- Add `io::wrap_bytes` for reading bytes with `io` functions.
- Add `rnd` and `rand_in_range` default random functions.
- Additional timezone related functions for `datetime`.
- Added MD5 and crypto::safe_compare.
- Added generic HMAC.
- Added generic PBKDF2 implementation.
- DString `reverse`.
- `DString.insert_at` now has variants for other types.

## 0.6.2 Change list

### Changes / improvements

- Updated LLVM passes
- Added `is_substruct` type property.
- Scalar -> vector not implicit in call or assign.
- Added `--vector-conv` to enable the old scalar->vector conversion behaviour.
- Added "weak" type aliases `def Foo = my_foo::Foo @weak;`
- `*-add` keys in targets in `manifest.json` and `project.json` are deprecated.
- Made "add" the default for things like `sources`, `dependencies` and other keys in project and library files.
- Give some symbol name suggestions when the path is matched.
- Don't generate .o files on `compile` and `compile-run` if there is no `main`.
- c3c init-lib does not create the directory with the .c3l suffix #1253
- Permit foreach values to be optional.
- Add `--show-backtrace` option to disable backtrace for even smaller binary.
- Untested Xtensa support.
- && doesn't work correctly with lambdas #1279.
- Fix incorrect override of optimization levels when using projects.
- Add experimental `@noalias` attribute.
- Add a `--run-once` option to delete the output file after running it.
- Add `@const` attribute for macros, for better error messages with constant macros.
- Add `wincrt` setting to libraries.
- Add `+++` `&&&` `|||` as replacement for `$concat`, `$and` and `$or`.
- Add `methodsof` to type info for struct, union and bitstruct.
- Added `@tag` `tagof` and `has_tagof` to user defined types and members.
- Added `c-include-dirs` project/manifest setting.
- The compiler now skips UTF8 BOM.
- Printable values passed to the Formatter as pointers, will print as if passed by value.
- Pointers are rendered with "0x" prefix when passed to '%s'.
- Add temp allocator scribble.
- Use PIC by default on Linux.
- `$exec` may now provide a stdin parameter.
- Introduce `$vaarg[...]` syntax and deprecate the old `$vaarg(...)`.
- Similar change to `$vasplat`: `$vasplat` and `$vasplat[1..]`.
- Add `$member.get(value)` to replace `value.$eval($member.nameof)`
- Improve the error message when the compilation does not produce any files #1390.
- Add `fmod` implementation for nolibc.
- Allow `(Foo) { 1, 2 }` syntax for compound literals.

### Fixes

- Broken WASM library code.
- Regression: Invalid is_random implementation due to changes in 0.6.
- `dbghelp.lib` was linked even on nolibc on Windows.
- Fix incorrect linker selection on some platforms.
- Struct members declared in a single line declaration were not sharing attributes. #1266
- `opt` project setting now properly documented.
- Incorrect justify formatting of integers.
- Assertion with duplicate function pointer signatures #1286
- Distinct func type would not accept direct function address assign. #1287
- Distinct inline would not implement protocol if the inlined implemented it. #1292
- Distinct inline can now be called if it is aliasing a function pointer.
- Bug in List add_array when reserving memory.
- Fix issue where a compile time parameter is followed by "...".
- Fix issue with some conversions to untyped list.
- Issue where a `if (catch e = ...)` in a defer would be incorrectly copied. Causing codegen error.
- Variable in if-try / if-catch cannot be a reused variable name.
- Vararg interfaces were broken.
- LLVM codegen for constants in enums could fail.
- Fixes to the socket functions.
- Improved output when pointer is out of range.
- Better error when casting to a distinct fails.
- With single module, name the .o file after what `-o` provides. #1306
- Bitstruct members can now have attributes.
- `%` analysis was incorrect for int vectors.
- When resolving inherited interfaces, the interface type wasn't always resolved.
- Fix issues when checking methods and interfaces hasn't been resolved yet.
- Fix Vec2.angle
- Update to libc::setjmp on Win32, to do no stack unwinding.
- Recursively follow interfaces when looking up method.
- Int128 alignment change in LLVM fixed on x64.
- Fix interface lazy resolution errors.
- Interface resolution when part of generics #1348.
- Assert not properly traced #1354.
- Ordering issues with `$include` / `$exec` fixed #1302.
- Issues with wincrt linking.
- Debug info with recursive canonical type usage could cause segfault.
- Missing check on optional left hand side for `s.x`.
- Incorrect zero analysis on `foo["test"] = {}` #1360.
- Bug converting untyped list #1360.
- Benchmark / test no longer suppresses debug info. #1364.
- Bug when compile time subtracting a distinct type.
- `insert_at` incorrectly prevented inserts at the end of a list.
- Fix aligned alloc for Win32 targets.
- Compiler didn't detect when a module name was used both as a generic and regular module.
- Assigning a const zero to an aliased distinct caused an error.
- `--path` is now properly respected.
- `--test` will now provide the full filename and the column.
- Fix of bug in `defer (catch err)` with a direct return error.
- Too restrictive compile time checks for @const.
- Fixes to wasm nolibc in the standard library.
- Fixed int128 div/mod.
- Fix WASM memory init priority.
- Fix bug with `defer (catch err)` when used together with regular defer.
- Methods can now properly be aliased using `def` #1393.
- Memory leak in Object when not using temp allocators.
- Tracking allocator would double the allocations in the report.
- `printf` will now show errors in the output when there are errors.
- Bug where `if try` would work incorrectly in a macro.
- Prevent loading / storing large structs with LLVM.

### Stdlib changes

- `send` and `recv` added to `libc` for Posix / Win32.
- Add support to destroy temp allocators.
- Deprecated `path.append`, `path.tappend`, `getcwd`, `tgetcwd`, `path.absolute`, `ls`.
- Deprecated `env::get_config_dir`, replaced by `env::new_get_config_dir`.
- Added `path.has_extension`, `path.new_append`, `path.temp_append`, `new_cwd`, `temp_cwd`, `path.new_absolute`, `new_ls`, `temp_ls`.
- Added `dstring.replace`
- New hashmap type, `Map`
- Added `ElasticArray`.
- Added `types::is_signed`, `types::is_unsigned` and `types::inner_type`.

## 0.6.1 Change list

### Changes / improvements
- Addition of $append and $concat functions.
- Added $$str_hash, $$str_upper, $$str_lower, $$str_find builtins.
- Improved error notes when call expressions have errors.
- Trailing body arguments may now be `&ref`, `#hash`, `$const` and `$Type` arguments.
- "panic-msg" setting to suppress panic message output.
- Require `@export` functions to have `@export` types.
- Disallow leading/trailing/duplicate '_' in module names.
- Updated mangling.
- Added `$$unaligned_load` and `$$unaligned_store`.
- `--no-headers` option to suppress creating headers when generating a library.
- Support c-file compilation in libraries.
- Allow using $defined(&a[1]) to check if the operation is supported.
- Max number of members in a struct is limited to 65535.
- The maximum number of parameters in a call is now 255, up from 127.
- Array comparison now uses built-in memcmp on LLVM to enable optimizations.
- Prevent implicit array casts to pointers with higher alignment #1237.
- Macro `$case` statements now pick the first match and does not evaluate the rest.
- `manifest.json` is now checked for incorrect keys.
- Added `--list-manifest-properties` to list the available properties in `manifest.json`.
- Indexing into a constant array / struct now works at compile time.
- Improved error message when trying user foreach with an untyped list.
- RISCV asm support.

### Fixes
- Error with unsigned compare in `@ensure` when early returning 0 #1207.
- Prevent Mach-O from removing `@init` and `@dynamic` in a more reliable way #1200.
- Fix of missing copy of parameterized custom attributes.
- Fixed crash on certain recursive function definitions #1209.
- Return the typekind "FUNC" for a function pointer.
- No longer possible to dereference a function pointer.
- Fix bug with @jump miscompile.
- Bit negate does implicit integer promotion.
- Bitstructs, unions and flexible arrays now correctly emitted in headers.
- Fix distinct inline conversions.
- Bit negating const zero flags would give an incorrect result.
- Fix to scalar -> vector conversions.
- Bug fix for rethrow + defer catch.
- Wrong size for structs containing overaligned structs #1219
- $typeof(*x) should be valid when x is an `[out]` parameter #1226
- Fix ABI lowering for 128 bit vectors on Linux.
- Bad error message when using a generic method without generic parameters #1228
- Private function called from nested macro not visible to linker #1232
- Bitstructs in structs would not be correctly be handled in some cases.
- Fix problem where a $$FUNC would return "<GLOBAL>" when evaluated for a static in a function #1236.
- `ordinal` is no longer a valid associated value name for enums.
- Constants defined by indexing into another constant could fail codegen.
- Stdlib nolibc code bugs fixed.
- Regression: duplicate symbols with static variable declared in macro #1248.
- Unsplat with named parameters was accidentally disallowed.
- Reference parameter doesn't work with vector subscript #1250.
- The msvc_sdk script failed to work properly on windows when run in folders with spaces.
- Using winmain would call the wrong definition #1265.
- DynamicArenaAllocator would not correctly free.

### Stdlib changes
- Added `remove_first_item` `remove_last_item` and `remove_item` as aliases for the `match` functions.
- Added @str_hash, @str_upper, @str_lower, @str_find compile time macros.
- Remove "panic" text from unreachable() when safe mode is turned off.
- Added `@unaligned_store` and `@unaligned_load`.
- Null ZString, DString or pointer prints "(null)" for printf.
- Updated sorting API.
- Insertion sort and counting sort added.
- Added missing `mem` and `mem::allocator` functions for aligned allocations.
- Added `new_init_with_array` and `temp_init_with_array` for List.
- Fixed posix `NativeMutex.lock_timeout`.
- Fixed `env::ARCH_32_BIT` and `env::ARCH_64_BIT`.
- Added `time::us`.

## 0.6.0 Change list

### Changes / improvements
- `@default` implementations for interfaces removed.
- `any*` => `any`, same for interfaces.
- Private / local globals now have `internal` visibility in LLVM.
- Updated enum syntax.
- 'rgba' also available for swizzling.
- The name "subarray" has been replaced by the more well known name "slice' across the codebase.
- Improved alignment handling.
- Add `--output-dir` to command line. #1155
- Allow making distinct types out of "void", "typeid", "anyfault" and faults.
- Removed `--system-linker` setting.
- "Try" expressions may not be any binary or unary expressions. So for example `try foo() + 1` is disallowed.
- Added `$$REGISTER_SIZE` for int register size.
- `assert(false)` only allowed in unused branches or in tests. Compile time failed asserts is a compile time error.
- Require expression blocks returning values to have the value used.
- Detect "unsigned >= 0" as errors.
- Improve callstack debug information #1184.
- Request jump table using @jump for switches.
- Improved inline debug information.
- Improved error messages on inlined macros.
- Introduce MSVC compatible SIMD ABI.
- `$foreach` doesn't create an implicit syntactic scope.
- Error of `@if` depends on `@if`
- Support `defer (catch err)`
- Added `print-input` command argument to print all files used for compilation
- Allow recursive function definitions as long as they are pointers. #1182
- Default CPU to native if less than AVX, otherwise use AVX.
- Bounds checking on length for `foo[1:2]` slicing #1191.
- Foreach uses non-wrapping add/dec.

### Fixes
- Fixed issue in safe mode when converting enums.
- Better checking of operator methods.
- Bug when assigning an optional from an optional.
- Lambdas were not type checked thoroughly #1185.
- Fix problems using reflection on interface types #1203.
- `@param` with unnamed macro varargs could crash the compiler.
- Compiler crash using enum nameof from different module #1205.
- Incorrect length passed to scratch buffer printf.
- Casting to a bitstruct would be allowed even if the type was the wrong size.
- Generic modules parameterized with constants would sometimes get the wrong parameterized module name causing conversion errors #1192.
- Duplicate emit of expressions on negation would incorrectly compile negated macros.
- Casting a slice address to its pointer type should not compile #1193.
- Union is not properly zero-initialized with designated initializer #1194.
- Compile time fmod evaluates to 0 #1195.
- Assertion failed when casting (unsigned) argument to enum #1196
- Correct debug info on parameters without storage.
- Fix location on foreach debug output.
- Compiler crash on designated initializer for structs with bitstruct.

### Stdlib changes
- "init_new/init_temp" removed.
- LinkedList API rewritten.
- List "pop" and "remove" function now return Optionals.
- RingBuffer API rewritten. Allocator interface changed.
- Deprecated Allocator, DString and mem functions removed.
- "identity" functions are now constants for Matrix and Complex numbers.
- Removed 'append' from Object and List, replaced by 'push'.
- `GenericList` renamed `AnyList`.
- Proper handling of '.' and Win32 '//server' paths.
- Path normalization - fix possible null terminator out of bounds.
- Add 'zstr' variants for `string::new_format` / `string::tformat`.
- Fix mutex and wait signatures for Win32.

## 0.5.5 Change list

### Changes / improvements
- Disallow multiple `_` in a row in digits, e.g. `1__000`.
- Added `@link` attribute.
- New 'linker' build option.
- "linker" project setting updated, "system-linker" removed.

### Fixes
- Struct/union members now correctly rejects members without storage size #1147.
- `math::pow` will now correctly promote integer arguments.
- Pointer difference would fail where alignment != size (structs etc) #1150
- Fixed array calculation for npot2 vectors.
- $$memcpy_inline and $$memset_inline fixed.
- `.$Type = ...` and `.$foo = ...` now works #1156.
- `int.min` incorrect behaviour #1154.
- Bitstruct cast to other bitstruct by way of underlying type would fail #1159.
- Bug in `time.add_seconds` #1162.
- Remove initial './' in Win32 and convert '/' to '\' for paths when running a binary.
- 'output' directory for projects was incorrect in templates.
- Regression: no stacktrace.
- For MacOS, running with higher optimization would crash as initializers were removed.
- `compile-run` and `run` now returns the proper return code.
- Allow String constants -> ichar*, and allow integer pointers to explicitly convert between unsigned signed.
- Bug in unaligned return value lowering for Aarch64.

### Stdlib changes
- Added `new_aligned` and `alloc_aligned` functions to prevent accidental under-alignment when allocating simd.
- Fixes to realloc of aligned allocations
- Use native Windows calls on aligned allocations on Windows.
- mem::copy_inline, mem::clear_inline and mem::set_inline added.
- mem::copy / clear / set no longer has an `$inline` attribute.
- Native aligned libc malloc on Windows & POSIX.
- Simplification of the allocator interface.
- CoreFoundation only linked on MacOS when used.

## 0.5.4 Change list

### Changes / improvements
- Hash variables may now take a designated initializer.
- Added @safemacro to override the `@` requirement for non-function-like macros.
- More information available with debug log in non debug builds.
- Removed install_win_reqs.bat which didn't work well.
- Support `**` to mean `./**`
- MacOS init/finalizer now respects priority.
- Bitstructs supports `!=` and `==`.
- Support Windows `.def` files using `--windef`.
- Bitstructs now fold compile time constant bit ops.
- Fix issue where in some cases a constant global with a string wasn't folded (e.g. in asm stmts)
- Lateral implicit imports removed.
- Default to '.' if no libdir is specified.
- Improved error messages for `--lib`.
- Added `--linker` to set the linker #1067.

### Fixes
- Fixes to macro context evaluation with macro varargs.
- Dynamic methods registered before init functions on MacOS.
- Fixed clobber on x86 `cpuid` instruction.
- Removed invalid syntax from grammar.y.
- `output` project setting now respected.
- Aliased declarations caused errors when used in initializers.
- Aliased consts used as constant initializers caused errors.
- Exported module names replace `::` by `_`.
- Const ternary would evaluate incorrectly for ?:
- `$$MODULE` would report the incorrect module name in macros.
- Fixed debug info for globals and for/switch scopes.
- `out` now correctly detects subscript[] use.
- Ambiguous recursive imports are now correctly detected.
- Overzealous local escape check corrected #1127.
- Fixes to the matrix functions #1130.

### Stdlib changes
- Deprecated `Allocator` helper functions.
- Added `mem::allocator` functions corresponding to removed allocator functions.
- Changed `mem::new` / `mem::temp_new` to accept an optional initializer, and will clear by default.
- Mem `_clear` and `_zero` variants deprecated. "new_*" functions will clear by default.
- Mem "alloc_*" functions replace old "new_*" behaviour.
- Fixed temp memory issue with formatter.
- Added temp_push and temp_pop for pushing / popping the temp allocator manually (or from C).
- Added byte_size to `List`
- Added `GenericList`.

## 0.5.3 Change list

### Changes / improvements
- Migrate from using actual type with GEP, use i8 or i8 array instead.
- Optimize foreach for single element arrays.
- Move all calls to panic due to checks to the end of the function.

### Fixes
- Single module command line option was not respected.
- Fixed issue with compile time defined types (String in this case), which would crash the compiler in certain cases.
- Projects now correctly respect optimization directives.
- Generic modules now correctly follow the implicit import rules of regular modules.
- Passing an untyped list to a macro and then using it as a vaarg would crash the compiler.
- Extern const globals now work correctly.

### Stdlib changes
- init_new/init_temp deprecated, replaced by new_init and temp_init.

## 0.5.2 Change list

### Changes / improvements
- Allow trailing comma in calls and parameters #1092.

### Fixes
- Fixes issue where single character filenames like 'a.c3' would be rejected.
- Better errors when index type doesn't match len() when doing user defined foreach.
- Fixes to `to_int` for hexadecimal strings.
- Fixed issue when using a generic type from a generic type.
- Bug with vector parameters when the size > 2 and modified.
- Missing error on assigning to in-parameters through subscripting.
- Inference of a vector on the lhs of a binary expression would cause a crash.
- Fixes to PriorityQueue

### Stdlib changes
- Allow `to_int` family functions take a base, parsing base 2-10 and 16.

## 0.5.1 Change list

### Changes / improvements
- Improved error messages for const errors.
- Do not link with debug libraries unless using static libraries.
- Add 'print-linking' build option.
- System linker may be used even if the target arch is different from current.
- Slice -> array/vector works for constant slice lengths.

### Fixes
- On Aarch64 use the correct frame pointer type.
- On Aarch64 macOS, ensure the minimum version is 11.0 (Big Sur)
- Fixes to the yacc grammar.
- Dsym generation on macOS will correctly emit -arch.
- Stacktrace on signals on Linux when backtrace is available.

### Stdlib changes
- `delete` and `delete_range` added to DString.
- `Splitter` iterator added.
- `splitter` and `iterator` String methods.
- `load_new`, `load_buffer` and `load_temp` std::io::file functions.

## 0.5.0 Change List

### Changes / improvements
- Trackable allocator with leak allocation backtraces.
- `$defined` can take a list of expressions.
- `$and` compile time "and" which does not check expressions after the first is an error.
- `$is_const` returns true if an expression is compile time const.
- `$assignable` returns true is an expression may be implicitly cast to a type.
- `$checks` and `@checked` removed, replaced by an improved `$defined`
- Asm string blocks use AT&T syntax for better reliability.
- Distinct methods changed to separate syntax.
- 'exec' directive to run scripts at compile time.
- Project key descriptions in --list command.
- Added `init-lib` to simplify library creation.
- Local `const` work like namespaced global `const`.
- Added `$$atomic_fetch_*` builtins.
- vectors may now contain pointers.
- `void!` does not convert to `anyfault`.
- `$$masked_load` / `$$masked_store` / `$$gather` / `$$scatter` for vector masked load/store.
- `$$select` builtin for vector masked select.
- Added builtin benchmarks by `benchmark`, `compile-benchmark` commands and `@benchmark` attribute.
- Subtype matching in type switches.
- Added parentof typeid property.
- Slice assignment is expanded.
- Enforced optional handling.
- Better dead code analysis, and added dead code errors.
- Exhaustive switches with enums has better analysis.
- Globals may now be initialized with optional values.
- New generic syntax.
- Slice initialization.
- `$feature` for feature flags.
- Native stacktrace for Linux, MacOS and Windows.
- Macro ref parameters are now of pointer type and ref parameters are not assignable.
- Added `nextcase default`.
- Added `$embed` to embed binary data.
- Ad hoc generics are now allowed.
- Allow inferred type on method first argument.
- Fix to void expression blocks
- Temporary objects may now invoke methods using ref parameters.
- Delete object files after successful linking.
- Compile time subscript of constant strings and bytes.
- `@if` introduced, other top level conditional compilation removed.
- Dynamically dispatched interfaces with optional methods.
- `$if` now uses `$if <expr>:` syntax.
- `$assert` now uses `$assert <expr> : <optional message>`
- `$error` is syntax sugar for `$assert false : "Some message"`
- `$include`, `$echo` no longer has mandatory `()` around the arguments.
- `$exec` for including the output of files.
- `assert` no longer allows "try unwrap"
- Updated cpu arguments for x86
- Removed support for ranged case statements that were floats or enums, or non-constant.
- `nextcase` with a constant expression that does not match any case is an error.
- Dropped support for LLVM 13-14.
- Updated grammar and lexer definition.
- Removal of `$elif`.
- any / anyfault may now be aliased.
- `@stdcall` etc removed in favor of `@callconv`
- Empty fault definitions is now an error.
- Better errors on incorrect bitstruct syntax.
- Internal use wildcard type rather than optional wildcard.
- Experimental scaled vector type removed.
- Disallow parameterize attributes without parameters eg `define @Foo() = { @inline }`.
- Handle `@optreturn` contract, renamed `@return!`.
- Restrict interface style functions.
- Optional propagation and assignment '!' and '?' are flipped.
- Add `l` suffix (alias for i64).
- Allow getting the underlying type of anyfault.
- De-duplicate string constants.
- Change @extname => @extern.
- `define` and `typedef` removed.
- `define` is replaced by `def`.
- LLVM "wrapper" library compilation is exception free.
- `private` is replaced by attribute `@private`.
- Addition of `@local` for file local visibility.
- Addition of `@public` for overriding default visibility.
- Default visibility can be overridden per module compile unit. Eg `module foo @private`.
- Optimized macro codegen for -O0.
- Addition of unary `+`.
- Remove possibility to elide length when using ':' for slices.
- Remove the `:` and `;` used in $if, $switch etc.
- Faults have an ordinal.
- Generic module contracts.
- Type inference on enum comparisons, e.g `foo_enum == ABC`.
- Allow {} to initialize basic types.
- String literals default to `String`.
- More const modification detection.
- C3L zip support.
- Support printing object files.
- Downloading of libraries using vendor "fetch".
- Structural casts removed.
- Added "native" option for vector capability.
- `$$shufflevector` replaced with `$$swizzle` and `$$swizzle2`.
- Builtin swizzle accessors.
- Lambdas, e.g `a = int(x, y) => x + y`.
- $$FILEPATH builtin constant.
- `variant` renamed `any`.
- `anyerr` renamed `anyfault`.
- Added `$$wasm_memory_size` and `$$wasm_memory_grow` builtins.
- Add "link-args" for project.
- Possible to suppress entry points using `--no-entry`.
- Added `memory-env` option.
- Use the .wasm extension on WASM binaries.
- Update precedence clarification rules for ^|&.
- Support for casting any expression to `void`.
- Win 32-bit processor target removed.
- Insert null-check for contracts declaring & params.
- Support user defined attributes in generic modules.
- `--strip-unused` directive for small binaries.
- `$$atomic_store` and `$$atomic_load` added.
- `usz`/`isz` replaces `usize` and `isize`.
- `@export` attribute to determine what is visible in precompiled libraries.
- Disallow obviously wrong code returning a pointer to a stack variable.
- Add &^| operations for bitstructs.
- `@noinit` replaces `= void` to opt-out of implicit zeroing.
- Multiple declarations are now allowed in most places, eg `int a, b;`.
- Allow simplified (boolean) bitstruct definitions.
- Allow `@test` to be placed on module declarations.
- Updated name mangling for non-exports.
- `defer catch` and `defer try` statements added.
- Better errors from `$assert`.
- `@deprecated` attribute added.
- Allow complex array length inference, eg `int[*][2][*] a = ...`.
- Cleanup of cast code.
- Removal of `generic` keyword.
- Remove implicit cast enum <-> int.
- Allow enums to use a distinct type as the backing type.
- Update addition and subtraction on enums.
- `@ensure` checks only non-optional results.
- `assert` may now take varargs for formatting.

### Stdlib changes

- Tracking allocator with location.
- `init_new`/`init_temp` for allocating init methods.
- `DString.printf` is now `DString.appendf`.
- Tuple and Maybe types.
- `.as_str()` replaced by `.str_view()`
- Added `math::log(x , base)` and `math::ln(x)`.
- Hashmap keys implicitly copied if copy/free are defined.
- Socket handling.
- `csv` package.
- Many random functions.
- Updated posix/win32 stdlib namespacing
- `process` stdlib
- Stdlib updates to string.
- Many additions to `List`: `remove`, `array_view`, `add_all`, `compact` etc
- Added dstringwriter.
- Improved printf formatting.
- is_finite/is_nam/is_inf added.
- OnStack allocator to easily allocate a stack buffer.
- File enhancements: mkdir, rmdir, chdir.
- Path type for file path handling.
- Distinct `String` type.
- VarString replaced by DString.
- Removal of std::core::str.
- JSON parser and general Object type.
- Addition of `EnumMap`.
- RC4 crypto.
- Matrix identity macros.
- compare_exchange added.
- `printfln` and `println` renamed `printfn` and `printn`.
- Support of roundeven.
- Added easings.
- Updated complex/matrix, added quaternion maths.
- Improved support for freestanding.
- Improved windows main support, with @winmain annotations.
- `SimpleHeapAllocator` added.
- Added win32 standard types.
- Added `saturated` math.
- Added `@expect`, `@unlikely` and `@likely` macros.
- Temp allocator uses memory-env to determine starting size.
- Temp allocator is now accessed using `mem::temp()`, heap allocator using `allocator::heap()`.
- Float parsing added.
- Additions to std::net, ipv4/ipv6 parsing.
- Stream api.
- Random api.
- Sha1 hash function.
- Extended enumset functionality.
- Updated malloc/calloc/realloc/free removing old helper functions.
- Added TrackingAllocator.
- Add checks to prevent incorrect alignment on malloc.
- Updated clamp.
- Added `Clock` and `DateTime`.
- Added posix socket functions.

### Fixes
- Structs returned from macros and then indexed into directly could previously be miscompiled.
- Naked functions now correctly handles `asm`.
- Indexing into arrays would not always widen the index safely.
- Macros with implicit return didn't correctly deduct the return type.
- Reevaluating a bitstruct (due to checked) would break.
- Fix missing comparison between `any`.
- Fix issue of designated initializers containing bitstructs.
- Fix issue of designated initializers that had optional arguments.
- Fixed ++ and -- for bitstructs.
- Fix to bug where library source files were sometimes ignored.
- Types of arrays and vectors are consistently checked to be valid.
- Anonymous bitstructs check of duplicate member names fixed.
- Assignment to anonymous bitstruct members in structs.
- Fix casts on empty initializers.
- Fix to DString reserve.
- Fix where aliases did not do arithmetic promotion.
- @local declarations in generic modules available by accident.
- Fixes missing checks to body arguments.
- Do not create debug declaration for value-only parameter.
- Bug in alignment for atomics.
- Fix to bug when comparing nested arrays.
- Fix to bug when a macro is using rethrow.
- Fixes bug initializing a const struct with a const struct value.
- Fixes bug when `void` is passed to an "any"-vararg.
- Fixed defer/return value ordering in certain cases.
- Fixes to the x64 ABI.
- Updates to how variadics are implemented.
- Fixes to shift checks.
- Fixes to string parsing.
- Bug when rethrowing an optional from a macro which didn't return an optional.
- Fixed issues with ranged cases.
- Disallow trailing ',' in function parameter list.
- Fixed errors on flexible array slices.
- Fix of `readdir` issues on macOS.
- Fix to slice assignment of distinct types.
- Fix of issue casting slices to distinct types.
- Fixes to `split`, `rindex_of`.
- List no longer uses the temp allocator by default.
- Remove test global when not in test mode.
- Fix sum/product on floats.
- Fix error on void! return of macros.
- Removed too permissive casts on slices.
- Using C files correctly places objects in the build folder.
- Fix of overaligned deref.
- Fix negating a float vector.
- Fix where $typeof(x) { ... } would not be a valid compound literal.
- Fix so that using `var` in `if (var x = ...)` works correctly.
- Fix int[] -> void* casts.
- Fix in utf8to16 conversions.
- Updated builtin checking.
- Reduce formatter register memory usage.
- Fixes to the "any" type.
- Fix bug in associated values.
- More RISC-V tests and fixes to the ABI.
- Fix issue with hex floats assumed being double despite `f` suffix.
- Fix of the `tan` function.
- Fixes to the aarch64 ABI when passing invalid vectors.
- Fix creating typed compile time variables.
- Fix bug in !floatval codegen.
- Fix of visibility issues for generic methods.
- Fixes to `$include`.
- Fix of LLVM codegen for optionals in certain cases.
- Fix of `$vasplat` when invoked repeatedly.
- Fix to `$$DATE`.
- Fix of attributes on nested bitstructs.
- Fix comparing const values > 64 bits.
- Defer now correctly invoked in expressions like `return a > 0 ? Foo.ABC! : 1`.
- Fix conversion in `if (int x = foo())`.
- Delay C ABI lowering until requested to prevent circular dependencies.
- Fix issue with decls accidentally invalidated during `$checked` eval.
- Fold optional when casting slice to pointer.
- Fixed issue when using named arguments after varargs.
- Fix bug initializing nested struct/unions.
- Fix of bool -> vector cast.
- Correctly widen C style varargs for distinct types and optionals.
- Fix of too aggressive codegen in ternary codegen with array indexing.

## 0.4.0 Change List

- Compatibility with LLVM 16.
- Dropped LLVM 12 support.
- Updated vector comparisons.
- Built in unit testing with @test and compile-test
- Updated memory allocators. Added `@scoped` and `@pool` macros.
- Various bug fixes.
- Generic modules may now be generic over integers and booleans.
- Constant pointers may be compile time evaluated.
- Added many new builtins.
- Emit asm using `--emit-asm`.
- Added `--nostdlib` and `--nolibc`.
- Compiling for AVX can now select "native".
- Fixes to adding libraries at link time.
- Various improved error messages.
- Windows debug info fixes.
- Add of `foreach_r` for reverse list traversal.
- Script downloading the MSVC SDK to cross compile to windows.
- Many standard library additions.
- Extension methods may be added for built-in types.
- Macros may take vector and array arguments generic over length.
- Macro varargs with $vaarg, $vacount etc.
- Many vector builtins added as dot methods.
- in / out / inout doc parameters checked.
- Initial inline asm support for aarch64 and x64.
- Single line short function declaration.
- Added `$checks` builtin.
- Added `$include` builtin for including other text files.
- Optional single module compilation.
- Static initialization / finalization to have code running at start/end.
- C3 custom printf function in the stdlib.
- `[]=` overload now works correctly.
- Static libraries may now be built.
- More compile time reflection added and general cleanup done.
- usize/isize/iptrdiff/uptrdiff replaced by usz/isz.
- Add `var` to allow type inference on regular variables.
- LLVM codegen optimizations.
- `??` now allows chaining another optional.
- int128 support on all platforms.
- `import` is now allowed anywhere at the top level.
- `project.c3p` renamed `project.json`
- Update to project properties, e.g. "libs" -> "dependencies" etc.
- $$TIME, $$DATE and $$FUNCTION builtin defines added.
- `$echo` function to print messages at compile time.
- Improvements to untyped lists.
- Various builtins added: $$prefetch, $$reverse, $$shufflevector etc.

## 0.3.0 Change List

### Changes / improvements:

- Allow any expression as default expression.
- Allow using enums for indexing arrays.
- Added $convertable / $castable compile time functions.
- Removed ´func´ deprecated keyword
- Slicing a distinct type now returns the distinct type.
- Renamed @autoimport -> @builtin
- Zero length arrays not allowed
- Allow methods may use ref, pointer and value arguments as "self"
- Updated external name mangling
- More advanced introspection.
- @maydiscard and @nodiscard annotations
- New type promotion rules: The common type of int[x]* and int[y]* is int[]
- Added type.inner and type.len reflection.
- Support float mod operations.
- Add float.max/min.
- Allow [in] contract to be used on slices.
- Add linker and linked dir arguments to build files.
- Auto-import std::core.
- LLVM 15 support.
- Beter native file handling for MSVC
- New import rules – recursive imports
- Add lld linking for FreeBSD
- User defined attributes. @Foo = @inline
- Support enum associated values.
- @ is now part of the name of an attribute or a macro. Macros without '@' must be function-like.
- Ordinal based enums.
- Allow locals to shadow global variables.
- Prefer inferred constant over global in the case of MyEnum f = BAR;
- Enum and fault name reflection.
- Deref null error now panics in safe mode.

### Changes to stdlib:

- Updated allocators.
- Added enum_by_name.
- Moved bitcast to builtin module.
- Native printf for files and strings.
- Updated String.
- Comparison macros
- Added Binary-Heap Based Priority Queue by David Kopec
- Matrix Math Library by PixelRifts
- UTF conversions in "conv" module.

### Fixes:

- Attributes correctly checks for recursive definitions now.
- Added a max bitstruct size.
- Fix of expr location in args.
- Fixing distinct, typedef and bitstruct copying. Fix where global constants did not need to be constant.
- Better error on all upper parameter names.
- Fix constant typeid comparisons.
- Simplify and corrected if try/catch parsing.
- Fix bug with { [A] = 1 }
- Conversion unsigned int -> enum fixed.
- Fix bug preventing implicit & on optionals.
- More efficient int[] a = {}
- Fix bug in extension methods for generic types and typedefs
- Fix to extension methods in other modules.
- Disallow complist as a regular macro parameter.
- Fix in nested block handling
- Fix of error where {| |} with value return could have no final return
- Vararg abi fix
- Fix "libs" in project creation
- Fix bug with bit struct initialization and zeros
- Reduce size of memory pages used.
- Fix issues with union of const.
- Fix initialization of anonymous structs.
- Fix conversion between distinct void* and null
- Fix of default project creation target format.
- Fix of $sizeof(Type) => Type.sizeof
- Fix stack setting after error return.
- Fix module assignment of declarations
- Global @align fixed
- Fixes enum set with new ordinal based enums
- SysV ABI fix for passing certain things by struct.
- Fix implicitly converting to float in the case of myfloat *= -1
