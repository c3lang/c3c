# C3C Release Notes

## 0.6.7 Change list

### Changes / improvements
- None yet.

### Fixes
- Fix issue requiring prefix on a generic interface declaration.
- Fix bug in SHA1 for longer blocks #1854.
- Fix lack of location for reporting lambdas with missing return statement #1857.
- Compiler allows a generic module to be declared with different parameters #1856.

### Stdlib changes
- Added '%h' and '%H' for printing out binary data in hexadecimal using the formatter.
- Added weakly linked `__powidf2`

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
