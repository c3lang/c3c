# C3C Release Notes

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
- `$expand` macro, to expand a string into code.
- && doesn't work correctly with lambdas #1279.
- Fix incorrect override of optimization levels when using projects.
- Add experimental `@noalias` attribute.
- Add a `--run-once` option to delete the output file after running it.
- Add `@const` attribute for macros, for better error messages with constant macros.
- Add `wincrt` setting to libraries.

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

### Stdlib changes

None

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
