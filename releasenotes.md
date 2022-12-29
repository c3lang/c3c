Release Notes

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
- Allow [in] contract to be used on subarray types.
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
