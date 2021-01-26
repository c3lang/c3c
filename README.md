# C3 Language

C3 is a C-like language trying to be "an incremental improvement over C" rather than a whole new language. 
C3 owes a lot to the ideas of the [C2 language](http://c2lang.org): to iterate on top of C without trying to be a 
whole new language.

C3 tries to be an alternative in the the C/C++ niche: fast and close to the metal.

### Design Principles
- Procedural "get things done"-type of language.
- Try to stay close to C - only change where truly needed.
- C ABI compatibility and excellent C integration.
- Learning C3 should be easy for a C programmer.
- Data is inert.
- Avoid "big ideas" & the "more is better" fallacy.
- Introduce some higher level conveniences where the value is great.

### Example code

```c++
module hello_world;
import std::io;

func void main()
{
   io::printf("Hello, world!\n");
}
```

### In what ways do C3 differ from C?

- No mandatory header files
- New semantic macro system
- Generic modules
- Module based
- Subarrays (slices) and vararrays built in
- Compile time reflection
- Enhanced compile time execution
- "Result" based zero overhead error handling
- Defer
- Value methods
- Associated enum data
- Built in strings
- No preprocessor
- Undefined behaviour trapped on debug by default
- Optional pre and post conditions

### Current status

It's possible to try out the current C3 compiler in the browser: https://ide.judge0.com/?1EFo – this is courtesy of the
developer of Judge0. 

Design work is still being done in the design draft here: https://c3lang.github.io/c3docs/. If you have any suggestions, send a mail to [christoffer@aegik.com](mailto:christoffer@aegik.com), [file an issue] (https://github.com/c3lang/c3c/issues) or discuss 
C3 on its dedicated Discord: https://discord.gg/qN76R87
you are also welcome to discuss in the C3 channel on 
the r/ProgrammingLanguages Discord: https://discord.gg/cfu4wdk


#### Todo / done

- [x] For/while/do
- [x] `if`/ternary
- [x] Structs
- [x] Union
- [x] Enums
- [x] Value methods
- [x] Compound literals
- [x] Designated initalizers
- [x] Slicing syntax
- [x] Arrays and subarrays
- [x] Modules
- [x] `$unreachable`
- [x] Compile time assert with `$assert`
- [x] Compiler guiding `assert` 
- [x] C code calling by declaring methods `extern`
- [x] Compile time variables
- [x] Basic macros
- [x] 4cc, 8cc, 2cc
- [x] Enum type inference in switch/assignment
- [x] Integer type inference
- [x] Error type
- [x] Failable error handling
- [x] `try` for conditional execution
- [x] `catch` for error handling
- [x] Implicit unwrap after `catch`
- [x] `sizeof`
- [x] `typeof`
- [x] 2s complement wrapping operators
- [x] Labelled break / continue
- [x] `next` statement
- [x] Expression blocks
- [x] Do-without-while
- [x] Foreach statement
- [ ] All attributes
- [ ] Associative array literals
- [ ] CT type constants
- [ ] Reflection methods
- [ ] Anonymous structs
- [x] Distinct types
- [ ] LTO/ThinLTO setup
- [ ] Built-in linking
- [ ] `global` / `shared` for globals 
- [ ] Complex macros
- [x] CT only macros evaluating to constants
- [ ] Escape macros
- [ ] Implicit capturing macros
- [ ] Trailing body macros
- [ ] Subarray initializers
- [x] range initializers e.g. `{ [1..2] = 2 }`
- [ ] Bitstructs
- [ ] `asm` section
- [ ] `$switch`
- [ ] `$for`
- [ ] Pre-post conditions
- [ ] Stdlib inclusion
- [ ] Generic modules
- [ ] String functions
- [ ] Vararrays e.g. `int[*]`
- [ ] Compile time incremental arrays
- [ ] Complete C ABI conformance *in progress*
- [ ] Generic functions
- [ ] Debug info *in progress*
- [ ] Simd vector types
- [ ] Complex types

#### What can you help with?

- If you wish to contribute with ideas, please file issues on the c3docs: https://github.com/c3lang/c3docs instead of the compiler.
- Discuss the language on discord to help iron out syntax.
- Stdlib work will soon start, do you want to help out building the C3 std lib?
- Do you want to do real compiler work? Everyone is welcome to contribute.
