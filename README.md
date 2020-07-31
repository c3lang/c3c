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
- Dare introducing some conveniences not "close to metal" if the value is great.


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

Design work is still being done in the design draft here: https://c3lang.github.io/c3docs/. If you have suggestions, send a mail to [christoffer@aegik.com](mailto:christoffer@aegik.com), [file an issue](https://github.com/c3lang/c3c/issues) or discuss C3 on the r/ProgrammingLanguages Discord server: https://discord.gg/cfu4wdk


#### What's currently missing

- `asm` sections.
- bitstructs
- array range initializers e.g. `{ [1..2] = 2 }`
- `assert` - with compiler hint
- `$switch` `$for` - compile time iteration / switch
- Pre/post conditions
- `generic` - explicit overloading
- `malloc` / `free`
- `string` not fully implemented
- vararrays, e.g. `int[*]` not working
- `unreachable` for compiler hinting
- Generic modules
- Stdlib not linked.

Also see: https://github.com/c3lang/c3c/issues

#### What's working?

- Lexing/parsing/semantic analysis/codegen.
- "Regular code" should mostly work.
- You can use any C function by declaring it as a normal C3 function with external 

#### What can you help with?

- If you wish to contribute with ideas, please file issues on the c3docs: https://github.com/c3lang/c3docs instead of the compiler.
- Discuss the language on discord to help iron out syntax.
- Stdlib work will soon start, do you want to help out building the C3 std lib?
- Do you want do do real compiler work? Everyone is welcome to contribute.