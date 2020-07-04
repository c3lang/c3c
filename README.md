# C3 Language

C3 is a C-like language trying to be "an incremental improvement over C" rather than a whole new language. 
C3 owes a lot to the ideas of the [C2 language](http://c2lang.org): to iterate on top of C without trying to be a 
whole new language.

C3 tries to be an alternative in the the C/C++ niche: fast and close to the metal.

### Design Principles
- Procedural "get things done"-type of language.
- Try to stay close to C - only change where truly needed.
- C ABI compatibility and C C integration.
- Learning C3 should be easy for a C programmer.
- Dare violating the "close to metal" principle if the value is great.
- Data is inert.
- Avoid "big ideas".
- Avoid the kitchen sink language trap.

### Current status

It's possible to try out the current C3 compiler in the browser: https://ide.judge0.com/?1EFo – this is courtesy of the
developer of Judge0. 

Design work is still being done in the design draft here: https://c3lang.github.io/c3docs/. If you have suggestions, send a mail to [christoffer@aegik.com](mailto:christoffer@aegik.com), [file an issue](https://github.com/c3lang/c3c/issues) or discuss C3 on the r/ProgrammingLanguages Discord server: https://discord.gg/cfu4wdk

There are some small work being done on the parser here, but most of the structure is still missing:

#### What's missing in the parser

- `asm` sections.
- bitstructs
- array range initializers e.g. `{ [1..2] = 2 }`
- assert/$assert as keywords
- Docs not linked to statements/functions/declarations.

#### What's missing in the semantic analyser

- Incomplete handling of imports.
- Function signatures incomplete.
- Function typedef not done.
- `asm` not done.
- `generic` not analysed.
- `$switch` and `$for` not handled.
- Enums not correctly handled.
- Type resolution not complete for all types.
- Macro call not handled completely.

#### What's missing overall

- Improved integration with C.
- Generic macros.
- Update of error system

#### What's working?

- Lexing and parsing works (except for the exceptions noted above).
- Simple "hello world"
- Most simpler C code.

(For more details see missing.txt)

If you wish to contribute with ideas, please file issues on the c3docs: https://github.com/c3lang/c3docs instead of the compiler.
