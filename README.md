# C3 Language

C3 is a C-like language striving to be an evolution of C, rather than a 
completely new language. As an alternative in the C/C++ niche it
aims to be fast and close to the metal.

The manual for C3 can be found at [www.c3-lang.org](http://www.c3-lang.org).


### Design Principles
- Procedural "get things done"-type of language.
- Try to stay close to C - only change what's really necessary.
- C ABI compatibility and excellent C integration.
- Learning C3 should be easy for a C programmer.
- Data is inert.
- Avoid "big ideas" & the "more is better" fallacy.
- Introduce some higher level conveniences where the value is great.

C3 owes its inspiration to the [C2 language](http://c2lang.org): to iterate on top of C without trying to be a
whole new language.

### Example code

The following code shows some of the syntactic changes from C.

Create a `main.c3` file with:
```c++
module hello_world;
import std::io;

fn void main()
{
   io::println("Hello, world!");
}
```

Make sure you have the standard libraries at either `../lib/std/` or `/lib/std/`.

Then run
```sh
c3c compile main.c3
```

The generated binary will be called `a.out`.


### In what ways do C3 differ from C?

- No mandatory header files
- New semantic macro system
- Module based name spacing
- Subarrays (slices)
- Compile time reflection
- Enhanced compile time execution
- Generics based on generic modules
- "Result"-based zero overhead error handling
- Defer
- Value methods
- Associated enum data
- No preprocessor
- Less undefined behaviour and runtime checks in "safe" mode
- Limited operator overloading to enable userland dynamic arrays
- Optional pre and post conditions

### Current status

It's possible to try out the current C3 compiler in the browser: https://ide.judge0.com/ – this is courtesy of the
developer of Judge0. 

Design work on C3 is mostly done, but there are some areas that are unfinished, such
as inline asm. Follow the issues [here](https://github.com/c3lang/c3c/issues).

If you have any suggestions on how to improve the language, either [file an issue](https://github.com/c3lang/c3c/issues) 
or discuss C3 on its dedicated Discord: [https://discord.gg/qN76R87](https://discord.gg/qN76R87).

The compiler should compile on Linux, Windows (under MSVC, Mingw or MSYS2) and MacOS, 
but needs some install documentation for Windows. 

Due to its ABI compatibility with C, it's possible to mix C and C3 in the same project.
As a demonstration, vkQuake was compiled with a small portion of the code converted
to C3 and compiled with the c3c compiler:

![vkQuake](https://github.com/c3lang/c3c/blob/master/resources/images/vkQuake.png?raw=true)

(The vkFork is at https://github.com/c3lang/vkQuake)

#### What can you help with?

- If you wish to contribute with ideas, please file issues or discuss on Discord.
- Interested in contributing to the stdlib? Please get in touch on Discord.
- Are you a Windows dev and know your way around Github CI? Please help us get MSVC CI working!
- Install instructions for other Linux and Unix variants are appreciated.
- Would you like to contribute bindings to some library? It would be nice to have support for SDL, Raylib and more.
- Build something with C3 and show it off and give feedback. The language is still open for significant tweaks.
- Start work on the C -> C3 converter which takes C code and does a "best effort" to translate it to C3. The first version only needs to work on C headers.
- Do you have some specific area you have deep knowledge of and could help make C3 even better at doing? File or comment on issues.

### Installing

#### Installing on Ubuntu 20.10

1. Make sure you have a C compiler that handles C11 and a C++ compiler, such as GCC or Clang. Git also needs to be installed.
2. Install CMake: `sudo apt install cmake`
3. Install LLVM 11: `sudo apt-get install clang-11 zlib1g zlib1g-dev libllvm11 llvm-11 llvm-11-dev llvm-11-runtime liblld-11-dev liblld-11`
4. Clone the C3C github repository: `git clone https://github.com/c3lang/c3c.git`
5. Enter the C3C directory `cd c3c`.
6. Create a build directory `mkdir build`
7. Change directory to the build directory `cd build`
8. Set up CMake build for debug: `cmake -DLLVM_DIR=/usr/lib/llvm-11/cmake -DCMAKE_BUILD_TYPE=Debug ..`
9. Build: `cmake --build .`

You should now have a `c3c` executable.

You can try it out by running some sample code: `./c3c compile ../resources/examples/hash.c3`

#### Building via Docker

You can build `c3c` using either an Ubuntu 18.04 or 20.04 container:

```
./build-with-docker.sh 18
```

Replace `18` with `20` to build through Ubuntu 20.04.

For a release build specify:
```
./build-with-docker.sh 20 Release
```

A `c3c` executable will be found under `bin/`.

#### Installing on OS X using Homebrew

2. Install CMake: `brew install cmake`
3. Install LLVM 13: `brew install llvm`
4. Clone the C3C github repository: `git clone https://github.com/c3lang/c3c.git`
5. Enter the C3C directory `cd c3c`.
6. Create a build directory `mkdir build`
7. Change directory to the build directory `cd build`
8. Set up CMake build for debug: `cmake ..`
9. Build: `cmake --build .`
