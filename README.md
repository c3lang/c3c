# C3 Language

C3 is a programming language that builds on the syntax and semantics of the C language,
with the goal of evolving it while still retaining familiarity for C programmers. 

It's an evolution, not a revolution: the C-like 
for programmers who like C.

Precompiled binaries for the following operating systems are available:
 
- Windows x64 [download](https://github.com/c3lang/c3c/releases/download/latest/c3-windows.zip), [install instructions](#installing-on-windows-with-precompiled-binaries).
- Debian x64 [download](https://github.com/c3lang/c3c/releases/download/latest/c3-linux.tar.gz), [install instructions](#installing-on-debian-with-precompiled-binaries).
- Ubuntu x86 [download](https://github.com/c3lang/c3c/releases/download/latest/c3-ubuntu-20.tar.gz), [install instructions](#installing-on-ubuntu-with-precompiled-binaries).
- MacOS Arm64 [download](https://github.com/c3lang/c3c/releases/download/latest/c3-macos.zip), [install instructions](#installing-on-mac-with-precompiled-binaries).

The manual for C3 can be found at [www.c3-lang.org](http://www.c3-lang.org).

![vkQuake](https://github.com/c3lang/c3c/blob/master/resources/images/vkQuake.png?raw=true)

Thanks to full ABI compatibility with C, it's possible to mix C and C3 in the same project with no effort. As a demonstration, vkQuake was compiled with a small portion of the code converted to C3 and compiled with the c3c compiler. (The fork can be found at https://github.com/c3lang/vkQuake)

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

The following code shows [generic modules](https://c3-lang.org/references/docs/generics/) (more examples can be found at https://c3-lang.org/references/docs/examples/). 

```cpp
module stack (<Type>);
// Above: the parameterized type is applied to the entire module.

struct Stack
{
    usz capacity;
    usz size;
    Type* elems;
}

// The type methods offers dot syntax calls,
// so this function can either be called 
// Stack.push(&my_stack, ...) or
// my_stack.push(...)
fn void Stack.push(Stack* this, Type element)
{
    if (this.capacity == this.size)
    {
        this.capacity *= 2;
        if (this.capacity < 16) this.capacity = 16;
        this.elems = realloc(this.elems, Type.sizeof * this.capacity);
    }
    this.elems[this.size++] = element;
}

fn Type Stack.pop(Stack* this)
{
    assert(this.size > 0);
    return this.elems[--this.size];
}

fn bool Stack.empty(Stack* this)
{
    return !this.size;
}
```

Testing it out:

```cpp
import stack;

// Define our new types, the first will implicitly create 
// a complete copy of the entire Stack module with "Type" set to "int"
def IntStack = Stack(<int>);
// The second creates another copy with "Type" set to "double"
def DoubleStack = Stack(<double>);

// If we had added "define IntStack2 = Stack(<int>)"
// no additional copy would have been made (since we already
// have an parameterization of Stack(<int>)) so it would
// be same as declaring IntStack2 an alias of IntStack

// Importing an external C function is straightforward
// here is an example of importing libc's printf:
extern fn int printf(char* format, ...);

fn void main()
{
    IntStack stack;
    // Note that C3 uses zero initialization by default
    // so the above is equivalent to IntStack stack = {};
    
    stack.push(1);
    // The above can also be written IntStack.push(&stack, 1); 
    
    stack.push(2);
    
    // Prints pop: 2
    printf("pop: %d\n", stack.pop());
    // Prints pop: 1
    printf("pop: %d\n", stack.pop());
    
    DoubleStack dstack;
    dstack.push(2.3);
    dstack.push(3.141);
    dstack.push(1.1235);
    // Prints pop: 1.123500
    printf("pop: %f\n", dstack.pop());
}
```

### In what ways does C3 differ from C?

- No mandatory header files
- New semantic macro system
- Module based name spacing
- Slices
- Compile time reflection
- Enhanced compile time execution
- Generics based on generic modules
- "Result"-based zero overhead error handling
- Defer
- Value methods
- Associated enum data
- No preprocessor
- Less undefined behaviour and added runtime checks in "safe" mode
- Limited operator overloading to enable userland dynamic arrays
- Optional pre and post conditions

### Current status

The current stable version of the compiler is **version 0.6.6**.

The upcoming 0.6.x releases will focus on expanding the standard library.
Follow the issues [here](https://github.com/c3lang/c3c/issues).

If you have suggestions on how to improve the language, either [file an issue](https://github.com/c3lang/c3c/issues) 
or discuss C3 on its dedicated Discord: [https://discord.gg/qN76R87](https://discord.gg/qN76R87).

The compiler is currently verified to compile on Linux, Windows and MacOS.

**Support matrix**

| Platform                 | Native C3 compiler available? | Target supported        | Stack trace | Threads  | Sockets  | Inline asm |
|--------------------------|-------------------------------|-------------------------|-------------|----------|----------|------------|
| Win32 x64                | Yes                           | Yes + cross compilation | Yes         | Yes      | Yes      | Yes*       |
| Win32 Aarch64            | Untested                      | Untested                | Untested    | Untested | Untested | Yes*       |
| MacOS x64                | Yes                           | Yes + cross compilation | Yes         | Yes      | Yes      | Yes*       |
| MacOS Aarch64            | Yes                           | Yes + cross compilation | Yes         | Yes      | Yes      | Yes*       |
| iOS Aarch64              | No                            | Untested                | Untested    | Yes      | Yes      | Yes*       |
| Linux x86                | Yes                           | Yes                     | Yes         | Yes      | Yes      | Yes*       |
| Linux x64                | Yes                           | Yes                     | Yes         | Yes      | Yes      | Yes*       |
| Linux Aarch64            | Yes                           | Yes                     | Yes         | Yes      | Yes      | Yes*       |
| Linux Riscv32            | Yes                           | Yes                     | Yes         | Yes      | Yes      | Untested   |
| Linux Riscv64            | Yes                           | Yes                     | Yes         | Yes      | Yes      | Untested   |
| ELF freestanding x86     | No                            | Untested                | No          | No       | No       | Yes*       |
| ELF freestanding x64     | No                            | Untested                | No          | No       | No       | Yes*       |
| ELF freestanding Aarch64 | No                            | Untested                | No          | No       | No       | Yes*       |
| ELF freestanding Riscv64 | No                            | Untested                | No          | No       | No       | Untested   |
| ELF freestanding Riscv32 | No                            | Untested                | No          | No       | No       | Untested   |
| FreeBSD x86              | Untested                      | Untested                | No          | Yes      | Untested | Yes*       |
| FreeBSD x64              | Untested                      | Untested                | No          | Yes      | Untested | Yes*       |
| NetBSD x86               | Untested                      | Untested                | No          | Yes      | Untested | Yes*       |
| NetBSD x64               | Untested                      | Untested                | No          | Yes      | Untested | Yes*       |
| OpenBSD x86              | Untested                      | Untested                | No          | Yes      | Untested | Yes*       |
| OpenBSD x64              | Untested                      | Untested                | No          | Yes      | Untested | Yes*       |
| MCU x86                  | No                            | Untested                | No          | No       | No       | Yes*       |
| Wasm32                   | No                            | Yes                     | No          | No       | No       | No         |
| Wasm64                   | No                            | Untested                | No          | No       | No       | No         |

*\* Inline asm is still a work in progress*

More platforms will be supported in the future.

#### What can you help with?

- If you wish to contribute with ideas, please file issues or discuss on Discord.
- Interested in contributing to the stdlib? Please get in touch on Discord.
- Compilation instructions for other Linux and Unix variants are appreciated.
- Would you like to contribute bindings to some library? It would be nice to have support for SDL, Raylib and more.
- Build something with C3 and show it off and give feedback. The language is still open for significant tweaks.
- Start work on the C -> C3 converter which takes C code and does a "best effort" to translate it to C3. The first version only needs to work on C headers.
- Do you have some specific area you have deep knowledge of and could help make C3 even better at doing? File or comment on issues.

### Installing

#### Installing on Windows with precompiled binaries
1. Download the zip file: [https://github.com/c3lang/c3c/releases/download/latest/c3-windows.zip](https://github.com/c3lang/c3c/releases/download/latest/c3-windows.zip) 
(debug version [here](https://github.com/c3lang/c3c/releases/download/latest/c3-windows-debug.zip))
2. Unzip exe and standard lib.
3. If you don't have Visual Studio 17 installed you can either do so, or run the `msvc_build_libraries.py` Python script which will download the necessary files to compile on Windows. 
4. Run `c3c.exe`.

#### Installing on Debian with precompiled binaries
1. Download tar file: [https://github.com/c3lang/c3c/releases/download/latest/c3-linux.tar.gz](https://github.com/c3lang/c3c/releases/download/latest/c3-linux.tar.gz)
   (debug version [here](https://github.com/c3lang/c3c/releases/download/latest/c3-linux-debug.tar.gz))
2. Unpack executable and standard lib.
3. Run `./c3c`.

#### Installing on Ubuntu with precompiled binaries
1. Download tar file: [https://github.com/c3lang/c3c/releases/download/latest/c3-ubuntu-20.tar.gz](https://github.com/c3lang/c3c/releases/download/latest/c3-ubuntu-20.tar.gz)
   (debug version [here](https://github.com/c3lang/c3c/releases/download/latest/c3-ubuntu-20-debug.tar.gz))
2. Unpack executable and standard lib.
3. Run `./c3c`.

#### Installing on MacOS with precompiled binaries
1. Make sure you have XCode with command line tools installed.
2. Download the zip file: [https://github.com/c3lang/c3c/releases/download/latest/c3-macos.zip](https://github.com/c3lang/c3c/releases/download/latest/c3-macos.zip)
   (debug version [here](https://github.com/c3lang/c3c/releases/download/latest/c3-macos-debug.zip))
3. Unzip executable and standard lib.
4. Run `./c3c`.

(*Note that there is a known issue with debug symbol generation on MacOS 13, see [issue #1086](https://github.com/c3lang/c3c/issues/1086))

#### Installing on Arch Linux
Arch includes c3c in the official 'extra' repo. It can be easily installed the usual way:

```sh
sudo pacman -S c3c
# or paru -S c3c
# or yay -S c3c
# or aura -A c3c
```

There is also an AUR package for the c3c compiler : [c3c-git](https://aur.archlinux.org/packages/c3c-git).

You can use your AUR package manager:
```sh
paru -S c3c-git
# or yay -S c3c-git
# or aura -A c3c-git
```

Or clone it manually:
```sh
git clone https://aur.archlinux.org/c3c-git.git
cd c3c-git
makepkg -si
```

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
3. Install LLVM 17+: `brew install llvm`
4. Clone the C3C github repository: `git clone https://github.com/c3lang/c3c.git`
5. Enter the C3C directory `cd c3c`.
6. Create a build directory `mkdir build`
7. Change directory to the build directory `cd build`
8. Set up CMake build for debug: `cmake ..`
9. Build: `cmake --build .`

#### Getting started with a "hello world"

Create a `main.c3` file with:
```c++
module hello_world;
import std::io;

fn void main()
{
   io::printn("Hello, world!");
}
```

Make sure you have the standard libraries at either `../lib/std/` or `/lib/std/`.

Then run
```sh
c3c compile main.c3
```

The generated binary will by default be named after the module that contains the main
function. In our case that is `hello_world`, so the resulting binary will be
called `hello_world` or `hello_world.exe`depending on platform.

### Compiling

#### Compiling on Windows

1. Make sure you have Visual Studio 17 2022 installed or alternatively install the "Buildtools for Visual Studio" (https://aka.ms/vs/17/release/vs_BuildTools.exe) and then select "Desktop development with C++"
2. Install CMake
3. Clone the C3C github repository: `git clone https://github.com/c3lang/c3c.git`
4. Enter the C3C directory `cd c3c`.
5. Set up the CMake build `cmake -B build -G "Visual Studio 17 2022" -A x64 -DCMAKE_BUILD_TYPE=Release`
6. Build: `cmake --build build --config Release`
7. You should now have the c3c.exe

You should now have a `c3c` executable.

You can try it out by running some sample code: `c3c.exe compile ../resources/examples/hash.c3`

*Note that if you run into linking issues when building, make sure that you are using the latest version of VS17.*


#### Compiling on Ubuntu 24.04 LTS

1. Make sure you have a C compiler that handles C11 and a C++ compiler, such as GCC or Clang. Git also needs to be installed.
2. Install LLVM 18 `sudo apt-get install cmake git clang zlib1g zlib1g-dev libllvm18 llvm llvm-dev llvm-runtime liblld-dev liblld-18 libpolly-18-dev`
3. Clone the C3C github repository: `git clone https://github.com/c3lang/c3c.git`
4. Enter the C3C directory `cd c3c`.
5. Create a build directory `mkdir build`
6. Change directory to the build directory `cd build`
7. Set up CMake build: `cmake ..`
8. Build: `cmake --build .`

You should now have a `c3c` executable.

You can try it out by running some sample code: `./c3c compile ../resources/examples/hash.c3`


#### Compiling on Void Linux

1. As root, ensure that all project dependencies are installed: `xbps-install git cmake llvm17 llvm17-devel lld17-devel libcurl-devel ncurses-devel zlib-devel libzstd-devel libxml2-devel`
2. Clone the C3C repository: `git clone https://github.com/c3lang/c3c.git`
    - If you only need the latest commit, you may want to make a shallow clone instead: `git clone https://github.com/c3lang/c3c.git --depth=1`
3. Enter the directory: `cd c3c`
4. Create a build directory: `mkdir build`
5. Enter the build directory: `cd build`
6. Create the CMake build cache: `cmake ..`
7. Build: `cmake --build .`

Your c3c executable should have compiled properly. You may want to test it: `./c3c compile ../resources/examples/hash.c3`  
For a sytem-wide installation, run the following as root: `cmake --install .`


#### Compiling on Fedora

1. Install required project dependencies: `dnf install cmake clang git llvm llvm-devel lld lld-devel ncurses-devel`
2. Optionally, install additional dependencies: `dnf install libcurl-devel zlib-devel libzstd-devel libxml2-devel libffi-devel`
3. Clone the C3C repository: `git clone https://github.com/c3lang/c3c.git`
    - If you only need the latest commit, you may want to make a shallow clone: `git clone https://github.com/c3lang/c3c.git --depth=1`
4. Enter the C3C directory: `cd c3c`
5. Create a build directory and navigate into it: `mkdir build && cd build`
6. Create the CMake build cache. The Fedora repositories provide `.so` libraries for lld, so you need to set the C3_LINK_DYNAMIC flag: `cmake .. -DC3_LINK_DYNAMIC=1`
7. Build the project: `cmake --build .`

The c3c binary should be created in the build directory. You can try it out by running some sample code: `./c3c compile ../resources/examples/hash.c3`


#### Compiling on other Linux / Unix variants

1. Install CMake.
2. Install or compile LLVM and LLD *libraries* (version 17+ or higher)
3. Clone the C3C github repository: `git clone https://github.com/c3lang/c3c.git`
4. Enter the C3C directory `cd c3c`.
5. Create a build directory `mkdir build`
6. Change directory to the build directory `cd build`
7. Set up CMake build for debug: `cmake ..`. At this point you may need to manually 
provide the link path to the LLVM CMake directories, e.g. `cmake -DLLVM_DIR=/usr/local/opt/llvm/lib/cmake/llvm/ ..`
8. Build: `cmake --build .`

*A note on compiling for Linux/Unix/MacOS: to be able to fetch vendor libraries
libcurl is needed. The CMake script should detect it if it is available. Note that
this functionality is non-essential and it is perfectly fine to user the compiler without it.*

#### Licensing

The C3 compiler is licensed under LGPL 3.0, the standard library itself is
MIT licensed.

#### Editor plugins

Editor plugins can be found at https://github.com/c3lang/editor-plugins.

#### Contributing unit tests

1. Write the test, either adding to existing test files in `/test/unit/` or add
   a new file. (If testing the standard library, put it in the `/test/unit/stdlib/` subdirectory).
2. Make sure that the test functions have the `@test` attribute. 
3. Run tests and see that they pass. (Recommended settings: `c3c compile-test -O0 test/unit`.
   - in this example `test/unit/` is the relative path to the test directory, so adjust as required)
4. Make a pull request for the new tests.

## Thank yous

A huge **THANK YOU** goes out to all contributors and sponsors.

A special thank you to sponsors [Caleb-o](https://github.com/Caleb-o) and [devdad](https://github.com/devdad) for going the extra mile.
