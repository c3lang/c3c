# C3 Language
<img src="https://c3-lang.org/logo.svg" align="right" height="120" />

C3 is a programming language that builds on the syntax and semantics of the C language,
with the goal of evolving it while still retaining familiarity for C programmers.

It's an evolution, not a revolution: the C-like
for programmers who like C.

Precompiled binaries for the following operating systems are available:

- Windows x64 [download](https://github.com/c3lang/c3c/releases/latest/download/c3-windows.zip), [install instructions](#windows).
- Linux x64 [download](https://github.com/c3lang/c3c/releases/latest/download/c3-linux-static.tar.gz), [install instructions](#linux).
- MacOS Arm64 [download](https://github.com/c3lang/c3c/releases/latest/download/c3-macos.zip), [install instructions](#macos).
- OpenBSD x64 [download](https://github.com/c3lang/c3c/releases/latest/download/c3-openbsd.tar.gz), [install instructions](#openbsd).

To build the compiler from source instead, see the [instructions for building C3 from source](#building-c3-from-source).

<h3 align="center"><a href="https://c3-lang.org">[Read the C3 Manual]</a></h3>

---

<img src="https://github.com/c3lang/c3c/blob/master/resources/images/vkQuake.png?raw=true" align="right" hspace="50" alt="vkQuake">

> [!NOTE]
> Thanks to full ABI compatibility with C, it's possible to mix C and C3 in the same project with no effort.
> As a demonstration, vkQuake was compiled with a small portion of the code converted to C3 and compiled with the c3c compiler.
> (The aging fork can be found at https://github.com/c3lang/vkQuake)
<br clear="all">

A non-curated list of user written projects and other resources can be found [here](https://github.com/c3lang/c3-showcase).

---

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

The following code shows [generics](https://c3-lang.org/generic-programming/generics/) (more examples can be found at https://c3-lang.org/language-overview/examples/).

```c3
module stack <Type>;
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

```c3
import stack;

// Define our new types, the first will implicitly create
// a complete copy of the entire Stack module with "Type" set to "int"
alias IntStack = Stack {int};
// The second creates another copy with "Type" set to "double"
alias DoubleStack = Stack {double};

// If we had added "alias IntStack2 = Stack {int}"
// no additional copy would have been made (since we already
// have an parameterization of Stack {int} so it would
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
- Operator overloading
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

The current stable version of the compiler is **version 0.7.11**.

The upcoming 0.8.0 release will introduce breaking changes, including the removal of deprecated functionality and a major transition from unsigned to signed sizes by default.
Follow the issues [here](https://github.com/c3lang/c3c/issues).

If you have suggestions on how to improve the language, either [file an issue](https://github.com/c3lang/c3c/issues)
or discuss C3 on its dedicated Discord: [https://discord.gg/qN76R87](https://discord.gg/qN76R87).

The compiler is currently verified to compile on Windows, MacOS, Linux, OpenBSD, NetBSD.

<details><summary><b>Support matrix</b></summary>

| Platform                 | Native C3 compiler available? | Target supported        | Stack trace | Threads  | Sockets  | Inline asm |
|--------------------------|-------------------------------|-------------------------|-------------|----------|----------|------------|
| Win32 x64                | Yes                           | Yes + cross compilation | Yes         | Yes      | Yes      | Yes*       |
| Win32 Aarch64            | Untested                      | Untested                | Untested    | Untested | Untested | Yes*       |
| MacOS x64                | Yes                           | Yes + cross compilation | Yes         | Yes      | Yes      | Yes*       |
| MacOS Aarch64            | Yes                           | Yes + cross compilation | Yes         | Yes      | Yes      | Yes*       |
| iOS Aarch64              | No                            | Untested                | Untested    | Yes      | Yes      | Yes*       |
| Android Aarch64          | No                            | Untested                | Untested    | Untested | Untested | Yes*       |
| Android x64              | No                            | Untested                | Untested    | Untested | Untested | Yes*       |
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
| ELF freestanding Xtensa* | No                            | Untested                | No          | No       | No       | Untested   |
| FreeBSD x86              | Untested                      | Untested                | No          | Yes      | Untested | Yes*       |
| FreeBSD x64              | Untested                      | Untested                | No          | Yes      | Untested | Yes*       |
| NetBSD x86               | Untested                      | Untested                | No          | Yes      | Untested | Yes*       |
| NetBSD x64               | Untested                      | Untested                | No          | Yes      | Untested | Yes*       |
| OpenBSD x86              | Untested                      | Untested                | No          | Yes      | Untested | Yes*       |
| OpenBSD x64              | Yes*                          | Yes                     | Yes*        | Yes      | Untested | Yes*       |
| MCU x86                  | No                            | Untested                | No          | No       | No       | Yes*       |
| Wasm32                   | No                            | Yes                     | No          | No       | No       | No         |
| Wasm64                   | No                            | Untested                | No          | No       | No       | No         |

*\* Inline asm is still a work in progress*<br>
*\* OpenBSD 7.8 is the only tested version*<br>
*\* OpenBSD has limited stacktrace, needs to be tested further*<br>
*\* Xtensa support is enabled by compiling with `-DXTENSA_ENABLE`. The [espressif llvm fork](https://github.com/espressif/llvm-project) is recommended for best compatibility*

More platforms will be supported in the future.
</details>

#### What can you help with?

- If you wish to contribute with ideas, please file issues or discuss on Discord.
- Interested in contributing to the stdlib? Please get in touch on Discord.
- Compilation instructions for other Linux and Unix variants are appreciated.
- Would you like to contribute bindings to some library? It would be nice to have support for SDL3 and more. If you have created some bindings, please submit them to https://github.com/c3lang/vendor.
- Build something with C3 and show it off and give feedback. The language is still open for significant tweaks.
- Start work on the C -> C3 converter which takes C code and does a "best effort" to translate it to C3. The first version only needs to work on C headers.
- Do you have some specific area you have deep knowledge of and could help make C3 even better at doing? File or comment on issues.

---

## Installing

The instructions below install the latest stable release. If you wish to use the bleeding-edge nightly build instead, you can download the [latest prerelease binaries here](https://github.com/c3lang/c3c/releases/tag/latest-prerelease-tag).

#### Windows

**Precompiled binaries**
1. Download the zip file: [https://github.com/c3lang/c3c/releases/latest/download/c3-windows.zip](https://github.com/c3lang/c3c/releases/latest/download/c3-windows.zip)
   (debug version [here](https://github.com/c3lang/c3c/releases/latest/download/c3-windows-debug.zip))
2. Unzip exe and standard lib.
3. Run `c3c.exe`.

**Install script**

Open a PowerShell terminal (you may need to run it as an administrator) and run the following command:
```bash
iwr -useb https://raw.githubusercontent.com/c3lang/c3c/refs/heads/master/install/install.ps1 | iex
```
The script will inform you once the installation is successful and add the `~/.c3` directory to your PATH, which will allow you to run the c3c command from any location.

You can choose another version with option `C3_VERSION`.
For example, you can force the installation of the 0.7.11 version:
```bash
$env:C3_VERSION='0.7.11'; powershell -ExecutionPolicy Bypass -Command "iwr -useb https://raw.githubusercontent.com/c3lang/c3c/refs/heads/master/install/install.ps1 | iex"
```

**(Optional)** If you don't have Visual Studio 17 installed, you can run the `c3c fetch windows-sdk` command which will download the necessary files to compile on Windows.

**Scoop**

c3c is included in the 'Main' bucket.

```sh
scoop install c3
```

---

#### Linux

**Precompiled binaries**
1. Download tar file: [https://github.com/c3lang/c3c/releases/latest/download/c3-linux-static.tar.gz](https://github.com/c3lang/c3c/releases/latest/download/c3-linux-static.tar.gz)
   (debug version [here](https://github.com/c3lang/c3c/releases/latest/download/c3-linux-static-debug.tar.gz))
2. Unpack executable and standard lib.
3. Run `./c3c`.

**Install script**

Open a terminal and run the following command:
```bash
curl -fsSL https://raw.githubusercontent.com/c3lang/c3c/refs/heads/master/install/install.sh | bash
```
The C3 compiler will be installed, and the script will also update your ~/.bashrc to include `~/.c3` in your PATH, allowing you to invoke the c3c command from anywhere. You might need to restart your terminal or source your shell for the changes to take effect.

You can choose another version with option `C3_VERSION`.
For example, you can force the installation of the 0.7.11 version:
```bash
curl -fsSL https://raw.githubusercontent.com/c3lang/c3c/refs/heads/master/install/install.sh | C3_VERSION=0.7.11 bash
```

**Arch Linux**

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

**Fedora**

C3 is available as a community maintained COPR package for Fedora.  
To install via dnf:
```sh
sudo dnf copr enable sisyphus1813/c3
sudo dnf install c3
```

**NixOS / Nix**

You can access `c3c` via [flake.nix](./flake.nix), which will contain the latest commit of the compiler. To add `c3c` to your `flake.nix`, do the following:

<details><summary>flake.nix example</summary>

```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    c3c.url = "github:c3lang/c3c";
    # Those are desired if you don't want to copy extra nixpkgs
    c3c.inputs = {
      nixpkgs.follows = "nixpkgs";
      flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, ... } @ inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import inputs.nixpkgs { inherit system; };
      c3c = inputs.c3c.packages.${system}.c3c;
    in
    {
      devShells.default = pkgs.mkShell {
        buildInputs = [
          pkgs.c3c
        ];
      };
    }
  );
}
```
</details>


**Gentoo**

`c3c` is available in the [Gentoo GURU overlay](https://wiki.gentoo.org/wiki/Project:GURU).

Enable and sync the GURU repository (if not already done):

```sh
sudo eselect repository enable guru
sudo emaint sync -r guru
```

Install `c3c` with:

```sh
sudo emerge -av dev-lang/c3c
```

* The compiler binary is installed to `/usr/bin/c3c`.
* The standard library is installed to `/usr/lib/c3`.

For Gentoo-specific issues, please use the [Gentoo Bugzilla](https://bugs.gentoo.org/) (Product: *GURU*).


---

#### MacOS

**Precompiled binaries**
1. Make sure you have XCode with command line tools installed.
2. Download the zip file: [https://github.com/c3lang/c3c/releases/latest/download/c3-macos.zip](https://github.com/c3lang/c3c/releases/latest/download/c3-macos.zip)
   (debug version [here](https://github.com/c3lang/c3c/releases/latest/download/c3-macos-debug.zip))
3. Unzip executable and standard lib.
4. Run `./c3c`.

(*Note that there is a known issue with debug symbol generation on MacOS 13, see [issue #1086](https://github.com/c3lang/c3c/issues/1086))

---

#### OpenBSD

**Precompiled binaries**
1. Download tar file: [https://github.com/c3lang/c3c/releases/latest/download/c3-openbsd.tar.gz](https://github.com/c3lang/c3c/releases/latest/download/c3-openbsd.tar.gz)
   (debug version [here](https://github.com/c3lang/c3c/releases/latest/download/c3-openbsd-debug.tar.gz))
2. Unpack executable and standard lib.
3. Run `./c3c`.

(*Note that this is specifically for OpenBSD 7.8, running it on any other version is prone to ABI breaks)

---

#### Docker

You can build `c3c` using an Ubuntu container. By default, the script will build through Ubuntu 22.04. You can specify the version by passing the `UBUNTU_VERSION` environment variable.

```
UBUNTU_VERSION=20.04 ./build-with-docker.sh
```

See the `build-with-docker.sh` script for more information on other configurable environment variables.

#### Getting started with a "hello world"

Create a `main.c3` file with:
```c3
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

## Building C3 from Source

#### Compiling on MacOS

1. Install [Homebrew](https://brew.sh/)
2. Install CMake: `brew install cmake`
3. Clone the C3C github repository: `git clone https://github.com/c3lang/c3c.git`
4. Enter the C3C directory `cd c3c`.
5. Set up CMake build: `cmake -B build -S . -DC3_FETCH_LLVM=ON`
6. Build: `cmake --build build`
7. Change directory to the build directory `cd build`

#### Compiling on Windows

1. Make sure you have Visual Studio 17 2022 installed or alternatively install the "Buildtools for Visual Studio" (https://aka.ms/vs/17/release/vs_BuildTools.exe) and then select "Desktop development with C++"
2. Install CMake
3. Clone the C3C github repository: `git clone https://github.com/c3lang/c3c.git`
4. Enter the C3C directory: `cd c3c`.
5. Set up the CMake build: `cmake --preset windows-vs-2022-release`
6. Build: `cmake --build --preset windows-vs-2022-release`

You should now have a `c3c` executable in `build\Release`.

You can try it out by running some sample code: `c3c.exe compile ../../resources/examples/hash.c3`

Building `c3c` using Visual Studio Code is also supported when using the `CMake Tools` extension. Simply select the `Windows x64 Visual Studio 17 2022` configure preset and build.

*Note that if you run into linking issues when building, make sure that you are using the latest version of VS17.*

> [!NOTE]
> **Debug Build:**
> To avoid LLVM library conflicts, configure and build using the debug preset instead:
> ```bash
> cmake --preset windows-vs-2022-debug
> cmake --build --preset windows-vs-2022-debug
> ```
> *(Your executable will be located in `build-debug\Debug`)*

#### Compiling on Linux

1. Install required build dependencies using your distribution's package manager:
   - **Ubuntu / Debian:** `sudo apt-get install cmake git clang libcurl4-openssl-dev`
   - **Fedora:** `sudo dnf install cmake clang git libcurl-devel`
   - **Arch Linux:** `sudo pacman -S curl clang cmake git`
   - **Void Linux:** `sudo xbps-install git cmake clang libcurl-devel`
   - *Other distributions: Install CMake, Git, a C compiler (like Clang), and libcurl development headers.*

2. Clone the C3C repository and enter the directory:
   ```bash
   git clone https://github.com/c3lang/c3c.git
   cd c3c
   ```
   *(If you only need the latest commit, add `--depth=1` to the clone command)*

3. Create the CMake build cache:
   ```bash
   cmake -B build -S . -DC3_FETCH_LLVM=ON -DCMAKE_BUILD_TYPE=Release
   ```

4. Build the compiler:
   ```bash
   cmake --build build
   ```

5. You should now have a `c3c` executable in the `build` directory. You can test it by compiling an example:
   ```bash
   ./build/c3c compile resources/examples/hash.c3
   ```

   *(Optional) To install the compiler globally: `sudo cmake --install build`*

#### Compiling on NixOS

1. Enter nix shell, by typing `nix develop` in root directory
2. Configure cmake via `cmake . -Bbuild $=C3_CMAKE_FLAGS`. Note: passing `C3_CMAKE_FLAGS` is needed to generate `compile_commands.json` and find missing libs.
3. Build it `cmake --build build`
4. Test it out: `./build/c3c -V`
5. If you use `clangd` lsp server for your editor, it is recommended to make a symbolic link to `compile_command.json` in the root: `ln -s ./build/compile_commands.json compile_commands.json`

*A note on compiling for Linux/Unix/MacOS: to be able to [fetch vendor libraries](# "`c3c vendor-fetch` will list easily downloadable bindings. For example: `c3c vendor-fetch raylib`")
libcurl is needed. The CMake script should detect it if it is available. Note that
this functionality is non-essential and it is perfectly fine to use the compiler without it.*

#### Licensing

Unless specified otherwise, the code in this repository is MIT licensed.
The exception is the compiler source code (the source code under `src`),
which is licensed under LGPL 3.0.

This means you are free to use all parts of standard library,
tests, benchmarks, grammar, examples and so on under the MIT license, including
using those libraries and tests if you build your own C3 compiler.

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

A special thank you to sponsors [Zack Puhl](https://github.com/NotsoanoNimus) and [konimarti](https://github.com/konimarti) for going the extra mile.

And honorable mention goes to past sponsors:
[Ygor Pontelo](https://github.com/ygorpontelo), [Simone Raimondi](https://github.com/SRaimondi),
[Jan Válek](https://github.com/jan-valek), [Pierre Curto](https://github.com/pierrec),
[Caleb-o](https://github.com/Caleb-o) and [devdad](https://github.com/devdad)

## Star History

<a href="https://www.star-history.com/?repos=c3lang%2Fc3c&type=date&legend=top-left">
 <picture>
   <source media="(prefers-color-scheme: dark)" srcset="https://api.star-history.com/chart?repos=c3lang/c3c&type=date&theme=dark&legend=top-left" />
   <source media="(prefers-color-scheme: light)" srcset="https://api.star-history.com/chart?repos=c3lang/c3c&type=date&legend=top-left" />
   <img alt="Star History Chart" src="https://api.star-history.com/chart?repos=c3lang/c3c&type=date&legend=top-left" />
 </picture>
</a>
