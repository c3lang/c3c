# Building and using C3C on windows

Windows support in C3C is currently experimental and unstable. It has many limitations and likely has bugs. With that aside, here's how you can build and use it:

## Compiling LLVM

First, follow the steps outlined here to set up and compile LLVM:

https://llvm.org/docs/GettingStartedVS.html

When running cmake, use the following options to enable all of the libraries needed by C3C:

```
cmake .. -DLLVM_ENABLE_PROJECTS="clang;lld" -DLLVM_TARGETS_TO_BUILD="ARM;AArch64;RISCV;WebAssembly;X86" -Thost=x64
```

## Compiling C3C

Clone the C3C github repository and open its directory in visual studio. VS should detect the CMake configuration, allowing you to simply use Build -> Build All to compile C3C. A `c3c.exe` will be placed in `out/build/build`.

## Using C3C on Windows

At this point, you should be able to use C3C normally. For example:

```
/c3c/out/build/build/c3c.exe compile ./hello_world.c3c
```

Note that on windows, linker arguments passed through c3c using `-z` will need to be of the format expected by `lld-link`, which uses MSVC `link.exe`-format arguments rather than the GCC/Clang format.
