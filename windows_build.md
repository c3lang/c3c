TODO add more info here

cmake build command
```
cmake .. -DLLVM_ENABLE_PROJECTS="clang;lld" -DLLVM_TARGETS_TO_BUILD="ARM;AArch64;RISCV;WebAssembly;X86" -Thost=x64
```