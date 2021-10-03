TODO add more info here

cmake build command
```
cmake .. -DLLVM_ENABLE_PROJECTS="clang;lld" -DLLVM_TARGETS_TO_BUILD="ARM;AArch64;RISCV;WebAssembly;X86" -Thost=x64
```

To successfully compile c3 code, `--target x86-windows` is required:
```
c3c compile hello_world.c3 --target x86-windows
```
and manual linking is currently necessary:
```
link.exe /entry:mainCRTSetup /defaultlib:libcmt /subsystem:console ./hw.o
```
This will need to be done with a developer command prompt