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
"\Program Files (x86)\Microsoft Visual Studio\2019\BuildTools\VC\Tools\MSVC\14.28.29910\bin\Hostx64\x64\cl.exe" hw.obj /link /defaultlib:libcmt /subsystem:console


linking with lld:
"C:\\Program Files\\LLVM\\bin\\lld-link" \
-out:hw.exe \
"-libpath:C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\BuildTools\\VC\\Tools\\MSVC\\14.28.29910\\lib\\x64" \
"-libpath:C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\BuildTools\\VC\\Tools\\MSVC\\14.28.29910\\atlmfc\\lib\\x64" \
"-libpath:C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.19041.0\\ucrt\\x64" \
"-libpath:C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.19041.0\\um\\x64" \
"-libpath:C:\\Program Files\\LLVM\\lib\\clang\\12.0.1\\lib\\windows" \
/defaultlib:libcmt \
-nologo \
".\\hw.obj"
