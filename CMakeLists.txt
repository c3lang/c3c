cmake_minimum_required(VERSION 3.13)
project(c3c)

SET(CMAKE_FIND_PACKAGE_SORT_ORDER NATURAL)
SET(CMAKE_FIND_PACKAGE_SORT_DIRECTION DEC)

#set(CMAKE_BUILD_TYPE Release)
#set(CMAKE_CXX_FLAGS_RELEASE "-O3")

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message(STATUS "Libraries located in: ${LLVM_LIBRARY_DIRS}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 17)

set(LLVM_LINK_COMPONENTS
        AllTargetsAsmParsers
        AllTargetsDescs
        AllTargetsDisassemblers
        AllTargetsInfos
        AllTargetsCodeGens
        Analysis
        BitReader
        Core
        InstCombine
        MC
        MCDisassembler
        Object
        ScalarOpts
        Support
        Target
        TransformUtils
        native
        nativecodegen
        AsmPrinter
        Linker
        LTO
        WindowsManifest
        DebugInfoPDB
        LibDriver
        Option
        IrReader
        )


llvm_map_components_to_libnames(llvm_libs ${LLVM_LINK_COMPONENTS})

file(COPY ${CMAKE_SOURCE_DIR}/resources/lib DESTINATION ${CMAKE_BINARY_DIR})

find_library(LLD_COFF NAMES lldCOFF.a liblldCOFF.a PATHS ${LLVM_LIBRARY_DIRS})
find_library(LLD_COMMON NAMES lldCommon.a liblldCommon.a PATHS ${LLVM_LIBRARY_DIRS})
find_library(LLD_CORE NAMES lldCore.a liblldCore.a PATHS ${LLVM_LIBRARY_DIRS})
find_library(LLD_WASM NAMES lldWasm.a liblldWasm.a PATHS ${LLVM_LIBRARY_DIRS})
find_library(LLD_MINGW NAMES lldMinGW.a liblldMinGW.a PATHS ${LLVM_LIBRARY_DIRS})
find_library(LLD_ELF NAMES lldELF.a liblldELF.a PATHS ${LLVM_LIBRARY_DIRS})
find_library(LLD_DRIVER NAMES lldDriver.a liblldDriver.a PATHS ${LLVM_LIBRARY_DIRS})
find_library(LLD_READER_WRITER NAMES lldReaderWriter.a liblldReaderWriter.a PATHS ${LLVM_LIBRARY_DIRS})
find_library(LLD_MACHO NAMES lldMachO.a liblldMachO.a PATHS ${LLVM_LIBRARY_DIRS})
find_library(LLD_YAML NAMES lldYAML.a liblldYAML.a PATHS ${LLVM_LIBRARY_DIRS})

set(lld_libs
        ${LLD_COFF}
        ${LLD_COMMON}
        ${LLD_WASM}
        ${LLD_MINGW}
        ${LLD_ELF}
        ${LLD_DRIVER}
        ${LLD_READER_WRITER}
        ${LLD_MACHO}
        ${LLD_YAML}
        ${LLD_CORE}
        )
add_library(c3c_wrappers STATIC wrapper/src/wrapper.cpp)

add_executable(c3c
        src/main.c
        src/build/build_options.c
        src/build/project_creation.c
        src/utils/errors.c
        src/utils/file_utils.c
        src/compiler/lexer.c
        src/compiler/tokens.c
        src/compiler/symtab.c
        src/compiler/parser.c
        src/compiler_tests/tests.c
        src/compiler_tests/benchmark.c
        src/utils/malloc.c
        src/compiler/compiler.c
        src/compiler/semantic_analyser.c
        src/compiler/source_file.c
        src/compiler/diagnostics.c
        src/compiler/ast.c
        src/compiler/bigint.c
        src/compiler/context.c
        src/compiler/sema_expr.c
        src/compiler/enums.h
        src/compiler/sema_casts.c
        src/compiler/target.c
        src/compiler/compiler.h
        src/compiler/types.c
        src/compiler/module.c
        src/compiler/llvm_codegen.c
        src/utils/stringutils.c
        src/compiler/dwarf.h
        src/compiler/copying.c
        src/compiler/llvm_codegen_stmt.c
        src/compiler/llvm_codegen_expr.c
        src/compiler/llvm_codegen_debug_info.c
        src/compiler/llvm_codegen_module.c
        src/compiler/llvm_codegen_type.c
        src/compiler/llvm_codegen_function.c
        src/compiler/llvm_codegen_c_abi.c
        src/build/builder.c
        src/utils/toml.c src/build/project.c
        src/compiler/sema_name_resolution.c
        src/target_info/target_info.c
        src/compiler/parse_expr.c
        src/compiler/parser_internal.h
        src/compiler/parse_stmt.c
        src/compiler/parse_global.c
        src/compiler/sema_passes.c
        src/compiler/sema_internal.h
        src/compiler/sema_decls.c
        src/compiler/sema_types.c
        src/compiler/sema_stmts.c
        src/compiler/number.c
        src/compiler/linker.c
        src/utils/vmem.c
        src/utils/vmem.h
        src/utils/whereami.c
        src/utils/taskqueue.c
        src/compiler/llvm_codegen_c_abi_x86.c
        src/compiler/c_abi_internal.h
        src/compiler/llvm_codegen_c_abi_x64.c
        src/compiler/llvm_codegen_c_abi_win64.c
        src/compiler/llvm_codegen_c_abi_aarch64.c
        src/compiler/headers.c
        src/compiler/llvm_codegen_c_abi_riscv.c
        src/compiler/llvm_codegen_c_abi_wasm.c)

target_compile_options(c3c PRIVATE -Wall -Werror -Wno-unknown-pragmas -Wno-unused-result
        -Wno-unused-function -Wno-unused-variable -Wno-unused-parameter)


target_include_directories(c3c PRIVATE
        "${CMAKE_SOURCE_DIR}/src/")

target_include_directories(c3c_wrappers PRIVATE
        "${CMAKE_SOURCE_DIR}/wrapper/src/")


message(STATUS "Found LLD ${lld_libs}")

target_link_libraries(c3c_wrappers ${llvm_libs} ${lld_libs})
#target_link_libraries(c3c m ${llvm_libs} c3c_wrappers lldCommon lldCore lldCOFF lldWASM lldMinGW lldELF lldDriver lldReaderWriter lldMachO lldYAML)
target_link_libraries(c3c m ${llvm_libs} c3c_wrappers ${lld_libs})

install(TARGETS c3c DESTINATION bin)

