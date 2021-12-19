cmake_minimum_required(VERSION 3.10)
project(c3c)
include(FeatureSummary)

set(CMAKE_FIND_PACKAGE_SORT_ORDER NATURAL)
set(CMAKE_FIND_PACKAGE_SORT_DIRECTION DEC)
set(C3_LLVM_VERSION "auto" CACHE STRING "Use LLVM version [default: auto]")


if(NOT C3_LLVM_VERSION STREQUAL "auto")
    if(${C3_LLVM_VERSION} VERSION_LESS 12 OR ${C3_LLVM_VERSION} VERSION_GREATER 14)
        message(FATAL_ERROR "LLVM ${C3_LLVM_VERSION} is not supported!")
    endif()
    find_package(LLVM ${C3_LLVM_VERSION} REQUIRED CONFIG)
else()
   find_package(LLVM REQUIRED CONFIG)
endif()

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message(STATUS "Libraries located in: ${LLVM_LIBRARY_DIRS}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 17)

set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3")
set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3")

set(LLVM_LINK_COMPONENTS
        AllTargetsAsmParsers
        AllTargetsCodeGens
        AllTargetsDescs
        AllTargetsDisassemblers
        AllTargetsInfos
        Analysis
        AsmPrinter
        BitReader
        Core
        DebugInfoPDB
        InstCombine
        IrReader
        LibDriver
        Linker
        LTO
        MC
        MCDisassembler
        native
        nativecodegen
        Object
        Option
        ScalarOpts
        Support
        Target
        TransformUtils
        WindowsManifest
        )


llvm_map_components_to_libnames(llvm_libs ${LLVM_LINK_COMPONENTS})

file(COPY ${CMAKE_SOURCE_DIR}/resources/lib DESTINATION ${CMAKE_BINARY_DIR})

# These don't seem to be reliable on windows.
if(UNIX)
    message(STATUS "using find_library")
#    find_library(TB_LIB NAMES tinybackend.a PATHS ${CMAKE_SOURCE_DIR}/resources/tblib)
    find_library(LLD_COFF NAMES lldCOFF.a liblldCOFF.a PATHS ${LLVM_LIBRARY_DIRS})
    find_library(LLD_COMMON NAMES lldCommon.a liblldCommon.a PATHS ${LLVM_LIBRARY_DIRS})
    find_library(LLD_ELF NAMES lldELF.a liblldELF.a PATHS ${LLVM_LIBRARY_DIRS})
    find_library(LLD_MACHO NAMES lldMachO.a liblldMachO.a PATHS ${LLVM_LIBRARY_DIRS})
    find_library(LLD_MINGW NAMES lldMinGW.a liblldMinGW.a PATHS ${LLVM_LIBRARY_DIRS})
    find_library(LLD_WASM NAMES lldWasm.a liblldWasm.a PATHS ${LLVM_LIBRARY_DIRS})

    if(${LLVM_PACKAGE_VERSION} VERSION_LESS 14)
        find_library(LLD_CORE NAMES lldCore.a liblldCore.a PATHS ${LLVM_LIBRARY_DIRS})
        find_library(LLD_DRIVER NAMES lldDriver.a liblldDriver.a PATHS ${LLVM_LIBRARY_DIRS})
        find_library(LLD_READER_WRITER NAMES lldReaderWriter.a liblldReaderWriter.a PATHS ${LLVM_LIBRARY_DIRS})
        find_library(LLD_YAML NAMES lldYAML.a liblldYAML.a PATHS ${LLVM_LIBRARY_DIRS})
    endif()

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
    message(STATUS "linking to llvm libs ${llvm_libs} ${lld_libs}")
endif()

message(STATUS "Found LLD ${lld_libs}")

add_library(c3c_wrappers STATIC wrapper/src/wrapper.cpp)

add_executable(c3c
        src/build/builder.c
        src/build/build_options.c
        src/build/project_creation.c
        src/compiler/ast.c
        src/compiler/bigint.c
        src/compiler/c_abi_internal.h
        src/compiler/codegen_general.c
        src/compiler/compiler.c
        src/compiler/compiler.h
        src/compiler/context.c
        src/compiler/copying.c
        src/compiler/diagnostics.c
        src/compiler/dwarf.h
        src/compiler/enums.h
        src/compiler/float.c
        src/compiler/headers.c
        src/compiler/lexer.c
        src/compiler/linker.c
        src/compiler/llvm_codegen.c
        src/compiler/llvm_codegen_c_abi_aarch64.c
        src/compiler/llvm_codegen_c_abi.c
        src/compiler/llvm_codegen_c_abi_riscv.c
        src/compiler/llvm_codegen_c_abi_wasm.c
        src/compiler/llvm_codegen_c_abi_win64.c
        src/compiler/llvm_codegen_c_abi_x64.c
        src/compiler/llvm_codegen_c_abi_x86.c
        src/compiler/llvm_codegen_debug_info.c
        src/compiler/llvm_codegen_expr.c
        src/compiler/llvm_codegen_function.c
        src/compiler/llvm_codegen_module.c
        src/compiler/llvm_codegen_stmt.c
        src/compiler/llvm_codegen_type.c
        src/compiler/module.c
        src/compiler/number.c
        src/compiler/parse_expr.c
        src/compiler/parse_global.c
        src/compiler/parser.c
        src/compiler/parser_internal.h
        src/compiler/parse_stmt.c
        src/compiler/sema_casts.c
        src/compiler/sema_decls.c
        src/compiler/sema_expr.c
        src/compiler/sema_internal.h
        src/compiler/sema_name_resolution.c
        src/compiler/semantic_analyser.c
        src/compiler/sema_passes.c
        src/compiler/sema_stmts.c
        src/compiler/sema_types.c
        src/compiler/source_file.c
        src/compiler/symtab.c
        src/compiler/target.c
        src/compiler/tb_codegen.c
        src/compiler_tests/benchmark.c
        src/compiler_tests/tests.c
        src/compiler/tokens.c
        src/compiler/types.c
        src/main.c
        src/target_info/target_info.c
        src/utils/errors.c
        src/utils/file_utils.c
        src/utils/find_msvc.c
        src/utils/malloc.c
        src/utils/stringutils.c
        src/utils/taskqueue.c
        src/utils/toml.c src/build/project.c
        src/utils/vmem.c
        src/utils/vmem.h
        src/utils/whereami.c)

if(NOT CMAKE_C_COMPILER_ID STREQUAL "MSVC")
    message(STATUS "using gcc/clang warning switches")
    target_compile_options(c3c PRIVATE -Wall -Werror -Wno-unknown-pragmas -Wno-unused-result
             -Wno-unused-function -Wno-unused-variable -Wno-unused-parameter)
endif()

target_include_directories(c3c PRIVATE
        "${CMAKE_SOURCE_DIR}/src/")

target_include_directories(c3c PRIVATE
        "${CMAKE_SOURCE_DIR}/tb/")

target_include_directories(c3c_wrappers PRIVATE
        "${CMAKE_SOURCE_DIR}/wrapper/src/")



if(UNIX)
    target_link_libraries(c3c_wrappers ${llvm_libs} ${lld_libs})
    target_link_libraries(c3c m ${llvm_libs} c3c_wrappers ${lld_libs})
#    target_link_libraries(c3c m ${llvm_libs} c3c_wrappers ${TB_LIB} ${lld_libs})
else()
    # todo: maybe get this from llvm-config somehow? it should be in LLVM_DIR\..\..\..\bin I think.
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -LIBPATH:C:\\llvm\\llvm\\build\\Release\\lib") # needed for lldCommon.lib
    #set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -LIBPATH:${LLVM_LIBPATH}") # This doesn't seem to work for some reason
    message(STATUS "${LLVM_LIBPATH}")
    target_link_libraries(c3c debug ${llvm_libs} c3c_wrappers lldCommon lldCore lldCOFF lldWASM lldMinGW lldELF lldDriver lldReaderWriter lldMachO lldYAML Advapi32)
    target_link_libraries(c3c optimized ${llvm_libs} c3c_wrappers lldCommon lldCore lldCOFF lldWASM lldMinGW lldELF lldDriver lldReaderWriter lldMachO lldYAML Advapi32)
endif()

if (WIN32)
    if (CMAKE_C_COMPILER_ID STREQUAL "Clang" OR CMAKE_C_COMPILE_ID STREQUAL "GNU")
        target_link_options(c3c PRIVATE -pthread)
        target_compile_definitions(c3c PRIVATE USE_PTHREAD=1)
        target_compile_options(c3c PRIVATE -mlong-double-64)
    endif()
endif()

install(TARGETS c3c DESTINATION bin)

feature_summary(WHAT ALL)
