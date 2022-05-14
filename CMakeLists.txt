cmake_minimum_required(VERSION 3.15)
project(c3c)
include(FetchContent)
include(FeatureSummary)

set(CMAKE_FIND_PACKAGE_SORT_ORDER NATURAL)
set(CMAKE_FIND_PACKAGE_SORT_DIRECTION DEC)
set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 17)

set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O1")
set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -gdwarf-3 -O3")
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -gdwarf-3")

option(C3_USE_TB "Enable TB" OFF)
set(C3_LLVM_VERSION "auto" CACHE STRING "Use LLVM version [default: auto]")
option(C3_USE_MIMALLOC "Use built-in mimalloc" OFF)
set(C3_MIMALLOC_TAG "v1.7.3" CACHE STRING "Used version of mimalloc")

set(C3_USE_MIMALLOC OFF)
if(C3_USE_MIMALLOC)
    option(MI_BUILD_TESTS OFF)
    option(MI_BUILD_SHARED OFF)
    option(MI_PADDING OFF)
    option(MI_DEBUG_FULL OFF)
    FetchContent_Declare(
    mimalloc
    GIT_REPOSITORY https://github.com/microsoft/mimalloc.git
    GIT_TAG ${C3_MIMALLOC_TAG}
    )
    FetchContent_MakeAvailable(mimalloc)
endif()

if (NOT C3_LLVM_VERSION STREQUAL "auto")
    if (${C3_LLVM_VERSION} VERSION_LESS 12 OR ${C3_LLVM_VERSION} VERSION_GREATER 15)
        message(FATAL_ERROR "LLVM ${C3_LLVM_VERSION} is not supported!")
    endif()
endif()

if(CMAKE_C_COMPILER_ID STREQUAL "MSVC")
    if (C3_LLVM_VERSION STREQUAL "auto")
        set(C3_LLVM_VERSION "14")
    endif()
    FetchContent_Declare(
            LLVM_Windows
            URL https://github.com/c3lang/win-llvm/releases/download/14.0.1_VS22/llvm-14.0.1-windows-amd64-msvc16-libcmt.7z
    )
    FetchContent_Declare(
            LLVM_Windows_debug
            URL https://github.com/c3lang/win-llvm/releases/download/14.0.1_VS22/llvm-14.0.1-windows-amd64-msvc16-libcmt-dbg.7z
    )
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        message("Loading Windows LLVM debug libraries, this may take a while...")
        FetchContent_MakeAvailable(LLVM_Windows_debug)
        set(CMAKE_SYSTEM_PREFIX_PATH ${llvm_windows_debug_SOURCE_DIR} ${CMAKE_SYSTEM_PREFIX_PATH})
    else()
        message("Loading Windows LLVM libraries, this may take a while...")
        FetchContent_MakeAvailable(LLVM_Windows)
        set(CMAKE_SYSTEM_PREFIX_PATH ${llvm_windows_SOURCE_DIR} ${CMAKE_SYSTEM_PREFIX_PATH})
    endif()
    find_package(LLVM REQUIRED CONFIG)
    find_package(LLD REQUIRED CONFIG)
else()
    if (NOT C3_LLVM_VERSION STREQUAL "auto")
        find_package(LLVM ${C3_LLVM_VERSION} REQUIRED CONFIG)
    else()
        find_package(LLVM REQUIRED CONFIG)
    endif()
endif()

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message(STATUS "Libraries located in: ${LLVM_LIBRARY_DIRS}")
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

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

if (${LLVM_PACKAGE_VERSION} VERSION_GREATER 14.1)
    set(LLVM_LINK_COMPONENTS ${LLVM_LINK_COMPONENTS} WindowsDriver)
endif()

llvm_map_components_to_libnames(llvm_libs ${LLVM_LINK_COMPONENTS})

file(COPY ${CMAKE_SOURCE_DIR}/lib DESTINATION ${CMAKE_BINARY_DIR})


# These don't seem to be reliable on windows.
message(STATUS "using find_library")
if(C3_USE_TB)
    find_library(TB_LIB NAMES tinybackend.a tinybackend.lib PATHS ${CMAKE_SOURCE_DIR}/resources/tblib)
endif()
find_library(LLD_COFF NAMES lldCOFF.lib lldCOFF.a liblldCOFF.a PATHS ${LLVM_LIBRARY_DIRS})
find_library(LLD_COMMON NAMES lldCommon.lib lldCommon.a liblldCommon.a PATHS ${LLVM_LIBRARY_DIRS})
find_library(LLD_ELF NAMES lldELF.lib lldELF.a liblldELF.a PATHS ${LLVM_LIBRARY_DIRS})
if (${LLVM_PACKAGE_VERSION} VERSION_LESS 14)
    find_library(LLD_MACHO NAMES lldMachO2.lib lldMachO2.a liblldMachO2.a PATHS ${LLVM_LIBRARY_DIRS})
else ()
    find_library(LLD_MACHO NAMES lldMachO.lib lldMachO.a liblldMachO.a PATHS ${LLVM_LIBRARY_DIRS})
endif ()
find_library(LLD_MINGW NAMES lldMinGW.lib lldMinGW.a liblldMinGW.a PATHS ${LLVM_LIBRARY_DIRS})
find_library(LLD_WASM NAMES lldWasm.lib lldWasm.a liblldWasm.a PATHS ${LLVM_LIBRARY_DIRS})

if (${LLVM_PACKAGE_VERSION} VERSION_LESS 14)
    find_library(LLD_CORE NAMES lldCore.lib lldCore.a liblldCore.a PATHS ${LLVM_LIBRARY_DIRS})
    find_library(LLD_DRIVER NAMES lldDriver.lib lldDriver.a liblldDriver.a PATHS ${LLVM_LIBRARY_DIRS})
    find_library(LLD_READER_WRITER NAMES lldReaderWriter.lib lldReaderWriter.a liblldReaderWriter.a PATHS ${LLVM_LIBRARY_DIRS})
    find_library(LLD_YAML NAMES lldYAML.lib lldYAML.a liblldYAML.a PATHS ${LLVM_LIBRARY_DIRS})
endif ()

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
if (APPLE)
    set(lld_libs ${lld_libs} xar)
endif ()

message(STATUS "linking to llvm libs ${lld_libs}")
message(STATUS "Found lld libs ${lld_libs}")

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
        src/compiler/libraries.c
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
        src/compiler/llvm_codegen_instr.c
        src/compiler/llvm_codegen_module.c
        src/compiler/llvm_codegen_stmt.c
        src/compiler/llvm_codegen_type.c
        src/compiler/llvm_codegen_value.c
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
        src/compiler/tilde_codegen.c
        src/compiler/tilde_codegen_instr.c
        src/compiler/tilde_codegen_value.c
        src/compiler/tilde_codegen_storeload.c
        src/compiler_tests/benchmark.c
        src/compiler_tests/tests.c
        src/compiler/tokens.c
        src/compiler/types.c
        src/main.c
        src/utils/errors.c
        src/utils/file_utils.c
        src/utils/find_msvc.c
        src/utils/malloc.c
        src/utils/stringutils.c
        src/utils/taskqueue.c
        src/utils/json.c
        src/build/project.c
        src/utils/vmem.c
        src/utils/vmem.h
        src/utils/whereami.c
        src/compiler/decltable.c
        src/compiler/mac_support.c
        src/compiler/tilde_codegen_storeload.c
        src/compiler/llvm_codegen_storeload.c
        src/compiler/tilde_codegen_expr.c
        src/compiler/tilde_codegen_stmt.c
        src/compiler/tilde_codegen_type.c
        src/compiler/windows_support.c)

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



target_link_libraries(c3c_wrappers ${llvm_libs} ${lld_libs})
target_link_libraries(c3c ${llvm_libs} c3c_wrappers ${lld_libs})
if(C3_USE_TB)
    target_link_libraries(c3c c3c_wrappers ${TB_LIB})
    target_compile_definitions(c3c PUBLIC TB_BACKEND=1)
else()
    target_compile_definitions(c3c PUBLIC TB_BACKEND=0)
endif()

if(C3_USE_MIMALLOC)
    target_link_libraries(c3c mimalloc-static)
endif()

if(CMAKE_C_COMPILER_ID STREQUAL "MSVC")
    message("Adding MSVC options")
    target_compile_options(c3c PRIVATE /wd4068 /wd4090 /WX)
    target_compile_options(c3c_wrappers PUBLIC /wd4624 /wd4267 /wd4244 /WX)
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        target_compile_options(c3c PUBLIC /MTd)
        target_compile_options(c3c_wrappers PUBLIC /MTd)
    else()
        target_compile_options(c3c PUBLIC /MT)
        target_compile_options(c3c_wrappers PUBLIC /MT)
    endif()

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
