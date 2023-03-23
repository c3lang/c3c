cmake_minimum_required(VERSION 3.15)

# Grab the version
file(READ "src/version.h" ver)
if (NOT ${ver} MATCHES "COMPILER_VERSION \"([0-9]+.[0-9]+.[0-9]+)\"")
    message(FATAL_ERROR "Compiler version could not be parsed from version.h")
endif()

# Set the project and version
project(c3c VERSION ${CMAKE_MATCH_1})
message("C3C version: ${CMAKE_PROJECT_VERSION}")

# Enable fetching (for Windows)
include(FetchContent)
include(FeatureSummary)


set(CMAKE_FIND_PACKAGE_SORT_ORDER NATURAL)
set(CMAKE_FIND_PACKAGE_SORT_DIRECTION DEC)

# We use C11 and C++17
set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 17)

if(MSVC)
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /O2 /EHsc")
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} /O2 /EHsc")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /Od /Zi /EHsc")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} /Od /Zi /EHsc")
else()
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -fno-exceptions")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -gdwarf-3 -fno-exceptions")
set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -gdwarf-3 -O3 -fno-exceptions")
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -gdwarf-3 -fno-exceptions")
endif()
#set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O1 -fsanitize=undefined")
#set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -O1 -fsanitize=undefined")
#set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -gdwarf-3 -O3 -fsanitize=undefined")
#set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -gdwarf-3 -fsanitize=undefined")
#set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fsanitize=undefined")

set(C3_LLVM_VERSION "auto" CACHE STRING "Use LLVM version [default: auto]")
option(C3_USE_MIMALLOC "Use built-in mimalloc" OFF)
option(C3_USE_TB "Use TB" OFF)
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
if (NOT WIN32)
    find_package(CURL)
endif()
if (NOT C3_LLVM_VERSION STREQUAL "auto")
    if (${C3_LLVM_VERSION} VERSION_LESS 15 OR ${C3_LLVM_VERSION} VERSION_GREATER 17)
        message(FATAL_ERROR "LLVM ${C3_LLVM_VERSION} is not supported!")
    endif()
endif()

find_package(Git QUIET)
if(C3_USE_TB AND GIT_FOUND AND EXISTS "${CMAKE_SOURCE_DIR}/.git")
    # Update submodules as needed
    option(GIT_SUBMODULE "Check submodules during build" ON)
    if(GIT_SUBMODULE)
        message(STATUS "Submodule update")
        execute_process(COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive
                WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                RESULT_VARIABLE GIT_SUBMOD_RESULT)
        if(NOT GIT_SUBMOD_RESULT EQUAL "0")
            message(FATAL_ERROR "git submodule update --init --recursive failed with ${GIT_SUBMOD_RESULT}, please checkout submodules")
        endif()
    endif()
endif()

if(CMAKE_C_COMPILER_ID STREQUAL "MSVC")
    if (C3_LLVM_VERSION STREQUAL "auto")
        set(C3_LLVM_VERSION "15")
    endif()
    FetchContent_Declare(
            LLVM_Windows
            URL https://github.com/c3lang/win-llvm/releases/download/llvm_15_0_6/llvm-15.0.6-windows-amd64-msvc17-libcmt.7z
    )
    FetchContent_Declare(
            LLVM_Windows_debug
            URL https://github.com/c3lang/win-llvm/releases/download/llvm_15_0_6/llvm-15.0.6-windows-amd64-msvc17-libcmt-dbg.7z
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
link_directories(${LLVM_LIBRARY_DIRS})
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
        WindowsDriver
)

llvm_map_components_to_libnames(llvm_libs ${LLVM_LINK_COMPONENTS})
file(REMOVE_RECURSE ${CMAKE_BINARY_DIR}/lib)
file(COPY ${CMAKE_SOURCE_DIR}/lib DESTINATION ${CMAKE_BINARY_DIR})


# These don't seem to be reliable on windows.
message(STATUS "using find_library")
find_library(LLD_COFF NAMES lldCOFF.lib lldCOFF.a liblldCOFF.a PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH)
find_library(LLD_COMMON NAMES lldCommon.lib lldCommon.a liblldCommon.a PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH)
find_library(LLD_ELF NAMES lldELF.lib lldELF.a liblldELF.a PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH)
find_library(LLD_MACHO NAMES lldMachO.lib lldMachO.a liblldMachO.a PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH)
find_library(LLD_MINGW NAMES lldMinGW.lib lldMinGW.a liblldMinGW.a PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH)
find_library(LLD_WASM NAMES lldWasm.lib lldWasm.a liblldWasm.a PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH)

if (${LLVM_PACKAGE_VERSION} VERSION_GREATER_EQUAL 16)
    find_library(LLD_LOONG NAMES libLLVMLoongArchCodeGen.lib libLLVMLoongArchAsmParser.lib libLLVMLoongArchCodeGen.a libLLVMLoongArchAsmParser.a PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH)
    set(lld_libs
        ${LLD_COFF}
        ${LLD_COMMON}
        ${LLD_WASM}
        ${LLD_MINGW}
        ${LLD_ELF}
        ${LLD_LOONG}
        ${LLD_MACHO}
        )
else()
    set(lld_libs
        ${LLD_COFF}
        ${LLD_COMMON}
        ${LLD_WASM}
        ${LLD_MINGW}
        ${LLD_ELF}
        ${LLD_MACHO}
        )
endif()

if (APPLE)
    set(lld_libs ${lld_libs} xar)
endif ()

message(STATUS "linking to llvm libs ${lld_libs}")
message(STATUS "Found lld libs ${lld_libs}")


add_library(c3c_wrappers STATIC wrapper/src/wrapper.cpp)
add_library(miniz STATIC dependencies/miniz/miniz.c)

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
        src/compiler/abi/c_abi_aarch64.c
        src/compiler/abi/c_abi.c
        src/compiler/abi/c_abi_riscv.c
        src/compiler/abi/c_abi_wasm.c
        src/compiler/abi/c_abi_win64.c
        src/compiler/abi/c_abi_x64.c
        src/compiler/abi/c_abi_x86.c
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
        src/compiler/sema_errors.c
        src/compiler/sema_builtins.c
        src/compiler/sema_initializers.c
        src/compiler/semantic_analyser.c
        src/compiler/sema_passes.c
        src/compiler/sema_stmts.c
        src/compiler/sema_types.c
        src/compiler/source_file.c
        src/compiler/symtab.c
        src/compiler/target.c
        src/compiler/sema_asm.c
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
        src/utils/cpus.c
        src/utils/unzipper.c
        src/compiler/decltable.c
        src/compiler/mac_support.c
        src/compiler/llvm_codegen_storeload.c
        src/compiler/windows_support.c
        src/compiler/codegen_asm.c
        src/compiler/asm_target.c
        src/compiler/llvm_codegen_builtins.c
        src/compiler/expr.c
        src/utils/time.c
        src/utils/http.c
        src/compiler/sema_liveness.c)


if (C3_USE_TB)
    file(GLOB tilde-sources
            tilde-backend/src/tb/*.c
            tilde-backend/src/tb/codegen/*.c
            tilde-backend/src/tb/bigint/*.c
            tilde-backend/src/tb/objects/*.c
            tilde-backend/src/tb/system/*.c
            tilde-backend/src/tb/debug/cv/*.c
            tilde-backend/src/tb/opt/*.c
            tilde-backend/src/tb/x64/*.c
            tilde-backend/src/tb/wasm/*.c
            tilde-backend/src/tb/aarch64/*.c
            )
    target_sources(c3c PRIVATE
            src/compiler/tilde_codegen.c
            src/compiler/tilde_codegen_instr.c
            src/compiler/tilde_codegen_value.c
            src/compiler/tilde_codegen_storeload.c
            src/compiler/tilde_codegen_expr.c
            src/compiler/tilde_codegen_stmt.c
            src/compiler/tilde_codegen_type.c
            src/compiler/tilde_codegen_abi.c
            src/compiler/tilde_codegen_storeload.c)

    target_compile_definitions(c3c PUBLIC TB_AVAILABLE=1)
    target_link_libraries(c3c tilde-backend)
    add_library(tilde-backend STATIC ${tilde-sources})
    target_include_directories(tilde-backend PRIVATE
            "${CMAKE_SOURCE_DIR}/tilde-backend/src/" "${CMAKE_SOURCE_DIR}/tilde-backend/include")

    target_include_directories(c3c PRIVATE
            "${CMAKE_SOURCE_DIR}/tilde-backend/include/")
else()
    target_compile_definitions(c3c PUBLIC TB_AVAILABLE=0)
endif()


target_include_directories(c3c PRIVATE
        "${CMAKE_SOURCE_DIR}/src/")


target_include_directories(c3c_wrappers PRIVATE
        "${CMAKE_SOURCE_DIR}/wrapper/src/")

target_include_directories(miniz PUBLIC
        "${CMAKE_SOURCE_DIR}/dependencies/miniz/")

target_link_libraries(c3c_wrappers ${llvm_libs} ${lld_libs})
target_link_libraries(c3c ${llvm_libs} miniz c3c_wrappers ${lld_libs})

if(C3_USE_MIMALLOC)
    target_link_libraries(c3c mimalloc-static)
endif()

if (WIN32)
    target_link_libraries(c3c Winhttp.lib)
endif()

if (CURL_FOUND)
    target_link_libraries(c3c ${CURL_LIBRARIES})
    target_include_directories(c3c PRIVATE ${CURL_INCLUDES})
    target_compile_definitions(c3c PUBLIC CURL_FOUND=1)
else()
    target_compile_definitions(c3c PUBLIC CURL_FOUND=0)
endif()

if(MSVC)
    message("Adding MSVC options")
    target_compile_options(c3c PRIVATE /wd4068 /wd4090 /WX /Wv:18)
    target_compile_options(c3c_wrappers PUBLIC /wd4624 /wd4267 /wd4244 /WX /Wv:18)
    target_link_options(c3c_wrappers PUBLIC /ignore:4099)
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        target_compile_options(c3c PUBLIC /MTd)
        target_compile_options(c3c_wrappers PUBLIC /MTd)
        target_compile_options(miniz PUBLIC /MTd)
        if (C3_USE_TB)
            target_compile_options(tilde-backend PUBLIC /MTd)
        endif()
    else()
        target_compile_options(c3c PUBLIC /MT)
        target_compile_options(c3c_wrappers PUBLIC /MT)
        target_compile_options(miniz PUBLIC /MT)
        if (C3_USE_TB)
            target_compile_options(tilde-backend PUBLIC /MT)
        endif()
    endif()

else()
    message(STATUS "using gcc/clang warning switches")
    target_link_options(c3c PRIVATE -pthread)
    target_compile_options(c3c PRIVATE -pthread -Wall -Werror -Wno-unknown-pragmas -Wno-unused-result
            -Wno-unused-function -Wno-unused-variable -Wno-unused-parameter)
    if (WIN32)
        target_compile_definitions(c3c PRIVATE USE_PTHREAD=1)
    endif()
endif()


install(TARGETS c3c DESTINATION bin)

feature_summary(WHAT ALL)
