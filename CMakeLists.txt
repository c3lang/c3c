cmake_minimum_required(VERSION 3.20)

set(C3_LLVM_MIN_VERSION 19)
set(C3_LLVM_MAX_VERSION 22)
set(C3_LLVM_DEFAULT_VERSION 21)

if (CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_CURRENT_BINARY_DIR)
    message(FATAL_ERROR "In-tree build detected, please build in a separate directory")
endif()

# Grab the version
file(READ "src/version.h" ver)
if (NOT ${ver} MATCHES "COMPILER_VERSION \"([0-9]+.[0-9]+.[0-9]+)\"")
    message(FATAL_ERROR "Compiler version could not be parsed from version.h")
endif()

# Set the project and version
project(c3c VERSION ${CMAKE_MATCH_1} LANGUAGES C CXX)
message("Configuring C3C ${CMAKE_PROJECT_VERSION} for ${CMAKE_SYSTEM_NAME}")

# Helper functions
function(c3_print_variables)
    set(msg "")
    foreach(var ${ARGN})
        if(msg)
            string(APPEND msg " ; ")
        endif()
        string(APPEND msg "${c3_print_prefix}${var}=\"${${var}}\"")
    endforeach()
    message(STATUS "${msg}")
endfunction()

# Avoid warning for FetchContent
if (CMAKE_VERSION VERSION_GREATER_EQUAL "3.24.0")
    cmake_policy(SET CMP0135 NEW)
endif()

if (NOT DEFINED CMAKE_INSTALL_LIBDIR)
    if (WIN32)
        set(CMAKE_INSTALL_LIBDIR "c:\\c3c\\lib")
        set(CMAKE_INSTALL_BINDIR "c:\\c3c")
    else ()
        set(CMAKE_INSTALL_LIBDIR "/usr/local/lib/c3")
        set(CMAKE_INSTALL_BINDIR "/usr/local/bin/c3c")
    endif()
endif ()

# Enable fetching (for Windows)
include(FetchContent)
include(FeatureSummary)

set(CMAKE_FIND_PACKAGE_SORT_ORDER NATURAL)
set(CMAKE_FIND_PACKAGE_SORT_DIRECTION DEC)

# We use C11 and C++17
set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 17)

if(MSVC)
    message(STATUS "MSVC version ${MSVC_VERSION}")
    add_compile_options(/utf-8)

    if(C3_WITH_LLVM)
        FetchContent_GetProperties(LLVM_Windows)
        if(NOT LLVM_Windows_URL MATCHES "msvcrt")
            set(MSVC_CRT_SUFFIX "")
            message(STATUS "Detected STATIC LLVM (libcmt)")
        else()
            set(MSVC_CRT_SUFFIX "DLL")
            message(STATUS "Detected DYNAMIC LLVM (msvcrt)")
        endif()
    endif()

    # Force the Runtime to Release (/MT or /MD) even in Debug
    # This is required to match our RelWithDebInfo LLVM builds
    set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded${MSVC_CRT_SUFFIX}")
    set_property(GLOBAL PROPERTY MSVC_RUNTIME_LIBRARY "MultiThreaded${MSVC_CRT_SUFFIX}")

    add_compile_definitions(_ITERATOR_DEBUG_LEVEL=0)

    # Suppresses the LNK4098 mismatch warning in Debug builds
    set(CMAKE_EXE_LINKER_FLAGS_DEBUG "${CMAKE_EXE_LINKER_FLAGS_DEBUG} /NODEFAULTLIB:libcmtd /NODEFAULTLIB:msvcrtd")
else()
    add_compile_options(-gdwarf-3 -fno-exceptions)

    #add_compile_options(-fsanitize=address,undefined)
    #add_link_options(-fsanitize=address,undefined)
endif()

# Options
set(C3_LINK_DYNAMIC         OFF                         CACHE BOOL   "Link dynamically with LLVM/LLD libs")
set(C3_WITH_LLVM            ON                          CACHE BOOL   "Build with LLVM")
set(C3_LLVM_VERSION         "auto"                      CACHE STRING "Use LLVM version [default: auto]")
set(C3_USE_MIMALLOC         OFF                         CACHE BOOL   "Use built-in mimalloc")
set(C3_MIMALLOC_TAG         "v1.7.3"                    CACHE STRING "Used version of mimalloc")
set(C3_USE_TB               OFF                         CACHE BOOL   "Use TB")
set(C3_LLD_DIR              ""                          CACHE STRING "Use custom LLD directory")
set(C3_LLD_INCLUDE_DIR      ""                          CACHE STRING "Use custom LLD include directory")
set(C3_ENABLE_CLANGD_LSP    OFF                         CACHE BOOL   "Enable/Disable output of compile commands during generation")
set(C3_FETCH_LLVM           OFF                         CACHE BOOL   "Automatically download LLVM artifacts")
set(C3_LLVM_TAG             "llvm_21.x"                 CACHE STRING "Tag/Branch to download LLVM from")
set(LLVM_CRT_LIBRARY_DIR    ""                          CACHE STRING "Use custom llvm's compiler-rt directory")
set(TCC_LIB_PATH            "/usr/lib/tcc/libtcc1.a"    CACHE STRING "Use custom libtcc1.a path")

set(C3_OPTIONS
        C3_LINK_DYNAMIC
        C3_WITH_LLVM
        C3_FETCH_LLVM
        C3_LLVM_TAG
        C3_LLVM_VERSION
        C3_USE_MIMALLOC
        C3_MIMALLOC_TAG
        C3_USE_TB
        C3_LLD_DIR
        C3_ENABLE_CLANGD_LSP
        LLVM_CRT_LIBRARY_DIR
)

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

# Clangd LSP support
if(C3_ENABLE_CLANGD_LSP)
    set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
    execute_process(
            COMMAND ${CMAKE_COMMAND} -E create_symlink
            ${CMAKE_BINARY_DIR}/compile_commands.json
            ${CMAKE_SOURCE_DIR}/compile_commands.json
    )
endif(C3_ENABLE_CLANGD_LSP)

if(C3_WITH_LLVM)
    if(C3_FETCH_LLVM)
        # 1. Determine local platform ID
        if (WIN32)
            set(C3_LLVM_PLATFORM "windows-amd64")
        elseif (APPLE)
            if (CMAKE_SYSTEM_PROCESSOR STREQUAL "arm64" OR CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "arm64")
                set(C3_LLVM_PLATFORM "darwin-aarch64")
            else()
                set(C3_LLVM_PLATFORM "darwin-amd64")
            endif()
        else() # Linux
            if (CMAKE_SYSTEM_PROCESSOR STREQUAL "aarch64" OR CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "aarch64")
                set(C3_LLVM_PLATFORM "linux-aarch64")
            elseif (CMAKE_SYSTEM_PROCESSOR STREQUAL "riscv64" OR CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "riscv64")
                set(C3_LLVM_PLATFORM "linux-riscv64")
            else()
                set(C3_LLVM_PLATFORM "linux-amd64")
            endif()
        endif()

        # 2. Determine if we want Debug or Release
        set(C3_LLVM_SUFFIX "")
        set(C3_LLVM_TYPE "Release")
        if(CMAKE_BUILD_TYPE STREQUAL "Debug")
            set(C3_LLVM_SUFFIX "-dbg")
            set(C3_LLVM_TYPE "Debug")
        endif()

        # 3. Construct URL
        set(C3_LLVM_ARTIFACT_NAME "llvm-${C3_LLVM_PLATFORM}${C3_LLVM_SUFFIX}")
        #set(C3_LLVM_URL "https://github.com/c3lang/c3-llvm/releases/download/${C3_LLVM_TAG}/${C3_LLVM_ARTIFACT_NAME}.tar.gz")
        #set(C3_LLVM_URL "https://github.com//ManuLinares/llvm-custom-builds/releases/download/${C3_LLVM_TAG}/${C3_LLVM_ARTIFACT_NAME}.tar.gz")

        # We could also just set "latest" here to always fetch the latest release
        set(C3_LLVM_URL "https://github.com/c3lang/llvm-for-c3/releases/latest/download/${C3_LLVM_ARTIFACT_NAME}.tar.gz")

        message(STATUS "Fetching ${C3_LLVM_TYPE} LLVM artifact for ${C3_LLVM_PLATFORM}...")
        message(STATUS "URL: ${C3_LLVM_URL}")

        FetchContent_Declare(
                LLVM_Artifact
                URL ${C3_LLVM_URL}
        )
        FetchContent_MakeAvailable(LLVM_Artifact)

        # 4. Point CMake to the fetched location
        set(llvm_dir ${llvm_artifact_SOURCE_DIR})
        set(CMAKE_PREFIX_PATH ${llvm_dir} ${CMAKE_PREFIX_PATH})
        set(LLVM_DIR "${llvm_dir}/lib/cmake/llvm")
        set(LLD_DIR "${llvm_dir}/lib/cmake/lld")

        # TEST: For Windows, we might need to add the bin dir to prefix path for find_package to work well
        if (WIN32)
            set(CMAKE_SYSTEM_PREFIX_PATH ${llvm_dir} ${CMAKE_SYSTEM_PREFIX_PATH})
        endif()

        find_package(LLVM REQUIRED CONFIG NO_DEFAULT_PATH)
        find_package(LLD REQUIRED CONFIG NO_DEFAULT_PATH)
    else()
        if(CMAKE_C_COMPILER_ID STREQUAL "MSVC")
            # Legacy MSVC path if someone explicitly disabled fetching but is on MSVC
            find_package(LLVM REQUIRED CONFIG)
            find_package(LLD REQUIRED CONFIG)
        else()
            # Default system search
            file (GLOB LLVM_CMAKE_PATHS "/usr/lib/llvm/*/lib/cmake/llvm/")
            list (APPEND CMAKE_PREFIX_PATH ${LLVM_CMAKE_PATHS} "/usr/lib/")

            if (NOT C3_LLVM_VERSION STREQUAL "auto")
                find_package(LLVM ${C3_LLVM_VERSION} REQUIRED CONFIG)
            else()
                find_package(LLVM REQUIRED CONFIG)
            endif()
        endif()
    endif()

    if (NOT C3_FETCH_LLVM AND EXISTS /opt/homebrew/lib)
        list(APPEND LLVM_LIBRARY_DIRS /opt/homebrew/lib)
    endif()

    if (EXISTS /usr/lib)
        # Some systems (such as Alpine Linux) seem to put some of the relevant
        # LLVM files in /usr/lib, but this doesn't seem to be included in the
        # value of LLVM_LIBRARY_DIRS.
        list(APPEND LLVM_LIBRARY_DIRS /usr/lib)
    endif()

    list(REMOVE_DUPLICATES LLVM_LIBRARY_DIRS)

    message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
    message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
    message(STATUS "LLVM libraries located in: ${LLVM_LIBRARY_DIRS}")

    if (${LLVM_PACKAGE_VERSION} VERSION_LESS C3_LLVM_MIN_VERSION OR
        ${LLVM_PACKAGE_VERSION} VERSION_GREATER C3_LLVM_MAX_VERSION)
        message(FATAL_ERROR "LLVM ${LLVM_PACKAGE_VERSION} is not supported! LLVM version between ${C3_LLVM_MIN_VERSION} and ${C3_LLVM_MAX_VERSION} is required.")
    endif()

    if(LLVM_ENABLE_RTTI)
        message(STATUS "LLVM was built with RTTI")
    else()
        message(STATUS "LLVM was not built with RTTI")
    endif()

    string(REPLACE "." ";" VERSION_LIST ${LLVM_PACKAGE_VERSION})
    list(GET VERSION_LIST 0 LLVM_MAJOR_VERSION)

    include_directories(${LLVM_INCLUDE_DIRS})
    link_directories(${LLVM_LIBRARY_DIRS})
    add_definitions(${LLVM_DEFINITIONS})

    if(NOT C3_LINK_DYNAMIC)
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

        if(NOT ${C3_LLD_DIR} EQUAL "" AND EXISTS ${C3_LLD_DIR})
            list(APPEND LLVM_LIBRARY_DIRS ${C3_LLD_DIR})
            list(REMOVE_DUPLICATES LLVM_LIBRARY_DIRS)
        endif()

        message(STATUS "Looking for static lld libraries in ${LLVM_LIBRARY_DIRS}")

        # These don't seem to be reliable on windows.
        find_library(LLD_COFF NAMES lldCOFF.a liblldCOFF.a liblldCOFF.dylib lldCOFF.lib liblldCOFF.dll.a PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH REQUIRED)
        find_library(LLD_COMMON NAMES lldCommon.a liblldCommon.a liblldCommon.dylib lldCommon.lib liblldCommon.dll.a PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH REQUIRED)
        find_library(LLD_ELF NAMES lldELF.a liblldELF.a liblldELF.dylib lldELF.lib liblldELF.dll.a PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH REQUIRED)
        if (NOT ${CMAKE_SYSTEM_NAME} STREQUAL "OpenBSD")
            find_library(LLD_MACHO NAMES lldMachO.a liblldMachO.a liblldMachO.dylib lldMachO.lib liblldMachO.dll.a PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH REQUIRED)
        else()
            set(LLD_MACHO "")
        endif()
        find_library(LLD_MINGW NAMES lldMinGW.a liblldMinGW.a liblldMinGW.dylib lldMinGW.lib liblldMinGW.dll.a PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH REQUIRED)
        find_library(LLD_WASM NAMES lldWasm.a liblldWasm.a liblldWasm.dylib lldWasm.lib liblldWasm.dll.a PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH REQUIRED)
    else()
        message(STATUS "Looking for shared lld libraries in ${LLVM_LIBRARY_DIRS}")

        #find_library(LLVM NAMES libLLVM.so PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH REQUIRED)
        if(UNIX AND NOT WIN32)
            find_library(LLVM NAMES libLLVM.so PATHS ${LLVM_LIBRARY_DIRS} REQUIRED)
        else()
            find_library(LLVM NAMES libLLVM.a LLVM.lib PATHS ${LLVM_LIBRARY_DIRS} REQUIRED)
        endif()
        set(llvm_libs ${LLVM})

        # These don't seem to be reliable on windows.
        find_library(LLD_COFF NAMES liblldCOFF.so PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH REQUIRED)
        find_library(LLD_COMMON NAMES liblldCommon.so PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH REQUIRED)
        find_library(LLD_ELF NAMES liblldELF.so PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH REQUIRED)
        if (NOT ${CMAKE_SYSTEM_NAME} STREQUAL "OpenBSD")
            find_library(LLD_MACHO NAMES liblldMachO.so PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH REQUIRED)
        else()
            set(LLD_MACHO "")
        endif()
        find_library(LLD_MINGW NAMES liblldMinGW.so PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH REQUIRED)
        find_library(LLD_WASM NAMES liblldWasm.so PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH REQUIRED)
    endif()

    # find_library(LLD_LOONG NAMES libLLVMLoongArchCodeGen.lib libLLVMLoongArchAsmParser.lib libLLVMLoongArchCodeGen.a libLLVMLoongArchAsmParser.a PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH)
    set(lld_libs
            ${LLD_COFF}
            ${LLD_WASM}
            ${LLD_MINGW}
            ${LLD_ELF}
            ${LLD_MACHO}
            ${LLD_COMMON}
    )

    if (APPLE)
        set(lld_libs ${lld_libs} xar)
        if (llvm_dir)
            file(GLOB_RECURSE RT_ASAN_DYNAMIC "${llvm_dir}/*libclang_rt.asan_osx_dynamic.dylib")
            file(GLOB_RECURSE RT_TSAN_DYNAMIC "${llvm_dir}/*libclang_rt.tsan_osx_dynamic.dylib")
        endif()

        if (NOT RT_ASAN_DYNAMIC OR NOT RT_TSAN_DYNAMIC)
            # Fallback to searching in LLVM_LIBRARY_DIRS (for non-fetched LLVM)
            find_file(RT_ASAN_DYNAMIC_PATH NAMES libclang_rt.asan_osx_dynamic.dylib PATHS ${LLVM_LIBRARY_DIRS} PATH_SUFFIXES "clang/${LLVM_MAJOR_VERSION}/lib/darwin" "clang/${LLVM_PACKAGE_VERSION}/lib/darwin")
            find_file(RT_TSAN_DYNAMIC_PATH NAMES libclang_rt.tsan_osx_dynamic.dylib PATHS ${LLVM_LIBRARY_DIRS} PATH_SUFFIXES "clang/${LLVM_MAJOR_VERSION}/lib/darwin" "clang/${LLVM_PACKAGE_VERSION}/lib/darwin")
            if (RT_ASAN_DYNAMIC_PATH)
                set(RT_ASAN_DYNAMIC ${RT_ASAN_DYNAMIC_PATH})
            endif()
            if (RT_TSAN_DYNAMIC_PATH)
                set(RT_TSAN_DYNAMIC ${RT_TSAN_DYNAMIC_PATH})
            endif()
        endif()

        if (RT_ASAN_DYNAMIC)
            list(GET RT_ASAN_DYNAMIC 0 RT_ASAN_DYNAMIC)
        endif()
        if (RT_TSAN_DYNAMIC)
            list(GET RT_TSAN_DYNAMIC 0 RT_TSAN_DYNAMIC)
        endif()

        if (RT_ASAN_DYNAMIC AND RT_TSAN_DYNAMIC)
            set(sanitizer_runtime_libraries
                    ${RT_ASAN_DYNAMIC}
                    ${RT_TSAN_DYNAMIC}
            )
        endif()
    endif()

    message(STATUS "Linking to llvm libs ${llvm_libs}")
    message(STATUS "Linking to lld libs ${lld_libs}")
endif()

add_library(miniz STATIC dependencies/miniz/miniz.c)

add_executable(c3c
        src/build/builder.c
        src/build/build_options.c
        src/build/project_creation.c
        src/build/project_manipulation.c
        src/build/libraries.c
        src/compiler/ast.c
        src/compiler/bigint.c
        src/compiler/codegen_general.c
        src/compiler/compiler.c
        src/compiler/compiler.h
        src/compiler/subprocess.c
        src/compiler/subprocess.h
        src/compiler/context.c
        src/compiler/copying.c
        src/compiler/diagnostics.c
        src/compiler/float.c
        src/compiler/headers.c
        src/compiler/json_output.c
        src/compiler/lexer.c
        src/compiler/linker.c
        src/compiler/abi/c_abi_aarch64.c
        src/compiler/abi/c_abi.c
        src/compiler/abi/c_abi_riscv.c
        src/compiler/abi/c_abi_wasm.c
        src/compiler/abi/c_abi_win64.c
        src/compiler/abi/c_abi_x64.c
        src/compiler/abi/c_abi_x86.c
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
        src/utils/msi.c
        src/compiler/c_codegen.c
        src/compiler/decltable.c
        src/compiler/methodtable.c
        src/compiler/mac_support.c
        src/utils/fetch_msvc.c
        src/compiler/windows_support.c
        src/compiler/codegen_asm.c
        src/compiler/asm_target.c
        src/compiler/expr.c
        src/utils/time.c
        src/utils/http.c
        src/compiler/sema_liveness.c
        src/build/common_build.c
        src/compiler/sema_const.c
        ${CMAKE_BINARY_DIR}/git_hash.h
)

if(GIT_FOUND AND EXISTS "${CMAKE_SOURCE_DIR}/.git")
    # We are inside of a git repository so rebuilding the hash every time something changes.
    add_custom_command(
            OUTPUT ${CMAKE_BINARY_DIR}/git_hash.h
            COMMAND ${CMAKE_COMMAND} -P "${CMAKE_CURRENT_LIST_DIR}/git_hash.cmake"
            DEPENDS "${CMAKE_CURRENT_LIST_DIR}/.git")
else()
    # We are NOT inside of a git repository. Building the has only once.
    add_custom_command(
            OUTPUT ${CMAKE_BINARY_DIR}/git_hash.h
            COMMAND ${CMAKE_COMMAND} -P "${CMAKE_CURRENT_LIST_DIR}/git_hash.cmake")
endif()

if(C3_WITH_LLVM)
    target_sources(c3c PRIVATE
            src/compiler/llvm_codegen.c
            src/compiler/llvm_codegen_debug_info.c
            src/compiler/llvm_codegen_expr.c
            src/compiler/llvm_codegen_function.c
            src/compiler/llvm_codegen_instr.c
            src/compiler/llvm_codegen_module.c
            src/compiler/llvm_codegen_stmt.c
            src/compiler/llvm_codegen_type.c
            src/compiler/llvm_codegen_value.c
            src/compiler/llvm_codegen_storeload.c
            src/compiler/llvm_codegen_builtins.c)

    target_compile_definitions(c3c PUBLIC LLVM_AVAILABLE=1)
    add_library(c3c_wrappers STATIC wrapper/src/wrapper.cpp)
    if (MSVC)
        # Use the same detected CRT for the wrapper
        set_target_properties(c3c_wrappers PROPERTIES MSVC_RUNTIME_LIBRARY "MultiThreaded${MSVC_CRT_SUFFIX}")
        set_target_properties(miniz PROPERTIES MSVC_RUNTIME_LIBRARY "MultiThreaded${MSVC_CRT_SUFFIX}")

        target_compile_options(c3c PRIVATE
                "$<$<CONFIG:Debug>:/EHa>"
                "$<$<CONFIG:Release>:/EHsc>")
    endif()

    if(C3_LLD_INCLUDE_DIR)
        target_include_directories(c3c_wrappers PRIVATE ${C3_LLD_INCLUDE_DIR})
    endif()
else()
    target_sources(c3c PRIVATE src/utils/hostinfo.c)
    target_compile_definitions(c3c PUBLIC LLVM_AVAILABLE=0)
    target_link_libraries(c3c m)
endif()

target_include_directories(c3c PRIVATE
        "${CMAKE_SOURCE_DIR}/src/"
        "${CMAKE_BINARY_DIR}")

target_include_directories(miniz PUBLIC
        "${CMAKE_SOURCE_DIR}/dependencies/miniz/")

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

if(C3_WITH_LLVM)
    target_link_libraries(c3c miniz c3c_wrappers)

    target_include_directories(c3c PRIVATE
            "${CMAKE_SOURCE_DIR}/wrapper/include/")

    target_include_directories(c3c_wrappers PRIVATE
            "${CMAKE_SOURCE_DIR}/wrapper/include/")

    target_link_libraries(c3c_wrappers PUBLIC ${lld_libs} ${llvm_libs})

else()

    target_link_libraries(c3c miniz ${lld_libs} ${llvm_libs})

endif()

if(C3_USE_MIMALLOC)
    target_link_libraries(c3c mimalloc-static)
endif()

if (WIN32)
    target_link_libraries(c3c Winhttp.lib)
endif()

if(MINGW)
    message("Increase stack for msys")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,--stack,8388608")
endif ()

if (NOT WIN32)
    # For dlopen support
    if (CMAKE_DL_LIBS)
        target_link_libraries(c3c ${CMAKE_DL_LIBS})
    endif()
endif()


if(MSVC)
    target_compile_options(c3c PRIVATE
        /wd4068
        /wd4090
        /WX
        /Wv:18
    )

    if(C3_WITH_LLVM)
        target_compile_options(c3c_wrappers PUBLIC
            /wd4624
            /wd4267
            /wd4244
            /WX
            /Wv:18
        )
        if(NOT LLVM_ENABLE_RTTI)
            target_compile_options(c3c_wrappers PUBLIC /GR-)
        endif()
        target_link_options(c3c_wrappers PUBLIC /ignore:4099)
    endif()

    if(C3_WITH_LLVM)
        # The the sanitizer libs are in the folder "lib/clang/21/lib/windows/" but use the find anyway
        file(GLOB_RECURSE FOUND_ASAN_LIB "${llvm_dir}/*clang_rt.asan_dynamic-x86_64.lib")
        if(FOUND_ASAN_LIB)
            list(GET FOUND_ASAN_LIB 0 _asan_path)
            get_filename_component(_asan_dir "${_asan_path}" DIRECTORY)
            set(sanitizer_runtime_libraries
                    ${_asan_dir}/clang_rt.asan_dynamic-x86_64.lib
                    ${_asan_dir}/clang_rt.asan_dynamic-x86_64.dll
                    ${_asan_dir}/clang_rt.asan_dynamic_runtime_thunk-x86_64.lib)
            message(STATUS "Found Sanitizer binaries at: ${_asan_dir}")
        else()
            message(WARNING "Could not find sanitizer runtime libraries in ${llvm_dir}")
        endif()
    endif()

else()
    if (C3_WITH_LLVM AND NOT LLVM_ENABLE_RTTI)
        target_compile_options(c3c_wrappers PRIVATE -fno-rtti)
    endif()
    target_compile_options(c3c PRIVATE
        -pthread
        -Wall
        -Werror
        -Wno-unknown-pragmas
        -Wno-unused-result
        -Wno-unused-function
        -Wno-unused-variable
        -Wno-unused-parameter
        -Wno-char-subscripts
    )
    target_link_options(c3c PRIVATE -pthread)
endif()

if(CMAKE_C_COMPILER_ID STREQUAL "TinyCC")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wl,--no-eh-frame-hdr -z noexecstack")

    # Link the static tcc runtime archive if it exists
    if(EXISTS "${TCC_LIB_PATH}")
        target_link_libraries(c3c "${TCC_LIB_PATH}")
    else()
        message(FATAL_ERROR "TCC runtime not found at ${TCC_LIB_PATH}; Ensure the path is correct.")
    endif()
endif()

install(TARGETS c3c DESTINATION bin)
install(DIRECTORY lib/ DESTINATION lib/c3)

# Man page install (OSX/Linux only)
if (NOT WIN32)
    install(FILES c3c.1 DESTINATION "share/man/man1")
endif()

# Copy stdlib
if (NOT ${CMAKE_BINARY_DIR} EQUAL ${CMAKE_SOURCE_DIR})
    file(REMOVE_RECURSE ${CMAKE_BINARY_DIR}/lib)
    file(COPY ${CMAKE_SOURCE_DIR}/lib DESTINATION ${CMAKE_BINARY_DIR})
endif()

if (C3_WITH_LLVM AND DEFINED sanitizer_runtime_libraries)
    add_custom_command(TARGET c3c POST_BUILD
            COMMAND "${CMAKE_COMMAND}" -E rm -rf -- $<TARGET_FILE_DIR:c3c>/c3c_rt
            COMMAND "${CMAKE_COMMAND}" -E make_directory $<TARGET_FILE_DIR:c3c>/c3c_rt
            COMMAND "${CMAKE_COMMAND}" -E copy ${sanitizer_runtime_libraries} $<TARGET_FILE_DIR:c3c>/c3c_rt
            VERBATIM
            COMMENT "Copying sanitizer runtime libraries to output directory")

    if (APPLE)
        # Change LC_ID_DYLIB to be rpath-based instead of having an absolute path
        # We set DYLD_LIBRARY_PATH so the tools (which might be dynamic) can find libLLVM.dylib in the artifact
        string(REPLACE ";" ":" _dyld_path "${LLVM_LIBRARY_DIRS}")
        add_custom_command(TARGET c3c POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E env "DYLD_LIBRARY_PATH=${_dyld_path}:$ENV{DYLD_LIBRARY_PATH}" find $<TARGET_FILE_DIR:c3c>/c3c_rt -type f -name "*.dylib" -execdir ${LLVM_TOOLS_BINARY_DIR}/llvm-install-name-tool -id @rpath/{} {} $<SEMICOLON>
                VERBATIM)
    endif()

    install(DIRECTORY $<TARGET_FILE_DIR:c3c>/c3c_rt/ DESTINATION bin/c3c_rt)
endif()

feature_summary(WHAT ALL)

message(STATUS "Building ${CMAKE_PROJECT_NAME} with the following configuration:")

set(c3_print_prefix "  ")

foreach(option IN LISTS C3_OPTIONS)
    if (DEFINED ${option})
        c3_print_variables(${option})
    endif()
endforeach()

foreach(flag_var
        CMAKE_BUILD_TYPE
        CMAKE_C_COMPILER
        CMAKE_CXX_COMPILER
        CMAKE_LINKER
        CMAKE_OBJCOPY
        CMAKE_STRIP
        CMAKE_DLLTOOL)
    c3_print_variables(${flag_var})
endforeach()

message(STATUS "Build flags:")
foreach(flag_var
        CMAKE_C_FLAGS CMAKE_C_FLAGS_DEBUG CMAKE_C_FLAGS_RELEASE
        CMAKE_C_FLAGS_MINSIZEREL CMAKE_C_FLAGS_RELWITHDEBINFO
        CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE
        CMAKE_CXX_FLAGS_MINSIZEREL CMAKE_CXX_FLAGS_RELWITHDEBINFO)
    c3_print_variables(${flag_var})
endforeach()

message(STATUS "Output to: \"${CMAKE_BINARY_DIR}\"")
