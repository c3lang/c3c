cmake_minimum_required(VERSION 3.20)

set(C3_LLVM_MIN_VERSION 17)
set(C3_LLVM_MAX_VERSION 21)
set(C3_LLVM_DEFAULT_VERSION 19)

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

# Use /MT or /MTd
set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")

if(MSVC)
    message(STATUS "MSVC version ${MSVC_VERSION}")
    add_compile_options(/utf-8)
else()
    add_compile_options(-gdwarf-3 -fno-exceptions)

    # add_compile_options(-fsanitize=address,undefined)
    # add_link_options(-fsanitize=address,undefined)
endif()

# Options
set(C3_LINK_DYNAMIC         OFF      CACHE BOOL   "Link dynamically with LLVM/LLD libs")
set(C3_WITH_LLVM            ON       CACHE BOOL   "Build with LLVM")
set(C3_LLVM_VERSION         "auto"   CACHE STRING "Use LLVM version [default: auto]")
set(C3_USE_MIMALLOC         OFF      CACHE BOOL   "Use built-in mimalloc")
set(C3_MIMALLOC_TAG         "v1.7.3" CACHE STRING "Used version of mimalloc")
set(C3_USE_TB               OFF      CACHE BOOL   "Use TB")
set(C3_LLD_DIR              ""       CACHE STRING "Use custom LLD directory")
set(C3_ENABLE_CLANGD_LSP    OFF      CACHE BOOL   "Enable/Disable output of compile commands during generation")
set(LLVM_CRT_LIBRARY_DIR    ""       CACHE STRING "Use custom llvm's compiler-rt directory")

set(C3_OPTIONS
        C3_LINK_DYNAMIC
        C3_WITH_LLVM
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
if (NOT WIN32)
    find_package(CURL)
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
    if(CMAKE_C_COMPILER_ID STREQUAL "MSVC")
        if (C3_LLVM_VERSION STREQUAL "auto")
            set(C3_LLVM_VERSION ${C3_LLVM_DEFAULT_VERSION})
        endif()
        FetchContent_Declare(
                LLVM_Windows
                URL https://github.com/c3lang/win-llvm/releases/download/llvm_19_1_5/llvm-19.1.5-windows-amd64-msvc17-libcmt.7z
        )
        FetchContent_Declare(
                LLVM_Windows_debug
                URL https://github.com/c3lang/win-llvm/releases/download/llvm_19_1_5/llvm-19.1.5-windows-amd64-msvc17-libcmt-dbg.7z
        )
        if(CMAKE_BUILD_TYPE STREQUAL "Debug")
            message("Loading Windows LLVM debug libraries, this may take a while...")
            FetchContent_MakeAvailable(LLVM_Windows_debug)
            set(llvm_dir ${llvm_windows_debug_SOURCE_DIR})
        else()
            message("Loading Windows LLVM libraries, this may take a while...")
            FetchContent_MakeAvailable(LLVM_Windows)
            set(llvm_dir ${llvm_windows_SOURCE_DIR})
        endif()
        message("Loaded Windows LLVM libraries into ${llvm_dir}")
        set(CMAKE_SYSTEM_PREFIX_PATH ${llvm_dir} ${CMAKE_SYSTEM_PREFIX_PATH})

        find_package(LLVM REQUIRED CONFIG)
        find_package(LLD REQUIRED CONFIG)
    else()
        # Add paths for LLVM CMake files of version 19 and higher as they follow a new installation 
        # layout and are now in /usr/lib/llvm/*/lib/cmake/llvm/ rather than /usr/lib/cmake/llvm/
        # 
        # Because of CMAKE_FIND_PACKAGE_SORT_ORDER CMAKE_FIND_PACKAGE_SORT_DIRECTION,
        # the newest version will always be found first.
        c3_print_variables(CMAKE_PREFIX_PATH)
        if (DEFINED LLVM_DIR) 
            message(STATUS "Looking for LLVM CMake files in user-specified directory ${LLVM_DIR}")
        else()
            file (GLOB LLVM_CMAKE_PATHS "/usr/lib/llvm/*/lib/cmake/llvm/")
            list (APPEND CMAKE_PREFIX_PATH ${LLVM_CMAKE_PATHS} "/usr/lib/")
            message(STATUS "No LLVM_DIR specified, searching default directories ${CMAKE_PREFIX_PATH}")
        endif()

        if (NOT C3_LLVM_VERSION STREQUAL "auto")
            find_package(LLVM ${C3_LLVM_VERSION} REQUIRED CONFIG)
        else()
            find_package(LLVM REQUIRED CONFIG)
        endif()
    endif()

    if (EXISTS /opt/homebrew/lib)
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
        find_library(LLD_COFF NAMES liblldCOFF.dylib lldCOFF.lib lldCOFF.a liblldCOFF.dll.a liblldCOFF.a PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH REQUIRED)
        find_library(LLD_COMMON NAMES liblldCommon.dylib lldCommon.lib lldCommon.a liblldCommon.dll.a liblldCommon.a PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH REQUIRED)
        find_library(LLD_ELF NAMES liblldELF.dylib lldELF.lib lldELF.a liblldELF.dll.a liblldELF.a PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH REQUIRED)
        if (NOT ${CMAKE_SYSTEM_NAME} STREQUAL "OpenBSD")
            find_library(LLD_MACHO NAMES liblldMachO.dylib lldMachO.lib lldMachO.a liblldMachO.dll.a liblldMachO.a PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH REQUIRED)
        else()
            set(LLD_MACHO "")
        endif()
        find_library(LLD_MINGW NAMES liblldMinGW.dylib lldMinGW.lib lldMinGW.a liblldMinGW.dll.a liblldMinGW.a PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH REQUIRED)
        find_library(LLD_WASM NAMES liblldWasm.dylib lldWasm.lib lldWasm.a liblldWasm.dll.a liblldWasm.a PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH REQUIRED)
    else()
        message(STATUS "Looking for shared lld libraries in ${LLVM_LIBRARY_DIRS}")

        find_library(LLVM NAMES libLLVM.so PATHS ${LLVM_LIBRARY_DIRS} NO_DEFAULT_PATH REQUIRED)
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
        find_file(RT_ASAN_DYNAMIC NAMES libclang_rt.asan_osx_dynamic.dylib PATHS "${LLVM_LIBRARY_DIR}/clang/${LLVM_MAJOR_VERSION}/lib/darwin" ${LLVM_CRT_LIBRARY_DIR})
        find_file(RT_TSAN_DYNAMIC NAMES libclang_rt.tsan_osx_dynamic.dylib PATHS "${LLVM_LIBRARY_DIR}/clang/${LLVM_MAJOR_VERSION}/lib/darwin" ${LLVM_CRT_LIBRARY_DIR})
        find_file(RT_UBSAN_DYNAMIC NAMES libclang_rt.ubsan_osx_dynamic.dylib PATHS "${LLVM_LIBRARY_DIR}/clang/${LLVM_MAJOR_VERSION}/lib/darwin" ${LLVM_CRT_LIBRARY_DIR})
        find_file(RT_LSAN_DYNAMIC NAMES libclang_rt.lsan_osx_dynamic.dylib PATHS "${LLVM_LIBRARY_DIR}/clang/${LLVM_MAJOR_VERSION}/lib/darwin" ${LLVM_CRT_LIBRARY_DIR})
        set(sanitizer_runtime_libraries
                ${RT_ASAN_DYNAMIC}
                ${RT_TSAN_DYNAMIC}
                # Unused
                # ${RT_UBSAN_DYNAMIC}
                # ${RT_LSAN_DYNAMIC}
        )
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
        src/compiler/c_codegen.c
        src/compiler/decltable.c
        src/compiler/methodtable.c
        src/compiler/mac_support.c
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
        target_compile_options(c3c PRIVATE
                "$<$<CONFIG:Debug>:/EHa>"
                "$<$<CONFIG:Release>:/EHsc>")
    endif()
else()
    target_sources(c3c PRIVATE src/utils/hostinfo.c)
    target_compile_definitions(c3c PUBLIC LLVM_AVAILABLE=0)
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
    target_link_libraries(c3c ${llvm_libs} miniz c3c_wrappers ${lld_libs})

    target_include_directories(c3c PRIVATE
            "${CMAKE_SOURCE_DIR}/wrapper/include/")

    target_include_directories(c3c_wrappers PRIVATE
            "${CMAKE_SOURCE_DIR}/wrapper/include/")

    target_link_libraries(c3c_wrappers ${llvm_libs} ${lld_libs})

else()

    target_link_libraries(c3c ${llvm_libs} miniz ${lld_libs})

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

if (CURL_FOUND)
    target_link_libraries(c3c ${CURL_LIBRARIES})
    target_include_directories(c3c PRIVATE ${CURL_INCLUDE_DIRS})
    target_compile_definitions(c3c PUBLIC CURL_FOUND=1)
else()
    target_compile_definitions(c3c PUBLIC CURL_FOUND=0)
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
        set(clang_lib_dir ${llvm_dir}/lib/clang/${C3_LLVM_VERSION}/lib/windows)
        set(sanitizer_runtime_libraries
                ${clang_lib_dir}/clang_rt.asan-x86_64.lib
                ${clang_lib_dir}/clang_rt.asan_dynamic-x86_64.lib
                ${clang_lib_dir}/clang_rt.asan_dynamic-x86_64.dll
                ${clang_lib_dir}/clang_rt.asan_dynamic_runtime_thunk-x86_64.lib)
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
    )
    target_link_options(c3c PRIVATE -pthread)
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
        add_custom_command(TARGET c3c POST_BUILD
                COMMAND find $<TARGET_FILE_DIR:c3c>/c3c_rt -type f -name "*.dylib" -execdir ${LLVM_TOOLS_BINARY_DIR}/llvm-install-name-tool -id @rpath/{} {} $<SEMICOLON>
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
