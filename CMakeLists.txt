cmake_minimum_required(VERSION 3.13)
project(c3c)

#set(CMAKE_BUILD_TYPE Release)
#set(CMAKE_CXX_FLAGS_RELEASE "-O3")

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

set(CMAKE_C_STANDARD 11)

set(LLVM_LINK_COMPONENTS
        AllTargetsAsmParsers
        AllTargetsDescs
        AllTargetsDisassemblers
        AllTargetsInfos
        AllTargetsCodeGens
        Analysis
        BitReader
        Core
        ExecutionEngine
        InstCombine
        Interpreter
        MC
        MCDisassembler
        MCJIT
        Object
        OrcJIT
        RuntimeDyld
        ScalarOpts
        Support
        Target
        TransformUtils
        native
        nativecodegen
        AsmPrinter
        )

llvm_map_components_to_libnames(llvm_libs support core irreader ${LLVM_LINK_COMPONENTS})

file(COPY ${CMAKE_SOURCE_DIR}/resources/lib DESTINATION ${CMAKE_BINARY_DIR})

include_directories(
        "${CMAKE_SOURCE_DIR}/src/")

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
        src/compiler/bigint.h
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
        src/compiler/llvm_codegen_stmt.c
        src/compiler/llvm_codegen_expr.c
        src/compiler/llvm_codegen_debug_info.c
        src/compiler/llvm_codegen_module.c
        src/compiler/llvm_codegen_type.c
        src/compiler/llvm_codegen_function.c
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
        src/utils/vmem.c
        src/utils/vmem.h
        src/utils/whereami.c
        )

target_compile_options(c3c PRIVATE -Wimplicit-int -Werror -Wall -Wno-unknown-pragmas -Wextra -Wno-unused-function -Wno-unused-variable -Wno-unused-parameter)

target_link_libraries(c3c m ${llvm_libs})

install(TARGETS c3c DESTINATION bin)

