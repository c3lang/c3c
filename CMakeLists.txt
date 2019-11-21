cmake_minimum_required(VERSION 3.13)
project(c3c)
#set(CMAKE_CXX_FLAGS_RELEASE "-O3")

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

set(CMAKE_C_STANDARD 11)

set(LLVM_LINK_COMPONENTS
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
        AArch64Info
        AArch64Desc
        AArch64AsmParser
        AArch64Disassembler
        AArch64CodeGen
        ARMInfo
        ARMDesc
        ARMAsmParser
        ARMDisassembler
        ARMCodeGen
        AMDGPUInfo
        AMDGPUAsmParser
        AMDGPUCodeGen
        BPFInfo
        BPFAsmParser
        BPFDisassembler
        BPFCodeGen
        HexagonInfo
        HexagonDesc
        HexagonAsmParser
        HexagonDisassembler
        HexagonCodeGen
        LanaiInfo
        LanaiDesc
        LanaiAsmParser
        LanaiDisassembler
        LanaiCodeGen
        MSP430Info
        MSP430Desc
        MSP430AsmParser
        MSP430Disassembler
        MSP430CodeGen
        MipsInfo
        MipsDesc
        MipsAsmParser
        MipsDisassembler
        MipsCodeGen
        NVPTXInfo
        NVPTXCodeGen
        PowerPCInfo
        PowerPCDesc
        PowerPCAsmParser
        PowerPCDisassembler
        PowerPCCodeGen
        RISCVInfo
        RISCVDesc
        RISCVAsmParser
        RISCVDisassembler
        RISCVCodeGen
        RISCVUtils
        SparcInfo
        SparcDesc
        SparcAsmParser
        SparcDisassembler
        SparcCodeGen
        SystemZInfo
        SystemZDesc
        SystemZAsmParser
        SystemZDisassembler
        SystemZCodeGen
        WebAssemblyInfo
        WebAssemblyDesc
        WebAssemblyAsmParser
        WebAssemblyDisassembler
        WebAssemblyCodeGen
        X86Info
        X86Desc
        X86AsmParser
        X86Disassembler
        X86CodeGen
        X86Utils
        XCoreInfo
        XCoreDesc
        XCoreDisassembler
        XCoreCodeGen
        )

llvm_map_components_to_libnames(llvm_libs support core irreader ${LLVM_LINK_COMPONENTS})

include_directories(
        "${CMAKE_SOURCE_DIR}/src/"
        "${CMAKE_SOURCE_DIR}/build/")

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
        src/compiler/expr_analysis.c
        src/compiler/enums.h
        src/compiler/casts.c
        src/compiler/target.c
        src/compiler/compiler.h
        src/compiler/types.c
        src/compiler/module.c
        src/compiler/llvm_codegen.c
        src/utils/stringutils.c
        src/compiler/dwarf.h
        src/compiler/llvm_codegen_stmt.c
        src/compiler/llvm_codegen_internal.h
        src/compiler/llvm_codegen_expr.c
        src/compiler/llvm_codegen_debug_info.c
        src/compiler/llvm_codegen_module.c
        src/compiler/llvm_codegen_type.c
        src/compiler/llvm_codegen_function.c)

target_compile_options(c3c PRIVATE -Werror -Wall -Wextra -Wno-unused-function -Wno-unused-variable -Wno-unused-parameter)

target_link_libraries(c3c ${llvm_libs})