project('c3c', ['c', 'cpp'])
cc = meson.get_compiler('c')

if get_option('hide_unused_function_warning')
    add_global_arguments('-Wno-unused-function', language: 'c')
    add_global_arguments('-Wno-unused-function', language: 'cpp')
endif

if get_option('hide_unused_variable_warning')
    add_global_arguments('-Wno-unused-variable', language: 'c')
    add_global_arguments('-Wno-unused-variable', language: 'cpp')
endif

if get_option('hide_unknown_pragmas')
    add_global_arguments('-Wno-unknown-pragmas', language: 'c')
    add_global_arguments('-Wno-unknown-pragmas', language: 'cpp')
endif

llvm_lib = dependency('llvm', required : true)
lld_common_lib = cc.find_library('lldCommon', required : true)
lld_elf_lib = cc.find_library('lldELF', required : true)
lld_coff_lib = cc.find_library('lldCOFF', required : true)
lld_wasm_lib = cc.find_library('lldWasm', required : true)
lld_macho_lib = cc.find_library('lldMachO', required : true)
curl_dep = dependency('libcurl', required : true)
libedit_dep = dependency('libedit', required : true)
zlib_dep = dependency('zlib', required : true)
zstd_dep = dependency('libzstd', required : true)
libxml2_dep = dependency('libxml2', required : true)

c3c_deps = [
    llvm_lib,
    lld_common_lib,
    lld_elf_lib,
    lld_coff_lib,
    lld_wasm_lib,
    lld_macho_lib,
    curl_dep,
    libedit_dep,
    zlib_dep,
    zstd_dep,
    libxml2_dep
]

miniz_src = [
    './dependencies/miniz/miniz.c'
]

compiler_abi_src = [
    'src/compiler/abi/c_abi_aarch64.c',
    'src/compiler/abi/c_abi.c',
    'src/compiler/abi/c_abi_riscv.c',
    'src/compiler/abi/c_abi_wasm.c',
    'src/compiler/abi/c_abi_win64.c',
    'src/compiler/abi/c_abi_x64.c',
    'src/compiler/abi/c_abi_x86.c'
]

compiler_asm_src = [
    'src/compiler/asm/aarch64.h',
    'src/compiler/asm/x86.h'
]

compiler_build_src = [
    'src/build/builder.c',
    'src/build/build_options.c',
    'src/build/project.c',
    'src/build/project_creation.c'
]

compiler_tb_src = [
    'src/compiler/tilde_codegen_abi.c',
    'src/compiler/tilde_codegen_storeload.c',
    'src/compiler/tilde_codegen_stmt.c',
    'src/compiler/tilde_codegen.c',
    'src/compiler/tilde_codegen_type.c',
    'src/compiler/tilde_codegen_expr.c',
    'src/compiler/tilde_codegen_value.c',
    'src/compiler/tilde_codegen_instr.c',
]
compiler_src = [
    compiler_abi_src,
    compiler_asm_src,
    compiler_build_src,
    'src/compiler/asm_target.c',
    'src/compiler/compiler.c',
    'src/compiler/expr.c',
    'src/compiler/libraries.c',
    'src/compiler/llvm_codegen_expr.c',
    'src/compiler/llvm_codegen_storeload.c',
    'src/compiler/number.c',
    'src/compiler/sema_asm.c',
    'src/compiler/sema_expr.c',
    'src/compiler/sema_passes.c',
    'src/compiler/target.c',
    'src/compiler/types.c',
    'src/compiler/ast.c',
    'src/compiler/context.c',
    'src/compiler/float.c',
    'src/compiler/linker.c',
    'src/compiler/llvm_codegen_function.c',
    'src/compiler/llvm_codegen_type.c',
    'src/compiler/parse_expr.c',
    'src/compiler/sema_builtins.c',
    'src/compiler/sema_initializers.c',
    'src/compiler/sema_stmts.c',
    'src/compiler/bigint.c',
    'src/compiler/copying.c',
    'src/compiler/headers.c',
    'src/compiler/llvm_codegen_builtins.c',
    'src/compiler/llvm_codegen_instr.c',
    'src/compiler/llvm_codegen_value.c',
    'src/compiler/parse_global.c',
    'src/compiler/sema_casts.c',
    'src/compiler/sema_liveness.c',
    'src/compiler/sema_types.c',
    'src/compiler/codegen_asm.c',
    'src/compiler/decltable.c',
    'src/compiler/json_output.c',
    'src/compiler/llvm_codegen.c',
    'src/compiler/llvm_codegen_module.c',
    'src/compiler/parser.c',
    'src/compiler/sema_decls.c',
    'src/compiler/sema_name_resolution.c',
    'src/compiler/source_file.c',
    'src/compiler/codegen_general.c',
    'src/compiler/diagnostics.c',
    'src/compiler/lexer.c',
    'src/compiler/llvm_codegen_debug_info.c',
    'src/compiler/llvm_codegen_stmt.c',
    'src/compiler/module.c',
    'src/compiler/parse_stmt.c',
    'src/compiler/sema_errors.c',
    'src/compiler/semantic_analyser.c',
    'src/compiler/symtab.c',
    'src/compiler/tokens.c',
    'src/compiler/windows_support.c',
    'src/compiler/mac_support.c'
]


compiler_benchmark_src = [
    'src/compiler_tests/benchmark.c',
    'src/compiler_tests/tests.c',
    'src/compiler_tests/shorttest.c'
]

utils_src = [
    miniz_src,
    'src/utils/cpus.c',
    'src/utils/errors.c',
    'src/utils/file_utils.c',
    'src/utils/find_msvc.c',
    'src/utils/http.c',
    'src/utils/json.c',
    'src/utils/malloc.c',
    'src/utils/stringutils.c',
    'src/utils/taskqueue.c',
    'src/utils/time.c',
    'src/utils/unzipper.c',
    'src/utils/vmem.c',
    'src/utils/whereami.c'
]

wrapper_src = [
    'wrapper/src/wrapper.cpp'
]

c3c_src = [
    'src/main.c',
    utils_src,
    compiler_src,
    compiler_benchmark_src,
    wrapper_src
]

c3c_include = [
    'src/',
    './dependencies/miniz/'
]

c3c_exe = executable('c3c', c3c_src, dependencies: c3c_deps, include_directories: c3c_include, install: true)
install_subdir('lib/std', install_dir: 'lib/c3')
