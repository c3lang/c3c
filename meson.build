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

py3 = find_program('python3', required: true)

test_args = ['../test/src/check.py', '../test/src/tester.py', c3c_exe]

test('Test Suite - initialize - initializer_var', py3, args: [test_args, '../test/test_suite/initialize/initializer_var.c3t'])
test('Test Suite - initialize - initialize_finalize', py3, args: [test_args, '../test/test_suite/initialize/initialize_finalize.c3t'])

test('Test Suite - dynamic - null_and_protocol', py3, args: [test_args, '../test/test_suite/dynamic/null_and_protocol.c3t'])
test('Test Suite - dynamic - overlapping_function', py3, args: [test_args, '../test/test_suite/dynamic/overlapping_function.c3t'])
test('Test Suite - dynamic - dynamic_with_default', py3, args: [test_args, '../test/test_suite/dynamic/dynamic_with_default.c3t'])
test('Test Suite - dynamic - inherit', py3, args: [test_args, '../test/test_suite/dynamic/inherit.c3t'])

test('Test Suite - from_docs - examples_if_catch', py3, args: [test_args, '../test/test_suite/from_docs/examples_if_catch.c3t'])
test('Test Suite - from_docs - examples_functionpointer', py3, args: [test_args, '../test/test_suite/from_docs/examples_functionpointer.c3t'])
test('Test Suite - from_docs - examples_macro_function', py3, args: [test_args, '../test/test_suite/from_docs/examples_macro_function.c3t'])
test('Test Suite - from_docs - examples_defer', py3, args: [test_args, '../test/test_suite/from_docs/examples_defer.c3t'])
test('Test Suite - from_docs - examples_forswitch', py3, args: [test_args, '../test/test_suite/from_docs/examples_forswitch.c3t'])

test('Test Suite - builtins - rounding_builtins', py3, args: [test_args, '../test/test_suite/builtins/rounding_builtins.c3t'])
test('Test Suite - builtins - reduce_arithmetics', py3, args: [test_args, '../test/test_suite/builtins/reduce_arithmetics.c3t'])
test('Test Suite - builtins - reverse_builtin', py3, args: [test_args, '../test/test_suite/builtins/reverse_builtin.c3t'])
test('Test Suite - builtins - simple_builtins', py3, args: [test_args, '../test/test_suite/builtins/simple_builtins.c3t'])
test('Test Suite - builtins - mem', py3, args: [test_args, '../test/test_suite/builtins/mem.c3t'])
test('Test Suite - builtins - prefetch', py3, args: [test_args, '../test/test_suite/builtins/prefetch.c3t'])
test('Test Suite - builtins - overflows', py3, args: [test_args, '../test/test_suite/builtins/overflows.c3t'])
test('Test Suite - builtins - builtin_vector_abs', py3, args: [test_args, '../test/test_suite/builtins/builtin_vector_abs.c3t'])
test('Test Suite - builtins - sat_builtins', py3, args: [test_args, '../test/test_suite/builtins/sat_builtins.c3t'])
test('Test Suite - builtins - exacts', py3, args: [test_args, '../test/test_suite/builtins/exacts.c3t'])
test('Test Suite - builtins - shufflevector', py3, args: [test_args, '../test/test_suite/builtins/shufflevector.c3t'])
test('Test Suite - builtins - builtin_vector_min_max', py3, args: [test_args, '../test/test_suite/builtins/builtin_vector_min_max.c3t'])

test('Test Suite - any - variant_switch', py3, args: [test_args, '../test/test_suite/any/variant_switch.c3t'])
test('Test Suite - any - any_in_any', py3, args: [test_args, '../test/test_suite/any/any_in_any.c3t'])
test('Test Suite - any - variant_assign', py3, args: [test_args, '../test/test_suite/any/variant_assign.c3t'])
test('Test Suite - any - variant_test', py3, args: [test_args, '../test/test_suite/any/variant_test.c3t'])

test('Test Suite - methods - extension_method', py3, args: [test_args, '../test/test_suite/methods/extension_method.c3t'])
test('Test Suite - methods - enum_distinct_err_methods', py3, args: [test_args, '../test/test_suite/methods/enum_distinct_err_methods.c3t'])
test('Test Suite - methods - extension_method_in_other_modules', py3, args: [test_args, '../test/test_suite/methods/extension_method_in_other_modules.c3t'])
test('Test Suite - methods - methods_with_inferred_type', py3, args: [test_args, '../test/test_suite/methods/methods_with_inferred_type.c3t'])

test('Test Suite - distinct - distinct_shift', py3, args: [test_args, '../test/test_suite/distinct/distinct_shift.c3t'])

test('Test Suite - functions - assorted_tests', py3, args: [test_args, '../test/test_suite/functions/assorted_tests.c3t'])
test('Test Suite - functions - static_vars', py3, args: [test_args, '../test/test_suite/functions/static_vars.c3t'])
test('Test Suite - functions - naked_function', py3, args: [test_args, '../test/test_suite/functions/naked_function.c3t'])
test('Test Suite - functions - func_ptr_conversion_alias', py3, args: [test_args, '../test/test_suite/functions/func_ptr_conversion_alias.c3t'])
test('Test Suite - functions - c_vararg_expansion', py3, args: [test_args, '../test/test_suite/functions/c_vararg_expansion.c3t'])
test('Test Suite - functions - test_regression', py3, args: [test_args, '../test/test_suite/functions/test_regression.c3t'])
test('Test Suite - functions - splat_mingw', py3, args: [test_args, '../test/test_suite/functions/splat_mingw.c3t'])
test('Test Suite - functions - typeless_varargs', py3, args: [test_args, '../test/test_suite/functions/typeless_varargs.c3t'])
test('Test Suite - functions - varargs', py3, args: [test_args, '../test/test_suite/functions/varargs.c3t'])
test('Test Suite - functions - simple_test', py3, args: [test_args, '../test/test_suite/functions/simple_test.c3t'])
test('Test Suite - functions - distinct_fn_ptr_and_lambda', py3, args: [test_args, '../test/test_suite/functions/distinct_fn_ptr_and_lambda.c3t'])
test('Test Suite - functions - splat_aarch64', py3, args: [test_args, '../test/test_suite/functions/splat_aarch64.c3t'])
test('Test Suite - functions - test_regression_mingw', py3, args: [test_args, '../test/test_suite/functions/test_regression_mingw.c3t'])
test('Test Suite - functions - returning_void', py3, args: [test_args, '../test/test_suite/functions/returning_void.c3t'])
test('Test Suite - functions - varargs_followed_by_named', py3, args: [test_args, '../test/test_suite/functions/varargs_followed_by_named.c3t'])
test('Test Suite - functions - splat', py3, args: [test_args, '../test/test_suite/functions/splat.c3t'])
test('Test Suite - functions - func_ptr_conversions_and_names', py3, args: [test_args, '../test/test_suite/functions/func_ptr_conversions_and_names.c3t'])
test('Test Suite - functions - defered_default_arguments', py3, args: [test_args, '../test/test_suite/functions/defered_default_arguments.c3t'])

test('Test Suite - contracts - simple_test', py3, args: [test_args, '../test/test_suite/contracts/simple_test.c3t'])

test('Test Suite - statements - fallthough_do', py3, args: [test_args, '../test/test_suite/statements/fallthough_do.c3t'])
test('Test Suite - statements - defer_break_switch', py3, args: [test_args, '../test/test_suite/statements/defer_break_switch.c3t'])
test('Test Suite - statements - defer_next_switch', py3, args: [test_args, '../test/test_suite/statements/defer_next_switch.c3t'])
test('Test Suite - statements - return_switch', py3, args: [test_args, '../test/test_suite/statements/return_switch.c3t'])
test('Test Suite - statements - foreach_r_custom', py3, args: [test_args, '../test/test_suite/statements/foreach_r_custom.c3t'])
test('Test Suite - statements - defer_break', py3, args: [test_args, '../test/test_suite/statements/defer_break.c3t'])
test('Test Suite - statements - const_statements', py3, args: [test_args, '../test/test_suite/statements/const_statements.c3t'])
test('Test Suite - statements - defer_in_defer', py3, args: [test_args, '../test/test_suite/statements/defer_in_defer.c3t'])
test('Test Suite - statements - comparison_widening', py3, args: [test_args, '../test/test_suite/statements/comparison_widening.c3t'])
test('Test Suite - statements - foreach_custom', py3, args: [test_args, '../test/test_suite/statements/foreach_custom.c3t'])
test('Test Suite - statements - defer_break_simple', py3, args: [test_args, '../test/test_suite/statements/defer_break_simple.c3t'])
test('Test Suite - statements - foreach_r_custom_macro', py3, args: [test_args, '../test/test_suite/statements/foreach_r_custom_macro.c3t'])
test('Test Suite - statements - foreach_r_break', py3, args: [test_args, '../test/test_suite/statements/foreach_r_break.c3t'])
test('Test Suite - statements - foreach_common', py3, args: [test_args, '../test/test_suite/statements/foreach_common.c3t'])
test('Test Suite - statements - while_switch', py3, args: [test_args, '../test/test_suite/statements/while_switch.c3t'])
test('Test Suite - statements - nextcase_const', py3, args: [test_args, '../test/test_suite/statements/nextcase_const.c3t'])
test('Test Suite - statements - foreach_r_common', py3, args: [test_args, '../test/test_suite/statements/foreach_r_common.c3t'])
test('Test Suite - statements - defer_do_while', py3, args: [test_args, '../test/test_suite/statements/defer_do_while.c3t'])
test('Test Suite - statements - exhaustive_switch', py3, args: [test_args, '../test/test_suite/statements/exhaustive_switch.c3t'])
test('Test Suite - statements - foreach_break', py3, args: [test_args, '../test/test_suite/statements/foreach_break.c3t'])
test('Test Suite - statements - defer_with_loop', py3, args: [test_args, '../test/test_suite/statements/defer_with_loop.c3t'])
test('Test Suite - statements - custom_foreach_with_ref', py3, args: [test_args, '../test/test_suite/statements/custom_foreach_with_ref.c3t'])
test('Test Suite - statements - infinite_do_while', py3, args: [test_args, '../test/test_suite/statements/infinite_do_while.c3t'])
test('Test Suite - statements - if_tests', py3, args: [test_args, '../test/test_suite/statements/if_tests.c3t'])
test('Test Suite - statements - defer_in_block', py3, args: [test_args, '../test/test_suite/statements/defer_in_block.c3t'])
test('Test Suite - statements - defer_return', py3, args: [test_args, '../test/test_suite/statements/defer_return.c3t'])
test('Test Suite - statements - foreach_custom_macro', py3, args: [test_args, '../test/test_suite/statements/foreach_custom_macro.c3t'])
test('Test Suite - statements - labelled_continue_for', py3, args: [test_args, '../test/test_suite/statements/labelled_continue_for.c3t'])
test('Test Suite - statements - simple_do', py3, args: [test_args, '../test/test_suite/statements/simple_do.c3t'])
test('Test Suite - statements - defer_in_defer2', py3, args: [test_args, '../test/test_suite/statements/defer_in_defer2.c3t'])
test('Test Suite - statements - nextcase_default', py3, args: [test_args, '../test/test_suite/statements/nextcase_default.c3t'])
test('Test Suite - statements - ranged_switch', py3, args: [test_args, '../test/test_suite/statements/ranged_switch.c3t'])
test('Test Suite - statements - various_switching', py3, args: [test_args, '../test/test_suite/statements/various_switching.c3t'])
test('Test Suite - statements - foreach_more_implementations', py3, args: [test_args, '../test/test_suite/statements/foreach_more_implementations.c3t'])

test('Test Suite - macros - userland_bitcast', py3, args: [test_args, '../test/test_suite/macros/userland_bitcast.c3t'])
test('Test Suite - macros - macro_failable_return_rethrow', py3, args: [test_args, '../test/test_suite/macros/macro_failable_return_rethrow.c3t'])
test('Test Suite - macros - macro_with_body', py3, args: [test_args, '../test/test_suite/macros/macro_with_body.c3t'])
test('Test Suite - macros - macro_defer_with_body', py3, args: [test_args, '../test/test_suite/macros/macro_defer_with_body.c3t'])
test('Test Suite - macros - hash_ident_nested', py3, args: [test_args, '../test/test_suite/macros/hash_ident_nested.c3t'])
test('Test Suite - macros - macro_nested_labels', py3, args: [test_args, '../test/test_suite/macros/macro_nested_labels.c3t'])
test('Test Suite - macros - type_params', py3, args: [test_args, '../test/test_suite/macros/type_params.c3t'])
test('Test Suite - macros - macro_typed_varargs', py3, args: [test_args, '../test/test_suite/macros/macro_typed_varargs.c3t'])
test('Test Suite - macros - macro_defer_exit', py3, args: [test_args, '../test/test_suite/macros/macro_defer_exit.c3t'])
test('Test Suite - macros - macro_untyped_varargs_2', py3, args: [test_args, '../test/test_suite/macros/macro_untyped_varargs_2.c3t'])
test('Test Suite - macros - macro_defer_scope', py3, args: [test_args, '../test/test_suite/macros/macro_defer_scope.c3t'])
test('Test Suite - macros - unifying_implicit_void', py3, args: [test_args, '../test/test_suite/macros/unifying_implicit_void.c3t'])
test('Test Suite - macros - macro_common', py3, args: [test_args, '../test/test_suite/macros/macro_common.c3t'])
test('Test Suite - macros - macro_vasplat', py3, args: [test_args, '../test/test_suite/macros/macro_vasplat.c3t'])
test('Test Suite - macros - macro_body_defer', py3, args: [test_args, '../test/test_suite/macros/macro_body_defer.c3t'])
test('Test Suite - macros - macro_import_res_private', py3, args: [test_args, '../test/test_suite/macros/macro_import_res_private.c3t'])
test('Test Suite - macros - macro_chained_return_void_optional', py3, args: [test_args, '../test/test_suite/macros/macro_chained_return_void_optional.c3t'])

test('Test Suite - cast - implicit_infer_len_cast', py3, args: [test_args, '../test/test_suite/cast/implicit_infer_len_cast.c3t'])
test('Test Suite - cast - cast_from_ptr', py3, args: [test_args, '../test/test_suite/cast/cast_from_ptr.c3t'])
test('Test Suite - cast - cast_bitstruct_etc', py3, args: [test_args, '../test/test_suite/cast/cast_bitstruct_etc.c3t'])
test('Test Suite - cast - cast_narrow_alias', py3, args: [test_args, '../test/test_suite/cast/cast_narrow_alias.c3t'])
test('Test Suite - cast - top_down_casts', py3, args: [test_args, '../test/test_suite/cast/top_down_casts.c3t'])
test('Test Suite - cast - cast_subarray', py3, args: [test_args, '../test/test_suite/cast/cast_subarray.c3t'])

test('Test Suite - initializer_lists - ranges_to_dynamic', py3, args: [test_args, '../test/test_suite/initializer_lists/ranges_to_dynamic.c3t'])
test('Test Suite - initializer_lists - subarrays', py3, args: [test_args, '../test/test_suite/initializer_lists/subarrays.c3t'])
test('Test Suite - initializer_lists - fasta', py3, args: [test_args, '../test/test_suite/initializer_lists/fasta.c3t'])
test('Test Suite - initializer_lists - zero_init', py3, args: [test_args, '../test/test_suite/initializer_lists/zero_init.c3t'])
test('Test Suite - initializer_lists - statics', py3, args: [test_args, '../test/test_suite/initializer_lists/statics.c3t'])
test('Test Suite - initializer_lists - general_tests', py3, args: [test_args, '../test/test_suite/initializer_lists/general_tests.c3t'])

test('Test Suite - literals - bin_literal', py3, args: [test_args, '../test/test_suite/literals/bin_literal.c3t'])
test('Test Suite - literals - bin_literal2', py3, args: [test_args, '../test/test_suite/literals/bin_literal2.c3t'])
test('Test Suite - literals - literal_general', py3, args: [test_args, '../test/test_suite/literals/literal_general.c3t'])

test('Test Suite - safe - deref', py3, args: [test_args, '../test/test_suite/safe/deref.c3t'])

test('Test Suite - variables - var_init_multi', py3, args: [test_args, '../test/test_suite/variables/var_init_multi.c3t'])
test('Test Suite - variables - var_init', py3, args: [test_args, '../test/test_suite/variables/var_init.c3t'])

test('Test Suite - strings - string_len', py3, args: [test_args, '../test/test_suite/strings/string_len.c3t'])
test('Test Suite - strings - string_escape', py3, args: [test_args, '../test/test_suite/strings/string_escape.c3t'])
test('Test Suite - strings - multiline_strings', py3, args: [test_args, '../test/test_suite/strings/multiline_strings.c3t'])
test('Test Suite - strings - literal_to_subarray', py3, args: [test_args, '../test/test_suite/strings/literal_to_subarray.c3t'])
test('Test Suite - strings - string_to_array', py3, args: [test_args, '../test/test_suite/strings/string_to_array.c3t'])

test('Test Suite - enumerations - enum_conversions', py3, args: [test_args, '../test/test_suite/enumerations/enum_conversions.c3t'])
test('Test Suite - enumerations - enum_values', py3, args: [test_args, '../test/test_suite/enumerations/enum_values.c3t'])
test('Test Suite - enumerations - enum_add_sub', py3, args: [test_args, '../test/test_suite/enumerations/enum_add_sub.c3t'])
test('Test Suite - enumerations - enum_cast', py3, args: [test_args, '../test/test_suite/enumerations/enum_cast.c3t'])
test('Test Suite - enumerations - simple_inference', py3, args: [test_args, '../test/test_suite/enumerations/simple_inference.c3t'])
test('Test Suite - enumerations - enum_reflect_associated', py3, args: [test_args, '../test/test_suite/enumerations/enum_reflect_associated.c3t'])
test('Test Suite - enumerations - enum_associated_value', py3, args: [test_args, '../test/test_suite/enumerations/enum_associated_value.c3t'])
test('Test Suite - enumerations - enum_associated_values_other', py3, args: [test_args, '../test/test_suite/enumerations/enum_associated_values_other.c3t'])
test('Test Suite - enumerations - compile_time', py3, args: [test_args, '../test/test_suite/enumerations/compile_time.c3t'])

test('Test Suite - arrays - index_into_global', py3, args: [test_args, '../test/test_suite/arrays/index_into_global.c3t'])
test('Test Suite - arrays - array_comparison', py3, args: [test_args, '../test/test_suite/arrays/array_comparison.c3t'])
test('Test Suite - arrays - index_from_back', py3, args: [test_args, '../test/test_suite/arrays/index_from_back.c3t'])
test('Test Suite - arrays - global_init', py3, args: [test_args, '../test/test_suite/arrays/global_init.c3t'])
test('Test Suite - arrays - array_bounds_check', py3, args: [test_args, '../test/test_suite/arrays/array_bounds_check.c3t'])
test('Test Suite - arrays - complex_inferred_array', py3, args: [test_args, '../test/test_suite/arrays/complex_inferred_array.c3t'])
test('Test Suite - arrays - complex_array_const', py3, args: [test_args, '../test/test_suite/arrays/complex_array_const.c3t'])
test('Test Suite - arrays - array_comparison_2', py3, args: [test_args, '../test/test_suite/arrays/array_comparison_2.c3t'])
test('Test Suite - arrays - array_literal', py3, args: [test_args, '../test/test_suite/arrays/array_literal.c3t'])
test('Test Suite - arrays - array_struct', py3, args: [test_args, '../test/test_suite/arrays/array_struct.c3t'])
test('Test Suite - arrays - inferred_array_optional', py3, args: [test_args, '../test/test_suite/arrays/inferred_array_optional.c3t'])
test('Test Suite - arrays - array_casts', py3, args: [test_args, '../test/test_suite/arrays/array_casts.c3t'])

test('Test Suite - clang - 2002-04', py3, args: [test_args, '../test/test_suite/clang/2002-04.c3t'])
test('Test Suite - clang - 2002-01_02', py3, args: [test_args, '../test/test_suite/clang/2002-01_02.c3t'])
test('Test Suite - clang - 2002-03', py3, args: [test_args, '../test/test_suite/clang/2002-03.c3t'])
test('Test Suite - clang - 2002-07', py3, args: [test_args, '../test/test_suite/clang/2002-07.c3t'])

test('Test Suite - subarrays - slice_offset', py3, args: [test_args, '../test/test_suite/subarrays/slice_offset.c3t'])
test('Test Suite - subarrays - slice_offset_neg_end', py3, args: [test_args, '../test/test_suite/subarrays/slice_offset_neg_end.c3t'])
test('Test Suite - subarrays - slice_comparison', py3, args: [test_args, '../test/test_suite/subarrays/slice_comparison.c3t'])
test('Test Suite - subarrays - slice_offset_neg_start', py3, args: [test_args, '../test/test_suite/subarrays/slice_offset_neg_start.c3t'])
test('Test Suite - subarrays - slice_start', py3, args: [test_args, '../test/test_suite/subarrays/slice_start.c3t'])

test('Test Suite - errors - try_expr', py3, args: [test_args, '../test/test_suite/errors/try_expr.c3t'])
test('Test Suite - errors - general_error_regression', py3, args: [test_args, '../test/test_suite/errors/general_error_regression.c3t'])
test('Test Suite - errors - error_else', py3, args: [test_args, '../test/test_suite/errors/error_else.c3t'])
test('Test Suite - errors - try_catch_if', py3, args: [test_args, '../test/test_suite/errors/try_catch_if.c3t'])
test('Test Suite - errors - rethrow_mingw', py3, args: [test_args, '../test/test_suite/errors/rethrow_mingw.c3t'])
test('Test Suite - errors - macro_err2', py3, args: [test_args, '../test/test_suite/errors/macro_err2.c3t'])
test('Test Suite - errors - multiple_catch', py3, args: [test_args, '../test/test_suite/errors/multiple_catch.c3t'])
test('Test Suite - errors - or_and_rethrow', py3, args: [test_args, '../test/test_suite/errors/or_and_rethrow.c3t'])
test('Test Suite - errors - try_with_chained_unwrap', py3, args: [test_args, '../test/test_suite/errors/try_with_chained_unwrap.c3t'])
test('Test Suite - errors - optional_with_optional', py3, args: [test_args, '../test/test_suite/errors/optional_with_optional.c3t'])
test('Test Suite - errors - error_introspect', py3, args: [test_args, '../test/test_suite/errors/error_introspect.c3t'])
test('Test Suite - errors - macro_err3', py3, args: [test_args, '../test/test_suite/errors/macro_err3.c3t'])
test('Test Suite - errors - anyfault_void', py3, args: [test_args, '../test/test_suite/errors/anyfault_void.c3t'])
test('Test Suite - errors - error_regression_2', py3, args: [test_args, '../test/test_suite/errors/error_regression_2.c3t'])
test('Test Suite - errors - try_assign', py3, args: [test_args, '../test/test_suite/errors/try_assign.c3t'])
test('Test Suite - errors - simple_static_failable', py3, args: [test_args, '../test/test_suite/errors/simple_static_failable.c3t'])
test('Test Suite - errors - else_checks', py3, args: [test_args, '../test/test_suite/errors/else_checks.c3t'])
test('Test Suite - errors - try_with_unwrapper', py3, args: [test_args, '../test/test_suite/errors/try_with_unwrapper.c3t'])
test('Test Suite - errors - failable_catch', py3, args: [test_args, '../test/test_suite/errors/failable_catch.c3t'])
test('Test Suite - errors - rethrow', py3, args: [test_args, '../test/test_suite/errors/rethrow.c3t'])
test('Test Suite - errors - or_err_bool', py3, args: [test_args, '../test/test_suite/errors/or_err_bool.c3t'])
test('Test Suite - errors - try_with_unwrap', py3, args: [test_args, '../test/test_suite/errors/try_with_unwrap.c3t'])
test('Test Suite - errors - optional_taddr_and_access', py3, args: [test_args, '../test/test_suite/errors/optional_taddr_and_access.c3t'])
test('Test Suite - errors - printing_errors', py3, args: [test_args, '../test/test_suite/errors/printing_errors.c3t'])
test('Test Suite - errors - macro_err', py3, args: [test_args, '../test/test_suite/errors/macro_err.c3t'])
test('Test Suite - errors - optional_chained_init', py3, args: [test_args, '../test/test_suite/errors/optional_chained_init.c3t'])
test('Test Suite - errors - optional_inits', py3, args: [test_args, '../test/test_suite/errors/optional_inits.c3t'])

test('Test Suite - switch - switch_in_defer_macro', py3, args: [test_args, '../test/test_suite/switch/switch_in_defer_macro.c3t'])

test('Test Suite - attributes - attribute_visibility', py3, args: [test_args, '../test/test_suite/attributes/attribute_visibility.c3t'])
test('Test Suite - attributes - user_defined_attributes', py3, args: [test_args, '../test/test_suite/attributes/user_defined_attributes.c3t'])

test('Test Suite - vector - vector_init', py3, args: [test_args, '../test/test_suite/vector/vector_init.c3t'])
test('Test Suite - vector - vector_init_regression', py3, args: [test_args, '../test/test_suite/vector/vector_init_regression.c3t'])
test('Test Suite - vector - vector_to_array_cast', py3, args: [test_args, '../test/test_suite/vector/vector_to_array_cast.c3t'])
test('Test Suite - vector - vector_param', py3, args: [test_args, '../test/test_suite/vector/vector_param.c3t'])
test('Test Suite - vector - gather_scatter', py3, args: [test_args, '../test/test_suite/vector/gather_scatter.c3t'])
test('Test Suite - vector - vector_bit', py3, args: [test_args, '../test/test_suite/vector/vector_bit.c3t'])
test('Test Suite - vector - vector_consts', py3, args: [test_args, '../test/test_suite/vector/vector_consts.c3t'])
test('Test Suite - vector - vector_ops2', py3, args: [test_args, '../test/test_suite/vector/vector_ops2.c3t'])
test('Test Suite - vector - vector_incdec', py3, args: [test_args, '../test/test_suite/vector/vector_incdec.c3t'])

test('Test Suite - constants - char_literals', py3, args: [test_args, '../test/test_suite/constants/char_literals.c3t'])
test('Test Suite - constants - byte_literals', py3, args: [test_args, '../test/test_suite/constants/byte_literals.c3t'])
test('Test Suite - constants - constants', py3, args: [test_args, '../test/test_suite/constants/constants.c3t'])
test('Test Suite - constants - init_order', py3, args: [test_args, '../test/test_suite/constants/init_order.c3t'])
test('Test Suite - constants - float_type', py3, args: [test_args, '../test/test_suite/constants/float_type.c3t'])
test('Test Suite - constants - const_var_copy', py3, args: [test_args, '../test/test_suite/constants/const_var_copy.c3t'])

test('Test Suite - pointers - const_pointer', py3, args: [test_args, '../test/test_suite/pointers/const_pointer.c3t'])
test('Test Suite - pointers - subarray_variant_to_ptr', py3, args: [test_args, '../test/test_suite/pointers/subarray_variant_to_ptr.c3t'])
test('Test Suite - pointers - array_pointer_decay', py3, args: [test_args, '../test/test_suite/pointers/array_pointer_decay.c3t'])
test('Test Suite - pointers - pointer_index', py3, args: [test_args, '../test/test_suite/pointers/pointer_index.c3t'])

test('Test Suite - lambda - ct_lambda2', py3, args: [test_args, '../test/test_suite/lambda/ct_lambda2.c3t'])
test('Test Suite - lambda - ct_lambda', py3, args: [test_args, '../test/test_suite/lambda/ct_lambda.c3t'])
test('Test Suite - lambda - lambda_in_macro', py3, args: [test_args, '../test/test_suite/lambda/lambda_in_macro.c3t'])
test('Test Suite - lambda - nested_lambda_def', py3, args: [test_args, '../test/test_suite/lambda/nested_lambda_def.c3t'])
test('Test Suite - lambda - simple_lambda', py3, args: [test_args, '../test/test_suite/lambda/simple_lambda.c3t'])

test('Test Suite - slices - slice_init', py3, args: [test_args, '../test/test_suite/slices/slice_init.c3t'])
test('Test Suite - slices - slice_assign', py3, args: [test_args, '../test/test_suite/slices/slice_assign.c3t'])
test('Test Suite - slices - slice_assign2', py3, args: [test_args, '../test/test_suite/slices/slice_assign2.c3t'])
test('Test Suite - slices - slice_to_slice_assign', py3, args: [test_args, '../test/test_suite/slices/slice_to_slice_assign.c3t'])
test('Test Suite - slices - slice_to_slice_vector_assign', py3, args: [test_args, '../test/test_suite/slices/slice_to_slice_vector_assign.c3t'])

test('Test Suite - compile_time - compile_time_ptr_ref', py3, args: [test_args, '../test/test_suite/compile_time/compile_time_ptr_ref.c3t'])
test('Test Suite - compile_time - ct_for', py3, args: [test_args, '../test/test_suite/compile_time/ct_for.c3t'])
test('Test Suite - compile_time - typeof_example', py3, args: [test_args, '../test/test_suite/compile_time/typeof_example.c3t'])
test('Test Suite - compile_time - untyped_conversions', py3, args: [test_args, '../test/test_suite/compile_time/untyped_conversions.c3t'])
test('Test Suite - compile_time - ct_switch_type_check', py3, args: [test_args, '../test/test_suite/compile_time/ct_switch_type_check.c3t'])
test('Test Suite - compile_time - compile_time_array_ref', py3, args: [test_args, '../test/test_suite/compile_time/compile_time_array_ref.c3t'])
test('Test Suite - compile_time - typed_ct_vars', py3, args: [test_args, '../test/test_suite/compile_time/typed_ct_vars.c3t'])
test('Test Suite - compile_time - typefrom', py3, args: [test_args, '../test/test_suite/compile_time/typefrom.c3t'])
test('Test Suite - compile_time - cttype_reassign', py3, args: [test_args, '../test/test_suite/compile_time/cttype_reassign.c3t'])
test('Test Suite - compile_time - stringify2', py3, args: [test_args, '../test/test_suite/compile_time/stringify2.c3t'])
test('Test Suite - compile_time - compile_time_array', py3, args: [test_args, '../test/test_suite/compile_time/compile_time_array.c3t'])
test('Test Suite - compile_time - ct_switch_top_level', py3, args: [test_args, '../test/test_suite/compile_time/ct_switch_top_level.c3t'])
test('Test Suite - compile_time - ct_memberof', py3, args: [test_args, '../test/test_suite/compile_time/ct_memberof.c3t'])
test('Test Suite - compile_time - ct_through_constant', py3, args: [test_args, '../test/test_suite/compile_time/ct_through_constant.c3t'])
test('Test Suite - compile_time - typefrom_errors', py3, args: [test_args, '../test/test_suite/compile_time/typefrom_errors.c3t'])
test('Test Suite - compile_time - ct_funcptr', py3, args: [test_args, '../test/test_suite/compile_time/ct_funcptr.c3t'])
test('Test Suite - compile_time - stringify', py3, args: [test_args, '../test/test_suite/compile_time/stringify.c3t'])
test('Test Suite - compile_time - ct_if', py3, args: [test_args, '../test/test_suite/compile_time/ct_if.c3t'])
test('Test Suite - compile_time - ct_value_from_access', py3, args: [test_args, '../test/test_suite/compile_time/ct_value_from_access.c3t'])
test('Test Suite - compile_time - compile_time_bitops', py3, args: [test_args, '../test/test_suite/compile_time/compile_time_bitops.c3t'])
test('Test Suite - compile_time - ct_func', py3, args: [test_args, '../test/test_suite/compile_time/ct_func.c3t'])
test('Test Suite - compile_time - compile_time_access_subscript', py3, args: [test_args, '../test/test_suite/compile_time/compile_time_access_subscript.c3t'])
test('Test Suite - compile_time - ct_eval', py3, args: [test_args, '../test/test_suite/compile_time/ct_eval.c3t'])
test('Test Suite - compile_time - compile_time_pointers', py3, args: [test_args, '../test/test_suite/compile_time/compile_time_pointers.c3t'])
test('Test Suite - compile_time - ct_builtin_time_date', py3, args: [test_args, '../test/test_suite/compile_time/ct_builtin_time_date.c3t'])
test('Test Suite - compile_time - ct_switch', py3, args: [test_args, '../test/test_suite/compile_time/ct_switch.c3t'])
test('Test Suite - compile_time - ternary_folding', py3, args: [test_args, '../test/test_suite/compile_time/ternary_folding.c3t'])
test('Test Suite - compile_time - ct_foreach', py3, args: [test_args, '../test/test_suite/compile_time/ct_foreach.c3t'])
test('Test Suite - compile_time - ct_enum_values', py3, args: [test_args, '../test/test_suite/compile_time/ct_enum_values.c3t'])

test('Test Suite - import - import_works', py3, args: [test_args, '../test/test_suite/import/import_works.c3t'])
test('Test Suite - import - access_other_module', py3, args: [test_args, '../test/test_suite/import/access_other_module.c3t'])

test('Test Suite - globals - external_global', py3, args: [test_args, '../test/test_suite/globals/external_global.c3t'])
test('Test Suite - globals - global_no_init', py3, args: [test_args, '../test/test_suite/globals/global_no_init.c3t'])
test('Test Suite - globals - init_with_err', py3, args: [test_args, '../test/test_suite/globals/init_with_err.c3t'])
test('Test Suite - globals - global_extname', py3, args: [test_args, '../test/test_suite/globals/global_extname.c3t'])
test('Test Suite - globals - global_align', py3, args: [test_args, '../test/test_suite/globals/global_align.c3t'])

test('Test Suite - floats - mod', py3, args: [test_args, '../test/test_suite/floats/mod.c3t'])
test('Test Suite - floats - convert_float', py3, args: [test_args, '../test/test_suite/floats/convert_float.c3t'])
test('Test Suite - floats - inf_nan', py3, args: [test_args, '../test/test_suite/floats/inf_nan.c3t'])

test('Test Suite - bitstruct - bitstruct_intcontainer', py3, args: [test_args, '../test/test_suite/bitstruct/bitstruct_intcontainer.c3t'])
test('Test Suite - bitstruct - bitstruct_be', py3, args: [test_args, '../test/test_suite/bitstruct/bitstruct_be.c3t'])
test('Test Suite - bitstruct - bitstruct_cast_const_init', py3, args: [test_args, '../test/test_suite/bitstruct/bitstruct_cast_const_init.c3t'])
test('Test Suite - bitstruct - bitstruct_arrays', py3, args: [test_args, '../test/test_suite/bitstruct/bitstruct_arrays.c3t'])
test('Test Suite - bitstruct - param_bitstruct', py3, args: [test_args, '../test/test_suite/bitstruct/param_bitstruct.c3t'])
test('Test Suite - bitstruct - bitfield_access', py3, args: [test_args, '../test/test_suite/bitstruct/bitfield_access.c3t'])
test('Test Suite - bitstruct - bitstruct_arrays_be', py3, args: [test_args, '../test/test_suite/bitstruct/bitstruct_arrays_be.c3t'])
test('Test Suite - bitstruct - bitstruct_anon_in_struct_ok', py3, args: [test_args, '../test/test_suite/bitstruct/bitstruct_anon_in_struct_ok.c3t'])
test('Test Suite - bitstruct - bitstruct_ops', py3, args: [test_args, '../test/test_suite/bitstruct/bitstruct_ops.c3t'])
test('Test Suite - bitstruct - bitstruct_to_int', py3, args: [test_args, '../test/test_suite/bitstruct/bitstruct_to_int.c3t'])
test('Test Suite - bitstruct - bitstruct_init_bool', py3, args: [test_args, '../test/test_suite/bitstruct/bitstruct_init_bool.c3t'])
test('Test Suite - bitstruct - embedded_bitstruct', py3, args: [test_args, '../test/test_suite/bitstruct/embedded_bitstruct.c3t'])
test('Test Suite - bitstruct - bitstruct_in_subarray', py3, args: [test_args, '../test/test_suite/bitstruct/bitstruct_in_subarray.c3t'])
test('Test Suite - bitstruct - bitstruct_initializer', py3, args: [test_args, '../test/test_suite/bitstruct/bitstruct_initializer.c3t'])
test('Test Suite - bitstruct - designated_initializer_with_bitstruct', py3, args: [test_args, '../test/test_suite/bitstruct/designated_initializer_with_bitstruct.c3t'])
test('Test Suite - bitstruct - array_with_boolean', py3, args: [test_args, '../test/test_suite/bitstruct/array_with_boolean.c3t'])
test('Test Suite - bitstruct - bitstruct_access_signed', py3, args: [test_args, '../test/test_suite/bitstruct/bitstruct_access_signed.c3t'])

test('Test Suite - assignment - var_variable', py3, args: [test_args, '../test/test_suite/assignment/var_variable.c3t'])
test('Test Suite - assignment - int_assign', py3, args: [test_args, '../test/test_suite/assignment/int_assign.c3t'])

test('Test Suite - compile_time_introspection - alignof', py3, args: [test_args, '../test/test_suite/compile_time_introspection/alignof.c3t'])
test('Test Suite - compile_time_introspection - nameof', py3, args: [test_args, '../test/test_suite/compile_time_introspection/nameof.c3t'])
test('Test Suite - compile_time_introspection - parentof', py3, args: [test_args, '../test/test_suite/compile_time_introspection/parentof.c3t'])
test('Test Suite - compile_time_introspection - offsetof', py3, args: [test_args, '../test/test_suite/compile_time_introspection/offsetof.c3t'])
test('Test Suite - compile_time_introspection - defined_2', py3, args: [test_args, '../test/test_suite/compile_time_introspection/defined_2.c3t'])
test('Test Suite - compile_time_introspection - sizeof', py3, args: [test_args, '../test/test_suite/compile_time_introspection/sizeof.c3t'])
test('Test Suite - compile_time_introspection - defined', py3, args: [test_args, '../test/test_suite/compile_time_introspection/defined.c3t'])
test('Test Suite - compile_time_introspection - defined_builtin', py3, args: [test_args, '../test/test_suite/compile_time_introspection/defined_builtin.c3t'])
test('Test Suite - compile_time_introspection - qnameof', py3, args: [test_args, '../test/test_suite/compile_time_introspection/qnameof.c3t'])

test('Test Suite - visibility - not_visible', py3, args: [test_args, '../test/test_suite/visibility/not_visible.c3t'])
test('Test Suite - visibility - no_shared_imports', py3, args: [test_args, '../test/test_suite/visibility/no_shared_imports.c3t'])
test('Test Suite - visibility - shared_module', py3, args: [test_args, '../test/test_suite/visibility/shared_module.c3t'])
test('Test Suite - visibility - simple_visibility', py3, args: [test_args, '../test/test_suite/visibility/simple_visibility.c3t'])
test('Test Suite - visibility - ambiguous_var', py3, args: [test_args, '../test/test_suite/visibility/ambiguous_var.c3t'])

test('Test Suite - module - module_section_export', py3, args: [test_args, '../test/test_suite/module/module_section_export.c3t'])

test('Test Suite - defer - defer_static_var', py3, args: [test_args, '../test/test_suite/defer/defer_static_var.c3t'])
test('Test Suite - defer - defer_catch_try', py3, args: [test_args, '../test/test_suite/defer/defer_catch_try.c3t'])
test('Test Suite - defer - defer_and_expr_block', py3, args: [test_args, '../test/test_suite/defer/defer_and_expr_block.c3t'])

test('Test Suite - concurrency - atomic_load_store_debug', py3, args: [test_args, '../test/test_suite/concurrency/atomic_load_store_debug.c3t'])
test('Test Suite - concurrency - atomic_load_store', py3, args: [test_args, '../test/test_suite/concurrency/atomic_load_store.c3t'])

test('Test Suite - generic - generic_lambda_complex', py3, args: [test_args, '../test/test_suite/generic/generic_lambda_complex.c3t'])
test('Test Suite - generic - generic_with_enum', py3, args: [test_args, '../test/test_suite/generic/generic_with_enum.c3t'])
test('Test Suite - generic - enum_set_test', py3, args: [test_args, '../test/test_suite/generic/enum_set_test.c3t'])
test('Test Suite - generic - generic_recursion', py3, args: [test_args, '../test/test_suite/generic/generic_recursion.c3t'])
test('Test Suite - generic - generic_over_fn', py3, args: [test_args, '../test/test_suite/generic/generic_over_fn.c3t'])
test('Test Suite - generic - generic_num', py3, args: [test_args, '../test/test_suite/generic/generic_num.c3t'])
test('Test Suite - generic - generic_copy', py3, args: [test_args, '../test/test_suite/generic/generic_copy.c3t'])
test('Test Suite - generic - generic_idents', py3, args: [test_args, '../test/test_suite/generic/generic_idents.c3t'])

test('Test Suite - debug_symbols - constants_mingw', py3, args: [test_args, '../test/test_suite/debug_symbols/constants_mingw.c3t'])
test('Test Suite - debug_symbols - constants', py3, args: [test_args, '../test/test_suite/debug_symbols/constants.c3t'])

test('Test Suite - struct - struct_const_construct_simple', py3, args: [test_args, '../test/test_suite/struct/struct_const_construct_simple.c3t'])
test('Test Suite - struct - struct_codegen_fam', py3, args: [test_args, '../test/test_suite/struct/struct_codegen_fam.c3t'])
test('Test Suite - struct - nested_struct_union_init', py3, args: [test_args, '../test/test_suite/struct/nested_struct_union_init.c3t'])
test('Test Suite - struct - simple_struct', py3, args: [test_args, '../test/test_suite/struct/simple_struct.c3t'])
test('Test Suite - struct - struct_as_value', py3, args: [test_args, '../test/test_suite/struct/struct_as_value.c3t'])
test('Test Suite - struct - struct_codegen_empty', py3, args: [test_args, '../test/test_suite/struct/struct_codegen_empty.c3t'])
test('Test Suite - struct - struct_codegen', py3, args: [test_args, '../test/test_suite/struct/struct_codegen.c3t'])
test('Test Suite - struct - struct_as_value_aarch64', py3, args: [test_args, '../test/test_suite/struct/struct_as_value_aarch64.c3t'])
test('Test Suite - struct - nested_struct_init', py3, args: [test_args, '../test/test_suite/struct/nested_struct_init.c3t'])
test('Test Suite - struct - struct_pack_and_align', py3, args: [test_args, '../test/test_suite/struct/struct_pack_and_align.c3t'])
test('Test Suite - struct - sret', py3, args: [test_args, '../test/test_suite/struct/sret.c3t'])

test('Test Suite - macro_methods - macro_method_different_args', py3, args: [test_args, '../test/test_suite/macro_methods/macro_method_different_args.c3t'])

test('Test Suite - abi - small_struct_x64', py3, args: [test_args, '../test/test_suite/abi/small_struct_x64.c3t'])
test('Test Suite - abi - darwin_return_boolarray', py3, args: [test_args, '../test/test_suite/abi/darwin_return_boolarray.c3t'])
test('Test Suite - abi - macho_section', py3, args: [test_args, '../test/test_suite/abi/macho_section.c3t'])
test('Test Suite - abi - darwinx64_1', py3, args: [test_args, '../test/test_suite/abi/darwinx64_1.c3t'])
test('Test Suite - abi - vec2_x64', py3, args: [test_args, '../test/test_suite/abi/vec2_x64.c3t'])
test('Test Suite - abi - riscv32-ilp32-ilp32f-abi-1', py3, args: [test_args, '../test/test_suite/abi/riscv32-ilp32-ilp32f-abi-1.c3t'])
test('Test Suite - abi - riscv64-lp64-lp64f-lp64d-abi-1', py3, args: [test_args, '../test/test_suite/abi/riscv64-lp64-lp64f-lp64d-abi-1.c3t'])
test('Test Suite - abi - darwin64_sret', py3, args: [test_args, '../test/test_suite/abi/darwin64_sret.c3t'])
test('Test Suite - abi - riscv32-ilp32-ilp32f-ilp32d-abi-2', py3, args: [test_args, '../test/test_suite/abi/riscv32-ilp32-ilp32f-ilp32d-abi-2.c3t'])
test('Test Suite - abi - darwinx64_2', py3, args: [test_args, '../test/test_suite/abi/darwinx64_2.c3t'])
test('Test Suite - abi - riscv32-ilp32-ilp32f-ilp32d-abi-1', py3, args: [test_args, '../test/test_suite/abi/riscv32-ilp32-ilp32f-ilp32d-abi-1.c3t'])
test('Test Suite - abi - riscv64-lp64-lp64f-lp64d-abi-3', py3, args: [test_args, '../test/test_suite/abi/riscv64-lp64-lp64f-lp64d-abi-3.c3t'])
test('Test Suite - abi - aarch64_hfa_args', py3, args: [test_args, '../test/test_suite/abi/aarch64_hfa_args.c3t'])
test('Test Suite - abi - riscv64-lp64-lp64f-lp64d-abi-2', py3, args: [test_args, '../test/test_suite/abi/riscv64-lp64-lp64f-lp64d-abi-2.c3t'])
test('Test Suite - abi - test_sret', py3, args: [test_args, '../test/test_suite/abi/test_sret.c3t'])
test('Test Suite - abi - sysv_direct_coerce', py3, args: [test_args, '../test/test_suite/abi/sysv_direct_coerce.c3t'])
test('Test Suite - abi - sysv_abi_avx', py3, args: [test_args, '../test/test_suite/abi/sysv_abi_avx.c3t'])
test('Test Suite - abi - darwin64_avx', py3, args: [test_args, '../test/test_suite/abi/darwin64_avx.c3t'])
test('Test Suite - abi - x64alignarray', py3, args: [test_args, '../test/test_suite/abi/x64alignarray.c3t'])
test('Test Suite - abi - riscv64-lp64-lp64f-abi-2', py3, args: [test_args, '../test/test_suite/abi/riscv64-lp64-lp64f-abi-2.c3t'])
test('Test Suite - abi - literal_load_aarch64', py3, args: [test_args, '../test/test_suite/abi/literal_load_aarch64.c3t'])
test('Test Suite - abi - darwin_arg', py3, args: [test_args, '../test/test_suite/abi/darwin_arg.c3t'])
test('Test Suite - abi - vec2_wasm', py3, args: [test_args, '../test/test_suite/abi/vec2_wasm.c3t'])
test('Test Suite - abi - wasm_extern', py3, args: [test_args, '../test/test_suite/abi/wasm_extern.c3t'])
test('Test Suite - abi - riscv64-lp64f-lp64d-abi-2', py3, args: [test_args, '../test/test_suite/abi/riscv64-lp64f-lp64d-abi-2.c3t'])
test('Test Suite - abi - riscv32-ilp32f-abi', py3, args: [test_args, '../test/test_suite/abi/riscv32-ilp32f-abi.c3t'])
test('Test Suite - abi - riscv32-ilp32-ilp32f-ilp32d-abi-3', py3, args: [test_args, '../test/test_suite/abi/riscv32-ilp32-ilp32f-ilp32d-abi-3.c3t'])
test('Test Suite - abi - union_x64', py3, args: [test_args, '../test/test_suite/abi/union_x64.c3t'])
test('Test Suite - abi - literal_load', py3, args: [test_args, '../test/test_suite/abi/literal_load.c3t'])
test('Test Suite - abi - riscv64-lp64-abi', py3, args: [test_args, '../test/test_suite/abi/riscv64-lp64-abi.c3t'])
test('Test Suite - abi - darwin64_sse', py3, args: [test_args, '../test/test_suite/abi/darwin64_sse.c3t'])
test('Test Suite - abi - aarch64_args', py3, args: [test_args, '../test/test_suite/abi/aarch64_args.c3t'])
test('Test Suite - abi - literal_load_mingw', py3, args: [test_args, '../test/test_suite/abi/literal_load_mingw.c3t'])
test('Test Suite - abi - sysv_abi_noavx', py3, args: [test_args, '../test/test_suite/abi/sysv_abi_noavx.c3t'])
test('Test Suite - abi - riscv32-ilp32d-abi', py3, args: [test_args, '../test/test_suite/abi/riscv32-ilp32d-abi.c3t'])
test('Test Suite - abi - riscv64-lp64-lp64f-abi-1', py3, args: [test_args, '../test/test_suite/abi/riscv64-lp64-lp64f-abi-1.c3t'])
test('Test Suite - abi - riscv32-ilp32f-ilp32d-abi-2', py3, args: [test_args, '../test/test_suite/abi/riscv32-ilp32f-ilp32d-abi-2.c3t'])
test('Test Suite - abi - riscv32-ilp32-abi', py3, args: [test_args, '../test/test_suite/abi/riscv32-ilp32-abi.c3t'])
test('Test Suite - abi - riscv32-ilp32-ilp32f-abi-2', py3, args: [test_args, '../test/test_suite/abi/riscv32-ilp32-ilp32f-abi-2.c3t'])
test('Test Suite - abi - riscv64-lp64f-lp64d-abi-1', py3, args: [test_args, '../test/test_suite/abi/riscv64-lp64f-lp64d-abi-1.c3t'])
test('Test Suite - abi - riscv64-lp64d-abi', py3, args: [test_args, '../test/test_suite/abi/riscv64-lp64d-abi.c3t'])
test('Test Suite - abi - pass_large_aarch', py3, args: [test_args, '../test/test_suite/abi/pass_large_aarch.c3t'])
test('Test Suite - abi - vec2_aarch64', py3, args: [test_args, '../test/test_suite/abi/vec2_aarch64.c3t'])
test('Test Suite - abi - darwin64_avx512', py3, args: [test_args, '../test/test_suite/abi/darwin64_avx512.c3t'])
test('Test Suite - abi - avx512fp16-abi', py3, args: [test_args, '../test/test_suite/abi/avx512fp16-abi.c3t'])
test('Test Suite - abi - riscv32-ilp32f-ilp32d-abi-1', py3, args: [test_args, '../test/test_suite/abi/riscv32-ilp32f-ilp32d-abi-1.c3t'])

test('Test Suite - union - union_codegen_empty', py3, args: [test_args, '../test/test_suite/union/union_codegen_empty.c3t'])
test('Test Suite - union - union_in_struct', py3, args: [test_args, '../test/test_suite/union/union_in_struct.c3t'])
test('Test Suite - union - union_codegen_const', py3, args: [test_args, '../test/test_suite/union/union_codegen_const.c3t'])
test('Test Suite - union - union_codegen_overwrite_call', py3, args: [test_args, '../test/test_suite/union/union_codegen_overwrite_call.c3t'])

test('Test Suite - embed - embed_basic', py3, args: [test_args, '../test/test_suite/embed/embed_basic.c3t'])

test('Test Suite - assert - unreachable', py3, args: [test_args, '../test/test_suite/assert/unreachable.c3t'])
test('Test Suite - assert - assert_variants', py3, args: [test_args, '../test/test_suite/assert/assert_variants.c3t'])
test('Test Suite - assert - assertf', py3, args: [test_args, '../test/test_suite/assert/assertf.c3t'])

test('Test Suite - asm - asm_ops_x64_2', py3, args: [test_args, '../test/test_suite/asm/asm_ops_x64_2.c3t'])
test('Test Suite - asm - sideeffect', py3, args: [test_args, '../test/test_suite/asm/sideeffect.c3t'])
test('Test Suite - asm - naked', py3, args: [test_args, '../test/test_suite/asm/naked.c3t'])
test('Test Suite - asm - asm_regression', py3, args: [test_args, '../test/test_suite/asm/asm_regression.c3t'])
test('Test Suite - asm - asm_ops_x64_1', py3, args: [test_args, '../test/test_suite/asm/asm_ops_x64_1.c3t'])
test('Test Suite - asm - syscall', py3, args: [test_args, '../test/test_suite/asm/syscall.c3t'])

test('Test Suite - stdlib - memcomp', py3, args: [test_args, '../test/test_suite/stdlib/memcomp.c3t'])
test('Test Suite - stdlib - map', py3, args: [test_args, '../test/test_suite/stdlib/map.c3t'])
test('Test Suite - stdlib - priorityqueue', py3, args: [test_args, '../test/test_suite/stdlib/priorityqueue.c3t'])

test('Test Suite - overloading - set_overload', py3, args: [test_args, '../test/test_suite/overloading/set_overload.c3t'])

test('Test Suite - expressions - addr_compiles', py3, args: [test_args, '../test/test_suite/expressions/addr_compiles.c3t'])
test('Test Suite - expressions - assignment_precedence', py3, args: [test_args, '../test/test_suite/expressions/assignment_precedence.c3t'])
test('Test Suite - expressions - optional_ternary', py3, args: [test_args, '../test/test_suite/expressions/optional_ternary.c3t'])
test('Test Suite - expressions - parsed_numbers', py3, args: [test_args, '../test/test_suite/expressions/parsed_numbers.c3t'])
test('Test Suite - expressions - call_inline', py3, args: [test_args, '../test/test_suite/expressions/call_inline.c3t'])
test('Test Suite - expressions - check_implict_conversion_signed_unsigned', py3, args: [test_args, '../test/test_suite/expressions/check_implict_conversion_signed_unsigned.c3t'])
test('Test Suite - expressions - chained_ternary', py3, args: [test_args, '../test/test_suite/expressions/chained_ternary.c3t'])
test('Test Suite - expressions - ternary_bool', py3, args: [test_args, '../test/test_suite/expressions/ternary_bool.c3t'])
test('Test Suite - expressions - type_support', py3, args: [test_args, '../test/test_suite/expressions/type_support.c3t'])
test('Test Suite - expressions - chained_conditional', py3, args: [test_args, '../test/test_suite/expressions/chained_conditional.c3t'])
test('Test Suite - expressions - bool_conversions', py3, args: [test_args, '../test/test_suite/expressions/bool_conversions.c3t'])
test('Test Suite - expressions - bit_op_on_bool', py3, args: [test_args, '../test/test_suite/expressions/bit_op_on_bool.c3t'])
test('Test Suite - expressions - strings', py3, args: [test_args, '../test/test_suite/expressions/strings.c3t'])
test('Test Suite - expressions - fmuladd_err', py3, args: [test_args, '../test/test_suite/expressions/fmuladd_err.c3t'])
test('Test Suite - expressions - folding_ptr', py3, args: [test_args, '../test/test_suite/expressions/folding_ptr.c3t'])
test('Test Suite - expressions - fmuladd', py3, args: [test_args, '../test/test_suite/expressions/fmuladd.c3t'])
test('Test Suite - expressions - enum_ct_sub', py3, args: [test_args, '../test/test_suite/expressions/enum_ct_sub.c3t'])
test('Test Suite - expressions - simple_float_sub_neg', py3, args: [test_args, '../test/test_suite/expressions/simple_float_sub_neg.c3t'])
test('Test Suite - expressions - 2002-02-13-ConditionalInCall', py3, args: [test_args, '../test/test_suite/expressions/2002-02-13-ConditionalInCall.c3t'])
test('Test Suite - expressions - elvis', py3, args: [test_args, '../test/test_suite/expressions/elvis.c3t'])
test('Test Suite - expressions - take_address', py3, args: [test_args, '../test/test_suite/expressions/take_address.c3t'])
test('Test Suite - expressions - pointer_access', py3, args: [test_args, '../test/test_suite/expressions/pointer_access.c3t'])
test('Test Suite - expressions - incdec', py3, args: [test_args, '../test/test_suite/expressions/incdec.c3t'])

test('Test Suite - casts - cast_expr', py3, args: [test_args, '../test/test_suite/expressions/casts/cast_expr.c3t'])
