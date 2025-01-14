py3 = find_program('python3', required: true)
test_args = ['../test/src/check.py', '../test/src/tester.py', c3c_exe]

# Define the test suites so that we can programmatically generate the tests

abi = [
    'aarch64_args',
    'aarch64_hfa_args',
    'avx512fp16-abi',
    'darwin_arg',
    'darwin_return_boolarray',
    'darwin64_avx',
    'darwin64_avx512',
    'darwin64_sret',
    'darwin64_sse',
    'darwinx64_1',
    'darwinx64_2',
    'literal_load_aarch64',
    'literal_load_mingw',
    'literal_load',
    'macho_section',
    'pass_large_aarch',
    'riscv32-ilp32-abi',
    'riscv32-ilp32-ilp32f-abi-1',
    'riscv32-ilp32-ilp32f-abi-2',
    'riscv32-ilp32-ilp32f-ilp32d-abi-1',
    'riscv32-ilp32-ilp32f-ilp32d-abi-2',
    'riscv32-ilp32-ilp32f-ilp32d-abi-3',
    'riscv32-ilp32d-abi',
    'riscv32-ilp32f-abi',
    'riscv32-ilp32f-ilp32d-abi-1',
    'riscv32-ilp32f-ilp32d-abi-2',
    'riscv64-lp64-abi',
    'riscv64-lp64-lp64f-abi-1',
    'riscv64-lp64-lp64f-abi-2',
    'riscv64-lp64-lp64f-lp64d-abi-1',
    'riscv64-lp64-lp64f-lp64d-abi-2',
    'riscv64-lp64-lp64f-lp64d-abi-3',
    'riscv64-lp64d-abi',
    'riscv64-lp64f-lp64d-abi-1',
    'riscv64-lp64f-lp64d-abi-2',
    'small_struct_x64',
    'sysv_abi_avx',
    'sysv_abi_noavx',
    'sysv_direct_coerce',
    'test_sret',
    'union_x64',
    'vec2_aarch64',
    'vec2_wasm',
    'vec2_x64',
    'wasm_extern',
    'x64alignarray',
]

any = ['any_in_any', 'variant_assign', 'variant_switch', 'variant_test']

arrays = [
    'array_bounds_check',
    'array_casts',
    'array_comparison_2',
    'array_comparison',
    'array_literal',
    'array_struct',
    'complex_array_const',
    'complex_inferred_array',
    'global_init',
    'index_from_back',
    'index_into_global',
    'inferred_array_optional',
]

asm = [
    'asm_ops_x64_1',
    'asm_ops_x64_2',
    'asm_regression',
    'naked',
    'sideeffect',
    'syscall',
]

assert = [
    'assert_variants',
    'assertf',
    'unreachable',
]

assignment = [
    'alignment_index',
    'int_assign',
    'var_variable',
]

attributes = [
    'attribute_visibility',
    'user_defined_attributes',
]

bitstruct = [
    'array_with_boolean',
    'bitfield_access',
    'bitstruct_access_signed',
    'bitstruct_anon_in_struct_ok',
    'bitstruct_arrays_be',
    'bitstruct_arrays',
    'bitstruct_be',
    'bitstruct_cast_const_init',
    'bitstruct_in_subarray',
    'bitstruct_init_bool',
    'bitstruct_initializer',
    'bitstruct_intcontainer',
    'bitstruct_ops',
    'bitstruct_to_int',
    'designated_initializer_with_bitstruct',
    'embedded_bitstruct',
    'param_bitstruct',
]

builtins = [
    'builtin_vector_abs',
    'builtin_vector_min_max',
    'exacts',
    'mem',
    'overflows',
    'prefetch',
    'reduce_arithmetics',
    'reverse_builtin',
    'rounding_builtins',
    'sat_builtins',
    'shufflevector',
    'simple_builtins',
]

cast = [
    'cast_bitstruct_etc',
    'cast_from_ptr',
    'cast_narrow_alias',
    'cast_subarray',
    'implicit_infer_len_cast',
    'top_down_casts',
]

clang = [
    '2002-01_02',
    '2002-03',
    '2002-04',
    '2002-07',
]

compile_time = [
    'compile_time_access_subscript',
    'compile_time_array_ref',
    'compile_time_array',
    'compile_time_bitops',
    'compile_time_pointers',
    'compile_time_ptr_ref',
    'ct_builtin_time_date',
    'ct_enum_values',
    'ct_eval',
    'ct_for',
    'ct_foreach',
    'ct_func',
    'ct_funcptr',
    'ct_if',
    'ct_memberof',
    'ct_switch_top_level',
    'ct_switch_type_check',
    'ct_switch',
    'ct_through_constant',
    'ct_value_from_access',
    'cttype_reassign',
    'stringify',
    'stringify2',
    'ternary_folding',
    'typed_ct_vars',
    'typefrom_errors',
    'typefrom',
    'typeof_example',
    'untyped_conversions',
]

compile_time_introspection = [
    'alignof',
    'defined',
    'defined_2',
    'defined_builtin',
    'nameof',
    'offsetof',
    'parentof',
    'qnameof',
    'sizeof',
]

concurrency = ['atomic_load_store_debug', 'atomic_load_store']

constants = [
    'byte_literals',
    'char_literals',
    'const_var_copy',
    'constants',
    'float_type',
    'init_order',
]

contracts = [
    'simple_test'
]

debug_symbols = [
    'constants_mingw',
    'constants',
]

defer = [
    'defer_and_expr_block',
    'defer_catch_try',
    'defer_static_var',
]

distinct = [
    'distinct_shift',
]

dynamic = [
    'inherit_linux',
    'inherit_macos',
    'null_and_protocol',
    'overlapping_function_linux',
    'overlapping_function_macos',
]

embed = [
    'embed_basic',
]

enumerations = [
    'enum_add_sub',
    'enum_associated_value',
    'enum_associated_values_other',
    'enum_cast',
    'enum_conversions',
    'enum_reflect_associated',
    'enum_values',
    'simple_inference',
]

errors = [
    'anyfault_void',
    'else_checks',
    'error_else',
    'error_introspect',
    'error_regression_2',
    'failable_catch',
    'general_error_regression',
    'macro_err',
    'macro_err2',
    'macro_err3',
    'multiple_catch',
    'optional_chained_init',
    'optional_inits',
    'optional_taddr_and_access',
    'optional_with_optional',
    'or_and_rethrow',
    'or_err_bool',
    'printing_errors',
    'rethrow_mingw',
    'rethrow',
    'simple_static_failable',
    'try_assign',
    'try_catch_if',
    'try_expr',
    'try_with_chained_unwrap',
    'try_with_unwrap',
    'try_with_unwrapper',

]

expressions = [
    'casts/cast_expr', # Odd one out, should probably handle properly one day but this is fine for now.
    '2002-02-13-ConditionalInCall',
    'addr_compiles',
    'assignment_precedence',
    'bit_op_on_bool',
    'bool_conversions',
    'call_inline',
    'chained_conditional',
    'chained_ternary',
    'check_implict_conversion_signed_unsigned',
    'elvis',
    'enum_ct_sub',
    'fmuladd_err',
    'fmuladd',
    'folding_ptr',
    'incdec',
    'optional_ternary',
    'parsed_numbers',
    'pointer_access',
    'simple_float_sub_neg',
    'strings',
    'take_address',
    'ternary_bool',
    'type_support',
]

floats = [
    'convert_float',
    'inf_nan',
    'mod',
]

from_docs = [
    'examples_defer',
    'examples_forswitch',
    'examples_functionpointer',
    'examples_if_catch',
    'examples_macro_function',
]

functions = [
    'assorted_tests',
    'c_vararg_expansion',
    'defered_default_arguments',
    'distinct_fn_ptr_and_lambda',
    'func_ptr_conversion_alias',
    'func_ptr_conversions_and_names',
    'naked_function',
    'returning_void',
    'simple_test',
    'splat_aarch64',
    'splat_mingw',
    'splat',
    'static_vars',
    'test_regression_mingw',
    'test_regression',
    'typeless_varargs',
    'varargs_followed_by_named',
    'varargs',
]

generic = [
    'enum_set_test',
    'generic_copy',
    'generic_idents',
    'generic_lambda_complex',
    'generic_num',
    'generic_over_fn',
    'generic_recursion',
    'generic_with_enum',
]

globals = [
    'external_global',
    'global_align',
    'global_extname',
    'global_no_init',
    'init_with_err',
]

import = [
    'access_other_module',
    'import_works',
]

initialize = [
    'initializer_var',
    'initialize_finalize',
]

initializer_lists = [
    'fasta',
    'general_tests',
    'ranges_to_dynamic',
    'statics',
    'subarrays',
    'zero_init',
]

lambda = [
    'ct_lambda',
    'ct_lambda2',
    'lambda_in_macro',
    'nested_lambda_def',
    'simple_lambda',
]

literals = [
    'bin_literal',
    'bin_literal2',
    'literal_general',
]

macro_methods = ['macro_method_different_args']

macros = [
    'hash_ident_nested',
    'macro_body_defer',
    'macro_chained_return_void_optional',
    'macro_common',
    'macro_defer_exit',
    'macro_defer_scope',
    'macro_defer_with_body',
    'macro_failable_return_rethrow',
    'macro_import_res_private',
    'macro_nested_labels',
    'macro_typed_varargs',
    'macro_untyped_varargs_2',
    'macro_vasplat',
    'macro_with_body',
    'type_params',
    'unifying_implicit_void',
    'userland_bitcast',
]

methods = [
    'enum_distinct_err_methods',
    'extension_method',
    'extension_method_in_other_modules',
    'methods_with_inferred_type',
]

module = ['module_section_export']
overloading = ['set_overload']

pointers = [
    'array_pointer_decay',
    'const_pointer',
    'pointer_index',
    'subarray_variant_to_ptr',
]

safe = ['deref']

slices = [
    'slice_assign',
    'slice_assign2',
    'slice_comparison',
    'slice_init',
    'slice_offset_neg_end',
    'slice_offset_neg_start',
    'slice_offset',
    'slice_start',
    'slice_to_slice_assign',
    'slice_to_slice_vector_assign',
]

statements = [
    'comparison_widening',
    'const_statements',
    'custom_foreach_with_ref',
    'defer_break_simple',
    'defer_break_switch',
    'defer_break',
    'defer_do_while',
    'defer_in_block',
    'defer_in_defer',
    'defer_in_defer2',
    'defer_next_switch',
    'defer_return',
    'defer_with_loop',
    'exhaustive_switch',
    'fallthough_do',
    'foreach_break',
    'foreach_common',
    'foreach_custom_macro',
    'foreach_custom',
    'foreach_more_implementations',
    'foreach_r_break',
    'foreach_r_common',
    'foreach_r_custom_macro',
    'foreach_r_custom',
    'if_tests',
    'infinite_do_while',
    'labelled_continue_for',
    'nextcase_const',
    'nextcase_default',
    'ranged_switch',
    'return_switch',
    'simple_do',
    'various_switching',
    'while_switch',
]

stdlib = [
    'map_linux',
    'map_macos',
    'memcomp',
    'priorityqueue',
]

strings = [
    'literal_to_subarray',
    'multiline_strings',
    'string_escape',
    'string_len',
    'string_to_array',
]

struct = [
    'nested_struct_init',
    'nested_struct_union_init',
    'simple_struct',
    'sret',
    'struct_as_value',
    'struct_as_value_aarch64',
    'struct_codegen',
    'struct_codegen_empty',
    'struct_codegen_fam',
    'struct_const_construct_simple',
    'struct_pack_and_align',
]

switch = ['switch_in_defer_macro']

union = [
    'union_codegen_const',
    'union_codegen_empty',
    'union_codegen_overwrite_call',
    'union_in_struct',
]

variables = [
    'var_init_multi',
    'var_init',
]

vector = [
    'gather_scatter',
    'vector_bit',
    'vector_consts',
    'vector_incdec',
    'vector_init_regression',
    'vector_init',
    'vector_ops2',
    'vector_param',
    'vector_to_array_cast',
]

visibility = [
    'ambiguous_var',
    'no_shared_imports',
    'not_visible',
    'shared_module',
    'simple_visibility',
]

test_suites = {
    'abi': abi,
    'any': any,
    'arrays': arrays,
    'asm': asm,
    'assert': assert,
    'assignment': assignment,
    'attributes': attributes,
    'bitstruct': bitstruct,
    'builtins': builtins,
    'cast': cast,
    'clang': clang,
    'compile_time': compile_time,
    'compile_time_introspection': compile_time_introspection,
    'concurrency': concurrency,
    'constants': constants,
    'contracts': contracts,
    'debug_symbols': debug_symbols,
    'defer': defer,
    'distinct': distinct,
    'dynamic': dynamic,
    'embed': embed,
    'enumerations': enumerations,
    'errors': errors,
    'expressions': expressions,
    'floats': floats,
    'from_docs': from_docs,
    'functions': functions,
    'generic': generic,
    'globals': globals,
    'import': import,
    'initialize': initialize,
    'initializer_lists': initializer_lists,
    'lambda': lambda,
    'literals': literals,
    'macro_methods': macro_methods,
    'macros': macros,
    'methods': methods,
    'module': module,
    'overloading': overloading,
    'pointers': pointers,
    'safe': safe,
    'slices': slices,
    'statements': statements,
    'stdlib': stdlib,
    'strings': strings,
    'struct': struct,
    'switch': switch,
    'union': union,
    'variables': variables,
    'vector': vector,
    'visibility': visibility,
}

# And generate our tests!
foreach suite, tests : test_suites
    foreach test : tests
        test(
            f'Test Suite - @suite@ - @test@',
            py3,
            args: [test_args, f'../test/test_suite/@suite@/@test@.c3t'],
        )
    endforeach
endforeach
