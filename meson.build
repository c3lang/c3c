project(
    'c3c',
    'c',
    'cpp',
    version: '0.5.2',
    meson_version: '>=1.1.0', # meson.options
    default_options: [
        'c_std=gnu11,c11', # Prefer gnu11, fall back to c11 for msvc
        'cpp_std=c++17',
        'warning_level=2',
        'buildtype=debugoptimized',
    ],
)

fs = import('fs')

cc = meson.get_compiler('c')
cpp = meson.get_compiler('cpp')

if get_option('hide_unused_function_warning')
    if cc.get_id() == 'msvc'
        add_project_arguments('/wd4505', language: ['c', 'cpp']) # Disable warning C4505: unreferenced local function has been removed
    else
        add_project_arguments('-Wno-unused-function', language: ['c', 'cpp'])
    endif
endif

if get_option('hide_unused_variable_warning')
    if cc.get_id() == 'msvc'
        add_project_arguments('/wd4101', language: ['c', 'cpp']) # Disable warning C4101: unreferenced local variable
    else
        add_project_arguments('-Wno-unused-variable', language: ['c', 'cpp'])
    endif
endif

if get_option('hide_unknown_pragmas')
    # or just set CFLAGS
    if cc.get_id() == 'msvc'
        add_project_arguments('/wd4068', language: 'c')
    else
        add_project_arguments('-Wno-unknown-pragmas', language: 'c')
    endif
endif

if host_machine.system() == 'windows' and cc.get_id() == 'gcc'
    # Assuming MinGW/MSYS is used on Windows with GCC
    message('Increase stack for msys')
    add_project_link_arguments('-Wl,--stack,8388608', language: 'c') # I don't think cpp is requried
endif

wrapper_link_args = []
if cc.get_id() == 'msvc'
    add_project_arguments('/utf-8', language: ['c', 'cpp'])
    # 4090: different 'const' qualifiers,
    add_project_arguments('/wd4090', '/Wv:18', language: 'c')
    # /wd4624: Disable warning for destructor was implicitly defined as deleted.
    # /wd4267: Disable warning for conversion from 'size_t' to 'type', possible loss of data.
    # /wd4244: Disable warning for conversion from 'type1' to 'type2', possible loss of data.
    add_project_arguments('/wd4624', '/wd4267', '/wd4244', '/Wv:18', language: 'cpp')
    wrapper_link_args += ['/ignore:4099'] # Ignore LNK4099 warning (pdb not found).
    # /MT is the default, so we only really need to worry about handling /MTd when we want to build in debug mode
    if get_option('buildtype') == 'debug'
        add_project_arguments('/MTd', language: ['c', 'cpp']) # We can apply this to cpp even if we aren't building wrapper, it won't hurt
    endif
else
    add_project_arguments('-fno-exceptions', language: ['c', 'cpp'])
endif

# if not cc.get_define('_DEFAULT_SOURCE')
#     # We probably need to enable the appropriate FTM for glibc
#     # should be covered by gnu11, but just in case
#     add_project_arguments('-D_DEFAULT_SOURCE', language: 'c')
# endif

# if not cc.has_function('strdup') and not cc.get_define('_DEFAULT_SOURCE')
#     # _DEFAULT_SOURCE should cover strdup, but maybe that check didn't trigger?
#     add_project_arguments('-D_DEFAULT_SOURCE', language: 'c')
# endif

c3c_deps = []
c3c_link_with = []

# Default range in meson.options
llvm_version_spec = get_option('llvm_version') # Enable users to specify a version of LLVM
llvm_dep = dependency('llvm', required: get_option('with_llvm'), version: llvm_version_spec)
if llvm_dep.found()
    add_project_arguments('-DLLVM_AVAILABLE=1', language: 'c')
    llvm_bin_dir = llvm_dep.get_variable(configtool: 'bindir', cmake: 'LLVM_BINARY_DIR')
    llvm_lib_dir = llvm_dep.get_variable(configtool: 'libdir', cmake: 'LLVM_LIBRARY_DIR')
    llvm_rtti = llvm_dep.get_variable('has-rtti', cmake: 'LLVM_ENABLE_RTTI')
    if not ['ON', 'YES'].contains(llvm_rtti)
        # This should cover both cmake and llvm-config cases
        if cc.get_id() == 'msvc'
            add_project_arguments('/GR-', language: 'c')
        else
            add_project_arguments('-fno-rtti', language: 'c')
        endif
        message('LLVM was built without RTTI, disabling RTTI for c3c')
    endif

    # I thought we would automagically get these via llvm_dep but it seems not based on my testing
    # in CI. Let's be explicit. (LLVM really needs to adopt pkg-config...)
    _llvm_potential_deps = ['libedit', 'libxml2', 'zlib', 'zstd']
    foreach dep : _llvm_potential_deps
        cmake_var='LLVM_ENABLE_' + dep.to_upper()
        dep_var = llvm_dep.get_variable(cmake: cmake_var, default_value: 'OFF')
        if ['ON', 'TRUE', 'YES', '1'].contains(dep_var)
            real_dep = dep
            if dep == 'libxml2'
                # libxml2 is a bit special
                real_dep = 'libxml-2.0'
            elif dep == 'zstd'
                # so is zstd
                real_dep = 'libzstd'
            endif
            this_dep = dependency(real_dep, required: false)
            if this_dep.found()
                c3c_deps += this_dep
            else
                error('LLVM claims to require ' + dep + ' but it was not found.')
            endif
        endif
    endforeach

    llvm_majver = llvm_dep.version().split('.')[0]
    # c3c_rt is a directory in the build directory and contains the sanitizer runtime libraries (asan, tsan, etc.)
    if host_machine.system() == 'darwin'
        message(f'llvm bindir: @llvm_bin_dir@')
        install_name_tool = find_program(llvm_bin_dir / 'llvm-install-name-tool')
        clang_lib_dir = llvm_lib_dir + f'/clang/@llvm_majver@/lib/darwin'
        rt_libs = [
            'libclang_rt.asan_osx_dynamic.dylib',
            'libclang_rt.tsan_osx_dynamic.dylib',
        ] # ubsan and lsan are unused on darwin.
    elif host_machine.system() == 'windows' and cc.get_id() == 'msvc' # We don't want to do this on mingw
        clang_lib_dir = llvm_lib_dir / f'clang/@llvm_majver@/lib/windows'
        rt_libs = [
            'clang_rt.asan-x86_64.lib',
            'clang_rt.asan_dynamic-x86_64.lib',
            'clang_rt.asan_dynamic-x86_64.dll',
            'clang_rt.asan_dynamic_runtime_thunk-x86_64.lib',
        ]
    endif
    if is_variable('rt_libs')
        # only on platforms that need it
        foreach lib : rt_libs
            output_rt_lib = fs.copyfile(
                clang_lib_dir / lib,
                meson.current_build_dir() / 'c3c_rt' / lib,
            )
            if host_machine.system() == 'darwin'
                # Change LC_ID_DYLIB to be rpath-based instead of having an absolute path
                run_command(
                    install_name_tool,
                    '-id', f'@rpath/c3c_rt/@lib@',
                    output_rt_lib,
                )
            endif
        endforeach
    endif
    lld_common_lib = cc.find_library('lldCommon', dirs: llvm_lib_dir, required: true)
    lld_elf_lib = cc.find_library('lldELF', dirs: llvm_lib_dir, required: true)
    lld_coff_lib = cc.find_library('lldCOFF', dirs: llvm_lib_dir, required: true)
    lld_wasm_lib = cc.find_library('lldWasm', dirs: llvm_lib_dir, required: true)
    lld_macho_lib = cc.find_library('lldMachO', dirs: llvm_lib_dir, required: true)

    c3c_deps += [
        llvm_dep,
        lld_common_lib,
        lld_elf_lib,
        lld_coff_lib,
        lld_wasm_lib,
        lld_macho_lib,
    ]

    if host_machine.system() == 'darwin'
        xar_dep = dependency('xar', required: true) # eXtensible ARchiver for Apple's .pkg (libraries)
        # TODO: This is available on linux and may be useful for cross-compiling to darwin?
        c3c_deps += xar_dep
    endif
else
    # TODO: In the old CMake script, Windows used to be able to fetch LLVM from the internet. Not implemented here but maybe a wrapdb thing could serve that purpose.
    add_project_arguments('-DLLVM_AVAILABLE=0', language: 'c') # a lot of `#if` used, so let's be safe.
endif

m_dep = cc.find_library('m', required: false) # Required on some platforms (e.g. linux), but not all!
if m_dep.found()
    c3c_deps += m_dep
endif

if host_machine.system() == 'windows'
    add_project_arguments('-DPLATFORM_WINDOWS=1', language: 'c') # is this set implicitly in cmake?
    winhttp_lib = cc.find_library('winhttp', required: true)
    c3c_deps += winhttp_lib
else
    thread_dep = dependency('threads')
    if thread_dep.found()
        # chances are pretty low that we'll be on a system without threads, but just in case
        c3c_deps += thread_dep
    endif
    curl_dep = dependency('libcurl', required: get_option('with_libcurl'))
    if curl_dep.found()
        c3c_deps += curl_dep
        add_project_arguments('-DCURL_FOUND=1', language: 'c')
    endif
endif

compiler_src = files(
    'src/build/build_options.c',
    'src/build/builder.c',
    'src/build/common_build.c',
    'src/build/libraries.c',
    'src/build/project.c',
    'src/build/project_creation.c',
    'src/build/project_manipulation.c',
    'src/compiler/abi/c_abi.c',
    'src/compiler/abi/c_abi_aarch64.c',
    'src/compiler/abi/c_abi_riscv.c',
    'src/compiler/abi/c_abi_wasm.c',
    'src/compiler/abi/c_abi_win64.c',
    'src/compiler/abi/c_abi_x64.c',
    'src/compiler/abi/c_abi_x86.c',
    'src/compiler/asm_target.c',
    'src/compiler/ast.c',
    'src/compiler/bigint.c',
    'src/compiler/c_codegen.c',
    'src/compiler/codegen_asm.c',
    'src/compiler/codegen_general.c',
    'src/compiler/compiler.c',
    'src/compiler/context.c',
    'src/compiler/copying.c',
    'src/compiler/decltable.c',
    'src/compiler/diagnostics.c',
    'src/compiler/expr.c',
    'src/compiler/float.c',
    'src/compiler/headers.c',
    'src/compiler/json_output.c',
    'src/compiler/lexer.c',
    'src/compiler/linker.c',
    'src/compiler/mac_support.c',
    'src/compiler/module.c',
    'src/compiler/number.c',
    'src/compiler/parse_expr.c',
    'src/compiler/parse_global.c',
    'src/compiler/parse_stmt.c',
    'src/compiler/parser.c',
    'src/compiler/sema_asm.c',
    'src/compiler/sema_builtins.c',
    'src/compiler/sema_casts.c',
    'src/compiler/sema_const.c',
    'src/compiler/sema_decls.c',
    'src/compiler/sema_errors.c',
    'src/compiler/sema_expr.c',
    'src/compiler/sema_initializers.c',
    'src/compiler/sema_liveness.c',
    'src/compiler/sema_name_resolution.c',
    'src/compiler/sema_passes.c',
    'src/compiler/sema_stmts.c',
    'src/compiler/sema_types.c',
    'src/compiler/semantic_analyser.c',
    'src/compiler/source_file.c',
    'src/compiler/subprocess.c',
    'src/compiler/symtab.c',
    'src/compiler/target.c',
    'src/compiler/tokens.c',
    'src/compiler/types.c',
    'src/compiler/windows_support.c',
)

if llvm_dep.found()
    compiler_src += files(
        'src/compiler/llvm_codegen.c',
        'src/compiler/llvm_codegen_builtins.c',
        'src/compiler/llvm_codegen_debug_info.c',
        'src/compiler/llvm_codegen_expr.c',
        'src/compiler/llvm_codegen_function.c',
        'src/compiler/llvm_codegen_instr.c',
        'src/compiler/llvm_codegen_module.c',
        'src/compiler/llvm_codegen_stmt.c',
        'src/compiler/llvm_codegen_storeload.c',
        'src/compiler/llvm_codegen_type.c',
        'src/compiler/llvm_codegen_value.c',
    )
endif

compiler_benchmark_src = files(
    'src/compiler_tests/benchmark.c',
    'src/compiler_tests/shorttest.c',
    'src/compiler_tests/tests.c',
)

utils_src = files(
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
    'src/utils/whereami.c',
)

git_hash = vcs_tag(
    fallback: meson.project_version(),
    input: 'git_hash.h.in',
    output: 'git_hash.h',
)

c3c_include = ['src/']

c3c_src = [
    'src/main.c',
    utils_src,
    compiler_src,
    compiler_benchmark_src,
    git_hash,
]

miniz_lib = static_library(
    'miniz',
    'dependencies/miniz/miniz.c',
    include_directories: 'dependencies/miniz',
)
c3c_include += ['dependencies/miniz/']
c3c_link_with += [miniz_lib]

if llvm_dep.found()
    wrapper_lib = static_library(
        'wrapper',
        'wrapper/src/wrapper.cpp',
        include_directories: ['wrapper/include'],
        dependencies: llvm_dep,
        link_args: wrapper_link_args,
    )
    c3c_link_with += [wrapper_lib]
    c3c_include += ['wrapper/include']
else
    c3c_src += ['src/utils/hostinfo.c']
endif

c3c_exe = executable(
    'c3c',
    c3c_src,
    dependencies: c3c_deps,
    include_directories: c3c_include,
    link_with: c3c_link_with,
    install: true,
)

install_subdir('lib/std', install_dir: 'lib/c3')

if target_machine.system() != 'windows'
    install_man('c3c.1')
endif

subdir('test')

summary(
    {
        'prefix': get_option('prefix'),
        'bindir': get_option('bindir'),
        'datadir': get_option('datadir'),
        'libdir': get_option('libdir'),
    },
    section: 'Directories',
)

environment_summary = {
    'Build': build_machine.system(),
    'Build CPU Family': build_machine.cpu_family(),
    'Host CPU Family': host_machine.cpu_family(),
    'Cross-compiling': meson.is_cross_build(),
    'Build Endianness': build_machine.endian(),
    'Host Endianness': host_machine.endian(),
    'Target': host_machine.system(),
    'C Compiler': cc.get_id(),
    'C Compiler Version': cc.version(),
    'C++ Compiler': cpp.get_id(),
    'C++ Compiler Version': cpp.version(),
    'Linker': cc.get_linker_id(),
    'Python Path': py3.full_path(),
    'Python Version': py3.version(),
}

summary(
    environment_summary,
    section: 'Environment',
)

dependency_summary = {
    'LLVM': llvm_dep.found() ? llvm_dep : false,
}

if m_dep.found()
    dependency_summary += {'Maths Library': m_dep}
endif

if host_machine.system() != 'windows'
    if curl_dep.found()
        dependency_summary += {'cURL': curl_dep}
    endif
    if thread_dep.found()
        dependency_summary += {'Threads Library': thread_dep}
    endif
endif

summary(
    dependency_summary,
    bool_yn: true,
    section: 'Dependencies',
)
