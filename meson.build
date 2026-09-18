project('c3c', ['c', 'cpp'],
  version: run_command('python3', files('scripts/meson/get_version.py'), check: true).stdout().strip(),
  meson_version: '>= 1.1.0',
  default_options: [
    'c_std=gnu11,c11',
    'cpp_std=gnu++17,c++17',
    'warning_level=1',
    'werror=true',
    'b_vscrt=mt',
  ],
)

# -----------------------------------------------------------------------------
# 0. Global Setup & Tools
# -----------------------------------------------------------------------------
python = import('python').find_installation('python3')
fs = import('fs')

message('Configuring C3C ' + meson.project_version() + ' for ' + host_machine.system())

cc = meson.get_compiler('c')
cpp = meson.get_compiler('cpp')
is_msvc = cc.get_id() == 'msvc' or cc.get_id() == 'clang-cl'
is_darwin = host_machine.system() == 'darwin'
is_windows = host_machine.system() == 'windows'

if not is_msvc
  add_project_arguments('-D_GNU_SOURCE', language: ['c', 'cpp'])
endif

# -----------------------------------------------------------------------------
# 1. Compiler Flags & Options
# -----------------------------------------------------------------------------
c_args = []
cpp_args = []
link_args = []

if is_msvc
  c_args += ['/wd4068', '/wd4090', '/wd4244', '/wd4267', '/Wv:18']
  add_project_arguments('/utf-8', '-D_ITERATOR_DEBUG_LEVEL=0', language: ['c', 'cpp'])
  link_args += ['/STACK:8388608', '/ignore:4099']
else
  add_project_arguments('-fno-exceptions', language: ['c', 'cpp'])

  candidate_warnings = [
    '-Wno-error=#warnings',
    '-Wsign-compare',
    '-Wstrict-prototypes',
    '-Wno-implicit-fallthrough',
    '-Wno-stringop-overflow',
    '-Wno-stringop-overread',
    '-Wno-format-truncation',
    '-Wno-array-bounds',
    '-Wno-unused-command-line-argument',
    '-Wno-unknown-pragmas',
    '-Wno-gnu-auto-type',
    '-Wno-gnu-statement-expression-from-macro-expansion',
    '-Wno-gnu-zero-variadic-macro-arguments',
    '-Wno-unused-result',
    '-Wno-unused-function',
    '-Wno-unknown-attributes',
    '-Wno-unused-variable',
    '-Wno-unused-parameter',
    '-Wno-char-subscripts',
  ]
  c_args += cc.get_supported_arguments(candidate_warnings)

  if get_option('profiling') or get_option('buildtype') != 'release'
    add_project_arguments('-gdwarf-3', language: ['c', 'cpp'])
  endif

  if is_windows
    link_args += ['-Wl,--stack,8388608', '-Wl,--no-fatal-warnings']
  elif is_darwin
    link_args += ['-Wl,-no_warn_duplicate_libraries']
  endif

  if cc.get_id() == 'tcc' # Meson doesn't support tcc yet https://github.com/mesonbuild/meson/issues/15448
    add_project_link_arguments('-Wl,--no-eh-frame-hdr', '-z', 'noexecstack', language: 'c')
    tcc_lib = cc.find_library('tcc1', dirs: [fs.parent(get_option('tcc_lib_path'))], required: false)
    if not tcc_lib.found()
      tcc_lib = declare_dependency(link_args: [get_option('tcc_lib_path')])
    endif
    threads_dep = declare_dependency(dependencies: [threads_dep, tcc_lib])
  endif
endif

# -----------------------------------------------------------------------------
# 2. System Dependencies
# -----------------------------------------------------------------------------
threads_dep = dependency('threads', required: host_machine.system() != 'emscripten')
m_dep = cc.find_library('m', required: false)
dl_dep = cc.find_library('dl', required: false)
xar_dep = is_darwin ? cc.find_library('xar', required: false) : dependency('', required: false)

win_deps = []
if is_windows
  winhttp_lib = cc.find_library('winhttp', required: false)
  win_deps += winhttp_lib.found() ? [winhttp_lib] : [declare_dependency(link_args: ['-lwinhttp'])]
  if is_msvc
    win_deps += [cc.find_library('ntdll', required: true)]
  endif
endif

is_static_build = get_option('default_library') == 'static' or get_option('c_link_args').contains('-static')

curl_dep = dependency('libcurl', static: is_static_build, required: get_option('curl'))
if curl_dep.found()
  c_args += ['-DC3_LINK_CURL']
endif

mimalloc_dep = dependency('', required: false)
if get_option('use_mimalloc')
  mimalloc_dep = dependency('mimalloc', required: true)
endif

# -----------------------------------------------------------------------------
# 3. LLVM & LLD Dependencies
# -----------------------------------------------------------------------------
if get_option('fetch_llvm') and get_option('link_dynamic')
  error('fetch_llvm only provides static archives; cannot be used with link_dynamic=true')
endif

with_llvm = get_option('with_llvm')
c_args += ['-DLLVM_AVAILABLE=' + (with_llvm ? '1' : '0')]

llvm_dep = dependency('', required: false)
lld_libs = []

zstd_dep = dependency('libzstd', static: is_static_build, required: false)
if not zstd_dep.found()
  zstd_dep = cc.find_library('zstd', static: is_static_build, required: false)
endif

xml2_dep = dependency('libxml-2.0', static: is_static_build, required: false)
if not xml2_dep.found()
  xml2_dep = cc.find_library('xml2', static: is_static_build, required: false)
endif

if with_llvm
  lld_search_dirs = []

  if get_option('fetch_llvm')
    fetch_cmd = [
      python, files('scripts/meson/fetch_llvm.py'),
      '--system', host_machine.system(),
      '--cpu', host_machine.cpu_family(),
      '--buildtype', get_option('buildtype'),
      '--tag', get_option('llvm_tag'),
      '--rt-dest', meson.project_build_root() / 'c3c_rt',
    ]
    if is_static_build
      fetch_cmd += ['--static']
    endif
    fetch_res = run_command(fetch_cmd, check: false)
    if fetch_res.returncode() != 0
      error('fetch_llvm failed:\nSTDOUT:\n' + fetch_res.stdout() + '\nSTDERR:\n' + fetch_res.stderr())
    endif
    llvm_prefix = fetch_res.stdout().strip()
    message('Using external LLVM prefix: ' + llvm_prefix)

    llvm_config_h = fs.read(llvm_prefix / 'include' / 'llvm' / 'Config' / 'llvm-config.h')
    foreach line : llvm_config_h.split('\n')
      if line.startswith('#define LLVM_VERSION_STRING')
        message('Found fetched LLVM version: ' + line.split()[2].strip('"'))
        break
      endif
    endforeach

    fetched_libdir = llvm_prefix / 'lib'
    lld_search_dirs += [fetched_libdir]

    raw_flags = fs.read(llvm_prefix / 'llvm_link_flags.txt').strip().split()
    llvm_link_args = ['-L' + fetched_libdir]
    if host_machine.system() != 'darwin' and host_machine.system() != 'windows'
      llvm_link_args += ['-Wl,--start-group'] + raw_flags + ['-Wl,--end-group']
    else
      llvm_link_args += raw_flags
    endif

    llvm_dep = declare_dependency(
      dependencies: [threads_dep, m_dep, dl_dep, zstd_dep, xml2_dep],
      include_directories: include_directories(llvm_prefix / 'include'),
      link_args: llvm_link_args,
    )
  else
    llvm_modules = [
      'all-targets', 'analysis', 'asmprinter', 'bitreader', 'core', 'debuginfopdb',
      'instcombine', 'irreader', 'libdriver', 'linker', 'lto', 'mc', 'mcdisassembler',
      'native', 'nativecodegen', 'object', 'option', 'scalaropts', 'support',
      'target', 'transformutils', 'windowsmanifest', 'windowsdriver',
    ]

    prefer_static = not get_option('link_dynamic')
    llvm_dep = dependency(
      'llvm',
      version: ['>=19', '<=24'],
      modules: llvm_modules,
      optional_modules: ['dtlto'],
      static: prefer_static,
      method: 'config-tool',
      required: false,
    )

    if not llvm_dep.found() and prefer_static
      message('Static LLVM not found; falling back to shared LLVM...')
      llvm_dep = dependency(
        'llvm',
        version: ['>=19', '<=24'],
        modules: llvm_modules,
        static: false,
        method: 'config-tool',
        required: true,
      )
    endif

    llvm_libdir = llvm_dep.get_variable(configtool: 'libdir', default_value: '')
    if llvm_libdir != ''
      lld_search_dirs += [llvm_libdir]
    endif

    if zstd_dep.found()
      llvm_dep = declare_dependency(dependencies: [llvm_dep, zstd_dep])
    endif
  endif

  custom_lld_dir = get_option('lld_dir')
  if custom_lld_dir != ''
    lld_search_dirs = [custom_lld_dir] + lld_search_dirs
  endif

  custom_lld_inc = get_option('lld_include_dir')
  if custom_lld_inc != ''
    llvm_dep = declare_dependency(dependencies: [llvm_dep], include_directories: include_directories(custom_lld_inc))
  endif

  lld_names = ['lldCOFF', 'lldCommon', 'lldELF', 'lldMinGW', 'lldWasm']
  if host_machine.system() != 'openbsd'
    lld_names += ['lldMachO']
  endif

  foreach name : lld_names
    lib = cpp.find_library(name, dirs: lld_search_dirs, static: not get_option('link_dynamic'), required: false)
    if not lib.found()
      lib = cpp.find_library(name, dirs: lld_search_dirs, required: true)
    endif
    lld_libs += [lib]
  endforeach
endif

if get_option('avr_disable')
  c_args += ['-DAVR_DISABLE']
endif

if host_machine.system() == 'emscripten'
  c_args += ['-DARM_DISABLE', '-DAARCH64_DISABLE', '-DRISCV_DISABLE', '-DX86_DISABLE']
endif

# -----------------------------------------------------------------------------
# 4. Embedded Generators & Post-Configuration
# -----------------------------------------------------------------------------
git_hash_h = vcs_tag(
  input: 'scripts/meson/git_hash.h.in',
  output: 'git_hash.h',
  command: ['git', 'rev-parse', 'HEAD'],
  fallback: 'unknown',
)

docs_template_h = custom_target(
  'docs_template.h',
  input: 'resources/docs.html',
  output: 'docs_template.h',
  command: [python, files('scripts/meson/embed.py'), '@INPUT@', '@OUTPUT@', 'docs_html'],
)

meson.add_postconf_script(
  python, files('scripts/meson/copy_stdlib.py'),
  meson.project_source_root() / 'lib',
  meson.project_build_root() / 'lib',
)

# -----------------------------------------------------------------------------
# 5. Internal Libraries
# -----------------------------------------------------------------------------
miniz_c_args = []
if cc.get_id() == 'gcc'
  miniz_c_args += ['-fno-analyzer']
endif

miniz_lib = static_library(
  'miniz',
  sources: ['dependencies/miniz/miniz.c', 'dependencies/miniz/zlib_compat.c'],
  include_directories: include_directories('dependencies/miniz'),
  override_options: ['warning_level=0', 'werror=false'],
  c_args: miniz_c_args,
)
miniz_dep = declare_dependency(
  link_with: miniz_lib,
  include_directories: include_directories('dependencies/miniz'),
)

xz_lib = static_library(
  'xz_embedded',
  sources: ['dependencies/xz/standalone.c'],
  include_directories: include_directories('dependencies/xz'),
  override_options: ['warning_level=0', 'werror=false'],
)
xz_dep = declare_dependency(
  link_with: xz_lib,
  include_directories: include_directories('dependencies/xz'),
)

c3c_wrappers_dep = declare_dependency()
if with_llvm
  wrapper_cpp_args = is_msvc ? cpp_args : cpp_args + ['-fno-rtti']

  c3c_wrappers_lib = static_library(
    'c3c_wrappers',
    sources: ['wrapper/src/wrapper.cpp'],
    include_directories: include_directories('wrapper/include'),
    dependencies: [llvm_dep] + lld_libs,
    override_options: ['warning_level=0', 'werror=false'],
    cpp_args: wrapper_cpp_args,
  )
  c3c_wrappers_dep = declare_dependency(
    link_with: c3c_wrappers_lib,
    include_directories: include_directories('wrapper/include'),
    dependencies: [llvm_dep] + lld_libs,
  )
endif

# -----------------------------------------------------------------------------
# 6. Main Executable Sources
# -----------------------------------------------------------------------------
c3c_sources = files(
  'src/build/builder.c',
  'src/build/build_options.c',
  'src/build/project_creation.c',
  'src/build/project_manipulation.c',
  'src/build/libraries.c',
  'src/build/project.c',
  'src/build/common_build.c',
  'src/compiler/ast.c',
  'src/compiler/bigint.c',
  'src/compiler/codegen_general.c',
  'src/compiler/compiler.c',
  'src/compiler/subprocess.c',
  'src/compiler/context.c',
  'src/compiler/copying.c',
  'src/compiler/darwin_support.c',
  'src/compiler/diagnostics.c',
  'src/compiler/float.c',
  'src/compiler/headers.c',
  'src/compiler/json_output.c',
  'src/compiler/docgen.c',
  'src/compiler/lexer.c',
  'src/compiler/linker.c',
  'src/compiler/abi/c_abi_aarch64.c',
  'src/compiler/abi/c_abi.c',
  'src/compiler/abi/c_abi_riscv.c',
  'src/compiler/abi/c_abi_wasm.c',
  'src/compiler/abi/c_abi_win64.c',
  'src/compiler/abi/c_abi_x64.c',
  'src/compiler/abi/c_abi_x86.c',
  'src/compiler/module.c',
  'src/compiler/number.c',
  'src/compiler/parse_expr.c',
  'src/compiler/parse_global.c',
  'src/compiler/parser.c',
  'src/compiler/parse_stmt.c',
  'src/compiler/sema_casts.c',
  'src/compiler/sema_decls.c',
  'src/compiler/sema_expr.c',
  'src/compiler/sema_generics.c',
  'src/compiler/sema_name_resolution.c',
  'src/compiler/sema_errors.c',
  'src/compiler/sema_builtins.c',
  'src/compiler/sema_initializers.c',
  'src/compiler/semantic_analyser.c',
  'src/compiler/sema_passes.c',
  'src/compiler/sema_stmts.c',
  'src/compiler/sema_types.c',
  'src/compiler/source_file.c',
  'src/compiler/symtab.c',
  'src/compiler/target.c',
  'src/compiler/sema_asm.c',
  'src/compiler_tests/benchmark.c',
  'src/compiler_tests/tests.c',
  'src/compiler/tokens.c',
  'src/compiler/types.c',
  'src/main.c',
  'src/utils/errors.c',
  'src/utils/file_utils.c',
  'src/utils/find_msvc.c',
  'src/utils/malloc.c',
  'src/utils/stringutils.c',
  'src/utils/taskqueue.c',
  'src/utils/json.c',
  'src/utils/vmem.c',
  'src/utils/whereami.c',
  'src/utils/cpus.c',
  'src/utils/unzipper.c',
  'src/utils/msi.c',
  'src/compiler/c_codegen.c',
  'src/compiler/decltable.c',
  'src/compiler/methodtable.c',
  'src/utils/fetch_sdk/fetch_sdk.c',
  'src/utils/fetch_sdk/fetch_utils.c',
  'src/utils/fetch_sdk/windows_sdk.c',
  'src/utils/fetch_sdk/macos_sdk.c',
  'src/utils/fetch_sdk/android_ndk.c',
  'src/compiler/windows_support.c',
  'src/compiler/codegen_asm.c',
  'src/compiler/asm_target.c',
  'src/compiler/expr.c',
  'src/utils/time.c',
  'src/utils/http.c',
  'src/compiler/sema_liveness.c',
  'src/compiler/sema_const.c',
  'src/utils/xar.c',
  'src/utils/pbzx.c',
  'src/utils/xz_stream.c',
  'src/utils/cpio.c',
  'src/utils/byte_buffer.c',
  'src/utils/sucatalog.c',
  'src/utils/symlink.c',
)

if with_llvm
  c3c_sources += files(
    'src/compiler/llvm_codegen.c',
    'src/compiler/llvm_codegen_debug_info.c',
    'src/compiler/llvm_codegen_expr.c',
    'src/compiler/llvm_codegen_function.c',
    'src/compiler/llvm_codegen_instr.c',
    'src/compiler/llvm_codegen_module.c',
    'src/compiler/llvm_codegen_stmt.c',
    'src/compiler/llvm_codegen_type.c',
    'src/compiler/llvm_codegen_value.c',
    'src/compiler/llvm_codegen_storeload.c',
    'src/compiler/llvm_codegen_builtins.c',
  )
else
  c3c_sources += files('src/utils/hostinfo.c')
endif

# -----------------------------------------------------------------------------
# 7. Executable Target & Installation
# -----------------------------------------------------------------------------
c3c_exe = executable(
  'c3c',
  c3c_sources,
  git_hash_h,
  docs_template_h,
  include_directories: include_directories('src', '.'),
  dependencies: [
    threads_dep,
    m_dep,
    dl_dep,
    xar_dep,
    curl_dep,
    mimalloc_dep,
    miniz_dep,
    xz_dep,
    c3c_wrappers_dep,
  ] + win_deps,
  c_args: c_args,
  link_args: link_args,
  install: true,
)

install_subdir('lib', install_dir: get_option('libdir') / 'c3', strip_directory: true)

rt_dir = meson.project_build_root() / 'c3c_rt'
if fs.is_dir(rt_dir)
  install_subdir(rt_dir, install_dir: get_option('bindir'))
endif

if not is_windows
  install_man('c3c.1')
endif

run_target('package',
  command: [
    python, files('scripts/meson/package.py'),
    '--bin', c3c_exe.full_path(),
    '--out', 'c3c-' + meson.project_version() + '-' + host_machine.system() + '-' + host_machine.cpu_family(),
    '--format', host_machine.system() == 'windows' ? 'zip' : 'tar',
  ],
  depends: [c3c_exe],
)