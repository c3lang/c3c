{
  lib,
  llvmPackages,
  meson,
  ninja,
  pkg-config,
  python3,
  curl,
  libxml2,
  zstd,
  libffi,
  xar,
  rev,
  debug ? false,
  checks ? false,
}: let
  inherit (builtins) readFile elemAt;
  # inherit (lib.sources) cleanSourceWith cleanSource; 
  inherit (lib.lists) findFirst;
  inherit (lib.asserts) assertMsg;
  inherit (lib.strings) hasInfix splitString removeSuffix removePrefix optionalString;
in llvmPackages.stdenv.mkDerivation (_:
{
  pname = "c3c${optionalString debug "-debug"}";

  version = let
    foundLine = findFirst (x: hasInfix "COMPILER_VERSION" x) "none" ( splitString "\n" ( readFile ../src/version.h ) );
  in 
    assert assertMsg (foundLine != "none") "No COMPILER_VERSION substring was found in version.h";
    removeSuffix "\"" ( removePrefix "\"" ( elemAt ( splitString " " foundLine ) 2 ) );

  src = ../.;
 
  # See https://github.com/symphorien/nixseparatedebuginfod for usage
  separateDebugInfo = true;

  # Here we substitute GIT_HASH which is not set for cmake in nix builds.
  # Similar situation is with __DATE__ and __TIME__ macros, which are
  # set to "Jan 01 1980 00:00:00" by default.
  postPatch = ''
    substituteInPlace scripts/meson/git_hash.h.in --replace-fail "@VCS_TAG@" "${rev}"

    local FILE_NAMES="$(find src -type f)"
    substituteInPlace $FILE_NAMES --replace-quiet "__DATE__" "\"$(date '+%b %d %Y')\""
    substituteInPlace $FILE_NAMES --replace-quiet "__TIME__" "\"$(date '+%T')\""

    patchShebangs scripts/tools/ci_tests.sh

    # Skip library tests (dynlib/staticlib).
    substituteInPlace scripts/tools/ci_tests.sh \
      --replace-fail "run_dynlib_tests() {" "run_dynlib_tests() { return 0;" \
      --replace-fail "run_staticlib_tests() {" "run_staticlib_tests() { return 0;"

    # Remove '--linker=builtin' from run_testproject so it uses the working system linker.
    substituteInPlace scripts/tools/ci_tests.sh \
      --replace-fail 'ARGS="$ARGS --linker=builtin"' 'ARGS="$ARGS"'
  '';

  mesonBuildType = if debug then "debug" else "release";

  hardeningDisable = lib.optional debug "fortify";

  mesonFlags = [
    "-Dlld_dir=${llvmPackages.lld.lib}/lib"
    "-Dlld_include_dir=${llvmPackages.lld.dev}/include"
  ];

  nativeBuildInputs = [
    meson
    ninja
    pkg-config
    python3
    llvmPackages.llvm
    llvmPackages.lld 
    llvmPackages.compiler-rt
  ];

  buildInputs = [
    curl
    libxml2
    zstd
    libffi
  ] ++ lib.optionals llvmPackages.stdenv.hostPlatform.isDarwin [ xar ];

  doCheck = checks && lib.elem llvmPackages.stdenv.system [
    "x86_64-linux"
    "x86_64-darwin"
    "aarch64-darwin"
  ];

  # In check phase we preserve BUILD directory as
  # we need to return to it before install phase
  checkPhase = ''
    runHook preCheck
    local BUILD_DIR=$(pwd)

    export SKIP_NETWORK_TESTS=1
    ../scripts/tools/ci_tests.sh $(pwd)/c3c
    
    cd $BUILD_DIR
    runHook postCheck
  '';

  meta = with lib; {
    description = "Compiler for the C3 language";
    homepage = "https://github.com/c3lang/c3c";
    license = licenses.lgpl3Only;
    maintainers = with maintainers; [
      luc65r
      anas
      vssukharev
    ];
    platforms = platforms.all;
    mainProgram = "c3c";
  };
})

