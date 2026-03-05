{
  lib,
  llvmPackages,
  cmake,
  curl,
  libxml2,
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
    substituteInPlace git_hash.cmake --replace-fail "\''${GIT_HASH}" "${rev}"

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

  cmakeBuildType = if debug then "Debug" else "Release";

  # Only set LLVM_CRT_LIBRARY_DIR for Darwin.
  cmakeFlags = [
    "-DC3_ENABLE_CLANGD_LSP=${if debug then "ON" else "OFF"}"
    "-DC3_LLD_DIR=${llvmPackages.lld.lib}/lib"
  ] ++ lib.optionals llvmPackages.stdenv.hostPlatform.isDarwin [
    "-DLLVM_CRT_LIBRARY_DIR=${llvmPackages.compiler-rt}/lib/darwin"
  ];

  nativeBuildInputs = [
    cmake
    llvmPackages.llvm
    llvmPackages.lld 
    llvmPackages.compiler-rt
  ];

  buildInputs = [
    curl
    libxml2
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

