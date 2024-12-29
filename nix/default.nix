{
  lib,
  llvmPackages,
  cmake,
  python3,
  curl,
  libxml2,
  libffi,
  xar,
  rev ? "unknown",
  debug ? false,
  checks ? false,
}: let 
  inherit (builtins) readFile elemAt;
  # inherit (lib.sources) cleanSourceWith cleanSource; 
  inherit (lib.lists) findFirst;
  inherit (lib.asserts) assertMsg;
  inherit (lib.strings) hasInfix splitString removeSuffix removePrefix optionalString;
in llvmPackages.stdenv.mkDerivation (finalAttrs: {

  pname = "c3c${optionalString debug "-debug"}";

  version = let
    foundLine = findFirst (x: hasInfix "COMPILER_VERSION" x) "none" ( splitString "\n" ( readFile ../src/version.h ) );
  in 
    assert assertMsg (foundLine != "none") "No COMPILER_VERSION substring was found in version.h";
    removeSuffix "\"" ( removePrefix "\"" ( elemAt ( splitString " " foundLine ) 2 ) );

  src = ../.;
  
  cmakeBuildType = if debug then "Debug" else "Release";
  
  postPatch = ''
    substituteInPlace git_hash.cmake \
      --replace-fail "\''${GIT_HASH}" "${rev}"
  '';

  cmakeFlags = [
    "-DC3_ENABLE_CLANGD_LSP=${if debug then "ON" else "OFF"}"
    "-DC3_LLD_DIR=${llvmPackages.lld.lib}/lib"
  ];

  nativeBuildInputs = [ 
    cmake 
    llvmPackages.llvm
    llvmPackages.lld 
  ];

  buildInputs = [
    curl
    libxml2
    libffi
  ] ++ lib.optionals llvmPackages.stdenv.hostPlatform.isDarwin [ xar ];

  nativeCheckInputs = [ python3 ];

  doCheck = llvmPackages.stdenv.system == "x86_64-linux" && checks;

  checkPhase = ''
    runHook preCheck
    ( cd ../resources/testproject; ../../build/c3c build )
    ( cd ../test; python src/tester.py ../build/c3c test_suite )
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

