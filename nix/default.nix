{
  lib,
  llvmPackages,
  cmake,
  python3,
  curl,
  libxml2,
  libffi,
  xar,
  debug ? false,
  checks ? true,
}: let 
  inherit (builtins) baseNameOf toString readFile elemAt;
  inherit (lib.sources) cleanSourceWith cleanSource; 
  inherit (lib.lists) findFirst;
  inherit (lib.asserts) assertMsg;
  inherit (lib.strings) hasInfix hasSuffix splitString removeSuffix removePrefix optionalString;
in 
llvmPackages.stdenv.mkDerivation (finalAttrs: {
  pname = "c3c${optionalString debug "-debug"}";
  version = let
    findLine = findFirst (x: hasInfix "COMPILER_VERSION" x) "none";
    foundLine = findLine ( splitString "\n" ( readFile ../src/version.h ) );
    version = removeSuffix "\"" ( removePrefix "\"" ( elemAt ( splitString " " foundLine ) 2 ) );
  in 
    assert assertMsg (foundLine != "none") "No COMPILER_VERSION substring was found in version.h";
    version;

  src = cleanSourceWith {
    filter = _path: _type: !(hasSuffix ".nix" (baseNameOf(toString _path)));
    src = cleanSource ../.;
  };

  postPatch = ''
    substituteInPlace CMakeLists.txt \
      --replace-fail "\''${LLVM_LIBRARY_DIRS}" "${llvmPackages.lld.lib}/lib ${llvmPackages.llvm.lib}/lib"
  '';

  cmakeBuildType = if debug then "Debug" else "Release";

  cmakeFlags = [
    "-DC3_ENABLE_CLANGD_LSP=${if debug then "ON" else "OFF"}"
  ];

  nativeBuildInputs = [ cmake ];

  postBuild = optionalString debug ''
    mkdir $out
    substituteInPlace compile_commands.json \
      --replace "/build/source/" "$src/"
    cp compile_commands.json $out/compile_commands.json
  '';

  buildInputs = [
    llvmPackages.llvm
    llvmPackages.lld
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
    ];
    platforms = platforms.all;
    mainProgram = "c3c";
  };
})

